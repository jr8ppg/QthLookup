{
  Q T H L O O K U P

  COPYRIGHT (c) 2022 JR8PPG

  このサービス（ソフトウェア）は、総務省 電波利用ホームページのWeb-API 機能を
  利用して取得した情報をもとに作成しているが、サービスの内容は総務省によって
  保証されたものではありません。

  This software is released under the MIT License.
}
unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JSON, Math, System.UITypes, System.IniFiles, System.StrUtils,
  Vcl.ExtCtrls, Vcl.ComCtrls, WinApi.CommCtrl, Vcl.Menus, System.Generics.Collections,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  Jccg, LicenseInfo;

const
  WM_LOAD_STALIST = (WM_USER + 100);

type
  TForm1 = class(TForm)
    editCallsign: TEdit;
    buttonQuery: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    checkZlog: TCheckBox;
    checkAround: TCheckBox;
    ListView1: TListView;
    timerLogSync: TTimer;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    F1: TMenuItem;
    menuOpen: TMenuItem;
    N1: TMenuItem;
    menuSaveAs: TMenuItem;
    menuZlogSpc: TMenuItem;
    N3: TMenuItem;
    menuExit: TMenuItem;
    OpenFileDialog: TOpenDialog;
    SaveFileDialog: TSaveDialog;
    SaveFileDialog2: TSaveDialog;
    SaveFileDialog3: TSaveDialog;
    checkNoSameAddress: TCheckBox;
    menuNotice: TMenuItem;
    menuOffline: TMenuItem;
    N4: TMenuItem;
    NetHTTPClient1: TNetHTTPClient;
    NetHTTPRequest1: TNetHTTPRequest;
    procedure buttonQueryClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure timerLogSyncTimer(Sender: TObject);
    procedure checkZlogClick(Sender: TObject);
    procedure editCallsignEnter(Sender: TObject);
    procedure editCallsignExit(Sender: TObject);
    procedure checkAroundClick(Sender: TObject);
    procedure ListView1AdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure menuExitClick(Sender: TObject);
    procedure menuZlogSpcClick(Sender: TObject);
    procedure menuSaveAsClick(Sender: TObject);
    procedure menuOpenClick(Sender: TObject);
    procedure menuNoticeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LoadStationList(var Message: TMessage); message WM_LOAD_STALIST;
    function GetStationListName(): string;
    procedure menuOfflineClick(Sender: TObject);
  private
    { Private 宣言 }
    m_hLoggerWnd: THandle;
    m_strPrevCallsign: string;
    m_jccglist: TJccgList;
    m_strLastSaveFolder: string;
    m_strLastOpenFolder: string;
    m_strLastFileName: string;
    m_radiolist: TLicenseInfoList;
    m_zLogV28: Boolean;
    procedure DoLookup1(strCallsign: string; fShowDialog: Boolean; fCheckAround: Boolean; fOffline: Boolean);
    function LicenseLookup(strCallsign: string; fCheckAround: Boolean; var strError: string): TLicenseInfoList;
    function LicenseLookupOffline(strCallsign: string; fCheckAround: Boolean; var strError: string): TLicenseInfoList;
    function FindZlogWindow(): HWND;
    function GetCallsign(strCallsign: string): string;
    procedure SetCaption();
  public
    { Public 宣言 }
  end;

var
  Form1: TForm1;

const
  query_string = 'https://www.tele.soumu.go.jp/musen/list?ST=1&DA=1&SC=1&DC=1&OF=2&OW=AT';

implementation

uses
   Progress, SelectZlog, ImpotantNotice;

{$R *.dfm}

// ----------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
var
   ini: TIniFile;
   x, y: Integer;
begin
   m_jccglist := TJccgList.Create();
   m_jccglist.LoadFromResourceName(SysInit.HInstance, 'ID_JCCGLIST');
   m_hLoggerWnd := 0;
   m_strPrevCallsign := '';
   m_strLastFileName := '';
   m_strLastOpenFolder := '';
   m_zLogV28 := False;

   m_radiolist := TLicenseInfoList.Create();
   m_radiolist.JccgList := m_jccglist;

   ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
   try
      x := ini.ReadInteger('SETTINGS', 'X', -1);
      y := ini.ReadInteger('SETTINGS', 'Y', -1);
      if (x > -1) and (y > -1) then begin
         Left := x;
         Top := y;
         Position := poDesigned;
      end
      else begin
         Position := poDefaultPosOnly;
      end;
      m_strLastSaveFolder := ini.ReadString('SETTINGS', 'LAST_SAVE_FOLDER', '');
   finally
      ini.Free();
   end;
end;

// ----------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
var
   ini: TIniFile;
begin
   m_jccglist.Free();
   m_radiolist.Free();

   ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
   try
      ini.WriteInteger('SETTINGS', 'X', Left);
      ini.WriteInteger('SETTINGS', 'Y', Top);
      ini.WriteString('SETTINGS', 'LAST_SAVE_FOLDER', m_strLastSaveFolder);
   finally
      ini.Free();
   end;
end;

// ----------------------------------------------------------------------------

procedure TForm1.FormShow(Sender: TObject);
var
   filename: string;
begin
   filename := GetStationListName();
   if FileExists(filename) = True then begin
      PostMessage(Handle, WM_LOAD_STALIST, 0, 0);
      menuOffline.Checked := True;
   end
   else begin
      menuOffline.Checked := False;
   end;

   SetCaption();
end;

// ----------------------------------------------------------------------------

procedure TForm1.FormActivate(Sender: TObject);
begin
   editCallsign.SetFocus();
end;

// ----------------------------------------------------------------------------

procedure TForm1.buttonQueryClick(Sender: TObject);
begin
   if editCallsign.Text = '' then begin
      ListView1.Items.Clear();
      StatusBar1.Panels[0].Text := '';
      StatusBar1.Panels[1].Text := '';
      Exit;
   end;

   DoLookup1(editCallsign.Text, True, checkAround.Checked, menuOffline.Checked);

   editCallsign.SetFocus();
   editCallsign.SelectAll();
end;

// ----------------------------------------------------------------------------

procedure TForm1.editCallsignEnter(Sender: TObject);
begin
   buttonQuery.Default := True;
end;

// ----------------------------------------------------------------------------

procedure TForm1.editCallsignExit(Sender: TObject);
begin
   buttonQuery.Default := False;
end;

// ----------------------------------------------------------------------------

procedure TForm1.checkAroundClick(Sender: TObject);
begin
   if editCallsign.Text <> '' then begin
      editCallsign.SetFocus();
      buttonQuery.Click();
   end;
end;

// ----------------------------------------------------------------------------

procedure TForm1.checkZlogClick(Sender: TObject);
var
   hZlogWnd: HWND;
   wnd: HWND;
   ver: Integer;
begin
   if checkZLog.Checked = True then begin
      // zLogのコントロールを調べる
      hZlogWnd := FindZlogWindow();
      if (hZlogWnd = 0) then begin
         checkZLog.Checked := False;
         Application.MessageBox('zLog for Windowsが見つかりません', 'QTHLOOKUP', MB_OK or MB_ICONEXCLAMATION);
         Exit;
      end;

      // zLog V2.8以降か調べる
      ver := SendMessage(hZlogWnd, (WM_USER + 201), 0, 0);
      if ver >= 2800 then begin
         m_zLogV28 := True;
         m_hLoggerWnd := hZlogWnd;
         timerLogSync.Enabled := True;
         Exit;
      end
      else begin
         m_zLogV28 := False;
      end;

      // 最初の子ウインドウ
      wnd := GetWindow(hZlogWnd, GW_CHILD);
      if (wnd = 0) then begin
         checkZLog.Checked := False;
         Application.MessageBox('can not find first child window', 'QTHLOOKUP', MB_OK or MB_ICONEXCLAMATION);
         Exit;
      end;

      // 次のウインドウ　たぶんこれが対象のパネル
      wnd := GetWindow(wnd, GW_HWNDNEXT);
      if (wnd = 0) then begin
         checkZLog.Checked := False;
         Exit;
      end;

      // timeのTOvrEdit
      wnd := GetWindow(wnd, GW_CHILD);
      if (wnd = 0) then begin
         checkZLog.Checked := False;
         Exit;
      end;

      // memo欄
      wnd := GetWindow(wnd, GW_HWNDNEXT);
      if (wnd = 0) then begin
         checkZLog.Checked := False;
         Exit;
      end;

      // rcvd
      wnd := GetWindow(wnd, GW_HWNDNEXT);
      if (wnd = 0) then begin
         checkZLog.Checked := False;
         Exit;
      end;

      // callsign
      wnd := GetWindow(wnd, GW_HWNDNEXT);
      if (wnd = 0) then begin
         checkZLog.Checked := False;
         Exit;
      end;

      m_hLoggerWnd := wnd;
      timerLogSync.Enabled := True;
   end
   else begin
      timerLogSync.Enabled := False;
   end;
end;

// ----------------------------------------------------------------------------

procedure TForm1.DoLookup1(strCallsign: string; fShowDialog: Boolean; fCheckAround: Boolean; fOffline: Boolean);
var
   list: TLicenseInfoList;
   curobj: TLicenseInfoObject;
   preobj: TLicenseInfoObject;
   i: Integer;
   listitem: TListItem;
   dwTick: DWORD;
   strError: string;
begin
   dwTick := GetTickCount();
   ListView1.Items.BeginUpdate();
   ListView1.Items.Clear();
   StatusBar1.Panels[0].Text := 'QUERY';
   Application.ProcessMessages();

   // ポータブルはコールサイン本体部のみで
   strCallsign := GetCallsign(strCallsign);

   // 照会
   if fOffline = True then begin
      list := LicenseLookupOffline(strCallsign, fCheckAround, strError);
   end
   else begin
      list := LicenseLookup(strCallsign, fCheckAround, strError);
   end;
   try
      if list.Count = 0 then begin
         StatusBar1.Panels[0].Text := '';
         StatusBar1.Panels[1].Text := '';
         StatusBar1.Panels[2].Text := '登録はありません';
         StatusBar1.Panels[3].Text := '';
         StatusBar1.Panels[4].Text := strError;
         Exit;
      end;

      preobj := nil;
      for i := 0 to list.Count - 1 do begin
         curobj := list[i];

         if (preobj <> nil) and (checkNoSameAddress.checked = True) then begin
            if (curobj.Callsign = preobj.Callsign) and
               (curobj.Pref = preobj.Pref) and
               (curobj.Address = preobj.Address) then begin
               continue;
            end;
         end;

         listitem := ListView1.Items.Add();
         listitem.Caption := curobj.Callsign;         // コールサイン
         listitem.SubItems.Add(curobj.Pref);          // 都道府県
         listitem.SubItems.Add(curobj.Address);       // 市区町村
         listitem.SubItems.Add(curobj.Jccg.Pref);     // 県コード
         listitem.SubItems.Add(curobj.Jccg.Code);     // JCC/Gコード
         listitem.SubItems.Add(curobj.Jccg.Name);     // QTH
         listitem.SubItems.Add(curobj.Name);          // 名称
         listitem.SubItems.Add(curobj.LicenseDate);   // 免許の年月日
         if curobj.RadioSpecList.Count = 0 then begin
            listitem.SubItems.Add(IntToStr(curobj.MaxPower));   // max power
         end
         else begin
            listitem.SubItems.Add(IntToStr(curobj.RadioSpecList.GetMaxPower()));   // max power
         end;

         preobj := curobj;
      end;

      StatusBar1.Panels[0].Text := IntToStr(GetTickCount() - dwTick) + ' ms';
      StatusBar1.Panels[1].Text := IntToStr(list.TotalCount);
      StatusBar1.Panels[2].Text := strCallsign;
      StatusBar1.Panels[3].Text := list.LastUpdate;
      StatusBar1.Panels[4].Text := '';

   finally
      list.Free();
      ListView1.Items.EndUpdate();
   end;
end;

// ----------------------------------------------------------------------------

function TForm1.LicenseLookup(strCallsign: string; fCheckAround: Boolean; var strError: string): TLicenseInfoList;
var
   list: TLicenseInfoList;
var
   strPrefix: string;
   nArea: Integer;
   strQuery: string;
   strResponse: string;
   IT: string;
   so: TJSONObject;
   musen: TJSONArray;
   musen_info: TJSONObject;
   list_info: TJSONObject;
   listinfo: TJSONObject;
   detailinfo: TJSONObject;
   totalCount: Integer;
   lastUpdate: string;
   i: Integer;
   obj: TLicenseInfoObject;
   strName: string;
   strCall: string;
   Index: Integer;
   strPref: string;
   strCity: string;
   slJson: TStringList;
   strPrefix2: string;
   res: IHttpResponse;
begin
   slJson := TStringList.Create();
   list := TLicenseInfoList.Create();
   try
   try
      strPrefix := Copy(strCallsign, 1, 3);
      nArea := StrToIntDef(Copy(strCallsign, 3, 1), -1);

      // コールサインに５文字以上入力されている場合は周辺コールチェック可
      if (Length(strCallsign) >= 5) and (fCheckAround = True) then begin
         strCallsign := Copy(strCallsign, 1, 5);
      end;

      // １エリア特別
      // https://ja.wikipedia.org/wiki/%E6%97%A5%E6%9C%AC%E3%81%AE%E5%91%BC%E5%87%BA%E7%AC%A6%E5%8F%B7#アマチュア局
      strPrefix2 := LeftStr(strPrefix, 2);
      if ((strPrefix2 = '7K') or
          (strPrefix2 = '7L') or
          (strPrefix2 = '7M') or
          (strPrefix2 = '7N')) and
         ((nArea >= 1) and (nArea <= 4)) then begin
            nArea := 1;
      end;

      // 沖縄特別
      // https://ja.wikipedia.org/wiki/%E6%97%A5%E6%9C%AC%E3%81%AE%E5%91%BC%E5%87%BA%E7%AC%A6%E5%8F%B7#cite_note-27
      // ^ 沖縄の指定枠は、一般的なアマチュア局の場合、JR6AA - NZ、JR6QUA - ZZZ、JS6AAA - ZZZ。
      if (nArea = 6) then begin
         if Length(strCallsign) = 5 then begin
            if (strCallsign >= 'JR6AA') and (strCallsign <= 'JR6NZ') then begin
               nArea := 16;
            end;
         end;
         if Length(strCallsign) = 6 then begin
            if ((strCallsign >= 'JR6QUA') and (strCallsign <= 'JR6ZZZ')) or
               ((strCallsign >= 'JS6AAA') and (strCallsign <= 'JS6ZZZ')) then begin
               nArea := 16;
            end;
         end;
      end;

      case nArea of
         1: IT := '&IT=A';
         2: IT := '&IT=C';
         3: IT := '&IT=E';
         4: IT := '&IT=F';
         5: IT := '&IT=G';
         6: IT := '&IT=H';
         7: IT := '&IT=I';
         8: IT := '&IT=J';
         9: IT := '&IT=D';
         0: IT := '&IT=B';
         16: IT := '&IT=O';      // 沖縄
         else IT := '';
      end;

      strQuery := query_string + IT + '&MA=' + strCallsign;

      // http照会
      res := NetHTTPRequest1.Get(strQuery);
      strResponse := res.ContentAsString();

      slJson.Text := strResponse;

      {$IFDEF DEBUG}
      slJson.SaveToFile('http_response_' + strCallsign + '.txt');
      {$ENDIF}

      so := TJSONObject(TJSONObject.ParseJSONValue(strResponse));
      musen_info := TJSONObject(so.Get('musenInformation').JsonValue);
      totalCount := StrToIntDef(musen_info.Get('totalCount').JsonValue.Value, 0);
      lastUpdate := musen_info.Get('lastUpdateDate').JsonValue.Value;
      if totalCount = 0 then begin   // データ無し
         list.TotalCount := 0;
         list.LastUpdate := '';
         Exit;
      end;

      list.TotalCount := totalCount;
      list.LastUpdate := lastUpdate;

      musen := TJSONArray(so.Get('musen').JsonValue);

      for i:= 0 to Min(totalCount, 100) - 1 do begin
         list_info := TJSONObject(musen.Items[i]);

         detailinfo := TJSONObject(list_info.Pairs[0].JsonValue);
         listinfo   := TJSONObject(list_info.Pairs[1].JsonValue);

         strName := listinfo.Get('name').JsonValue.Value;
         Index := Pos('（', strName);
         strCall := Copy(strName, Index + 1);
         strName := Copy(strName, 1, Index - 1);

         Index := Pos('）', strCall);
         strCall := Copy(strCall, 1, Index - 1);

         obj := TLicenseInfoObject.Create();
         obj.Number        := StrToIntDef(listinfo.Get('no').JsonValue.Value, 0);
         obj.Name          := strName;
         obj.Callsign      := strCall;
         obj.Address       := listinfo.Get('tdfkCd').JsonValue.Value;
         obj.LicenseDate   := listinfo.Get('licenseDate').JsonValue.Value;
         obj.RadioSpecList.Parse(detailinfo.Get('radioSpec1').JsonValue.Value);

         // prefとcityを分割
         strPref := obj.Address;

         Index := Pos('北海道', strPref);
         if (Index > 0) then begin
            Index := Index + 2;
         end
         else begin
            Index := Pos('東京都', strPref);
            if (Index > 0) then begin
               Index := Index + 2;
            end
            else begin
               Index := Pos('大阪府', strPref);
               if (Index > 0) then begin
                  Index := Index + 2;
               end
               else begin
                  Index := Pos('京都府', strPref);
                  if (Index > 0) then begin
                     Index := Index + 2;
                  end
                  else begin
                     Index := Pos('県', strPref);
                  end;
               end;
            end;
         end;

         if (Index <= 0) then begin
            strPref := '';
            strCity := obj.Address;
         end
         else begin
            strPref := Copy(obj.Address, 1, Index);
            strCity := Copy(obj.Address, Index + 1);
         end;

         obj.Pref := strPref;
         obj.Address := strCity;

         Index := m_jccglist.IndexOf2(obj.Pref, obj.Address);
         if Index >= 0 then begin
            obj.Jccg := m_jccglist.Items[Index];
         end
         else begin
            obj.Jccg := EmptyJccg;
         end;

         list.Add(obj);
      end;

      strError := '';
   except
      on E: Exception do begin
         strError := E.Message;
      end;
   end;
   finally
      Result := list;
      slJson.Free();
   end;
end;

// ----------------------------------------------------------------------------

function TForm1.LicenseLookupOffline(strCallsign: string; fCheckAround: Boolean; var strError: string): TLicenseInfoList;
var
   list: TLicenseInfoList;
   i: Integer;
   obj: TLicenseInfoObject;
   Index: Integer;
   O: TLicenseInfoObject;
   L: Integer;
begin
   list := TLicenseInfoList.Create();
   try
   try
      // コールサインに５文字以上入力されている場合は周辺コールチェック可
      if (Length(strCallsign) >= 5) and (fCheckAround = True) then begin
         strCallsign := Copy(strCallsign, 1, 5);
      end;

      L := Length(strCallsign);

      Index := m_radiolist.Find(strCallsign, fCheckAround);
      if Index = -1 then begin   // データ無し
         list.TotalCount := 0;
         list.LastUpdate := '';
         Exit;
      end;

      Index := Max(Index - 15, 0);

      // 一致するまで送り
      for i := Index to m_radiolist.Count - 1 do begin
         O := m_radiolist[i];

         if Copy(O.Callsign, 1, L) = strCallsign then begin
            Index := i;
            Break;
         end;
      end;

      // 一致しなくなったら終了
      for i := Index to m_radiolist.Count - 1 do begin
         O := m_radiolist[i];

         if Copy(O.Callsign, 1, L) <> strCallsign then begin
            Break;
         end;

         obj := TLicenseInfoObject.Create();
         obj.Number        := i;
         obj.Name          := O.Name;
         obj.Callsign      := O.Callsign;
         obj.Address       := O.Address;
         obj.Pref          := O.Pref;
         obj.Address       := O.Address;
         obj.Jccg          := O.Jccg;
         obj.MaxPower      := O.MaxPower;
         obj.LicenseDate   := O.LicenseDate;

         list.Add(obj);
      end;

      strError := '';
   except
      on E: Exception do begin
         strError := E.Message;
      end;
   end;
   finally
      Result := list;
   end;
end;

// ----------------------------------------------------------------------------

procedure TForm1.ListView1AdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
var
   sx, sy, ex, ey: Integer;
   x, y: Integer;
   i: Integer;
   strText: string;
   bg, fg: TColor;
   fs: TFontStyles;
   backrect: TRect;
   rr: TRect;
   offset: Integer;
   colwidth: Integer;
   rect: TRect;
begin
   if Stage = cdPrePaint then begin
      // １行のRect
      rect := Item.DisplayRect(drBounds);

      sx := rect.Left;

      //
      // 文字の描画
      //
      for i := 0 to TListView(Sender).Columns.Count - 1 do begin
         colwidth := ListView_GetColumnWidth( Sender.Handle, i );

         if i = 0 then begin
            strText := Item.Caption;
         end
         else begin
            strText := Item.SubItems[i - 1];
         end;

         fg := clBlack;
         fs := Sender.Canvas.Font.Style;
         fs := fs - [fsBold];
         fs := fs - [fsItalic];
         bg := clWhite;

         // この行が検索対象なら
         if Item.Caption = editCallsign.Text then begin
            fs := fs + [fsBold];
            bg := clYellow;
         end;

         // 選択中
         if Item.Selected = True then begin
            if Focused then begin
               fg := clWhite;
               bg := $FF9933;
            end
            else begin
               fg := clBlack;
               bg := $F0F0F0;
            end;
         end;

         Sender.Canvas.Font.Color := fg;
         Sender.Canvas.Font.Size := 10;
         Sender.Canvas.Brush.Color := bg;
         Sender.Canvas.Font.Style := fs;

         // 背景を塗る

         // 描画領域の特定
         backrect.Top := rect.Top;
         backrect.Bottom := rect.Bottom;
         backrect.Left := sx + 1;
         backrect.Right := sx + colwidth;

         // 背景色の描画
         Sender.Canvas.FillRect(backrect);

         // rr:各アイテムのクリップ領域
         rr.Top := rect.Top;
         rr.Bottom := rect.Bottom;
         rr.Left := sx;
         rr.Right := sx + colwidth - 2;

         offset := 1;

         // 左右寄せ・センタリング
         sy := rect.Top + (((rect.Bottom - rect.Top) - Sender.Canvas.TextHeight('ABC')) div 2);

         x := sx + 2 + offset;
         y := sy;

         // TextRect()はExtTextOut()なのでBrush色では無く
         // 背景色(SetBkColor()した色)で塗られる
         SetTextColor(Sender.Canvas.Handle, fg);
         SetBkColor(Sender.Canvas.Handle, bg);
         SelectObject(Sender.Canvas.Handle, Sender.Canvas.Font.Handle);
         Sender.Canvas.TextRect(rr, x, y, strText);

         // 次の描画位置
         sx := sx + colwidth;
      end;

      //
      // グリッド線の描画
      //
      Sender.Canvas.Pen.Style := psSolid;
      Sender.Canvas.Pen.Color := $F0F0F0;
      Sender.Canvas.Brush.Style := bsClear;

      // 縦線の描画
      sx := rect.Left;
      for i := 0 to TListView(Sender).Columns.Count - 1 do begin
         sx := sx + ListView_GetColumnWidth(Sender.Handle, i);
         sy := rect.Top - 1;
         ex := sx;
         ey := rect.Bottom;

         Sender.Canvas.MoveTo(sx, sy);
         Sender.Canvas.LineTo(ex, ey);
      end;

      Sender.Canvas.Brush.Style := bsSolid;
      Sender.Canvas.Brush.Color := $F0F0F0;
      rr.Top := rect.Top - 1;
      rr.Left := rect.Left;
      rr.Right := rect.Right + 1;
      rr.Bottom := rect.Bottom;
      Sender.Canvas.FrameRect(rr);

      DefaultDraw := False;
   end
   else begin
      DefaultDraw := True;
   end;
end;

// ----------------------------------------------------------------------------

procedure TForm1.menuOfflineClick(Sender: TObject);
begin
   SetCaption();
end;

// ----------------------------------------------------------------------------

// メニュー：コールサインリストで照会

procedure TForm1.menuOpenClick(Sender: TObject);
var
   donelist: TStringList;
   strCallsign: string;
   strFileName: string;
   i: Integer;
   j: Integer;
   ret: Integer;
   slText: TStringList;
   slLine: TStringList;
   lics: TLicenseInfoList;
   obj: TLicenseInfoObject;
   progress: TformProgress;
   listitem: TListItem;
   strError: string;
begin
   slText := TStringList.Create();
   slLine := TStringList.Create();
   donelist := TStringList.Create();
   progress := TformProgress.Create(Self);
   try
      if (m_strLastOpenFolder <> '') then begin
          OpenFileDialog.InitialDir := m_strLastOpenFolder;
      end;

      // ファイル名を入力
      if (OpenFileDialog.Execute(Handle) <> True) then begin
         Exit;
      end;

      strFileName := OpenFileDialog.FileName;

      slText.LoadFromFile(strFileName);

      Enabled := False;

      ListView1.Items.BeginUpdate();
      ListView1.Items.Clear();

      progress.Show();

      for i := 0 to slText.Count - 1 do begin
         if (progress.IsAbort = True) then begin
            Break;
         end;

         slLine.CommaText := slText[i] + ',,';

         strCallsign := Trim(slLine[0]);

         // 移動局は無視
         if Pos('/', strCallsign) > 0 then begin
            Continue;
         end;

         // SWL局は無視
         if Pos('-', strCallsign) > 0 then begin
            Continue;
         end;

         // 照会済みならパス
         ret := donelist.IndexOf( strCallsign );
         if (ret >= 0) then begin     // 0以上=済み 0未満=未完
            Continue;
         end;


         //
//         while(true) do begin
            progress.MessageText := strCallsign + 'について照会中... ';
            Application.ProcessMessages();

            // 照会する
            lics := LicenseLookup(strCallsign, False, strError);
            for j := 0 to lics.Count - 1 do begin
               obj := lics[j];

               listitem := ListView1.Items.Add();
               listitem.Caption := obj.Callsign;         // コールサイン
               listitem.SubItems.Add(obj.Pref);          // 都道府県
               listitem.SubItems.Add(obj.Address);       // 市区町村
               listitem.SubItems.Add(obj.Jccg.Pref);     // 県コード
               listitem.SubItems.Add(obj.Jccg.Code);     // JCC/Gコード
               listitem.SubItems.Add(obj.Jccg.Name);     // QTH
               listitem.SubItems.Add(obj.Name);          // 名称
               listitem.SubItems.Add(obj.LicenseDate);   // 免許の年月日
               listitem.SubItems.Add(IntToStr(obj.RadioSpecList.GetMaxPower()));   // max power
            end;
//         end;

         // 照合済みリストに登録
         donelist.Add( strCallsign );
         donelist.Sort();

         FreeAndNil(lics);
      end;

      m_strLastFileName := ExtractFileName(strFileName);
      m_strLastOpenFolder := ExtractFilePath(strFileName);
   finally
      ListView1.Items.EndUpdate();
      Enabled := True;
      progress.Release();
      slText.Free();
      slLine.Free();
      donelist.Free();
   end;
end;

// ----------------------------------------------------------------------------

// メニュー：名前を付けて保存

procedure TForm1.menuSaveAsClick(Sender: TObject);
var
   i: Integer;
   strFileName: string;
   F: TextFile;
   strLine: string;
begin
   if (ListView1.Items.Count = 0) then begin
      Application.MessageBox('保存するデータがありません', 'QTHLOOKUP', MB_OK or MB_ICONEXCLAMATION);
      Exit;
   end;

   if (m_strLastSaveFolder <> '') then begin
       SaveFileDialog.InitialDir := m_strLastSaveFolder;
   end;

   if (SaveFileDialog.Execute(Self.Handle) = False) then begin
      Exit;
   end;

   strFileName := SaveFileDialog.FileName;

   try
      // あれば消す
      if (FileExists(strFileName) = True) then begin
         DeleteFile(strFileName);
      end;

      AssignFile(F, strFileName);
      Rewrite(F);

      for i := 0 to ListView1.Items.Count - 1 do begin
         strLine := ListView1.Items[i].Caption + ',' +
                    ListView1.Items[i].SubItems[0] + ',' +
                    ListView1.Items[i].SubItems[1] + ',' +
                    ListView1.Items[i].SubItems[2] + ',' +
                    ListView1.Items[i].SubItems[3] + ',' +
                    ListView1.Items[i].SubItems[4] + ',' +
                    ListView1.Items[i].SubItems[5];

         WriteLn(F, strLine);
      end;

      CloseFile(F);

      m_strLastSaveFolder := ExtractFilePath(strFileName);
   except
   end;
end;

// ----------------------------------------------------------------------------

// メニュー：ZLOG.SPCとして保存

// 00000000011111111122
// 12345678901234567890
// 7J1ABC     100120


procedure TForm1.menuZlogSpcClick(Sender: TObject);
var
   i: Integer;
   strFileName: string;
   F: TextFile;
   strLine: string;
begin
   if (ListView1.Items.Count = 0) then begin
      Application.MessageBox('保存するデータがありません', 'QTHLOOKUP', MB_OK or MB_ICONEXCLAMATION);
      Exit;
   end;

   if (m_strLastSaveFolder <> '') then begin
       SaveFileDialog.InitialDir := m_strLastSaveFolder;
   end;

   if (SaveFileDialog2.Execute(Self.Handle) = False) then begin
      Exit;
   end;

   strFileName := SaveFileDialog2.FileName;

   try
      // あれば消す
      if (FileExists(strFileName) = True) then begin
         DeleteFile(strFileName);
      end;

      AssignFile(F, strFileName);
      Rewrite(F);

      for i := 0 to ListView1.Items.Count - 1 do begin
         strLine := Copy(ListView1.Items[i].Caption + DupeString(' ', 11) , 1, 11);

         strLine := strLine + ListView1.Items[i].SubItems[3];

         WriteLn(F, strLine);
      end;

      CloseFile(F);

      m_strLastSaveFolder := ExtractFilePath(strFileName);
   except
   end;
end;

// ----------------------------------------------------------------------------

// メニュー：終了

procedure TForm1.menuExitClick(Sender: TObject);
begin
   Close();
end;

// ----------------------------------------------------------------------------

procedure TForm1.menuNoticeClick(Sender: TObject);
var
   f: TformImpotantNotice;
begin
   f := TformImpotantNotice.Create(Self);
   try
      f.ShowModal();
   finally
      f.Release();
   end;
end;

// ----------------------------------------------------------------------------

// タイマー

procedure TForm1.timerLogSyncTimer(Sender: TObject);
var
   strCallsign: string;
   szWindowText: array[0..255] of Char;
   nLen: Integer;
   callsign_atom: ATOM;
begin
   timerLogSync.Enabled := False;
   try
      if (m_hLoggerWnd = 0) then begin
         Exit;
      end;

      ZeroMemory(@szWindowText, SizeOf(szWindowText));

      if m_zLogV28 = True then begin
         nLen := SendMessage(m_hLoggerWnd, (WM_USER + 200), 0, 0);
         callsign_atom := LOWORD(nLen);
         if callsign_atom = 0 then begin
            Exit;
         end;

         nLen := GlobalGetAtomName(callsign_atom, PChar(@szWindowText), SizeOf(szWindowText));
         if (nLen = 0) then begin
            Exit;
         end;

         GlobalDeleteAtom(callsign_atom);
      end
      else begin
         nLen := SendMessage(m_hLoggerWnd, WM_GETTEXT, SizeOf(szWindowText), LPARAM(PChar(@szWindowText)));
      end;

      strCallsign := szWindowText;

      // ５文字の場合は先頭二文字がJAの場合のみ
      if (((nLen = 5) and (Copy(strCallsign, 1, 2) = 'JA')) or (nLen >= 6)) then begin

         editCallsign.Text := strCallsign;

         // 前回と同じコールは除く
         if (m_strPrevCallsign = strCallsign) then begin
            Exit;
         end;

         // 照会
         DoLookup1(strCallsign, false, checkAround.Checked, menuOffline.Checked);

         editCallsign.SelectAll();
         m_strPrevCallsign := strCallsign;

      end
      else if (nLen < 0) then begin
         m_hLoggerWnd := 0;
      end;

   finally
      if (m_hLoggerWnd <> 0) then begin
         timerLogSync.Enabled := True;
      end
      else begin
         checkZLog.Checked := False;
      end;
   end;
end;

// ----------------------------------------------------------------------------

function TForm1.GetCallsign(strCallsign: string): string;
var
   Index: Integer;
   strLeft, strRight: string;
begin
   Index := Pos('/', strCallsign);
   if Index = 0 then begin
      Result := strCallsign;
      Exit;
   end;

   strLeft := Copy(strCallsign, 1, Index - 1);
   strRight := Copy(strCallsign, Index + 1);

   if Length(strLeft) >= Length(strRight) then begin
      Result := strLeft;
   end
   else begin
      Result := strRight;
   end;
end;

// ----------------------------------------------------------------------------

function TForm1.FindZlogWindow(): HWND;
var
   hZlogWnd: HWND;
   szCaption: array[0..1024] of Char;
   strCaption: string;
   nLen: Integer;
   slWindows: TStringList;
   f: TformSelectZLog;
   childwnd: HWND;
begin
   f := TformSelectZLog.Create(Self);
   slWindows := TStringList.Create();
   try
      hZlogWnd := GetTopWindow(0);
      repeat
         nLen := GetWindowText(hZlogWnd, szCaption, SizeOf(szCaption));
         if nLen > 0 then begin
            strCaption := StrPas(szCaption);
            if Pos('zLog for Windows', strCaption) > 0 then begin
               // 子ウインドウを持たないウインドウは除外
               childwnd := GetWindow(hZlogWnd, GW_CHILD);
               if (childwnd <> 0) then begin
                  slWindows.AddObject(strCaption, TObject(hZlogWnd));
               end;
            end;
         end;

         hZlogWnd := GetNextWindow(hZlogWnd, GW_HWNDNEXT)
      until hZlogWnd = 0;

      if slWindows.Count = 0 then begin
         Result := 0;
      end
      else if slWindows.Count = 1 then begin
         Result := HWND(slWindows.Objects[0]);
      end
      else begin  // >= 2
         f.List := slWindows;
         if f.ShowModal() <> mrOK then begin
            Result := 0;
            Exit;
         end;

         Result := HWND(slWindows.Objects[f.SelectedIndex]);
      end;
   finally
      slWindows.Free();
      f.Release();
   end;
end;

// ----------------------------------------------------------------------------

procedure TForm1.LoadStationList(var Message: TMessage);
var
   dlg: TformProgress;
   filename: string;
   slText: TStringList;
   slLine: TStringList;
   O: TLicenseInfoObject;
   Index: Integer;
   TXT: TextFile;
   S: string;
   cnt: Integer;
begin
   slText := TStringList.Create();
   slText.StrictDelimiter := True;
   slLine := TStringList.Create();
   slLine.StrictDelimiter := True;
   filename := GetStationListName();
   dlg := TformProgress.Create(Self);
   Enabled := False;
   try
      dlg.MessageText := '無線局リストをロードしています';
      dlg.Show();
      cnt := 0;

      AssignFile(TXT, filename);
      Reset(TXT);
      ReadLn(TXT, S);

      while(EOF(TXT) = False) do begin
         if (cnt mod 100) = 0 then begin
            dlg.MessageText := '無線局リストをロードしています(' + IntToStr(cnt) + ')';
         end;
         Application.ProcessMessages();
         if dlg.IsAbort = True then begin
            Break;
         end;

         ReadLn(TXT, S);
         slLine.CommaText := S + ',,,,,,,,,';

         if slLine[0] = '' then begin
            Continue;
         end;

         O := TLicenseInfoObject.Create();
         O.Callsign := slLine[0];
         O.Name := slLine[1];
         O.Pref := slLine[2];
         O.Address := slLine[3];

         Index := m_jccglist.IndexOf1(slLine[4]);
         if Index <> -1 then begin
            O.Jccg := m_jccglist.Items[Index];
         end
         else begin
            O.Jccg := EmptyJccg;
         end;

         O.MaxPower := StrToIntDef(slLine[5], 0);
         O.LicenseDate := slLine[8];

         Index := m_radiolist.Find(O.Callsign, True);
         m_radiolist.Insert(Index, O);

         Inc(cnt);
      end;

      CloseFile(TXT);
      dlg.Hide();
   finally
      slText.Free();
      slLine.Free();
      dlg.Release();
      Enabled := True;
      SetCaption();
   end;
end;

// ----------------------------------------------------------------------------

function TForm1.GetStationListName(): string;
begin
   Result  := ExtractFilePath(Application.ExeName) + 'allstations.csv';
end;

// ----------------------------------------------------------------------------

procedure TForm1.SetCaption();
begin
   if menuOffline.Checked = True then begin
      Caption := 'QTHLOOKUP [OFFLINE] ' + IntToStr(m_radiolist.Count) + ' stations';
   end
   else begin
      Caption := 'QTHLOOKUP [ONLINE]';
   end;
end;

end.

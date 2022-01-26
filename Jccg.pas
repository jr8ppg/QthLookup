unit Jccg;

interface

uses
  System.Classes, System.Types, SysUtils, Generics.Collections, Generics.Defaults;

type
  TJccgObject = class(TObject)
  private
    FCode: string;
    FName: string;
    FPref: string;
    FCompareName: string;
  public
    constructor Create();
    property Code: string read FCode write FCode;
    property Name: string read FName write FName;
    property Pref: string read FPref write FPref;
    property CompareName: string read FCompareName write FCompareName;
  end;

  TJccgList = class(TObjectList<TJccgObject>)
  private
    FPreAddress: string;
    FPreIndex: Integer;
    procedure LoadStringList(SL: TStringList);
  public
    constructor Create(OwnsObjects: Boolean = True);
    procedure LoadFromFile(AFileName: string);
    procedure LoadFromResourceName(hinst: THandle; resname: string);
    function IndexOf1(strCode: string): Integer;
    function IndexOf2(strPref, strAddress: string): Integer;
    procedure Sort();
    function KanaUpper(strText: string): string;
  end;

  TJccgObjectComparer1 = class(TInterfacedObject, IComparer<TJccgObject>)
  public
    function Compare(const Left, Right: TJccgObject): Integer;
  end;

var
  EmptyJccg: TJccgObject;

implementation

{ TJccgObject }

constructor TJccgObject.Create();
begin
   FCode := '';
   FName := '';
   FPref := '';
   FCompareName := '';
end;

{ TJccgList }

constructor TJccgList.Create(OwnsObjects: Boolean = True);
begin
   inherited Create(OwnsObjects);

   FPreAddress := '';
   FPreIndex := -1;
end;

procedure TJccgList.LoadFromFile(AFileName: string);
var
   slText: TStringList;
begin
   slText := TStringList.Create();
   slText.StrictDelimiter := True;
   try
      slText.LoadFromFile(AFileName);
      LoadStringList(slText);
   finally
      slText.Free();
   end;
end;

procedure TJccgList.LoadFromResourceName(hinst: THandle; resname: string);
var
   RS: TResourceStream;
   SL: TStringList;
begin
   RS := TResourceStream.Create(hinst, resname, RT_RCDATA);
   SL := TStringList.Create();
   SL.StrictDelimiter := True;
   try
      SL.LoadFromStream(RS);
      LoadStringList(SL);
   finally
      RS.Free();
      SL.Free();
   end;
end;

procedure TJccgList.LoadStringList(SL: TStringList);
var
   i: Integer;
   slLine: TStringList;
   obj: TJccgObject;
   strText: string;
begin
   slLine := TStringList.Create();
   slLine.StrictDelimiter := True;
   slLine.Delimiter := #09;   // TAB
   try
      for i := 0 to SL.Count - 1 do begin
         strText := SL[i] + #09#09#09;
         slLine.DelimitedText := strText;
         if Copy(strText, 1, 1) = ';' then begin
            Continue;
         end;

         obj := TJccgObject.Create();
         obj.Code := slLine[0];
         obj.Name := slLine[1];
         obj.Pref := slLine[2];
         obj.CompareName := KanaUpper(slLine[1]);
         Self.Add(obj);
      end;

   finally
      slLine.Free();
   end;
end;

function TJccgList.IndexOf1(strCode: string): Integer;
var
   i: Integer;
begin
   for i := 0 to Count - 1 do begin
      if Items[i].Code = strCode then begin
         Result := i;
         Exit;
      end;
   end;
   Result := -1;
end;

function TJccgList.IndexOf2(strPref, strAddress: string): Integer;
var
   i: Integer;
   strTown: string;
   strGun: string;
   Index: Integer;
   strCompAddress: string;
   strCompPref: string;
begin
   if FPreAddress = strAddress then begin
      Result := FPreIndex;
      Exit;
   end;

   strPref := KanaUpper(strPref);
   strAddress := KanaUpper(strAddress);

   // 郡がある場合は、郡と町村に分ける
   Index := Pos('郡', strAddress);
   if Index > 0 then begin
      strGun := Copy(strAddress, 1, Index);
      strTown := Copy(strAddress, Index + 1);
      for i := 0 to Count - 1 do begin
         strCompPref := Copy(Items[i].CompareName, 1, Length(strPref));
         strCompAddress := Items[i].CompareName;
         if (strPref = strCompPref) and (Pos(strGun, strCompAddress) > 0) and (Pos(strTown, strCompAddress) > 0) then begin
            Result := i;
            FPreAddress := strAddress;
            FPreIndex := i;
            Exit;
         end;
      end;
   end
   else begin
      for i := 0 to Count - 1 do begin
         strCompPref := Copy(Items[i].CompareName, 1, Length(strPref));
         strCompAddress := Items[i].CompareName;
         if ((strPref = strCompPref) and (Pos(strAddress, strCompAddress) > 0)) or
            (Copy(strCompAddress, 1, Length(strAddress)) = strAddress) then begin
            Result := i;
            FPreAddress := strAddress;
            FPreIndex := i;
            Exit;
         end;
      end;
   end;
   FPreAddress := '';
   FPreIndex := -1;
   Result := -1;
end;

procedure TJccgList.Sort();
var
   Comparer: TJccgObjectComparer1;
begin
   Comparer := TJccgObjectComparer1.Create();
   inherited Sort(Comparer);
end;

function TJccgList.KanaUpper(strText: string): string;
begin
   strText := StringReplace(strText, 'ァ', 'ア', [rfReplaceAll]);
   strText := StringReplace(strText, 'ィ', 'イ', [rfReplaceAll]);
   strText := StringReplace(strText, 'ゥ', 'ウ', [rfReplaceAll]);
   strText := StringReplace(strText, 'ェ', 'エ', [rfReplaceAll]);
   strText := StringReplace(strText, 'ォ', 'オ', [rfReplaceAll]);
   strText := StringReplace(strText, 'ヵ', 'カ', [rfReplaceAll]);
   strText := StringReplace(strText, 'ヶ', 'ケ', [rfReplaceAll]);
   strText := StringReplace(strText, 'ッ', 'ツ', [rfReplaceAll]);
   strText := StringReplace(strText, 'ャ', 'ヤ', [rfReplaceAll]);
   strText := StringReplace(strText, 'ュ', 'ユ', [rfReplaceAll]);
   strText := StringReplace(strText, 'ョ', 'ヨ', [rfReplaceAll]);
   strText := StringReplace(strText, 'ヮ', 'ワ', [rfReplaceAll]);
   Result := strText;
end;

function TJccgObjectComparer1.Compare(const Left, Right: TJccgObject): Integer;
begin
   Result := CompareText(Left.Code, Right.Code);
end;

initialization
   EmptyJccg := TJccgObject.Create();

finalization
   EmptyJccg.Free();

end.

unit LicenseInfo;

interface

uses
  System.StrUtils, System.Math, Classes, SysUtils,
  Generics.Collections, Generics.Defaults,
  Jccg;

type
  // "3MA\\t1910 kHz\\t200   W\\n3HA\\t3537.5 kHz\\t200   W\\n3HD\\t3798 kHz\\t200   W\\nA1A\\t4630 kHz\\t200   W\\n3HA\\t7100 kHz\\t200   W\\n2HC\\t10125 kHz\\t200   W\\n2HA\\t14175 kHz\\t200   W\\n3HA\\t18118 kHz\\t200   W\\n3HA\\t21225 kHz\\t200   W\\n3HA\\t24940 kHz\\t200   W\\n3VA\\t28.85 MHz\\t200   W\\n3VA\\t52 MHz\\t200   W\\n4VF\\t145 MHz\\t50   W\\n4VF\\t435 MHz\\t50   W"
  TRadioSpec = class(TObject)
  private
    FWaveFormat: string;
    FFreq: string;
    FPower: Integer;
  public
    constructor Create();
    property WaveFormat: string read FWaveFormat write FWaveFormat;
    property Freq: string read FFreq write FFreq;
    property Power: Integer read FPower write FPower;
  end;

  TRadioSpecList = class(TObjectList<TRadioSpec>)
  private
    function ParsePower(S: string): Integer;
  public
    constructor Create(OwnsObjects: Boolean = True);
    procedure Parse(strRadioSpec: string);
    function GetMaxPower(): Integer;
  end;

  TLicenseInfoObject = class(TObject)
  private
    FNumber: Integer;
    FCallsign: string;
    FName: string;
    FAddress: string;
    FPref: string;
    FLicenseDate: string;
    FJccg: TJccgObject;
    FRadioSpecList: TRadioSpecList;
    FMaxPower: Integer;
  public
    constructor Create();
    destructor Destroy(); override;
    property Number: Integer read FNumber write FNumber;
    property Callsign: string read FCallsign write FCallsign;
    property Name: string read FName write FName;
    property Address: string read FAddress write FAddress;
    property Pref: string read FPref write FPref;
    property LicenseDate: string read FLicenseDate write FLicenseDate;
    property Jccg: TJccgObject read FJccg write FJccg;
    property RadioSpecList: TRadioSpecList read FRadioSpecList;
    property MaxPower: Integer read FMaxPower write FMaxPower;
  end;

  TLicenseInfoListComparer1 = class(TComparer<TLicenseInfoObject>)
  public
    function Compare(const Left, Right: TLicenseInfoObject): Integer; override;
  end;

  TLicenseInfoList = class(TObjectList<TLicenseInfoObject>)
  private
    FTotalCount: Integer;
    FLastUpdate: string;
    FJccgList: TJccgList;
    FCallsignComparer: TLicenseInfoListComparer1;
  public
    constructor Create(OwnsObjects: Boolean = True);
    destructor Destroy(); override;
    property TotalCount: Integer read FTotalCount write FTotalCount;
    property LastUpdate: string read FLastUpdate write FLastUpdate;
    property JccgList: TJccgList read FJccgList write FJccgList;
    procedure LoadFromFile(filename: string);
    function Find(strCallsign: string; fCheckAround: Boolean): Integer;
  end;

implementation

constructor TLicenseInfoObject.Create();
begin
   inherited;
   FNumber := 0;
   FCallsign := '';
   FName := '';
   FAddress := '';
   FPref := '';
   FLicenseDate := '';
   FJccg := nil;
   FRadioSpecList := TRadioSpecList.Create();
   FMaxPower := 0;
end;

destructor TLicenseInfoObject.Destroy();
begin
   FRadioSpecList.Free();
end;

constructor TLicenseInfoList.Create(OwnsObjects: Boolean = True);
begin
   inherited Create(OwnsObjects);
   FTotalCount := 0;
   FLastUpdate := '';
   FCallsignComparer := TLicenseInfoListComparer1.Create();
end;

destructor TLicenseInfoList.Destroy();
begin
   FCallsignComparer.Free();
   Inherited;
end;

constructor TRadioSpec.Create();
begin
   inherited;
   FWaveFormat := '';
   FFreq := '';
   FPower := 1;
end;

// 12345
function TRadioSpecList.ParsePower(S: string): Integer;
begin
   if RightStr(S, 1) = 'W' then begin
      S := Copy(S, 1, Length(S) - 1)
   end;

   S := StringReplace(S, ' ', '', [rfReplaceAll]);

   if S = '1k' then begin
      S := '1000';
   end;

   Result := StrToIntDef(S, 1);
end;

constructor TRadioSpecList.Create(OwnsObjects: Boolean = True);
begin
   inherited Create(OwnsObjects);
end;

procedure TRadioSpecList.Parse(strRadioSpec: string);
var
   slAll: TStringList;
   slSpec: TStringList;
   strTmp: string;
   i: Integer;
   obj: TRadioSpec;
begin
   slAll := TStringList.Create();
   slAll.StrictDelimiter := True;
   slAll.Delimiter := #09;
   slSpec := TStringList.Create();
   slSpec.StrictDelimiter := True;
   slSpec.Delimiter := #09;
   try
      Clear();

      strTmp := StringReplace(strRadioSpec, '\n', #09, [rfReplaceAll]);
      slAll.DelimitedText := strTmp;

      for i := 0 to slAll.Count - 1 do begin
         strTmp := slAll.Strings[i];
         strTmp := StringReplace(strTmp, '\t', #09, [rfReplaceAll]);
         slSpec.DelimitedText := strTmp;

         obj := TRadioSpec.Create();
         obj.WaveFormat := slSpec.Strings[0];
         obj.Freq       := slSpec.Strings[1];
         obj.Power      := ParsePower(slSpec.Strings[2]);
         Add(obj);
      end;

   finally
      slAll.Free();
      slSpec.Free();
   end;
end;

function TRadioSpecList.GetMaxPower(): Integer;
var
   i: Integer;
   p: Integer;
begin
   p := 0;
   for i := 0 to Count - 1 do begin
      p := Max(p, Items[i].Power);
   end;

   Result := p;
end;

// 0          1      2       3         4            5           6                7           8
// "callsign","name","pref.","address","jcc/g code","max power","municipal code","aja code" ,"license date"

procedure TLicenseInfoList.LoadFromFile(filename: string);
var
   slText: TStringList;
   slLine: TStringList;
   O: TLicenseInfoObject;
   Index: Integer;
   TXT: TextFile;
   S: string;
begin
   slText := TStringList.Create();
   slText.StrictDelimiter := True;
   slLine := TStringList.Create();
   slLine.StrictDelimiter := True;
   try
      AssignFile(TXT, filename);
      Reset(TXT);

      while(EOF(TXT) = False) do begin
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

         if FJccgList <> nil then begin
            Index := FJccgList.IndexOf1(slLine[4]);
            if Index <> -1 then begin
               O.Jccg := FJccgList.Items[Index];
            end
            else begin
               O.Jccg := EmptyJccg;
            end;
         end;

         O.MaxPower := StrToIntDef(slLine[5], 0);
         O.LicenseDate := slLine[8];

         BinarySearch(O, Index, FCallsignComparer);
         Insert(Index, O);
      end;

      CloseFile(TXT);
   finally
      slText.Free();
      slLine.Free();
   end;
end;

function TLicenseInfoList.Find(strCallsign: string; fCheckAround: Boolean): Integer;
var
   O: TLicenseInfoObject;
   Index: Integer;
begin
   O := TLicenseInfoObject.Create();
   try
      O.Callsign := strCallsign;
      if BinarySearch(O, Index, FCallsignComparer) = False then begin
         if fCheckAround = True then begin
            Result := Index;
         end
         else begin
            Result := -1;
         end;
      end
      else begin
         Result := Index;
      end;
   finally
      O.Free();
   end;
end;

function TLicenseInfoListComparer1.Compare(const Left, Right: TLicenseInfoObject): Integer;
begin
   Result := CompareText(Left.Callsign, Right.Callsign);
end;

end.

unit SelectZlog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TformSelectZLog = class(TForm)
    ListBox1: TListBox;
    Label1: TLabel;
    Panel1: TPanel;
    buttonOK: TButton;
    buttonCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure buttonOKClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private �錾 }
    FSelectedIndex: Integer;
    procedure SetList(L: TStringList);
    function GetList(): TStringList;
  public
    { Public �錾 }
    property List: TStringList read GetList write SetList;
    property SelectedIndex: Integer read FSelectedIndex write FSelectedIndex;
  end;

implementation

{$R *.dfm}

procedure TformSelectZLog.FormCreate(Sender: TObject);
begin
   ListBox1.Clear();
   FSelectedIndex := -1;
end;

procedure TformSelectZLog.FormDestroy(Sender: TObject);
begin
//
end;

procedure TformSelectZLog.FormShow(Sender: TObject);
begin
   ListBox1.SetFocus();
end;

procedure TformSelectZLog.buttonOKClick(Sender: TObject);
begin
   if ListBox1.ItemIndex = -1 then begin
      MessageBox(Handle, PChar('zLog��I�����ĉ�����'), PChar(Application.Title), MB_OK or MB_ICONEXCLAMATION);
      Exit;
   end;

   FSelectedIndex := ListBox1.ItemIndex;

   ModalResult := mrOK;
end;

procedure TformSelectZLog.SetList(L: TStringList);
begin
   ListBox1.Items.Assign(L);
end;

function TformSelectZLog.GetList(): TStringList;
begin
   Result := TStringList(ListBox1.Items);
end;

procedure TformSelectZLog.ListBox1DblClick(Sender: TObject);
begin
   buttonOK.Click();
end;

end.

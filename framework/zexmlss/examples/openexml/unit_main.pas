//Äåìîíñòðàöèÿ ZEXMLSS
//Ïðèìåð çàãðóçêè ôàéëà ôîðìàòà excel XML spreadsheet (SpreadsheetML)
//â StringGrid

unit unit_main;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls,
  {zexmlss} {õðàíèëèùå}{,}
  zexmlssutils, zexmlss, zexlsx {çàãðóçêà/ñîõðàíåíèå â ôîðìàòå Excel XML};

type

  { TfrmMain }

  TfrmMain = class(TForm)
    SGtest: TStringGrid;
    btnOpen: TButton;
    ODxml: TOpenDialog;
    CBList: TComboBox;
    lblList: TLabel;
    ZEXMLSStest: TZEXMLSS;
    procedure btnOpenClick(Sender: TObject);
    procedure CBListSelect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure OpenEXMLSS();
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

//Çàãðóæàåò ôàéë â ZEXMLSStest
procedure TfrmMain.OpenEXMLSS();
var
  i: integer;

begin
  if ODxml.Execute then
  begin
    //÷èòàåì ôàéë
    if ReadXLSX(ZEXMLSStest, ODxml.FileName) <> 0 then
      messagebox(0, 'Ïðè ÷òåíèè äîêóìåíòà âîçíèêëà îøèáêà!', 'Îøèáêà!', mb_Ok + mb_iconerror);

    CBList.Clear();
    CBList.Enabled := false;
    if ZEXMLSStest.Sheets.Count > 0 then
    begin
      for i := 0 to ZEXMLSStest.Sheets.Count - 1 do
        CBList.Items.Add(ZEXMLSStest.Sheets[i].Title);
      CBList.ItemIndex := 0;
      CBList.OnSelect(self);
      CBList.Enabled := true;  
    end;
  end;
end;

procedure TfrmMain.btnOpenClick(Sender: TObject);
begin
  OpenEXMLSS();
end;

//Ïðè âûáîðå ëèñòà çàãðóæàåì â SGTest äàííûå
procedure TfrmMain.CBListSelect(Sender: TObject);
var
  PageNum, ColCount, RowCount: integer;

begin
  PageNum := CBList.ItemIndex; // âûáðàííûé ëèñò
  ColCount := ZEXMLSStest.Sheets[PageNum].ColCount;
  RowCount := ZEXMLSStest.Sheets[PageNum].RowCount;
  SGTest.ColCount := ColCount; //óñòàíàâëèâàåì êîë-âî ñòîëáöîâ/ñòðîê
  SGTest.RowCount := RowCount;
  XmlSSToGrid(SGTest,     //Ãðèä
              ZEXMLSStest,//Õðàíèëèùå
              PageNum,    //Íîìåð ëèñòà â õðàíèëèùå
              0, 0,       //Êîîðäèíàòû âñòàâêè â ãðèäå
              0, 0,       //Ëåâûé âåðõíèé óãîë êîïèðóåìîé îáëàñòè èç ëèñòà õðàíèëèùà
              //ïðàâûé íèæíèé óãîë êîïèðóåìîé îáëàñòè èç ëèñòà õðàíèëèùà
              ColCount - 1, RowCount - 1,
              0           //ñïîñîá âñòàâêè (0 - áåç ñäâèãà)
              );
end;

end.

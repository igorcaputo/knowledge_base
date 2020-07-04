// Test main zexmlss features:
// 1. Create simple ods, excel xml and xlsx
// 2. Read previous created files
// 3. Test formulas
unit test_main;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  zexmlss,          //TZEXMLSS
  zeodfs,           //ReadODFS and SaveXmlssToODFS
  zexlsx,           //ReadXLSX and SaveXmlssToXLSX
  zexmlssutils      //ReadEXMLSS and SaveXmlssToEXML
  ;

procedure TestSimpleWrite(FilePath: string);
procedure AddStandartHeader(const Sheet: TZSheet; const HeaderText: string; HeaderWidth: integer = 10; AtCol: integer = 0; AtRow: integer = 0);
procedure CheckMaxCR(const Sheet: TZSheet; AtCol, AtRow: integer);

implementation

procedure CheckMaxCR(const Sheet: TZSheet; AtCol, AtRow: integer);
begin
  if (AtCol >= Sheet.ColCount) then
     Sheet.ColCount := AtCol + 1;
  if (AtRow >= Sheet.RowCount) then
     Sheet.RowCount := AtRow + 1;
end;

//Add standart header for sheet
//INPUT
//  const Sheet: TZSheet       - sheet for adding header
//  const HeaderText: string   - caption for header
//        HeaderWidth: integer - width for merged cell
//        AtCol: integer       - start column
//        AtRow: integer       - start row
procedure AddStandartHeader(const Sheet: TZSheet; const HeaderText: string; HeaderWidth: integer = 10; AtCol: integer = 0; AtRow: integer = 0);
begin
  CheckMaxCR(Sheet, AtCol + HeaderWidth, AtRow + 1);
  Sheet.Cell[AtCol, AtRow].Data := 'avemey.com';
  Sheet.Cell[AtCol, AtRow].HRef := 'http://avemey.com';

  Sheet.Cell[AtCol, AtRow + 1].Data := HeaderText;
  Sheet.MergeCells.AddRectXY(AtCol, AtRow + 1, AtCol + HeaderWidth, AtRow + 1);
end; //AddStandartHeader

procedure TestSimpleWrite(FilePath: string);
var
  ze: TZEXMLSS;  //Какая-то переменная
  //моя переменная
  _sheet: TZSheet;
  _fname: string;

begin
  ze := nil;

  try
    ze := TZEXMLSS.Create(nil);
    ze.Sheets.Count := 2;

    _sheet := ze.Sheets[0];

    _sheet.RowCount := 20;
    _sheet.ColCount := 20;

    AddStandartHeader(_sheet, 'Test simple write');

    _fname := FilePath + '1.xlsx';

    DeleteFile(_fname);

    SaveXmlssToXLSX(ze, _fname);

  finally
    if (Assigned(ze)) then
       FreeAndNil(ze);
  end;

end; //TestSimpleWrite

end.

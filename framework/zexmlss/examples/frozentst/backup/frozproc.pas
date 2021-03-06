unit frozproc;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, zexmlss, zexmlssutils, dateutils, zexlsx, zeodfs;

procedure TestSplit();

implementation

//Test split/frozen columns/rows
procedure TestSplit();
var
  tz: TZEXMLSS;
  _sh: TZSheet;
  i, j: integer;
  _path: string;
  tt: TDateTime;
  t: integer;
  z: integer;

  procedure _SetSplit(PageNum: integer; HorMode, VertMode: TZSplitMode; HorValue, VertValue: integer);
  begin
    tz.Sheets[PageNum].SheetOptions.SplitVerticalMode := VertMode;
    tz.Sheets[PageNum].SheetOptions.SplitHorizontalMode := HorMode;
    tz.Sheets[PageNum].SheetOptions.SplitVerticalValue := VertValue;
    tz.Sheets[PageNum].SheetOptions.SplitHorizontalValue := HorValue;;
  end;

  procedure _WriteTime(const stt: string);
  begin
    WriteLn(stt + ': ' + FloatToStr(MilliSecondsBetween(now(), tt)/1000) + 's');
    tt := now();
  end;

begin
  tz := nil;
  try
    tt := now();
    tz := TZEXMLSS.Create(nil);

    ReadXLSX(tz, _path + 'Pasta1.xlsx');
    _WriteTime(tz.Sheets[1].Cell[1, 2].data);
(*    tz.Sheets.Count := 9;
    _sh := tz.Sheets[0];


    _sh.RowCount := 100;
    _sh.ColCount := 20;

    for i := 1 to _sh.ColCount - 1 do
      _sh.Cell[i, 0].Data := 'Column ' + IntToStr(i);

    for i := 1 to _sh.RowCount - 1 do
    begin
      _sh.Cell[0, i].Data := 'Row ' + IntToStr(i);
      for j := 1 to _sh.ColCount - 1 do
        _sh.Cell[j, i].Data := IntToStr(i*j);
    end;

    for i := 1 to 8 do
      tz.Sheets[i].Assign(_sh);

    tz.Sheets[0].Title := 'Frozen hor';
    tz.Sheets[1].Title := 'Frozen vert';
    tz.Sheets[2].Title := 'Frozen hor + vert';

    tz.Sheets[3].Title := 'Split hor';
    tz.Sheets[4].Title := 'Split vert';
    tz.Sheets[5].Title := 'Split hor + vert';

    tz.Sheets[6].Title := 'Split hor and Frozen vert';
    tz.Sheets[7].Title := 'Split vert and Frozen hor';
    tz.Sheets[8].Title := 'no split and frozen';

    //Columns/rows count
    _SetSplit(0, ZSplitFrozen, ZSplitNone, 1, 0);
    _SetSplit(1, ZSplitNone, ZSplitFrozen, 0, 1);
    _SetSplit(2, ZSplitFrozen, ZSplitFrozen, 1, 1);

    //Pixels
    _SetSplit(3, ZSplitSplit, ZSplitNone, 300, 0);
    _SetSplit(4, ZSplitNone, ZSplitSplit, 0, 300);
    _SetSplit(5, ZSplitSplit, ZSplitSplit, 300, 300);

    //Only for Libre/Open office
    _SetSplit(6, ZSplitSplit, ZSplitFrozen, 300, 1);
    _SetSplit(7, ZSplitFrozen, ZSplitSplit, 1, 300);
    _SetSplit(8, ZSplitNone, ZSplitNone, 0, 0);

    _path := ExtractFilePath(Paramstr(0));

    _WriteTime('Prepare');

    SaveXmlssToEXML(tz, _path + 'split_test.xml', [], [], nil, 'UTF-8');
    _WriteTime('save split_test.xml');

    SaveXmlssToODFS(tz, _path + 'split_test.ods', [], [], nil, 'UTF-8');
    _WriteTime('save split_test.ods');

    SaveXmlssToXLSX(tz, _path + 'split_test.xlsx', [], [], nil, 'UTF-8');
    _WriteTime('save split_test.xlsx');

    ReadEXMLSS(tz, _path + 'split_test.xml');
    _WriteTime('*read split_test.xml');

    SaveXmlssToEXML(tz, _path + 'split_test_rxml.xml', [], [], nil, 'UTF-8');
    _WriteTime('save split_test_rxml.xml');

    SaveXmlssToODFS(tz, _path + 'split_test_rxml.ods', [], [], nil, 'UTF-8');
    _WriteTime('save split_test_rxml.ods');

    SaveXmlssToXLSX(tz, _path + 'split_test_rxml.xlsx', [], [], nil, 'UTF-8');
    _WriteTime('save split_test_rxml.xlsx');

    ReadODFS(tz, _path + 'split_test.ods');
    _WriteTime('*read split_test.ods');

    SaveXmlssToEXML(tz, _path + 'split_test_rods.xml', [], [], nil, 'UTF-8');
    _WriteTime('save split_test_rods.xml');

    SaveXmlssToODFS(tz, _path + 'split_test_rods.ods', [], [], nil, 'UTF-8');
    _WriteTime('save split_test_rods.ods');

    SaveXmlssToXLSX(tz, _path + 'split_test_rods.xlsx', [], [], nil, 'UTF-8');
    _WriteTime('save split_test_rods.xlsx');




    SaveXmlssToEXML(tz, _path + 'split_test_rxlsx.xml', [], [], nil, 'UTF-8');
    _WriteTime('save split_test_rxlsx.xml');

    SaveXmlssToODFS(tz, _path + 'split_test_rxlsx.ods', [], [], nil, 'UTF-8');
    _WriteTime('save split_test_rxlsx.ods');

    SaveXmlssToXLSX(tz, _path + 'split_test_rxlsx.xlsx', [], [], nil, 'UTF-8');
    _WriteTime('save split_test_rxlsx.xlsx');   *)

  finally
    if (Assigned(tz)) then
      FreeAndNil(tz)
  end;
end;

end.


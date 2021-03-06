{
implementation: function for ODS zip (delphi)
usign Synopse http://synopse.info
}

//��������� �������� � ������� Open Document
//INPUT
//  var XMLSS: TZEXMLSS                   - ���������
//      FileName: string                  - ��� ����� ��� ����������
//  const SheetsNumbers:array of integer  - ������ ������� ������� � ������ ������������������
//  const SheetsNames: array of string    - ������ �������� �������
//                                          (���������� ��������� � ���� �������� ������ ���������)
//      TextConverter: TAnsiToCPConverter - ���������
//      CodePageName: string              - ��� ���������
//      BOM: ansistring                   - Byte Order Mark
//RETURN
//      integer
function SaveXmlssToODFS(var XMLSS: TZEXMLSS; FileName: string; const SheetsNumbers:array of integer;
                         const SheetsNames: array of string; TextConverter: TAnsiToCPConverter; CodePageName: string; BOM: ansistring = ''): integer; overload;
var
  tmpdir: string;
  b: boolean;
  _isCreated: boolean;

begin
  _isCreated := false;
  try
    _isCreated := ZECreateUniqueTmpDir(ZEGetTempDir(), tmpdir);
    result := SaveXmlssToODFSPath(XMLSS, tmpdir, SheetsNumbers, SheetsNames, TextConverter, CodePageName, BOM);
    b := ZEZipPackSyn(FileName, tmpdir);
    if (not b) then
      result := -1;
  finally
    if (_isCreated) then
      ZEDelTree(tmpdir);
  end;
end; //SaveXmlssToODFS

//��������� �������� � ������� Open Document
//INPUT
//  var XMLSS: TZEXMLSS                   - ���������
//      FileName: string                  - ��� ����� ��� ����������
//  const SheetsNumbers:array of integer  - ������ ������� ������� � ������ ������������������
//  const SheetsNames: array of string    - ������ �������� �������
//                                          (���������� ��������� � ���� �������� ������ ���������)
//RETURN
//      integer
function SaveXmlssToODFS(var XMLSS: TZEXMLSS; FileName: string; const SheetsNumbers:array of integer;
                         const SheetsNames: array of string): integer; overload;
begin
  result := SaveXmlssToODFS(XMLSS, FileName, SheetsNumbers, SheetsNames, ZEGetDefaultUTF8Converter(), 'UTF-8', '');
end; //SaveXmlssToODFS

//��������� �������� � ������� Open Document
//INPUT
//  var XMLSS: TZEXMLSS                   - ���������
//      FileName: string                  - ��� ����� ��� ����������
//RETURN
//      integer
function SaveXmlssToODFS(var XMLSS: TZEXMLSS; FileName: string): integer; overload;
begin
  result := SaveXmlssToODFS(XMLSS, FileName, [], []);
end; //SaveXmlssToODFS

//������ ODS
//INPUT
//  var XMLSS: TZEXMLSS - ���������
//  FileName: string    - ��� �����
//RETURN
//      integer - ����� ������ (0 - �� OK)
function ReadODFS(var XMLSS: TZEXMLSS; FileName: string): integer;
var
  tmpdir: string;
  b: boolean;
  _isCreated: boolean;

begin
  result := 0;
  _isCreated := false;
  try
    _isCreated := ZECreateUniqueTmpDir(ZEGetTempDir(), tmpdir);
    b := ZEZipUnpackSyn(FileName, tmpdir);
    if (b) then
      result := ReadODFSPath(XMLSS, tmpdir)
    else
      result := -1;
  finally
    if (_isCreated) then
      ZEDelTree(tmpdir);
  end;
end; //ReadODFS

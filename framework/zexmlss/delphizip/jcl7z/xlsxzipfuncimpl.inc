{
implementation: function for XLSX zip (delphi)
using JEDI Code library (via www.7-zip.org) : http://sourceforge.net/projects/jcl/
}

//��������� �������������� �������� � ������� Office Open XML (OOXML)
//INPUT
//  var XMLSS: TZEXMLSS                   - ���������
//      PathName: string                  - ���� � ���������� ��� ���������� (������ ������������ ������������ ����������)
//  const SheetsNumbers:array of integer  - ������ ������� ������� � ������ ������������������
//  const SheetsNames: array of string    - ������ �������� �������
//                                          (���������� ��������� � ���� �������� ������ ���������)
//      TextConverter: TAnsiToCPConverter - ���������
//      CodePageName: string              - ��� ���������
//      BOM: ansistring                   - Byte Order Mark
//RETURN
//      integer
function SaveXmlssToXLSX(var XMLSS: TZEXMLSS; FileName: string; const SheetsNumbers:array of integer;
                         const SheetsNames: array of string; TextConverter: TAnsiToCPConverter; CodePageName: string; BOM: ansistring = ''): integer; overload;
var
  tmpdir: string;
  b: boolean;
  _isCreated: boolean;

begin
  _isCreated := false;
  try
    _isCreated := ZECreateUniqueTmpDir(ZEGetTempDir(), tmpdir);
    result := SaveXmlssToXLSXPath(XMLSS, tmpdir, SheetsNumbers, SheetsNames, TextConverter, CodePageName, BOM);
    b := ZEZipPackJCL7z(FileName, tmpdir);
    if (not b) then
      result := -1;
  finally
    if (_isCreated) then
      ZEDelTree(tmpdir);
  end;
end; //SaveXmlssToODFS

//��������� �������� � ������� Open Office XML (xlsx)
//INPUT
//  var XMLSS: TZEXMLSS                   - ���������
//      FileName: string                  - ��� ����� ��� ����������
//  const SheetsNumbers:array of integer  - ������ ������� ������� � ������ ������������������
//  const SheetsNames: array of string    - ������ �������� �������
//                                          (���������� ��������� � ���� �������� ������ ���������)
//RETURN
//      integer
function SaveXmlssToXLSX(var XMLSS: TZEXMLSS; FileName: string; const SheetsNumbers: array of integer;
                         const SheetsNames: array of string): integer; overload;
begin
  result := SaveXmlssToXLSX(XMLSS, FileName, SheetsNumbers, SheetsNames, ZEGetDefaultUTF8Converter(), 'UTF-8', '');
end; //SaveXmlssToXLSX

//��������� �������� � ������� Open Office XML (xlsx)
//INPUT
//  var XMLSS: TZEXMLSS                   - ���������
//      FileName: string                  - ��� ����� ��� ����������
//RETURN
//      integer
function SaveXmlssToXLSX(var XMLSS: TZEXMLSS; FileName: string): integer; overload;
begin
  result := SaveXmlssToXLSX(XMLSS, FileName, [], []);
end; //SaveXmlssToXLSX

//������ xlsx
//INPUT
//  var XMLSS: TZEXMLSS - ���������
//  FileName: string    - ��� �����
//RETURN
//      integer - ����� ������ (0 - �� OK)
function ReadXLSX(var XMLSS: TZEXMLSS; FileName: string): integer;
var
  tmpdir: string;
  b: boolean;
  _isCreated: boolean;

begin
  result := 0;
  _isCreated := false;
  try
    _isCreated := ZECreateUniqueTmpDir(ZEGetTempDir(), tmpdir);
    b := ZEZipUnpackJCL7z(FileName, tmpdir);
    if (b) then
      result := ReadXLSXPath(XMLSS, tmpdir)
    else
      result := -1;
  finally
    if (_isCreated) then
      ZEDelTree(tmpdir);
  end;
end; //ReadODFS

(****************************************)
(*                                      *)
(* Редатор для свойства ImageFile       *)
(****************************************)

unit ImgFlProp;

interface

uses Classes, DesignEditors, DesignIntf;

type

{ TImgFileProperty }

  TImgFileProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  procedure Register;

implementation
uses SysUtils, Controls, Forms, ExtDlgs;

function TImgFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

procedure TImgFileProperty.Edit;
var
  OpenDialog: TOpenPictureDialog;
  s : string;
begin
  OpenDialog := TOpenPictureDialog.Create(Application);
  s:=GetStrValue;
  try
    OpenDialog.Filter := 'BMP-файлы|*.bmp|JPG-файлы|*.jpg|Все файлы|*.*';
    OpenDialog.FileName:= s;
    OpenDialog.FilterIndex := 0;
    if OpenDialog.Execute then begin
      s := OpenDialog.FileName;
      SetStrValue(s);
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(String), TControl, 'ImageFile', TImgFileProperty);
end;

end.

unit fPzl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Puzzle, StdCtrls;

type
  TForm1 = class(TForm)
    Puzzle1: TPuzzle;
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Puzzle1GameOver(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Puzzle1.Shuffle;
end;

procedure TForm1.Puzzle1GameOver(Sender: TObject);
begin
  Application.MessageBox('Игра закончена','')
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Puzzle1.Visible := False;
  Puzzle1.ImageFile := 'Puzzle.bmp';
  Puzzle1.PuzzleSize := 4;
  Puzzle1.ShowNumber := False;
  CheckBox2.Checked := False;
  Puzzle1.Visible := True;
  Puzzle1.Repaint;
  Height := Puzzle1.Height+53;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Puzzle1.Visible := False;
  Puzzle1.ImageFile := '8.jpg';
  Puzzle1.PuzzleSize := 5;
  Puzzle1.ShowNumber := True;
  CheckBox2.Checked := True;
  Puzzle1.Visible := True;
  Puzzle1.Repaint;
  Height := Puzzle1.Height+53;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  Puzzle1.ShowGrid := TCheckBox(Sender).Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  Puzzle1.ShowNumber := TCheckBox(Sender).Checked;
end;

end.

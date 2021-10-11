
unit Puzzle;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type

  TPicPuzzleImages = array of TBitmap;

  TPuzzleState = array of integer;

  TPuzzle = class(TGraphicControl)
  private
    { Private declarations }
    FImageFile : string;
    FPuzzleSize : Byte;
    FGridColor : TColor;
    FShowGrid : boolean;
    FShowNumber: Boolean;
    FNumberFont: TFont;
    Loaded : boolean;
    Image: TBitmap;       //изображение для разбиения
    CellX, CellY : integer;
    FImages : TPicPuzzleImages;
    State : TPuzzleState;
    lngArr : integer;     //Размер массива элементов
    FGameOver : TNotifyEvent;

    procedure SetPuzzleSize(x : Byte);
    procedure SetFileName(x : string);
    procedure SetGridColor(x : TColor);
    procedure SetShowGrid(x : boolean);
    procedure SetNumberFont(x: TFont);
    procedure SetShowNumber(x: Boolean);
    procedure ResetImages;
    procedure CleareState;
    procedure CutImages;
    procedure UserClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Shuffle;
    procedure SaveToFile(FlName: String);
    procedure LoadFromFile(FlName: String);
  published
    property Align;
    property Visible;
    property ImageFile : string read FImageFile write SetFileName;
    property PuzzleSize : Byte read FPuzzleSize write SetPuzzleSize;
    property GridColor : TColor read FGridColor write SetGridColor;
    property NumberFont : TFont read FNumberFont write SetNumberFont;
    property ShowNumber : boolean read FShowNumber write SetShowNumber;
    property ShowGrid : boolean read FShowGrid write SetShowGrid;
    property OnGameOver : TNotifyEvent read FGameOver write FGameOver;
  end;

procedure Register;

implementation
uses JPeg;

{$R *.RES}

constructor TPuzzle.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Height := 160;
  Width := 160;
  PuzzleSize := 4;
  FNumberFont := TFont.Create;
  Image := TBitmap.Create;
  OnMouseDown := UserClick;
end;

destructor TPuzzle.Destroy;
begin
  ResetImages;
  Image.Free;
  FNumberFont.Free;
  inherited;
end;

procedure TPuzzle.UserClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  x1, y1, a, m, ad : integer;
  GameOver : boolean;
begin
  if (Button = mbLeft) and Loaded then begin
    x := x div CellX;   //Координаты
    y := y div CellY;
    m := FPuzzleSize - 1;
    a := y * FPuzzleSize + x; //Индех массива
    ad := -1;
    if a > lngArr - 1 then Exit;
    if x < m then {вправо?}
      if State[a + 1] = lngArr - 1 then
      begin
        ad := a + 1;
        x1 := x+1;
        y1 := y;
      end;
    if x > 0 then {влево?}
      if State[a - 1] = lngArr - 1 then
      begin
        ad := a - 1;
        x1 := x-1;
        y1 := y;
      end;
    if y < m then {вниз?}
      if State[a + m + 1] = lngArr - 1 then
      begin
        ad := a + m + 1;
        x1 := x;
        y1 := y+1;
      end;
    if y > 0 then {вверх?}
      if State[a - m - 1] = lngArr - 1 then
      begin
        ad := a - m - 1;
        x1 := x;
        y1 := y-1;
      end;
    if ad <> -1 then
    begin
      m := State[a];
      State[a] := State[ad];
      State[ad] := m;
      with inherited Canvas do
      begin
        //Меняем местами квадратики
        if FShowGrid then       //сетка есть
          Draw(x * (CellX+1), y * (CellY+1), FImages[state[a]])
        else                    //сетки нет
          Draw(x * CellX, y * CellY, FImages[state[a]]);
        if FShowGrid then       //сетка есть
          Draw(x1 * (CellX+1), y1 * (CellY+1), FImages[state[ad]])
        else                    //сетки нет
          Draw(x1 * CellX, y1 * CellY, FImages[state[ad]]);
        end;
        {Проверка окончания игры}
        GameOver := True;
        a := 0;
        while GameOver and (a < lngArr) do
          if State[a] = a then
            inc(a)
          else GameOver := FALSE;
        if GameOver then
          if Assigned(FGameOver)
            then FGameOver(self);
      end;
  end;
end;

procedure TPuzzle.Paint;
var
  i, j : byte;
  R: TRect;
  Tmp: TBitmap;
begin
  inherited Paint;
  { Пунктирная рамка, если компонент - в процессе проектирования }
  if csDesigning in ComponentState then
  with inherited Canvas do
  begin
    Pen.Style := psDash;
    Brush.Style := bsClear;
    Rectangle(0, 0, Width, Height);
  end;

  if Loaded then
  with inherited Canvas do
  begin
    Tmp := TBitmap.Create;
    //Определить размер поля для рисования
    if FShowGrid then begin     //сетка есть
      Width := (CellX*FPuzzleSize)+FPuzzleSize-1;
      Height := (CellY*FPuzzleSize)+FPuzzleSize-1;
    end else
    begin                       //сетки нет
      Width :=  CellX*FPuzzleSize;
      Height := CellY*FPuzzleSize;
    end;
    Tmp.Width := Width;
    Tmp.Height := Height;
    Tmp.Canvas.Brush.Color := FGridColor;
    R.Left := 0; R.Top := 0;
    R.Right := Width; R.Bottom := Height;
    Tmp.Canvas.FillRect(R);
    //Рисование фишек
    for i := 0 to FPuzzleSize - 1 do
     for j := 0 to FPuzzleSize - 1 do
        if FShowGrid then       //сетка есть
          Tmp.Canvas.Draw(j * (CellX+1), i * (CellY+1), FImages[state[i * FPuzzleSize + j]])
        else
          Tmp.Canvas.Draw(j * CellX, i * CellY, FImages[state[i * FPuzzleSize + j]]);
    Draw(0,0,Tmp);
    Tmp.Free;
  end;
end;

procedure TPuzzle.SetPuzzleSize(x : Byte);
begin
  FPuzzleSize := x;
  ResetImages;
  SetLength(FImages,sqr(FPuzzleSize));
  SetLength(State,sqr(FPuzzleSize));
  lngArr := sqr(FPuzzleSize);
  CleareState;
  CutImages;
end;

procedure TPuzzle.SetFileName(x : string);
Var
  JP : TJpegImage;
  ext: String;
begin
  ResetImages;
  CleareState;
  CleareState;
  Loaded := False;
  ext := AnsiLowerCase(ExtractFileExt(x));
  if FileExists(x) then
  begin
    if ext = '.jpg' then begin
      JP := TJpegImage.Create;
      JP.LoadFromFile(x);
      Image.Assign(JP);
      JP.Free;
    end else if ext = '.bmp' then
      Image.LoadFromFile(x);
    Loaded := TRUE;
    FImageFile := x;
    CutImages;
  end
  else begin
    FImageFile := '';
    Repaint;
  end;
end;

procedure TPuzzle.SetGridColor(x : TColor);
begin
  FGridColor := x;
  Repaint;
end;

procedure TPuzzle.SetShowGrid(x : boolean);
begin
  FShowGrid := x;
  Repaint;
end;

procedure TPuzzle.SetShowNumber(x: Boolean);
begin
  FShowNumber := x;
  ResetImages;
  CutImages;
end;

{-Очистка массива элементов }
procedure TPuzzle.ResetImages;
var
  i : byte;
begin
  if Loaded then
    for i := 0 to Length(FImages)-1 do
      FImages[i].Free;
end;
{ -Упорядочивание массива State }
procedure TPuzzle.CleareState;
var
  i: byte;
begin
  for i := 0 to lngArr - 1 do
    State[i] := i;
end;

{-Разбиение картинки на элементы }
procedure TPuzzle.CutImages;
var
  i, j : byte;
  s, d : TRect;
begin
  if Loaded and (FPuzzleSize > 0) then begin //Определяем размер элементов
    CellX := Image.Width div FPuzzleSize;
    CellY := Image.Height div FPuzzleSize;
    if (CellX > 0) and (CellY > 0) then
    begin
      with d do begin
        Top := 0; Left := 0;
        Bottom := CellY; Right := CellX;
      end;
      for i := 0 to FPuzzleSize - 1 do
        for j := 0 to FPuzzleSize - 1 do begin
          FImages[i * FPuzzleSize + j] := TBitmap.Create;
          with s do begin
            Top := i * CellY;
            Left := j * CellX;
            Bottom := Top + CellY;
            Right := Left + CellX;
          end;
          FImages[i * FPuzzleSize + j].Width := CellX;
          FImages[i * FPuzzleSize + j].Height := CellY;
          if (i = FPuzzleSize-1) and (j = FPuzzleSize-1) then   //Пустой квадрат
          begin
            FImages[i * FPuzzleSize + j].Canvas.Brush.Color := FGridColor; //clBlack;
//            FImages[i * FPuzzleSize + j].Canvas.Pen.Color := FGridColor;
//            FImages[i * FPuzzleSize + j].Canvas.Rectangle(0,0,CellX,CellY);
            FImages[i * FPuzzleSize + j].Canvas.FillRect(Rect(0,0,CellX,CellY));
          end else
          begin                                                 //элемент
            FImages[i * FPuzzleSize + j].Canvas.CopyRect(d, Image.Canvas, s);
            if FShowNumber then begin
              FImages[i * FPuzzleSize + j].Canvas.Brush.Style := bsClear;
              FImages[i * FPuzzleSize + j].Canvas.Font := FNumberFont;
              FImages[i * FPuzzleSize + j].Canvas.TextOut(0, 0, IntToStr((i * FPuzzleSize + j)+1));
            end;
          end;
        end;
    end;
    Repaint;
  end;
end;
{-Перемешать элементы }
procedure TPuzzle.Shuffle;
var
  i, a, b, c : integer;
begin
  Randomize;
  for i := 0 to lngArr-1 do
  begin
    repeat
      a := Random(LngArr);
    until ((a > i) or (a < i)) and (a >= 0);
    b := state[i];
    c := state[a];
    state[i] := c;
    state[a] := b;
  end; //for
  Repaint;
end;

procedure TPuzzle.SaveToFile(FlName: String);
Var
  Fl: TFileStream;
  i: Integer;
begin
  Fl := TFileStream.Create(FlName,fmCreate);
  try
    with TWriter.Create(Fl, 4096) do
    try
      WriteString(FImageFile);
      WriteInteger(FPuzzleSize);
      WriteBoolean(FShowGrid);
      WriteBoolean(FShowNumber);
      for i := 0 to lngArr - 1 do
        WriteInteger(State[i]);
    finally
      Free;
    end;
  finally
    Fl.Free;
  end
end;

procedure TPuzzle.LoadFromFile(FlName: String);
Var
  Fl: TFileStream;
  i: Integer;
begin
  Fl := TFileStream.Create(FlName, fmOpenRead);
  try
    with TReader.Create(Fl, 4096) do
    try
      Visible := False;
      ImageFile := ReadString;
      PuzzleSize := ReadInteger;
      ShowGrid := ReadBoolean;
      ShowNumber := ReadBoolean;
      ResetImages;
      SetLength(FImages,sqr(FPuzzleSize));
      SetLength(State,sqr(FPuzzleSize));
      lngArr := sqr(FPuzzleSize);
      for i := 0 to lngArr - 1 do
        State[i] := ReadInteger;
      CutImages;
      Visible := True;
    finally
      Free;
    end;
  finally
    Fl.Free;
  end;
end;

procedure TPuzzle.SetNumberFont(x: TFont);
begin
  FNumberFont.Assign(x);
  if FShowNumber then
  begin
    ResetImages;
    CutImages;
  end;
end;

procedure Register;
begin
  RegisterComponents('VCLGames', [TPuzzle]);
end;

end.

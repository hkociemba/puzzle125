unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

  Point = record
    x, y, z: Byte; // try Integer also
  end;

  Piece = array [0 .. 4] of Point;

  BigInteger = record
    b: array [0 .. 1] of UINT64;
  end;

var
  Form1: TForm1;
  pieces: array [0 .. 959] of Piece; // Alle möglichen pieces
  pieceHash: array [0 .. 959] of BigInteger;
  pcnt: Integer;
  pieceIndexOfPos: array [0 .. 124, 0 .. 71] of Int16; // 71 is enough
  pieceIndexOfPosMx: array [0 .. 124] of Int8;
  varnameToPieceIdx: array [1 .. 4800] of Int16;
  varname: array [0 .. 124, 0 .. 71] of Int16;
  n_clauses: Integer;
  clauses: TSTringList;

const
  p0: Piece = ((x: 0; y: 0; z: 0), (x: 1; y: 0; z: 0), (x: 2; y: 0; z: 0),
    (x: 3; y: 0; z: 0), (x: 1; y: 1; z: 0));
  p1: Piece = ((x: 0; y: 0; z: 0), (x: 1; y: 0; z: 0), (x: 2; y: 0; z: 0),
    (x: 3; y: 0; z: 0), (x: 2; y: 1; z: 0));
  p2: Piece = ((x: 0; y: 1; z: 0), (x: 1; y: 1; z: 0), (x: 2; y: 1; z: 0),
    (x: 3; y: 1; z: 0), (x: 1; y: 0; z: 0));
  p3: Piece = ((x: 0; y: 1; z: 0), (x: 1; y: 1; z: 0), (x: 2; y: 1; z: 0),
    (x: 3; y: 1; z: 0), (x: 2; y: 0; z: 0));

  p4: Piece = ((x: 0; y: 0; z: 0), (x: 1; y: 0; z: 0), (x: 2; y: 0; z: 0),
    (x: 3; y: 0; z: 0), (x: 1; y: 0; z: 1));
  p5: Piece = ((x: 0; y: 0; z: 0), (x: 1; y: 0; z: 0), (x: 2; y: 0; z: 0),
    (x: 3; y: 0; z: 0), (x: 2; y: 0; z: 1));
  p6: Piece = ((x: 0; y: 0; z: 1), (x: 1; y: 0; z: 1), (x: 2; y: 0; z: 1),
    (x: 3; y: 0; z: 1), (x: 1; y: 0; z: 0));
  p7: Piece = ((x: 0; y: 0; z: 1), (x: 1; y: 0; z: 1), (x: 2; y: 0; z: 1),
    (x: 3; y: 0; z: 1), (x: 2; y: 0; z: 0));

implementation

uses console, StrUtils;
{$R *.dfm}

function printPiece(n: Integer): String;
var
  pc: Piece;
  pt: Point;
  s: String;
  i: Integer;
begin
  s:='';
  pc := pieces[n];
  for i := 0 to 4 do
  begin
   pt:=pc[i];
   s:=s+'('+IntToStr(pt.x)+','+IntToStr(pt.y)+','+IntToStr(pt.z)+')';
   if i<>4 then s:=s+',';
  end;
  Result:=s;
end;

function pointToPos(pt: Point): Integer;
begin
  Result := pt.x;
  Result := Result * 5;
  Inc(Result, pt.y);
  Result := Result * 5;
  Inc(Result, pt.z)
end;

function posToPoint(pos: Integer): Point;
begin
  Result.z := pos mod 5;
  pos := pos div 5;
  Result.y := pos mod 5;
  Result.x := pos div 5;
end;

function translatePiece(pc: Piece; x, y, z: Integer): Piece;
var
  i: Integer;
begin
  for i := 0 to 4 do
  begin
    Result[i].x := pc[i].x + x;
    Result[i].y := pc[i].y + y;
    Result[i].z := pc[i].z + z;
  end;
end;

function rotatePiece(pc: Piece): Piece;
var
  i: Integer;
begin
  for i := 0 to 4 do
  begin
    Result[i].y := pc[i].x;
    Result[i].z := pc[i].y;
    Result[i].x := pc[i].z;
  end;
end;

function collideQ(n1, n2: Int16): Boolean;
var
  pidx1, pidx2: Int16;
begin
  pidx1 := varnameToPieceIdx[n1];
  pidx2 := varnameToPieceIdx[n2];
  if (pieceHash[pidx1].b[0] = pieceHash[pidx2].b[0]) and
    (pieceHash[pidx1].b[1] = pieceHash[pidx2].b[1]) then
    Exit(false); // Identical pieces do not collide
  if (pieceHash[pidx1].b[0] and pieceHash[pidx2].b[0] = 0) and
    (pieceHash[pidx1].b[1] and pieceHash[pidx2].b[1] = 0) then
    Exit(false);
  Result := true;
end;

function custom_split(input: string): TArray<string>;
var
  delimiterSet: array [0 .. 0] of char;
  // split works with char array, not a single char
begin
  delimiterSet[0] := ' '; // some character
  Result := input.Split(delimiterSet);
end;

procedure TForm1.FormCreate(Sender: TObject);

var
  i, j, k, n, fix: Integer;
  p: Point;
  pc1, pc2: Piece;
  base, offset, ps: Integer;
  s: String;
begin
  pcnt := 0;
  for i := 0 to 1 do
    for j := 0 to 3 do
      for k := 0 to 4 do
      begin
        pieces[pcnt] := translatePiece(p0, i, j, k);
        if (i = 0) and (j = 2) and (k = 2) then
          Memo1.Lines.Add(IntToStr(pcnt));
        // shows that pieces with pcnt=48 or 49 must be used
        Inc(pcnt);
        pieces[pcnt] := translatePiece(p1, i, j, k);
        Inc(pcnt);
        pieces[pcnt] := translatePiece(p2, i, j, k);
        Inc(pcnt);
        pieces[pcnt] := translatePiece(p3, i, j, k);
        Inc(pcnt);
      end;

  for i := 0 to 1 do
    for j := 0 to 4 do
      for k := 0 to 3 do
      begin
        pieces[pcnt] := translatePiece(p4, i, j, k);
        Inc(pcnt);
        pieces[pcnt] := translatePiece(p5, i, j, k);
        Inc(pcnt);
        pieces[pcnt] := translatePiece(p6, i, j, k);
        Inc(pcnt);
        pieces[pcnt] := translatePiece(p7, i, j, k);
        Inc(pcnt);
      end;

  for i := 0 to 320 - 1 do
  begin
    pieces[pcnt] := rotatePiece(pieces[i]);
    Inc(pcnt);
  end;
  for i := 320 to 640 - 1 do
  begin
    pieces[pcnt] := rotatePiece(pieces[i]);
    Inc(pcnt);
  end;

  for i := 0 to 959 do // set hashes for pieces
  begin
    pieceHash[i].b[0] := 0;
    pieceHash[i].b[1] := 0;
    for j := 0 to 4 do
    begin
      p := pieces[i, j];
      ps := pointToPos(p);
      base := ps div 64;
      offset := ps mod 64;
      pieceHash[i].b[base] := pieceHash[i].b[base] or (UINT64(1) shl offset);
    end;
  end;

  for i := 0 to 124 do
    pieceIndexOfPosMx[i] := -1;

  fix := 48; // for 48, 49
  // for each position i store the pieceIndices which are possible
  for i := 0 to 124 do
  begin
    for j := 0 to 959 do
    begin
      // do not use pieces which intersect piece 48/49
      if not((pieceHash[j].b[0] = pieceHash[fix].b[0]) and
        (pieceHash[j].b[1] = pieceHash[fix].b[1])) and
        ((pieceHash[j].b[0] and pieceHash[fix].b[0] <> 0) or
        (pieceHash[j].b[1] and pieceHash[fix].b[1] <> 0)) then
        continue;
      for k := 0 to 4 do
      begin
        if pointToPos(pieces[j, k]) = i then
        begin
          Inc(pieceIndexOfPosMx[i]);
          pieceIndexOfPos[i, pieceIndexOfPosMx[i]] := j;
        end;

      end;
    end;
  end;

  for i := 0 to 124 do
    for j := 0 to 71 do
      varname[i, j] := -1;
  k := 1;
  for i := 0 to 124 do
  begin
    for j := 0 to 71 do
    begin
      if j > pieceIndexOfPosMx[i] then
        break;
      varname[i, j] := k;
      varnameToPieceIdx[k] := pieceIndexOfPos[i, j];
      Inc(k);
    end;
  end;

  // cnf erzeugen
  clauses := TSTringList.Create;

  n_clauses := 0;
  clauses.Add('c CNF file in DIMACS format');
  clauses.Add('dummy');
  // clauses.Strings[1]
  // for each position at least one of the possible pieces is set
  for i := 0 to 124 do
  begin
    s := '';
    for j := 0 to pieceIndexOfPosMx[i] do
      s := s + IntToStr(varname[i, j]) + ' ';
    clauses.Add(s + '0');
    Inc(n_clauses);
  end;

  // now add the clauses for incompatible variables (intersecting pieces)
  for i := 1 to 4800 - 1 do
  begin
    for j := i + 1 to 4800 do
      if collideQ(i, j) then
      begin
        clauses.Add('-' + IntToStr(i) + ' -' + IntToStr(j) + ' 0');
        Inc(n_clauses);
      end;
  end;

  clauses.Strings[1] := 'p cnf ' + IntToStr(4800) + ' ' + IntToStr(n_clauses);
  clauses.SaveToFile('cnf.txt'); // Initial cnf file
  // run SAT solver, loop until no more solutions
  // repeat
  // GetConsoleOutput('java.exe -server  -jar org.sat4j.core.jar cnf.txt',
  // output, errors);
  //
  // solution_raw := '';
  // for i := 0 to output.Count - 1 do
  // begin
  // s := output.Strings[i];
  // if s[1] = 's' then
  // begin
  // if ContainsText(s, 'UNSATISFIABLE') then
  // begin
  // Memo1.Lines.Add('No more solutions');
  // Exit;
  // end;
  // end;
  //
  // if (s[1] = 'c') and ContainsText(s, 'Total wall clock time') then
  // Memo1.Lines.Add(copy(s, 3, Length(s)));
  //
  // if s[1] = 'v' then
  // begin
  // solution_raw := solution_raw + copy(s, 3, Length(s));
  // break;
  // end;
  // end;
  // solution_split := custom_split(solution_raw);
  // s := '';
  // for i := 0 to Length(solution_split) - 1 do
  // try
  // n := StrToInt(solution_split[i]);
  // if n > 0 then
  // begin
  // s := s + IntToStr(varnameToPieceIdx[n]) + ' ';
  // end;
  // except
  // on EConvertError do
  // end;
  // Memo1.Lines.Add(s);
  // Application.ProcessMessages;
  //
  // clauses.Add(solution_raw);
  // Inc(n_clauses);
  // clauses.Strings[1] := 'p cnf ' + IntToStr(4800) + ' ' + IntToStr(n_clauses);
  // clauses.SaveToFile('cnf.txt');
  // until false;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i, n, cnt: Integer;
  s, solution_raw, negated: String;
  output, errors: TSTringList;
  solution_split: TArray<String>;
  used: array [0 .. 959] of Boolean;
begin
  errors := TSTringList.Create;
  output := TSTringList.Create;
  cnt := 0;
  repeat
    GetConsoleOutput('java.exe -server  -jar org.sat4j.core.jar cnf.txt',
      output, errors);

    solution_raw := '';
    for i := 0 to output.Count - 1 do
    begin
      s := output.Strings[i];
      if s[1] = 's' then
      begin
        if ContainsText(s, 'UNSATISFIABLE') then
        begin
          Memo1.Lines.Add('No more solutions');
          Exit;
        end;
      end;

      if (s[1] = 'c') and ContainsText(s, 'Total wall clock time') then
        Memo1.Lines.Add(copy(s, 3, Length(s)));

      if s[1] = 'v' then
        solution_raw := solution_raw + copy(s, 3, Length(s));
    end;
    solution_split := custom_split(solution_raw);
    s := '';
    negated := '';
    for i := 0 to 959 do
      used[i] := false;

    for i := 0 to Length(solution_split) - 1 do
      try
        n := StrToInt(solution_split[i]);
        if n > 0 then
        begin
          if not used[varnameToPieceIdx[n]] then
          begin
            used[varnameToPieceIdx[n]] := true;
            //s := s + IntToStr(varnameToPieceIdx[n]) + ' ';
            //s:=s+ printPiece(varnameToPieceIdx[n]);
            Memo1.Lines.Add(printPiece(varnameToPieceIdx[n]));
          end;
          negated := negated + '-' + IntToStr(n) + ' ';
        end;
      except
        on EConvertError do
      end;
    //Memo1.Lines.Add(s);
     Memo1.Lines.Add('');
    negated := negated + '0';
    Application.ProcessMessages;

    clauses.Add(negated);
    Inc(n_clauses);
    clauses.Strings[1] := 'p cnf ' + IntToStr(4800) + ' ' + IntToStr(n_clauses);
    clauses.SaveToFile('cnf.txt');
    Inc(cnt);
  until cnt = 100;

end;

end.

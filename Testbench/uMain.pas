unit uMain;

{ ====================================================================================
  Projekt: PYEKVS (Pye-Key-Value-Storage)
  Description: simple key value serialization format
  Compiler: Embarcadero Delphi
  Author: Michael Koch (kxMaxx)
  License: MIT
  ====================================================================================
  Pascal API implementation:
    see Readme.md
  ====================================================================================
  uMain is a test environment to
  - create pyeKVS document
  - show pyeKVS document in memo (simple or JSON)
  - write pyeKVS document to file (stream)
  - read pyeKVS document from file
  - copy pyeKVS document to an other pyeKVS document (stream copy)
  ==================================================================================== }

interface

uses
    Winapi.Windows,
    Winapi.Messages,
    System.SysUtils,
    System.Variants,
    System.Classes,
    System.Math,
    System.Diagnostics,
    System.IOUtils,
    Vcl.Graphics,
    Vcl.Controls,
    Vcl.Forms,
    Vcl.Dialogs,
    Vcl.StdCtrls,
    Vcl.ExtCtrls,
    Vcl.ComCtrls,
    pyeKVS;

type
    TMain=class(TForm)
        PTop: TPanel;
        PMemo: TPanel;
        Memo: TMemo;
        StatusBar: TStatusBar;
        BWriteFile: TButton;
        BReadFile: TButton;
        BStreamCopy: TButton;
        RGLog: TRadioGroup;
    PButton1: TPanel;
    BNewRnd: TButton;
    PButton2: TPanel;
    BNewHandMade: TButton;
    BNewSimple1: TButton;
        procedure BNewRndClick(Sender: TObject);
        procedure BWriteFileClick(Sender: TObject);
        procedure BReadFileClick(Sender: TObject);
        procedure BStreamCopyClick(Sender: TObject);
        procedure BNewHandMadeClick(Sender: TObject);
        procedure RGLogClick(Sender: TObject);
    procedure BNewSimple1Click(Sender: TObject);
    private
        FPYEKVS   : TPYEKVSDocument;
        FStopWatch: TStopWatch;
        procedure DoRndFill();
        procedure DoRndFillLoop1();
        procedure DoStreamCopy();
        procedure RandomItemsAdd(const aBase: TPYEKVSList; const aItemCount: Integer);
        procedure UpdateAbout;
        procedure UpdateMemo();
        procedure UpdateStatusBar;

        procedure PyeKVS_Save();
        procedure PyeKVS_Load();
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
    end;

var
    Main: TMain;

implementation

{$R *.dfm}

var
    cData : array [0..127] of Byte;
    cString: string;

constructor TMain.Create(AOwner: TComponent);
var i:Integer;
begin
    inherited;
    Randomize();
    FPYEKVS:=TPYEKVSDocument.Create();

    //init some memory
    for i:=Low(cData) to High(cData) do
        cData[i]:=(i and $FF);

    //init long string
    SetLength(cString, 100000);     //StringL more then 65535 chars
    for i:=1 to Length(cString) do
        cString[i]:=Char(i+32 and $FF);


    UpdateAbout();
end;

destructor TMain.Destroy;
begin
    FPYEKVS.Free();
    inherited;
end;

procedure TMain.UpdateAbout;
begin
    Memo.Lines.Add('pyeKVS is a binary key-value-storage format');
    Memo.Lines.Add('Michael Koch (kxMaxx); (c)2021; MIT license');
end;


procedure TMain.UpdateMemo;
var SL: TStringList;
begin
    SL:=TStringList.Create();
    try
        // write in Memo.Lines is to slow ... use local StringList

        if RGLog.ItemIndex=0 then begin
            FPYEKVS.ToStringsSimple(SL, '    ');
        end else begin
            FPYEKVS.ToStringsJSON(SL, '    ');
        end;

        // Update Memo
        Memo.Lines.Assign(SL);
    finally
        SL.Free;
    end;

end;

procedure TMain.UpdateStatusBar;
begin
    StatusBar.Panels[0].Text:=Format('RunTime: %dms', [FStopWatch.ElapsedMilliseconds]);
end;

procedure TMain.BNewRndClick(Sender: TObject);
begin
    FStopWatch:=TStopWatch.StartNew;

    DoRndFill();
//    DoRndFillLoop1();

    FStopWatch.Stop;
    UpdateMemo();
    UpdateStatusBar();
end;

procedure TMain.BNewSimple1Click(Sender: TObject);
var L1: TPYEKVSList;
begin
    FStopWatch:=TStopWatch.StartNew;

    //Clear PYEDocument
    FPYEKVS.Clear();

    //Start data collection
    FPYEKVS.PutBegin;

    FPYEKVS.PutInt('MyValue1', 256);                    //will be a pyeInt16
    FPYEKVS.PutString('MyString1', 'Hello PYES.');      //will be a pyeStringUTF8S (small string)

    //End data collection
    FPYEKVS.PutEnd;

    FStopWatch.Stop;
    UpdateMemo();
    UpdateStatusBar();
end;

procedure TMain.BNewHandMadeClick(Sender: TObject);
var L1: TPYEKVSList;
    A1: TPYEKVSArray;
    M1: TPYEKVSArrayMap;
    i128: TPYEKVSInt128;
    u128: TPYEKVSUInt128;
    Stream1:TStream;
    s: string;
begin
    FStopWatch:=TStopWatch.StartNew;

    FPYEKVS.Clear();

    FPYEKVS.PutBegin;

    // Fundamental values test ...

    L1:=FPYEKVS.PutList('List_ZeroTest');
    // Zero
    L1.PutZero('Zero');

    L1:=FPYEKVS.PutList('List_Boolean');
    // Bool
    L1.PutBool('Bool_false', false); // vtZero
    L1.PutBool('Bool_true', true);

    L1:=FPYEKVS.PutList('List_Integer');
    // Int8, Int16, Int32 (depend on value size)
    L1.PutInt('Int_Zero', 0);
    L1.PutInt('Int_Bool', 1);
    L1.PutInt('Int_Int8', 2);
    L1.PutInt('Int_Int16', High(Byte)+1);
    L1.PutInt('Int_Int32', High(Word)+1);

    L1:=FPYEKVS.PutList('List_Int64');
    // Int8, Int16, Int32, Int64 (depend on value size)
    L1.PutInt64('Int64_Zero', 0);
    L1.PutInt64('Int64_Bool', 1);
    L1.PutInt64('Int64_Int8', 2);
    L1.PutInt64('Int64_Int16', High(Byte)+1);
    L1.PutInt64('Int64_Int32', High(Word)+1);
    L1.PutInt64('Int64_Int64', Int64(High(Int32))+1);
    L1.PutInt64('Int64_Int64max', High(Int64));
    L1.PutInt64('Int64_Int64min', - High(Int64));

    L1:=FPYEKVS.PutList('List_Int128');
    // Int128
    FillChar(i128, sizeof(i128), $FF);
    L1.PutInt128('Int128', i128);
    FillChar(u128, sizeof(u128), $1);
    L1.PutUInt128('UInt128', u128);


    L1:=FPYEKVS.PutList('List_UInt32');
    // UInt8, UInt16, UInt32 (depend on value size)
    L1.PutUInt('UInt_Zero', 0);
    L1.PutUInt('UInt_Bool', 1);
    L1.PutUInt('UInt_UInt8', 2);
    L1.PutUInt('UInt_UInt16', High(Byte)+1);
    L1.PutUInt('UInt_UInt32', High(Word)+1);

    L1:=FPYEKVS.PutList('List_UInt64');
    // UInt8, UInt16, UInt32, UInt64 (depend on value size)
    L1.PutUInt64('UInt64_Zero', 0);
    L1.PutUInt64('UInt64_Bool', 1);
    L1.PutUInt64('UInt64_UInt8', 2);
    L1.PutUInt64('UInt64_UInt16', High(Byte)+1);
    L1.PutUInt64('UInt64_UInt32', High(Word)+1);
    L1.PutUInt64('UInt64_UInt64', UInt64(High(UInt32))+1);
    L1.PutUInt64('UInt64_UInt64max', High(UInt64));

    L1:=FPYEKVS.PutList('List_Float32');
    L1.PutSingle('Single_Zero', 0);
    L1.PutSingle('Single_Pi', Pi);

    L1:=FPYEKVS.PutList('List_Float64');
    L1.PutDouble('Double_Zero', 0);
    L1.PutDouble('Double_Pi', Pi);

    L1:=FPYEKVS.PutList('List_String');
    L1.PutString('Text0 Zero', '');
    L1.PutString('Text1 StringS', 'Hello pyeKVS');
    s:='012345670123456701234567012345670123456701234567012345670123456701234567012345670123456701234567012345670123456701234567012345670123456701234567012345670123456701234567012345670123456701234567012345670123456701234567012345670123456701234567012345670123456';
    s:=s+s;
    L1.PutString('Text2 StringM', s);


    // Array test ...

    L1:=FPYEKVS.PutList('List_Array');
    A1:=L1.PutArray('Array_Int16', pyeInt16);
    A1.PutInt(-2);
    A1.PutInt(-1);
    A1.PutInt(0);
    A1.PutInt(1);
    A1.PutInt(2);
    A1.PutInt(High(Int16));

    A1:=L1.PutArray('Array_String', pyeStringUTF8S);
    A1.PutString('');
    A1.PutString('Hello pyeKVS');
    A1.PutString('Hello world');

    A1:=L1.PutArray('Array_Memory', pyeMemory);
    A1.PutMemory(nil, 0);
    A1.PutMemory(@cData, sizeof(cData));
    A1.PutMemory(@cData, sizeof(cData));


    // ArrayMap test ...

    L1:=FPYEKVS.PutList('List_ArrayMap');
    // create map (like a struct/record)
    M1:=L1.PutArrayMap('ArrayMap_Test1', TPYEKVSValueTypeMap_Maker.Map3(pyeInt8, pyeFloat64, pyeStringUTF8S));
    M1.PutItem([0, 0.01, '']);
    M1.PutItem([1, 1.01, 'Test1']);
    M1.PutItem([2, 2.02, 'Test2']);
    M1.PutItem([3, 3.03, 'Test3']);



    // Memory test ...
    L1:=FPYEKVS.PutList('List_Memory');
    L1.PutMemory('Mem0', nil, 0);
    L1.PutMemory('Mem1', @cData, sizeof(cData));

    Stream1:=L1.PutStream('Stream1');
    Stream1.Position:=0;
    //special case: late write in a stream; function must be finished before the next key is written!
    Stream1.Write(cData, sizeof(cData));
//    Stream1.Write(cData, sizeof(cData));
//    Stream1.Position:=0;
//    Stream1.Write(cData, sizeof(cData));


    FPYEKVS.PutEnd;

    FStopWatch.Stop;
    UpdateMemo();
    UpdateStatusBar();
end;



procedure TMain.BWriteFileClick(Sender: TObject);
var iFileName:string;
begin
    FStopWatch:=TStopWatch.StartNew;
    iFileName:=ExtractFilePath(Application.ExeName)+'pyeKVS1.pye';
    FPYEKVS.SaveToFile(iFileName);
    FStopWatch.Stop;
    UpdateStatusBar();

    Memo.Lines.Clear;
    Memo.Lines.Add('SaveToFile '+iFileName);
    Memo.Lines.Add(Format('size: %d bytes',[FPYEKVS.KVSStream.Size]));

end;


{$HINTS OFF}    //ignore warning "never used"
procedure TMain.BReadFileClick(Sender: TObject);
var i    : Integer;
    d    : Double;
    s    : string;
    Li   : TPYEKVSList;
    Ai   : TPYEKVSArray;
    Stream1:TStream;
begin
    FStopWatch:=TStopWatch.StartNew;
    FPYEKVS.LoadFromFile(ExtractFilePath(Application.ExeName)+'pyeKVS1.pye');


    i:=FPYEKVS.GetInt('1');
    i:=FPYEKVS.GetInt('2');
    i:=FPYEKVS.GetInt('3');
    d:=FPYEKVS.GetDouble('3');
    s:=FPYEKVS.GetString('3');

    i:=FPYEKVS.GetInt('4');
    i:=FPYEKVS.GetInt('4');
    d:=FPYEKVS.GetDouble('4');
    s:=FPYEKVS.GetString('4');

    i:=FPYEKVS.GetInt('4');

    Li:=FPYEKVS.GetList('L1');
    if (Li<>nil) then begin
        i:=Li.GetInt('1');
        i:=Li.GetInt('2');
        i:=Li.GetInt('3');
    end;

    Ai:=FPYEKVS.GetArray('A1');
    if (Ai<>nil) then begin
        i:=Ai.GetInt(0);
        // i:=Ai.GetInt(1);
        // i:=Ai.GetInt(2);
        // i:=Ai.GetInt(3);
        s:=Ai.GetString(2);
    end;

    Li:=FPYEKVS.GetList('List_Array');
    if (Li<>nil) then begin
        Ai:=Li.GetArray('Array_String');
        if (Ai<>nil) then begin
            s:=Ai.GetString(0);
            s:=Ai.GetString(1);
            s:=Ai.GetString(2);
        end;

        Ai:=Li.GetArray('Array_Memory');
        if (Ai<>nil) then begin
            i:=Ai.GetMemory(0, @cData, sizeof(cData));
            i:=Ai.GetMemory(1, @cData, sizeof(cData));
            i:=Ai.GetMemory(2, @cData, sizeof(cData));
            if (Ai.Count>=20) then begin
                i:=Ai.GetMemory(20, @cData, sizeof(cData));
            end;
        end;
    end;

    Li:=FPYEKVS.GetList('List_Memory');
    if (Li<>nil) then begin
        FillChar(cData, Length(cData), 0);
        Li.GetMemory('Mem0', @cData, sizeof(cData));
        Li.GetMemory('Mem1', @cData, sizeof(cData));

        FillChar(cData, Length(cData), 0);
        Stream1:=Li.GetStream('Stream1');
        //special case: late write in a stream; function must be finished before the next key is written!
        Stream1.Read(cData, sizeof(cData));
    end;


    FStopWatch.Stop;
    UpdateMemo();
    UpdateStatusBar();
end;
{$HINTS ON}    //ignore warning "never used"

procedure TMain.RGLogClick(Sender: TObject);
begin
    UpdateMemo();
end;


procedure TMain.BStreamCopyClick(Sender: TObject);
begin
    FStopWatch:=TStopWatch.StartNew;

    DoStreamCopy();

    FStopWatch.Stop;

    UpdateMemo();
    UpdateStatusBar();
end;

procedure TMain.DoRndFill();
begin
    FPYEKVS.Reset;

    FPYEKVS.PutBegin;
    RandomItemsAdd(FPYEKVS, 1000);
    FPYEKVS.PutEnd;
end;

procedure TMain.DoRndFillLoop1();
var i:Integer;
begin
    for i:=0 to 10000 do begin
        DoRndFill();
        DoStreamCopy();
        Memo.Lines.Add(IntToStr(i));
    end;
end;


procedure TMain.DoStreamCopy();
var MStream: TMemoryStream;
begin
    MStream:=TMemoryStream.Create();
    try
        // Write to memory stream
        FPYEKVS.WriteToStream(MStream);

        // Clear the PYEKVS-Doc
        FPYEKVS.Clear;

        // Read back from memory stream
        MStream.Seek(0, soBeginning);
        FPYEKVS.ReadFromStream(MStream);

    finally
        MStream.Free();
    end;
end;

procedure TMain.RandomItemsAdd(const aBase: TPYEKVSList; const aItemCount: Integer);


    function IntToPYEKVSKey(const aValue:Integer):TPYEKVSKey;
    begin
        result:=TPYEKVSKey(System.SysUtils.IntToStr(aValue));
    end;

    function RndStringS:string;
    begin
        //Small string; max 256 char; warnung UTF8 conversion can extend the string
        result:=Copy(cString, Random(100), Random(120));
    end;
    function RndStringL:string;
    begin
        //Large string; max 2GB char; warnung UTF8 conversion can extend the string
        result:=Copy(cString, Random(100), Random(Length(cString)));
    end;

const
    cItemName='Item';
var i, k: Integer;
    Li  : TPYEKVSList;
    Ai  : TPYEKVSArray;
    Mi  : TPYEKVSArrayMap;
    Si  : TStream;
begin

    // Fill FPYEKVS with random data
    for i:=1 to aItemCount do begin
        case Random(12) of
        0: aBase.PutInt(cItemName+IntToPYEKVSKey(i), -Random(10000));
        1: aBase.PutInt64(cItemName+IntToPYEKVSKey(i), -Int64(MAXInt)-Random(10000));
        2: aBase.PutUInt(cItemName+IntToPYEKVSKey(i), Random(10000));
        3: aBase.PutUInt64(cItemName+IntToPYEKVSKey(i), Int64(MAXInt)+Random(10000));
        4: aBase.PutSingle(cItemName+IntToPYEKVSKey(i), Random*10000/(1+Random(100000)));
        5: aBase.PutString(cItemName+IntToPYEKVSKey(i), RndStringL);
        6: aBase.PutDouble(cItemName+IntToPYEKVSKey(i), Random*1000000/(1+Random(100000)));
        7: begin
                Li:=aBase.PutList(cItemName+IntToPYEKVSKey(i));
                RandomItemsAdd(Li, Min(aItemCount div 2, 5)); // add some sub items
            end;
        8: begin
               case Random(4) of
               0: begin
                    Ai:=aBase.PutArray(cItemName+IntToPYEKVSKey(i), pyeInt64);
                    for k:=0 to Random(20) do begin
                        Ai.PutInt(Random(MAXInt));
                    end;
               end;
               1: begin
                    Ai:=aBase.PutArray(cItemName+IntToPYEKVSKey(i), pyeFloat64);
                    for k:=0 to Random(20) do begin
                        Ai.PutDouble(Random*1000000/(1+Random(100000)));
                    end;
               end;
               2: begin
                    Ai:=aBase.PutArray(cItemName+IntToPYEKVSKey(i), pyeStringUTF8S);
                    for k:=0 to Random(20) do begin
                        Ai.PutString(RndStringS);
                    end;
               end;
               3: begin
                    Ai:=aBase.PutArray(cItemName+IntToPYEKVSKey(i), pyeMemory);
                    for k:=0 to Random(20) do begin
                        Ai.PutMemory(@cData, sizeof(cData));
                    end;
               end;
               end;
            end;
        9: begin
                Mi:=aBase.PutArrayMap(cItemName+IntToPYEKVSKey(i), TPYEKVSValueTypeMap_Maker.Map4(pyeInt64, pyeStringUTF8S, pyeFloat64, pyeStringUTF8L));
                for k:=0 to Random(20) do begin
                    Mi.PutItem([Random(MAXInt), RndStringS, Random*100/(1+Random*10000), RndStringL]);
                end;
            end;
    10: aBase.PutMemory(cItemName+IntToPYEKVSKey(i), @cData, sizeof(cData));
    11: begin
            Si:=aBase.PutStream(cItemName+IntToPYEKVSKey(i));
            Si.Write(cData, sizeof(cData));
        end;

        end;
    end;

end;




procedure TMain.PyeKVS_Save();
var FPYEKVS: TPYEKVSDocument;
    L1     : TPYEKVSList;
    A1, A2 : TPYEKVSArray;
begin
    // create a document (pyeKVS-stream)
    FPYEKVS:=TPYEKVSDocument.Create();

    // start with storing values
    FPYEKVS.PutBegin;

    // simple data types
    FPYEKVS.PutInt('Key1', 1);
    FPYEKVS.PutInt('Key2', 2);
    FPYEKVS.PutInt64('Key3', 3);
    FPYEKVS.PutString('Key4', 'Hallo pyeKVS. Make UTF8: ‹÷ƒ¸ˆ‰ﬂ@µß|¥');

    // array type of vtInt32
    A1:=FPYEKVS.PutArray('KeyA1', pyeInt32);
    A1.PutInt(10);
    A1.PutInt(20);
    A1.PutInt(30);

    // array type of vtStringUTF8 (dynamic item length!)
    A2:=FPYEKVS.PutArray('KeyA2', pyeStringUTF8S);
    A2.PutString('String-1');
    A2.PutString('String-2');

    // list type
    L1:=FPYEKVS.PutList('KeyL1');
    L1.PutSingle('Key1', Random*10000);
    L1.PutDouble('Key2', Random*1000000);

    // memory type
    FPYEKVS.PutMemory('Mem1', @cData, sizeof(cData));

    // finish the collection (this call close the stram)
    FPYEKVS.PutEnd;

    // save the stream to file
    FPYEKVS.SaveToFile(ExtractFilePath(Application.ExeName)+'pyeKVS_01.data');

    // free the document
    FPYEKVS.Free();
end;

{$HINTS OFF}    //ignore warning "never used"
procedure TMain.PyeKVS_Load();
var cData  : array [0..127] of Byte;
    FPYEKVS: TPYEKVSDocument;
    L1     : TPYEKVSList;
    A1     : TPYEKVSArray;
    i      : Integer;
    s      : string;
    f      : Double;
begin
    // create a document (pyeKVS-stream)
    FPYEKVS:=TPYEKVSDocument.Create();

    // load stream from file
    FPYEKVS.LoadFromFile(ExtractFilePath(Application.ExeName)+'pyeKVS_01.data');

    // Get some values ...
    i:=FPYEKVS.GetInt('Key1');
    i:=FPYEKVS.GetInt('Key1');

    s:=FPYEKVS.GetString('Key4');

    A1:=FPYEKVS.GetArray('KeyA1');
    if (A1<>nil) then begin
        // Values by index
        i:=A1.GetInt(0);
        i:=A1.GetInt(1);
        i:=A1.GetInt(2);
    end;

    L1:=FPYEKVS.GetList('KeyL1');
    if (L1<>nil) then begin
        f:=L1.GetSingle('Key1');
        f:=L1.GetDouble('Key2');
    end;

    FPYEKVS.GetMemory('Mem1', @cData, sizeof(cData));

    // free the document
    FPYEKVS.Free();
end;
{$HINTS ON}    //ignore warning "never used"

end.

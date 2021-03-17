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
    pyeKVS,
    uFooData;

type
    // Userdef. Stream
    TUserFileStream=class(TFileStream)
    private
        FBeepMe: Integer;
    public
        constructor Create(aMode: Word); virtual;
        destructor Destroy; override;
        function Write(const Buffer; Count: Longint): Longint; override;
    end;



    TMain=class(TForm)
        PMemo: TPanel;
        Memo: TMemo;
        PCreator: TPanel;
        GBMemory: TGroupBox;
        GBUserStream: TGroupBox;
        BMemorySimple1: TButton;
        BMemoryHandMade: TButton;
        BMemoryRnd: TButton;
        BMemoryWriteFile: TButton;
        BMemoryReadFile: TButton;
        RGLog: TRadioGroup;
        StatusBar: TStatusBar;
        BUserStreamHandMade: TButton;
        BUserStreamRandom: TButton;
        GBInfo: TGroupBox;
        BAbout: TButton;
        BUserStreamlRead: TButton;
        GBFile: TGroupBox;
        BFileHandmade: TButton;
        BFileRnd: TButton;
        BFileRead: TButton;
        GBSpecial: TGroupBox;
        BMemoryStreamCopy: TButton;
    GBGeneral: TGroupBox;
    BClear: TButton;
        procedure BMemoryRndClick(Sender: TObject);
        procedure BMemoryWriteFileClick(Sender: TObject);
        procedure BMemoryReadFileClick(Sender: TObject);
        procedure BMemoryStreamCopyClick(Sender: TObject);
        procedure BMemoryHandMadeClick(Sender: TObject);
        procedure RGLogClick(Sender: TObject);
        procedure BMemorySimple1Click(Sender: TObject);
        procedure BUserStreamHandMadeClick(Sender: TObject);
        procedure BUserStreamRandomClick(Sender: TObject);
        procedure BAboutClick(Sender: TObject);
        procedure BUserStreamlReadClick(Sender: TObject);
        procedure BFileHandmadeClick(Sender: TObject);
        procedure BFileRndClick(Sender: TObject);
        procedure BFileReadClick(Sender: TObject);
    procedure BClearClick(Sender: TObject);
    private
        FFooData: TFooData;

        FPYEKVS   : TPYEKVSDocument;
        FStopWatch: TStopWatch;
        procedure UpdateAbout;
        procedure UpdateMemo();
        procedure UpdateStatusBar;

        procedure FreePYEKVS;
    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
    end;

var
    Main: TMain;

const
    cFileNameIntMemory='pyeKVS1.pye';
    cFileNameIntFile  ='pyeKVS2.pye';
    cFileNameExtUser  ='pyeKVS3.pye';

implementation

{$R *.dfm}


constructor TMain.Create(AOwner: TComponent);
begin
    inherited;
    FFooData:=TFooData.Create();

    FPYEKVS:=nil;

    UpdateAbout();
end;

destructor TMain.Destroy;
begin
    FreePYEKVS();

    FFooData.Free();
    inherited;
end;


procedure TMain.FreePYEKVS;
begin
    if (FPYEKVS<>nil) then
        FreeAndNil(FPYEKVS);
end;

procedure TMain.UpdateAbout;
begin
    Memo.Clear;
    Memo.Lines.Add('pyeKVS is a binary key-value-storage format');
    Memo.Lines.Add('This project is a DELPHI implementation unit and test environment.');
    Memo.Lines.Add('Michael Koch (kxMaxx); (c)2021; MIT license');
end;


procedure TMain.UpdateMemo();
var SL: TStringList;
begin
    SL:=TStringList.Create();
    try
        // write in Memo.Lines is to slow ... use local StringList

        if (FPYEKVS<>nil) then begin
            if RGLog.ItemIndex=0 then begin
                FPYEKVS.ToStringsSimple(SL, '    ');
            end else begin
                FPYEKVS.ToStringsJSON(SL, '    ');
            end;
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
    if (FPYEKVS<>nil) then begin
        StatusBar.Panels[1].Text:=Format('Size total: %d Bytes', [FPYEKVS.StreamSize]);
    end else begin
        StatusBar.Panels[1].Text:='';
    end;
end;


procedure TMain.RGLogClick(Sender: TObject);
begin
    UpdateMemo();
end;

procedure TMain.BClearClick(Sender: TObject);
begin
    if (FPYEKVS<>nil) then begin
        FPYEKVS.Clear;
    end;
    UpdateMemo();
    UpdateStatusBar();
end;


procedure TMain.BAboutClick(Sender: TObject);
begin
    UpdateAbout();
end;



procedure TMain.BMemorySimple1Click(Sender: TObject);
begin
    FStopWatch:=TStopWatch.StartNew;

    // release old document
    FreePYEKVS();

    // Create new document
    FPYEKVS:=TPYEKVSDocument.Create();

    // Start data collection
    FPYEKVS.PutBegin;

    // put some value from a data class
    FFooData.PutDataToStore_Simple(FPYEKVS.Root);

    // End data collection
    FPYEKVS.PutEnd;

    FStopWatch.Stop;
    UpdateMemo();
    UpdateStatusBar();
end;



procedure TMain.BMemoryHandMadeClick(Sender: TObject);
begin
    FStopWatch:=TStopWatch.StartNew;

    // release old document
    FreePYEKVS();

    // Create new document
    FPYEKVS:=TPYEKVSDocument.Create();

    // Start data collection
    FPYEKVS.PutBegin;

    // put some value from a data class
    FFooData.PutDataToStore_Handmade(FPYEKVS.Root);

    // End data collection
    FPYEKVS.PutEnd;

    FStopWatch.Stop;
    UpdateMemo();
    UpdateStatusBar();
end;

procedure TMain.BMemoryRndClick(Sender: TObject);
begin
    FStopWatch:=TStopWatch.StartNew;

    // release old document
    FreePYEKVS();

    // Create new document
    FPYEKVS:=TPYEKVSDocument.Create();

    // Start data collection
    FPYEKVS.PutBegin;

    // put some value from a data class
    FFooData.PutDataToStore_Random(FPYEKVS.Root, 1000);

    // End data collection
    FPYEKVS.PutEnd;


    FStopWatch.Stop;
    UpdateMemo();
    UpdateStatusBar();
end;




procedure TMain.BMemoryWriteFileClick(Sender: TObject);
var iFileName: string;
begin
    if (FPYEKVS=nil) then exit;

    FStopWatch:=TStopWatch.StartNew;

    iFileName:=ExtractFilePath(Application.ExeName)+cFileNameIntMemory;

    // Write collected values to file (internal stream copy)
    FPYEKVS.SaveToFile(iFileName);

    FStopWatch.Stop;
    UpdateStatusBar();

    Memo.Lines.Clear;
    Memo.Lines.Add('SaveToFile '+iFileName);
    Memo.Lines.Add(Format('size: %d bytes', [FPYEKVS.Stream.Size]));
end;


// ignore warning "never used"
procedure TMain.BMemoryReadFileClick(Sender: TObject);
begin
    // release old document
    FreePYEKVS();

    FStopWatch:=TStopWatch.StartNew;

    // Create new document
    FPYEKVS:=TPYEKVSDocument.Create();

    // Load the values from file to memorystream
    FPYEKVS.LoadFromFile(ExtractFilePath(Application.ExeName)+cFileNameIntMemory);

    // lets read some values ...

    // Enable fallback; not existing containers will be replaced be a fallback instance
    FPYEKVS.FallbackEnabled:=true;

    // maybe the file was not match with "Handmade", therefore we should have a fallback to the default settings
    FFooData.GetDataToStore_Handmade(FPYEKVS.Root);


    FStopWatch.Stop;
    UpdateMemo();
    UpdateStatusBar();
end;


procedure TMain.BFileHandmadeClick(Sender: TObject);
begin
    // pyeKVS in direct stream mode; less memory consumption; values go directly to the output (filestream)

    // release old document
    FreePYEKVS();

    FStopWatch:=TStopWatch.StartNew;

    // Create a pyeKVS with internal file stream
    // fmOpenWrite to store in file
    // fmOpenRead to use function ToStringsSimple and WriteToStream (read values again)
    FPYEKVS:=TPYEKVSDocument.Create(ExtractFilePath(Application.ExeName)+cFileNameIntFile, fmCreate or fmOpenReadWrite or fmShareExclusive);

    // Start data collection
    FPYEKVS.PutBegin;

    // put some value from a data class
    FFooData.PutDataToStore_Handmade(FPYEKVS.Root);

    // End data collection
    FPYEKVS.PutEnd;

    FStopWatch.Stop;

    // show collected data in memo
    UpdateMemo();
    UpdateStatusBar();

    // The file is still open; read values is possible until FPYEKVS.Free
end;


procedure TMain.BFileRndClick(Sender: TObject);
begin
    // pyeKVS in direct stream mode; less memory consumption; values go directly to the output (filestream)

    // release old document
    FreePYEKVS();

    FStopWatch:=TStopWatch.StartNew;

    // Create a pyeKVS with internal file stream
    // fmOpenWrite to store in file
    // fmOpenRead to use function ToStringsSimple and WriteToStream (read values again)
    FPYEKVS:=TPYEKVSDocument.Create(ExtractFilePath(Application.ExeName)+cFileNameIntFile, fmCreate or fmOpenReadWrite or fmShareExclusive);

    // Start data collection
    FPYEKVS.PutBegin;

    // put some value from a data class
    FFooData.PutDataToStore_Random(FPYEKVS.Root, 1000);

    // End data collection
    FPYEKVS.PutEnd;

    FStopWatch.Stop;

    // show collected data in memo
    UpdateMemo();
    UpdateStatusBar();

    // The file is still open; read values is possible until FPYEKVS.Free
end;



procedure TMain.BFileReadClick(Sender: TObject);
begin
    // release old document
    FreePYEKVS();

    FStopWatch:=TStopWatch.StartNew;

    // Create a pyeKVS with internal file stream
    // fmOpenRead to use function ToStringsSimple and WriteToStream (read values again)
    FPYEKVS:=TPYEKVSDocument.Create(ExtractFilePath(Application.ExeName)+cFileNameIntFile, fmOpenRead or fmShareExclusive);

    // read the stream and find the keys ans values
    FPYEKVS.Encode();


    // Enable fallback; not existing container objects will be replaced be a fallback instance
    FPYEKVS.FallbackEnabled:=true;

    // lets read some values ...

    // maybe the file was not match with "Handmade", therefore we should have a fallback to the default settings
    FFooData.GetDataToStore_Handmade(FPYEKVS.Root);


    FStopWatch.Stop;
    UpdateMemo();
    UpdateStatusBar();
end;




procedure TMain.BUserStreamHandMadeClick(Sender: TObject);
var iUserStream: TUserFileStream;
begin
    // pyeKVS in direct stream mode; less memory consumption; values go directly to the output (filestream)

    // release old document
    FreePYEKVS();

    FStopWatch:=TStopWatch.StartNew;

    // Userdef. Stream
    iUserStream:=TUserFileStream.Create(fmCreate or fmOpenReadWrite or fmShareDenyWrite);

    // Create a pyeKVS with option: write direct to my stream
    // FPYEKVS owns iFileStream; internal free
    FPYEKVS:=TPYEKVSDocument.Create(iUserStream, true);

    // Start data collection
    FPYEKVS.PutBegin;

    // put some value from a data class
    FFooData.PutDataToStore_Handmade(FPYEKVS.Root);

    // End data collection
    FPYEKVS.PutEnd;

    FStopWatch.Stop;

    // show collected data in memo
    UpdateMemo();
    UpdateStatusBar();

end;



procedure TMain.BUserStreamRandomClick(Sender: TObject);
var iUserStream: TUserFileStream;
begin
    // pyeKVS in direct stream mode; less memory consumption; values go directly to the output (filestream)

    // release old document
    FreePYEKVS();

    FStopWatch:=TStopWatch.StartNew;

    // Userdef. Stream
    iUserStream:=TUserFileStream.Create(fmCreate or fmOpenReadWrite or fmShareDenyWrite);

    // Create a pyeKVS with option: write direct to my stream
    // FPYEKVS owns iFileStream; internal free
    FPYEKVS:=TPYEKVSDocument.Create(iUserStream, true);

    // Start data collection
    FPYEKVS.PutBegin;

    // put some value from a data class
    FFooData.PutDataToStore_Random(FPYEKVS.Root, 1000);

    // End data collection
    FPYEKVS.PutEnd;

    FStopWatch.Stop;

    // show collected data in memo
    UpdateMemo();
    UpdateStatusBar();
end;


procedure TMain.BUserStreamlReadClick(Sender: TObject);
var iUserStream: TUserFileStream;
begin
    // release old document
    FreePYEKVS();

    FStopWatch:=TStopWatch.StartNew;

    // Userdef. Stream
    iUserStream:=TUserFileStream.Create(fmOpenReadWrite or fmShareDenyWrite);

    // Create a pyeKVS with option: write direct to my stream
    // FPYEKVS owns iFileStream; internal free
    FPYEKVS:=TPYEKVSDocument.Create(iUserStream, true);

    // read the stream and find the keys ans values
    FPYEKVS.Encode();

    // lets read some values ...

    // Enable fallback; not existing container objects will be replaced be a fallback instance
    FPYEKVS.FallbackEnabled:=true;

    // maybe the file was not match with "Handmade", therefore we should have a fallback to the default settings
    FFooData.GetDataToStore_Handmade(FPYEKVS.Root);


    FStopWatch.Stop;
    UpdateMemo();
    UpdateStatusBar();
end;


procedure TMain.BMemoryStreamCopyClick(Sender: TObject);
var MStream  : TMemoryStream;
    iBlock   : array [0..31] of Byte;
    iReadSize: Integer;
    iSize    : Integer;
begin
    // Use existing PYEKVSDocument  ...
    if (FPYEKVS=nil) then exit;

    FStopWatch:=TStopWatch.StartNew;

    MStream:=TMemoryStream.Create();
    try
        // Write existing document to memorystream
        FPYEKVS.WriteToStream(MStream);
        MStream.Seek(0, soBeginning);

        // release old document
        FreePYEKVS();


        // What next?
        // Let imaging the MStream is a TCP socket and we would read from the stream.
        // We dont know the version and the size !!!


        // Create a new pyeKVS document with internal memory stream
        FPYEKVS:=TPYEKVSDocument.Create();

        // Read only the header at first ...
        if (MStream.Size>=sizeof(TPYEKVSStreamHeader)) then begin

            // read block
            iReadSize:=MStream.Read(iBlock, sizeof(TPYEKVSStreamHeader));
            if (iReadSize<>sizeof(TPYEKVSStreamHeader)) then exit(); // check for error
            // write block to FPYEKVS.Stream
            FPYEKVS.Stream.Write(iBlock, iReadSize);

            // Check if Header valid; Version PYEKVS protocol
            if (FPYEKVS.StreamHeaderUpdate=true) then begin
                // valid ...

                // The header holds the size if the stream.
                // The property DataSizePending shows the pending (missing) size of the FPYEKVS.Stream

                // Read pending bytes from input ...
                while (FPYEKVS.DataSizePending>0) do begin
                    // Blockwise reading (excample, because TCP stream comes also in blocks)

                    // Block size
                    iSize:=Length(iBlock);
                    if (iSize>FPYEKVS.DataSizePending) then iSize:=FPYEKVS.DataSizePending;

                    iReadSize:=MStream.Read(iBlock, iSize);
                    if (iReadSize<>iSize) then exit(); // check for error
                    // write block to FPYEKVS.Stream
                    FPYEKVS.Stream.Write(iBlock, iReadSize);
                end;

            end;

            // FPYEKVS.Stream is now complete filled

            // Encode keys and values before any Get function calls
            FPYEKVS.Encode();

            // Enable fallback; not existing container objects will be replaced be a fallback instance
            FPYEKVS.FallbackEnabled:=true;

            // maybe the file was not match with "Handmade", therefore we should have a fallback to the default settings
            FFooData.GetDataToStore_Handmade(FPYEKVS.Root);

        end;

    finally
        MStream.Free();
    end;

    FStopWatch.Stop;

    UpdateMemo();
    UpdateStatusBar();
end;


{ TUserFileStream }

constructor TUserFileStream.Create(aMode: Word);
begin
    inherited Create(ExtractFilePath(Application.ExeName)+cFileNameExtUser, aMode);
end;

destructor TUserFileStream.Destroy;
begin
    inherited;
    Main.Memo.Color:=clWindow;
end;

function TUserFileStream.Write(const Buffer; Count: Integer): Longint;
begin
    result:=inherited Write(Buffer, Count);

    FBeepMe:=FBeepMe+Count;
    if (FBeepMe>5000) then begin
        // do some crazy things (sorry)
        Main.Memo.Color:=clRed;
        Application.ProcessMessages;
        if (FBeepMe>5000) then begin
            Main.Memo.Color:=clWindow;
            Application.ProcessMessages;
            FBeepMe:=0;
        end;
    end;
end;




end.

unit uFooData;

interface

uses
    System.Classes,
    System.Math,
    System.SysUtils,
    pyeKVS;

type
    TFooData=class(TObject)
    private
    var
        FData       :array [0..127] of Byte;
        FHugeString :string;
    var
        FMyValue1   :Integer;
        FMyString1  :String;

    var
        FBoolFalse  :Boolean;
        FBoolTrue   :Boolean;

        FInt_0     :Integer;
        FInt_1     :Integer;
        FInt_2     :Integer;
        FInt_x16   :Integer;
        FInt_x32   :Integer;

        FInt64_0   :Int64;
        FInt64_1   :Int64;
        FInt64_2   :Int64;
        FInt64_x16 :Int64;
        FInt64_x32 :Int64;
        FInt64_x64 :Int64;
        FInt64_max :Int64;
        FInt64_min :Int64;

        FUInt_0     :Cardinal;
        FUInt_1     :Cardinal;
        FUInt_2     :Cardinal;
        FUInt_x16   :Cardinal;
        FUInt_x32   :Cardinal;

        FUInt64_0   :UInt64;
        FUInt64_1   :UInt64;
        FUInt64_2   :UInt64;
        FUInt64_x16 :UInt64;
        FUInt64_x32 :UInt64;
        FUInt64_x64 :UInt64;
        FUInt64_max :UInt64;

        FInt128    :TPYEKVSInt128;
        FUInt128   :TPYEKVSUInt128;

        FSingle_0   :Single;
        FSingle_Pi  :Single;

        FDouble_0   :Double;
        FDouble_Pi  :Double;

        FTextS_0    :String;
        FTextS_1    :String;

        FTextL_0    :String;
        FTextL_1    :String;

        FArray_Int      :array of Integer;
        FArray_String   :array of String;

        FArrayMap       :array of record
            FInt1    :Integer;
            FStr1    :String;
            FSingle1 :Single;
            FBool1   :Boolean;
            FUInt64  :UInt64;
        end;





        procedure RandomItemsAdd(const aBase: TPYEKVSList; const aItemCount: Integer);
    public
        constructor Create(); virtual;
        destructor Destroy; override;

        procedure ValuesToDefault;

        procedure PutDataToStore_Simple(aStoreList:TPYEKVSList);
        procedure GetDataToStore_Simple(aStoreList:TPYEKVSList);

        procedure PutDataToStore_Handmade(aStoreList:TPYEKVSList);
        procedure GetDataToStore_Handmade(aStoreList:TPYEKVSList);

        procedure PutDataToStore_Random(aStoreList:TPYEKVSList; const aItemCount: Integer);
    end;


implementation



constructor TFooData.Create;
var i: Integer;
begin
    inherited;
    Randomize();

    // init some memory
    for i:=Low(FData) to High(FData) do
        FData[i]:=(i and $FF);

    // init long string
    SetLength(FHugeString, 100000); // StringL more then 65535 chars
    for i:=1 to Length(FHugeString) do
        FHugeString[i]:=Char(i+32 and $FF);

    ValuesToDefault();
end;

destructor TFooData.Destroy;
begin
    inherited;
end;


procedure TFooData.ValuesToDefault;
begin
    //init member vars

    FMyValue1:=256;
    FMyString1:='Hello PYES.';

    FBoolFalse  :=false;
    FBoolTrue   :=true;

    FInt_0     :=0;
    FInt_1     :=1;
    FInt_2     :=2;
    FInt_x16   :=High(Byte)+1;
    FInt_x32   :=High(Word)+1;

    FInt64_0   :=0;
    FInt64_1   :=1;
    FInt64_2   :=2;
    FInt64_x16 :=High(Byte)+1;
    FInt64_x32 :=High(Word)+1;
    FInt64_x64 :=Int64(High(Int32))+1;
    FInt64_max :=+High(Int64);
    FInt64_min :=-High(Int64);

    FUInt_0     :=0;
    FUInt_1     :=1;
    FUInt_2     :=2;
    FUInt_x16   :=High(Byte)+1;
    FUInt_x32   :=High(Word)+1;

    FUInt64_0   :=0;
    FUInt64_1   :=1;
    FUInt64_2   :=2;
    FUInt64_x16 :=High(Byte)+1;
    FUInt64_x32 :=High(Word)+1;
    FUInt64_x64 :=UInt64(High(UInt32))+1;
    FUInt64_max :=High(UInt64);

    FillChar(FInt128, sizeof(FInt128), $FF);
    FillChar(FUInt128, sizeof(FUInt128), $1);

    FSingle_0   :=0;
    FSingle_Pi  :=Pi;

    FDouble_0   :=0;
    FDouble_Pi  :=Pi;

    FTextS_0    :='';
    FTextS_1    :='Hello pyeKVS';

    FTextL_0    :='Large?';
    FTextL_1    :='012345670123456701234567012345670123456701234567012345670123456701234567012345670123456701234567012345670123456701234567012345670123456701234567012345670123456701234567012345670123456701234567012345670123456701234567012345670123456701234567012345670123456';
    FTextL_1    :=FTextL_1+FTextL_1;

    SetLength(FArray_Int, 7);
    FArray_Int[0]:=-High(Int32);
    FArray_Int[1]:=-High(Int16);
    FArray_Int[2]:=-1;
    FArray_Int[3]:=0;
    FArray_Int[4]:=+1;
    FArray_Int[5]:=+High(Int16);
    FArray_Int[6]:=+High(Int32);

    SetLength(FArray_String, 3);
    FArray_String[0]:='Hello pyeKVS';
    FArray_String[1]:='';
    FArray_String[2]:='Strings in pyeArray make the array dynamic and must be encoded.';

    SetLength(FArrayMap, 3);
    with FArrayMap[0] do begin
        FInt1    :=1;
        FStr1    :='Test1';
        FSingle1 :=1.01;
        FBool1   :=false;
        FUInt64  :=111111111;
    end;
    with FArrayMap[1] do begin
        FInt1    :=2;
        FStr1    :='Test2';
        FSingle1 :=2.02;
        FBool1   :=true;
        FUInt64  :=222222222;
    end;
    with FArrayMap[2] do begin
        FInt1    :=3;
        FStr1    :='Test3';
        FSingle1 :=3.03;
        FBool1   :=false;
        FUInt64  :=333333333;
    end;

end;


procedure TFooData.PutDataToStore_Simple(aStoreList:TPYEKVSList);
begin
    aStoreList.PutInt('MyValue1', FMyValue1);
    aStoreList.PutString('MyString1', FMyString1);
end;

procedure TFooData.GetDataToStore_Simple(aStoreList:TPYEKVSList);
begin
    FMyValue1:=aStoreList.GetInt('MyValue1');
    FMyString1:=aStoreList.GetString('MyString1');
end;


procedure TFooData.PutDataToStore_Handmade(aStoreList:TPYEKVSList);
var L1     : TPYEKVSList;
    A1     : TPYEKVSArray;
    M1     : TPYEKVSArrayMap;
    Stream1: TStream;
    i      : Integer;
begin
    //Reset to Default values ...
    ValuesToDefault();

    // Fundamental values test ...

    L1:=aStoreList.PutList('List_ZeroTest');
    // Zero
    L1.PutZero('Zero');


    L1:=aStoreList.PutList('List_Boolean');
    // Bool
    L1.PutBool('Bool_false', FBoolFalse); // vtZero
    L1.PutBool('Bool_true', FBoolTrue);

    L1:=aStoreList.PutList('List_Integer');
    // Int8, Int16, Int32 (depend on value size)
    L1.PutInt('Int_Zero', FInt_0);
    L1.PutInt('Int_Bool', FInt_1);
    L1.PutInt('Int_Int8', FInt_2);
    L1.PutInt('Int_Int16', FInt_x16);
    L1.PutInt('Int_Int32', FInt_x32);

    L1:=aStoreList.PutList('List_Int64');
    // Int8, Int16, Int32, Int64 (depend on value size)
    L1.PutInt64('Int64_Zero', FInt64_0);
    L1.PutInt64('Int64_Bool', FInt64_1);
    L1.PutInt64('Int64_Int8', FInt64_2);
    L1.PutInt64('Int64_Int16', FInt64_x16);
    L1.PutInt64('Int64_Int32', FInt64_x32);
    L1.PutInt64('Int64_Int64', FInt64_x64);
    L1.PutInt64('Int64_Int64max', FInt64_max);
    L1.PutInt64('Int64_Int64min', FInt64_min);


    L1:=aStoreList.PutList('List_UInt32');
    // UInt8, UInt16, UInt32 (depend on value size)
    L1.PutUInt('UInt_Zero', FUInt_0);
    L1.PutUInt('UInt_Bool', FUInt_1);
    L1.PutUInt('UInt_UInt8', FUInt_2);
    L1.PutUInt('UInt_UInt16', FUInt_x16);
    L1.PutUInt('UInt_UInt32', FUInt_x32);

    L1:=aStoreList.PutList('List_UInt64');
    // UInt8, UInt16, UInt32, UInt64 (depend on value size)
    L1.PutUInt64('UInt64_Zero', FUInt64_0);
    L1.PutUInt64('UInt64_Bool', FUInt64_1);
    L1.PutUInt64('UInt64_UInt8', FUInt64_2);
    L1.PutUInt64('UInt64_UInt16', FUInt64_x16);
    L1.PutUInt64('UInt64_UInt32', FUInt64_x32);
    L1.PutUInt64('UInt64_UInt64', FUInt64_x64);
    L1.PutUInt64('UInt64_UInt64max', FUInt64_max);

    L1:=aStoreList.PutList('List_Int128');
    // Int128
    L1.PutInt128('Int128', FInt128);
    L1.PutUInt128('UInt128', FUInt128);

    L1:=aStoreList.PutList('List_Float32');
    L1.PutSingle('Single_Zero', FSingle_0);
    L1.PutSingle('Single_Pi', FSingle_Pi);

    L1:=aStoreList.PutList('List_Float64');
    L1.PutDouble('Double_Zero', FDouble_0);
    L1.PutDouble('Double_Pi', FDouble_Pi);

    L1:=aStoreList.PutList('List_String');
    L1.PutString('TextS_0', FTextS_0);
    L1.PutString('TextS_1', FTextS_1);

    L1.PutStringVT('TextL_0', FTextL_0, pyeStringUTF8L);      //force a long string
    L1.PutString('TextL_1', FTextL_1);


    // Array test ...
    L1:=aStoreList.PutList('List_Array');
    A1:=L1.PutArray('Array_Int', pyeInt32);
    for i:=0 to High(FArray_Int) do begin
        A1.PutInt( FArray_Int[i] );
    end;

    A1:=L1.PutArray('Array_Str', pyeStringUTF8S);
    for i:=0 to High(FArray_String) do begin
        A1.PutString( FArray_String[i] );
    end;

    A1:=L1.PutArray('Array_Memory', pyeMemory);
    A1.PutMemory(nil, 0);
    A1.PutMemory(@FData, sizeof(FData));
    A1.PutMemory(@FData, sizeof(FData));


    // ArrayMap test ...
    L1:=aStoreList.PutList('List_ArrayMap');
    M1:=L1.PutArrayMap('ArrayMap_1', TPYEKVSValueTypeMap_Maker.Map5(pyeInt8, pyeStringUTF8S, pyeFloat32, pyeInt8, pyeFloat64));
    for i:=0 to High(FArrayMap) do begin
        M1.PutItem( [
            FArrayMap[i].FInt1,
            FArrayMap[i].FStr1,
            FArrayMap[i].FSingle1,
            FArrayMap[i].FBool1,        //Int8 !
            FArrayMap[i].FUInt64
        ]);
    end;


    // Memory test ...
    L1:=aStoreList.PutList('List_Memory');
    L1.PutMemory('Mem0', nil, 0);
    L1.PutMemory('Mem1', @FData, sizeof(FData));

    Stream1:=L1.PutStream('Stream1');
    Stream1.Position:=0;
    // special case: late write in a stream; function must be finished before the next key is written!
    Stream1.Write(FData, sizeof(FData));
    //re-position the stream
    Stream1.Position:=0;
    //and write again
    Stream1.Write(FData, sizeof(FData));

end;

procedure TFooData.GetDataToStore_Handmade(aStoreList:TPYEKVSList);
var L1     : TPYEKVSList;
    A1     : TPYEKVSArray;
    M1     : TPYEKVSArrayMap;
    Buffer :array [0..127] of Byte;
    BufferReaded:Integer;
    MapValues:TPYEKVSArrayMapValues;
    Stream1: TStream;
    i      : Integer;
begin

    // Fundamental values test ...

    L1:=aStoreList.GetList('List_ZeroTest');
    // Zero
    FBoolFalse:=L1.GetZero('Zero');


    L1:=aStoreList.GetList('List_Boolean');
    // Bool
    FBoolFalse:=L1.GetBool('Bool_false'); // vtZero
    FBoolTrue:=L1.GetBool('Bool_true');

    L1:=aStoreList.GetList('List_Integer');
    FInt_0:=L1.GetInt('Int_Zero');
    FInt_1:=L1.GetInt('Int_Bool');
    FInt_2:=L1.GetInt('Int_Int8');
    FInt_x16:=L1.GetInt('Int_Int16');
    FInt_x32:=L1.GetInt('Int_Int32');

    L1:=aStoreList.GetList('List_Int64');
    FInt64_0:=L1.GetInt64('Int64_Zero');
    FInt64_1:=L1.GetInt64('Int64_Bool');
    FInt64_2:=L1.GetInt64('Int64_Int8');
    FInt64_x16:=L1.GetInt64('Int64_Int16');
    FInt64_x32:=L1.GetInt64('Int64_Int32');
    FInt64_x64:=L1.GetInt64('Int64_Int64');
    FInt64_max:=L1.GetInt64('Int64_Int64max');
    FInt64_min:=L1.GetInt64('Int64_Int64min');


    L1:=aStoreList.GetList('List_UInt32');
    FUInt_0:=L1.GetUInt('UInt_Zero');
    FUInt_1:=L1.GetUInt('UInt_Bool');
    FUInt_2:=L1.GetUInt('UInt_UInt8');
    FUInt_x16:=L1.GetUInt('UInt_UInt16');
    FUInt_x32:=L1.GetUInt('UInt_UInt32');

    L1:=aStoreList.GetList('List_UInt64');
    FUInt64_0:=L1.GetUInt64('UInt64_Zero');
    FUInt64_1:=L1.GetUInt64('UInt64_Bool');
    FUInt64_2:=L1.GetUInt64('UInt64_UInt8');
    FUInt64_x16:=L1.GetUInt64('UInt64_UInt16');
    FUInt64_x32:=L1.GetUInt64('UInt64_UInt32');
    FUInt64_x64:=L1.GetUInt64('UInt64_UInt64');
    FUInt64_max:=L1.GetUInt64('UInt64_UInt64max');

    L1:=aStoreList.GetList('List_Int128');
    FInt128:=L1.GetInt128('Int128', FInt128);       //need a default
    FUInt128:=L1.GetUInt128('UInt128', FUInt128);   //need a default

    L1:=aStoreList.GetList('List_Float32');
    FSingle_0:=L1.GetSingle('Single_Zero');
    FSingle_Pi:=L1.GetSingle('Single_Pi');

    L1:=aStoreList.GetList('List_Float64');
    FDouble_0:=L1.GetDouble('Double_Zero');
    FDouble_Pi:=L1.GetDouble('Double_Pi');

    L1:=aStoreList.GetList('List_String');
    FTextS_0:=L1.GetString('TextS_0');
    FTextS_1:=L1.GetString('TextS_1');

    FTextL_0:=L1.GetString('TextL_0');
    FTextL_1:=L1.GetString('TextL_1');


    // Array test ...
    L1:=aStoreList.GetList('List_Array');
    A1:=L1.GetArray('Array_Int');
    if (A1.ValueType=pyeInt32) then begin           //expected ValueType
        SetLength(FArray_Int, A1.Count);            //get count of array
        for i:=0 to High(FArray_Int) do begin
            FArray_Int[i]:=A1.GetInt(i);            //get items of array
        end;
    end;

    A1:=L1.GetArray('Array_Str');
    if (A1.ValueType=pyeStringUTF8S) then begin     //expected ValueType
        SetLength(FArray_String, A1.Count);         //get count of array
        for i:=0 to High(FArray_String) do begin
            FArray_String[i]:=A1.GetString(i);      //get items of array
        end;
    end;

    A1:=L1.GetArray('Array_Memory');
    if (A1.ValueType=pyeMemory) then begin         //expected ValueType
        for i:=0 to A1.Count-1 do begin            //get count of array
            BufferReaded:=A1.GetMemory(i, @Buffer, sizeof(Buffer)); //get items of array
        end;
    end;


    // ArrayMap test ...
    L1:=aStoreList.GetList('List_ArrayMap');
    M1:=L1.GetArrayMap('ArrayMap_1');
    if (M1.ValueTypeMapEqual(TPYEKVSValueTypeMap_Maker.Map5(pyeInt8, pyeStringUTF8S, pyeFloat32, pyeBool, pyeFloat64))) then begin  //check loaded map with expected map
        SetLength(FArrayMap, M1.Count);            //get count of array
        for i:=0 to High(FArrayMap) do begin
            MapValues:=M1.GetItem(i);
            //MapValues to record
            if Length(MapValues)=5{elements of record} then begin
                //let the variant make the job ...
                FArrayMap[i].FInt1:=MapValues[0];
                FArrayMap[i].FStr1:=MapValues[1];
                FArrayMap[i].FSingle1:=MapValues[2];
                FArrayMap[i].FBool1:=MapValues[3];
                FArrayMap[i].FUInt64:=MapValues[4];
            end;
        end;
    end;


    // Memory test ...
    L1:=aStoreList.GetList('List_Memory');
    FillChar(Buffer, Length(Buffer), 0);
    L1.GetMemory('Mem0', @Buffer, sizeof(Buffer));
    L1.GetMemory('Mem1', @Buffer, sizeof(Buffer));

    Stream1:=L1.GetStream('Stream1');
    if (Stream1<>nil) then begin
        FillChar(Buffer, Length(Buffer), 0);
        Stream1.Read(Buffer, sizeof(Buffer));
    end;

end;



procedure TFooData.PutDataToStore_Random(aStoreList:TPYEKVSList; const aItemCount: Integer);


    function IntToPYEKVSKey(const aValue: Integer): TPYEKVSKey;
    begin
        result:=TPYEKVSKey(System.SysUtils.IntToStr(aValue));
    end;

    function RndStringS: string;
    begin
        // Small string; max 256 char; warnung UTF8 conversion can extend the string
        result:=Copy(FHugeString, Random(100), Random(120));
    end;
    function RndStringL: string;
    begin
        // Large string; max 2GB char; warnung UTF8 conversion can extend the string
        result:=Copy(FHugeString, Random(100), Random(Length(FHugeString)));
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
        0: aStoreList.PutInt(cItemName+IntToPYEKVSKey(i), -Random(10000));
        1: aStoreList.PutInt64(cItemName+IntToPYEKVSKey(i), -Int64(MAXInt)-Random(10000));
        2: aStoreList.PutUInt(cItemName+IntToPYEKVSKey(i), Random(10000));
        3: aStoreList.PutUInt64(cItemName+IntToPYEKVSKey(i), Int64(MAXInt)+Random(10000));
        4: aStoreList.PutSingle(cItemName+IntToPYEKVSKey(i), Random*10000/(1+Random(100000)));
        5: aStoreList.PutString(cItemName+IntToPYEKVSKey(i), RndStringL);
        6: aStoreList.PutDouble(cItemName+IntToPYEKVSKey(i), Random*1000000/(1+Random(100000)));
        7: begin
                Li:=aStoreList.PutList(cItemName+IntToPYEKVSKey(i));
                RandomItemsAdd(Li, Min(aItemCount div 2, 5)); // add some sub items
            end;
        8: begin
                case Random(4) of
                0: begin
                        Ai:=aStoreList.PutArray(cItemName+IntToPYEKVSKey(i), pyeInt64);
                        for k:=0 to Random(20) do begin
                            Ai.PutInt(Random(MAXInt));
                        end;
                    end;
                1: begin
                        Ai:=aStoreList.PutArray(cItemName+IntToPYEKVSKey(i), pyeFloat64);
                        for k:=0 to Random(20) do begin
                            Ai.PutDouble(Random*1000000/(1+Random(100000)));
                        end;
                    end;
                2: begin
                        Ai:=aStoreList.PutArray(cItemName+IntToPYEKVSKey(i), pyeStringUTF8S);
                        for k:=0 to Random(20) do begin
                            Ai.PutString(RndStringS);
                        end;
                    end;
                3: begin
                        Ai:=aStoreList.PutArray(cItemName+IntToPYEKVSKey(i), pyeMemory);
                        for k:=0 to Random(20) do begin
                            Ai.PutMemory(@FData, sizeof(FData));
                        end;
                    end;
                end;
            end;
        9: begin
                Mi:=aStoreList.PutArrayMap(cItemName+IntToPYEKVSKey(i), TPYEKVSValueTypeMap_Maker.Map4(pyeInt64, pyeStringUTF8S, pyeFloat64, pyeStringUTF8L));
                for k:=0 to Random(20) do begin
                    Mi.PutItem([Random(MAXInt), RndStringS, Random*100/(1+Random*10000), RndStringL]);
                end;
            end;
        10: aStoreList.PutMemory(cItemName+IntToPYEKVSKey(i), @FData, sizeof(FData));
        11: begin
                Si:=aStoreList.PutStream(cItemName+IntToPYEKVSKey(i));
                Si.Write(FData, sizeof(FData));
            end;

        end;
    end;

end;


procedure TFooData.RandomItemsAdd(const aBase: TPYEKVSList; const aItemCount: Integer);


    function IntToPYEKVSKey(const aValue: Integer): TPYEKVSKey;
    begin
        result:=TPYEKVSKey(System.SysUtils.IntToStr(aValue));
    end;

    function RndStringS: string;
    begin
        // Small string; max 256 char; warnung UTF8 conversion can extend the string
        result:=Copy(FHugeString, Random(100), Random(120));
    end;
    function RndStringL: string;
    begin
        // Large string; max 2GB char; warnung UTF8 conversion can extend the string
        result:=Copy(FHugeString, Random(100), Random(Length(FHugeString)));
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
                            Ai.PutMemory(@FData, sizeof(FData));
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
        10: aBase.PutMemory(cItemName+IntToPYEKVSKey(i), @FData, sizeof(FData));
        11: begin
                Si:=aBase.PutStream(cItemName+IntToPYEKVSKey(i));
                Si.Write(FData, sizeof(FData));
            end;

        end;
    end;

end;


end.

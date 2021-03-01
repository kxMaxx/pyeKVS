# pyeKVS - Pascal Implementation
PyeKVS is a Key-Value-Storage format for data serialization. The format corresponds to the JSON format, but the values are binary encoded.

### Format specification
see [pyeKVS_Spec.md](pyeKVS_Spec.md)  
Version: 1.0  
Date: 01.03.2021  
License:  [MIT](http://opensource.org/licenses/MIT)  
[pyeKVS home](https://www.kxtec.de/projects/pyekvs/specification)  


### Features
- Storage of an object as a set of key and value
- Key representation as AnsiString
- Value representation as value data type + optional value header [e.g. size] + optional value data
- Value data are stored in binary representation
- Specification for a header of the entire document including format version and size
- Low effort for coding and decoding
- Reduction to necessary data types


## Project
This pyeKVS project is a pascal implementation for Delphi. It's stores the data into a stream.

## License
License:  [MIT](http://opensource.org/licenses/MIT)

## Project state
Version: 1  
Date: 16.01.2021  
Author: Michael Koch  
[pyeKVS for Delphi](https://www.kxtec.de/projects/pyekvs/delphi_api)  

## Usage

**Limitations**
All values are saved directly in one bytestream. Because of that:
- the function named 'PutXX' will be saved the values in the order of the function call
- a list value must be completed; late write of list values is not possible
- the stream holds a complete copy of the source data plus keys (memory usage)

The stream is a complete copy of all source data (memory consumption).



```pascal
procedure TMain.PyeKVS_Save();
var cData:array [0..127] of Byte;
    FPYEKVS  : TPYEKVSDocument;
    L1: TPYEKVSList;
    A1,A2: TPYEKVSArray;
begin
    //create a document (pyeKVS-stream)
    FPYEKVS:=TPYEKVSDocument.Create();

    //start with storing values
    FPYEKVS.PutBegin;

    //simple data types
    FPYEKVS.PutInt('Key1', 1);
    FPYEKVS.PutInt('Key2', 2);
    FPYEKVS.PutInt64('Key3', 3);
    FPYEKVS.PutString('Key4', 'Hallo pyeKVS. Make UTF8: ‹÷ƒ¸ˆ‰ﬂ@µß|¥');

    //array type of vtInt32
    A1:=FPYEKVS.PutArray('KeyA1', vtInt32);
    A1.PutInt(10);
    A1.PutInt(20);
    A1.PutInt(30);

    //array type of vtStringUTF8 (dynamic item length!)
    A2:=FPYEKVS.PutArray('KeyA2', vtStringUTF8);
    A2.PutString('String-1');
    A2.PutString('String-2');

    //list type
    L1:=FPYEKVS.PutList('KeyL1');
    L1.PutSingle('Key1', Random*10000);
    L1.PutDouble('Key2', Random*1000000);

    //memory type
    FPYEKVS.PutMemory('Mem1', cData, sizeof(cData));

    //finish the collection (this call close the stram)
    FPYEKVS.PutEnd;

    //save the stream to file
    FPYEKVS.SaveToFile(ExtractFilePath(Application.ExeName)+'pyeKVS_01.data');
    
    //free the document
    FPYEKVS.Free();
end;
```

```pascal
procedure TMain.PyeKVS_Load();
var cData:array [0..127] of Byte;
    FPYEKVS  : TPYEKVSDocument;
    L1: TPYEKVSList;
    A1: TPYEKVSArray;
    i:Integer;
    s:string;
    f:Double;
begin
    //create a document (pyeKVS-stream)
    FPYEKVS:=TPYEKVSDocument.Create();

    //load stream from file
    FPYEKVS.LoadFromFile(ExtractFilePath(Application.ExeName)+'pyeKVS_01.data');

    //Get some values ...
    i:=FPYEKVS.GetInt('Key1');
    i:=FPYEKVS.GetInt('Key1');

    s:=FPYEKVS.GetString('Key4');

    A1:=FPYEKVS.GetArray('KeyA1');
    if (A1<>nil) then begin
        //Values by index
        i:=A1.GetInt(0);
        i:=A1.GetInt(1);
        i:=A1.GetInt(2);
    end;

    L1:=FPYEKVS.GetList('KeyL1');
    if (L1<>nil) then begin
        f:=L1.GetSingle('Key1');
        f:=L1.GetDouble('Key2');
    end;

    FPYEKVS.GetMemory('Mem1', cData, sizeof(cData));

    //free the document
    FPYEKVS.Free();
end;
```

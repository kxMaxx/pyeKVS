unit pyeKVS;

{ ====================================================================================
  Projekt: PYEKVS (Pye-Key-Value-Storage)
  Description: simple key value serialization format
  Compiler: Embarcadero Delphi
  Author: Michael Koch (kxMaxx)
  License: MIT
  ====================================================================================
  Copyright (c) 2021 Michael Koch

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
  ====================================================================================
  pyeKVS format specification:
  https://kxtec.de/projects/pyekvs/specification
  pyeKVS_Spec.md
  Version 1.0
  ====================================================================================
  Pascal API implementation:
  https://kxtec.de/projects/pyekvs/delphi_api
  Readme.md
  Version: 1 from 01.03.2021 for pyeKVS V 1.0
  - initial release
  Version: 2 from 14.03.2021 for pyeKVS V 1.0
  - update pyeList to use external interfaces
  - update pyeDocument
  - New constructor to define custom container classes for
  pyeKVSList, pyeKVSArray and pyeKVSArrayMap
  User can overwrite these classes to e.g. define own interfaces.
  - New constructor to define custom stream classes. Useful to write values
  direct in userdef stream class
  Version: 3 from 02.08.2021 for pyeKVS V 1.0
  - Interface TPYEKVSCSVOutput to create CSV output format (external unit)
  ==================================================================================== }

interface

uses
    System.Classes,
    System.DateUtils,
    System.SysUtils,
    System.Math,
    System.RTLConsts,
    System.Generics.Collections;

const
    // Version of the format implementation

    PYEKVSVersionH=1; { . } PYEKVSVersionL=0;

type
    EPYEKVSException=class(Exception);

    TPYEKVSKey=AnsiString;
    TPYEKVSKeySize=Byte;

    TPYEKVSValueTypeSize=Byte;

    TPYEKVSValueType=(
        pyeUnknown,
        pyeList,        // dynamic length; special header 4Bytes Size + 4Bytes Count
        pyeZero,        // 0 Bytes; Bool=false; Int=0; Float=0; UFT8=''; Mem=nil
        pyeBool,        // 0 Bytes; Bool=true
        pyeInt8,        // 1 Bytes; little endian; -128 - 127
        pyeUInt8,       // 1 Bytes; little endian;    0 - 255
        pyeInt16,       // 2 Bytes; little endian; -32.768 - 32.767
        pyeUInt16,      // 2 Bytes; little endian;       0 - 65.535
        pyeInt32,       // 4 Bytes; little endian; -2.147.483.648 - 2.147.483.647
        pyeUInt32,      // 4 Bytes; little endian;              0 - 4.294.967.295
        pyeInt64,       // 8 Bytes; little endian; -9.223.372.036.854.775.808 -  9.223.372.036.854.775.807
        pyeUInt64,      // 8 Bytes; little endian;                          0 - 18.446.744.073.709.551.615
        pyeInt128,      // 16 Bytes; little endian; -1,70141E38 - 1,70141E38
        pyeUInt128,     // 16 Bytes; little endian;           0 - 3,40282E38
        pyeFloat32,     // 8 Bytes; little endian; single
        pyeFloat64,     // 16 Bytes; little endian; double
        pyeFloat128,    // 32 Bytes; little endian; Reserve
        pyeStringUTF8S, // dynamic length; special header S-mall 1Byte char count + string as UTF8 chars; max 255 chars
        pyeStringUTF8L, // dynamic length; special header L-arge 4Byte char count + string as UTF8 chars; max 2147483647 chars
        pyeMemory,      // dynamic length; special header 4Byte mem size + values as byte stream
        pyeArray,       // dynamic length; special header 1Byte ValueType + 4Bytes Size + 4Bytes Count + values
        pyeArrayMap     // dynamic length; special header 2Byte map length + map +  4Bytes Size + 4Bytes Count + values
    );
    TPYEKVSValueTypeSet=set of TPYEKVSValueType;

    TPYEKVSValueTypeMap=array of TPYEKVSValueType;

    TPYEKVSValueTypeMap_Maker=record
        Map: TPYEKVSValueTypeMap;
        Index: Integer;
        // dynamic creation; CountSet+Add+Add+Add+...
        procedure CountSet(const aCount: Word);
        procedure Add(const aVT: TPYEKVSValueType);
        // static Map return for count 1..10
        class function Map1(const aVT1: TPYEKVSValueType): TPYEKVSValueTypeMap; static;
        class function Map2(const aVT1, aVT2: TPYEKVSValueType): TPYEKVSValueTypeMap; static;
        class function Map3(const aVT1, aVT2, aVT3: TPYEKVSValueType): TPYEKVSValueTypeMap; static;
        class function Map4(const aVT1, aVT2, aVT3, aVT4: TPYEKVSValueType): TPYEKVSValueTypeMap; static;
        class function Map5(const aVT1, aVT2, aVT3, aVT4, aVT5: TPYEKVSValueType): TPYEKVSValueTypeMap; static;
        class function Map6(const aVT1, aVT2, aVT3, aVT4, aVT5, aVT6: TPYEKVSValueType): TPYEKVSValueTypeMap; static;
        class function Map7(const aVT1, aVT2, aVT3, aVT4, aVT5, aVT6, aVT7: TPYEKVSValueType): TPYEKVSValueTypeMap; static;
        class function Map8(const aVT1, aVT2, aVT3, aVT4, aVT5, aVT6, aVT7, aVT8: TPYEKVSValueType): TPYEKVSValueTypeMap; static;
        class function Map9(const aVT1, aVT2, aVT3, aVT4, aVT5, aVT6, aVT7, aVT8, aVT9: TPYEKVSValueType): TPYEKVSValueTypeMap; static;
        class function Map10(const aVT1, aVT2, aVT3, aVT4, aVT5, aVT6, aVT7, aVT8, aVT9, aVT10: TPYEKVSValueType): TPYEKVSValueTypeMap; static;
    end;


    TPYEKVSBranchState=(
        srzstate_Idle,
        srzstate_PutValues,
        srzstate_Closed
    );

    /// internal Stream type for the document
    TPYEKVSStreamType=(
        pyeStreamMemory, // document use a internal memory stream; double memory consumption; good for TCP transmistions
        pyeStreamFile,   // document use a internal file stream; values Put function write direct in the file; low memory consumption
        pyeStreamUser    // document use a forign user stream; values Put function write direct in that stream
    );


    // Head of every store stream
    // useful by stream reading (e.g. socket operations)
    PPYEKVSStreamHeader=^TPYEKVSStreamHeader;

    TPYEKVSStreamHeader=packed record
        StreamPrefix: Longword; // 4Bytes constant
        StreamVersionH: Word;   // 2Bytes version high of pyeKVS protocol
        StreamVersionL: Word;   // 2Bytes version low of pyeKVS protocol
        StreamSize: UInt64;     // 8Bytes size of data after the StoreDataHeader; exluded header size
    end;

    // substitute for types that are not defined in Delphi
    TPYEKVSInt128=array [0..15] of Byte;
    TPYEKVSUInt128=array [0..15] of Byte;
    TPYEKVSFloat128=array [0..15] of Byte;

const
    cPYEKVSStreamPrefix: Longword=$53455950; // PYES (PYE-Store)


const
    cPYEKVSValueTypeName: array [TPYEKVSValueType] of string=(
        'Unknown',
        'List',
        'Zero',
        'Bool',
        'Int8',
        'UInt8',
        'Int16',
        'UInt16',
        'Int32',
        'UInt32',
        'Int64',
        'UInt64',
        'Int128',
        'UInt128',
        'Float32',
        'Float64',
        'Float128',
        'StringUTF8S',
        'StringUTF8L',
        'Memory',
        'Array',
        'ArrayMap'
    );


    // ToString output
    Str_PYEKVSSpace=' ';
    Str_PYEKVSStringQuote='"';
    Str_PYEKVSComma=',';
    Str_PYEKVSTypeSeperatorO='(';
    Str_PYEKVSTypeSeperatorC=')';
    Str_PYEKVSValueSeperator=':';
    Str_PYEKVSArraySeperatorO='[';
    Str_PYEKVSArraySeperatorC=']';
    Str_PYEKVSArrayTypeOf='of';
    Str_PYEKVSArrayCount='Count';
    Str_PYEKVSListSeperatorO='{';
    Str_PYEKVSListSeperatorC='}';
    Str_PYEKVSValueBinary='Len:%d Data:$%x';
    Str_PYEKVSUnknown='Unknown value type';
    Str_PYEKVSZero='0';
    Str_PYEKVSBool='1';


type
    TPYEKVSCSVOutput=interface
        ['{8FCFE8AB-D613-434D-BD3F-A9AC4EBDD7CF}']
        function GetRowCurrent: Integer;
        procedure SetRowCurrent(const aValue: Integer);

        //Clear CSV
        procedure Clear();

        //Row control
        property RowCurrent: Integer read GetRowCurrent write SetRowCurrent;
        procedure RowReset; //Row=0
        procedure RowNext;  //Row=Row+1

        //Add a Key/Value at specific row
        function AddKV(const aRow: Integer; const aKey: string; const aValue: string):Integer;
    end;

type
    TPYEKVSDocument=class;
    TPYEKVSList=class;

    TPYEKVSMethod=procedure() of object;


    /// <summary>
    /// TPYEKVSObject base class with QueryInterface implemention
    /// Gives the possibility that derived classes can use an interface without reference counter.
    /// </summary>
    TPYEKVSObject=class(TObject)
    protected
        function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
        function _AddRef: Integer; stdcall;
        function _Release: Integer; stdcall;
    end;


    TPYEKVSBranch=class(TPYEKVSObject)
    protected
        FStoreDocument: TPYEKVSDocument; // Owner; hold the stream
        FStoreParent  : TPYEKVSBranch;   // Parent

        FKey     : TPYEKVSKey; // Key (name) of the list
        FSizeData: Int64;      // calculated in WriteStreamBegin
        FState   : TPYEKVSBranchState;

        FOnSetPutClosed: TPYEKVSMethod;

        function Encode(): Integer; virtual;

        procedure SetPutValues(); virtual;
        procedure SetPutClosed(); virtual;
        procedure SizeChildBranch(const aSize: Int64);
        function DetailsToStrings(): string; virtual;
    public
        // Convert a Value to the best mached pyeValueType
        class function ValueTypeOf(const aValue: Boolean): TPYEKVSValueType; overload;
        class function ValueTypeOf(const aValue: Integer): TPYEKVSValueType; overload;
        class function ValueTypeTst(const aValue: Integer; const aVT: TPYEKVSValueType): Boolean; overload;
        class function ValueTypeOf(const aValue: Int64): TPYEKVSValueType; overload;
        class function ValueTypeTst(const aValue: Int64; const aVT: TPYEKVSValueType): Boolean; overload;
        class function ValueTypeOf(const aValue: UInt64): TPYEKVSValueType; overload;
        class function ValueTypeTst(const aValue: UInt64; const aVT: TPYEKVSValueType): Boolean; overload;
        class function ValueTypeOf(const aValue: Cardinal): TPYEKVSValueType; overload;
        class function ValueTypeTst(const aValue: Cardinal; const aVT: TPYEKVSValueType): Boolean; overload;
        class function ValueTypeOf(const aValue: Single): TPYEKVSValueType; overload;
        class function ValueTypeOf(const aValue: Double): TPYEKVSValueType; overload;
        class function ValueTypeOf(const aValue: UTF8String): TPYEKVSValueType; overload;
        class function ValueTypeTst(const aValue: UTF8String; const aVT: TPYEKVSValueType): Boolean; overload;

        constructor Create(const aStoreDocument: TPYEKVSDocument; const aStoreParent: TPYEKVSBranch; const aKey: TPYEKVSKey); virtual;
        destructor Destroy; override;
        procedure Clear(); virtual;

        procedure ToStringsJSON(const aString: TStrings; const aIndenting: string; const aActIndenting: string; const aOutName: Boolean); virtual;
        procedure ToStringsSimple(const aString: TStrings; const aIndenting: string; const aActIndenting: string); virtual;

        procedure ToOutputCSV(aCSVOutput: TPYEKVSCSVOutput); virtual;

        property Parent: TPYEKVSBranch read FStoreParent;
        property Key: TPYEKVSKey read FKey;
        procedure KeyAndParent(var aKeyName: TPYEKVSKey); virtual;

        property State: TPYEKVSBranchState read FState;

        property OnSetPutClosed: TPYEKVSMethod read FOnSetPutClosed write FOnSetPutClosed;
    end;

    RPYEKVSArray=class of TPYEKVSArray;

    TPYEKVSArray=class(TPYEKVSBranch)
    private
        FPosSize         : Int64;                        // StreamPosition of Size-Value
        FPosCount        : Int64;                        // StreamPosition of Count-Value
        FPosValues       : Int64;                        // StreamPosition of first value
        FPosList         : TDictionary<Cardinal, Int64>; // Index in array -> StreamPosition; only dynamic Values StringUTF8
        FValueType       : TPYEKVSValueType;
        FValueTypeAllowed: TPYEKVSValueTypeSet;
        FCount           : Cardinal;
        procedure PosListAdd(const aIndex: Cardinal; const aPos: Int64);
        function GetCount: Int64;
    protected
        procedure SetValueTypeAllowed(const aValueType: TPYEKVSValueType);
        function Encode(): Integer; override;
        function WriteHeader(const aValueType: TPYEKVSValueType): Integer;
        procedure SetPutClosed(); override;
        function DetailsToStrings(): string; override;

        procedure GetIndexPosCalc(const aIndex: Cardinal);
        procedure GetIndexPosList(const aIndex: Cardinal);
    public
        constructor Create(const aStoreDocument: TPYEKVSDocument; const aStoreParent: TPYEKVSBranch; const aKey: TPYEKVSKey); overload; override;
        constructor Create(const aStoreDocument: TPYEKVSDocument; const aStoreParent: TPYEKVSBranch; const aKey: TPYEKVSKey; const aValueType: TPYEKVSValueType); reintroduce; overload;
        destructor Destroy; override;
        procedure Clear(); override;

        property ValueType: TPYEKVSValueType read FValueType;
        property ValueTypeAllowed: TPYEKVSValueTypeSet read FValueTypeAllowed;
        property Count: Int64 read GetCount;

        procedure ToStringsJSON(const aString: TStrings; const aIndenting: string; const aActIndenting: string; const aOutName: Boolean); override;
        procedure ToStringsSimple(const aString: TStrings; const aIndenting: string; const aActIndenting: string); override;

        procedure ToOutputCSV(aCSVOutput: TPYEKVSCSVOutput); override;

        // == Put/Get-Functions based on the requirements of the programming language ==

        // Integer (Int8, Int16, Int32)
        procedure PutInt(const aValue: Integer);
        function GetInt(const aIndex: Cardinal; const aDefault: Integer=0): Integer;

        // Int64
        procedure PutInt64(const aValue: Int64);
        function GetInt64(const aIndex: Cardinal; const aDefault: Int64=0): Int64;

        // UInt (UInt8, UInt16, UInt32)
        procedure PutUInt(const aValue: Cardinal);
        function GetUInt(const aIndex: Cardinal; const aDefault: Cardinal=0): Cardinal;

        // UInt64
        procedure PutUInt64(const aValue: UInt64);
        function GetUInt64(const aIndex: Cardinal; const aDefault: UInt64=0): UInt64;

        // Float32
        procedure PutSingle(const aValue: Single);
        function GetSingle(const aIndex: Cardinal; const aDefault: Single=0): Single;

        // Float64
        procedure PutDouble(const aValue: Double);
        function GetDouble(const aIndex: Cardinal; const aDefault: Double=0): Double;

        // String as UTF8
        procedure PutString(const aValue: String);
        function GetString(const aIndex: Cardinal; const aDefault: String=''): String;

        procedure PutUTF8String(const aValue: UTF8String);
        function GetUTF8String(const aIndex: Cardinal; const aDefault: UTF8String=''): UTF8String;

        // Memory
        procedure PutMemory(const aValue: TStream); overload;
        function GetMemory(const aIndex: Cardinal; aValue: TStream): Integer; overload;

        procedure PutMemory(const aBuffer: Pointer; aSize: Cardinal); overload;
        function GetMemory(const aIndex: Cardinal; const aBuffer: Pointer; aSize: Cardinal): Integer; overload;
    end;


    TPYEKVSArrayMapValues=array of Variant;

    RPYEKVSArrayMap=class of TPYEKVSArrayMap;

    TPYEKVSArrayMap=class(TPYEKVSBranch)
    private
        FPosSize     : Int64;                        // StreamPosition of Size-Value
        FPosCount    : Int64;                        // StreamPosition of Count-Value
        FPosValues   : Int64;                        // StreamPosition of first value
        FPosList     : TDictionary<Cardinal, Int64>; // Index in array -> StreamPosition; only dynamic Values StringUTF8
        FValueTypeMap: TPYEKVSValueTypeMap;
        FCount       : Cardinal;
        procedure PosListAdd(const aIndex: Cardinal; const aPos: Int64);
        function GetCount: Int64;
    protected
        function Encode(): Integer; override;
        function WriteHeader(): Integer;
        procedure SetPutClosed(); override;
        function DetailsToStrings(): string; override;

        procedure GetIndexPosList(const aIndex: Cardinal);
    public
        constructor Create(const aStoreDocument: TPYEKVSDocument; const aStoreParent: TPYEKVSBranch; const aKey: TPYEKVSKey); overload; override;
        constructor Create(const aStoreDocument: TPYEKVSDocument; const aStoreParent: TPYEKVSBranch; const aKey: TPYEKVSKey; const aValueTypeMap: TPYEKVSValueTypeMap); reintroduce; overload;
        destructor Destroy; override;
        procedure Clear(); override;

        // ValueTypeMap of the pyeArrayMap; after PutItem or Encode
        property ValueTypeMap: TPYEKVSValueTypeMap read FValueTypeMap;

        // ValueTypeMap compare internal map with external aMap
        function ValueTypeMapEqual(const aMap: TPYEKVSValueTypeMap): Boolean;

        property Count: Int64 read GetCount;

        procedure ToStringsJSON(const aString: TStrings; const aIndenting: string; const aActIndenting: string; const aOutName: Boolean); override;
        procedure ToStringsSimple(const aString: TStrings; const aIndenting: string; const aActIndenting: string); override;

        procedure ToOutputCSV(aCSVOutput: TPYEKVSCSVOutput); override;

        // General access; aValueMap must match to aValueTypeMap of the constructor
        procedure PutItem(const aValueMap: array of Variant);
        function GetItem(const aIndex: Cardinal): TPYEKVSArrayMapValues;
    end;


    TPYEKVSMemoryScope=class(TStream)
    private
        FParentStream: TStream;
        FStartPos    : Int64;
        FCurrentPos  : Int64;
        FMaxSize     : Int64;
    protected
        procedure SetSize(const NewSize: Int64); override;
    public
        constructor Create(const aParentStream: TStream); overload;
        function Read(var Buffer; Count: Longint): Longint; override;
        function Write(const Buffer; Count: Longint): Longint; override;
        function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

        property ParentStream: TStream read FParentStream;
        property MaxSize: Int64 read FMaxSize;
    end;

    /// Helper class to create a stream for valuetype pyeMemory
    /// Accept late write. This means, it allowes to fill the Memorystream after the call PutStream.
    TPYEKVSMemoryStream=class(TPYEKVSBranch)
    private
        FValue  : TPYEKVSMemoryScope;
        FPosSize: Int64; // StreamPosition of Size-Value
    protected
        function WriteHeader(): Integer;
        procedure SetPutClosed(); override;
        function GetStream: TStream;
    public
        constructor Create(const aStoreDocument: TPYEKVSDocument; const aStoreParent: TPYEKVSBranch; const aKey: TPYEKVSKey); overload; override;
        constructor Create(const aStoreDocument: TPYEKVSDocument; const aStoreParent: TPYEKVSBranch; const aKey: TPYEKVSKey; const aPos: Int64); reintroduce; overload;

        destructor Destroy; override;
        procedure Clear(); override;
    end;


    RPYEKVSList=class of TPYEKVSList;

    TPYEKVSList=class(TPYEKVSBranch)
    private
        FKeys: TDictionary<TPYEKVSKey, Int64>;          // Key -> StreamPosition of FData (start with ValueType)
        FList: TObjectDictionary<Int64, TPYEKVSBranch>; // StreamPosition -> SubList

        FPosSize : Int64; // StreamPosition of Size-Value
        FPosCount: Int64; // StreamPosition of Count-Value

        procedure KeyAdd(const aKey: TPYEKVSKey; const aPosition: Int64); inline;
        function KeySorted: TArray<TPYEKVSKey>;

    protected
        function Encode(): Integer; override;

        function WriteHeader(): Integer;

        procedure SetPutClosed(); override;

        function DetailsToStrings(): string; override;
    public
        constructor Create(const aStoreDocument: TPYEKVSDocument; const aStoreParent: TPYEKVSBranch; const aKey: TPYEKVSKey); override;
        destructor Destroy; override;
        procedure Clear(); override;

        procedure ToStringsJSON(const aString: TStrings; const aIndenting: string; const aActIndenting: string; const aOutName: Boolean); override;
        procedure ToStringsSimple(const aString: TStrings; const aIndenting: string; const aActIndenting: string); override;

        procedure ToOutputCSV(aCSVOutput: TPYEKVSCSVOutput); override;

        // Check key in this list and return ValueType (or Unknown if not exist)
        function GetKey(const aKey: TPYEKVSKey): TPYEKVSValueType;
        function KeyExists(const aKey: TPYEKVSKey): Boolean;

        // == Put/Get-Functions based on the requirements of the programming language ==

        // fundamental values ...

        // Zero
        procedure PutZero(const aKey: TPYEKVSKey);
        function GetZero(const aKey: TPYEKVSKey): Boolean;

        // Boolean
        procedure PutBool(const aKey: TPYEKVSKey; const aValue: Boolean);
        function GetBool(const aKey: TPYEKVSKey; const aDefault: Boolean=false): Boolean;

        // Integer (Int8, Int16, Int32)
        procedure PutInt(const aKey: TPYEKVSKey; const aValue: Integer);
        procedure PutIntVT(const aKey: TPYEKVSKey; const aValue: Integer; const aForceValueType: TPYEKVSValueType);
        function GetInt(const aKey: TPYEKVSKey; const aDefault: Integer=0): Integer;
        procedure ChgInt(const aKey: TPYEKVSKey; const aValue: Integer);

        // Int64
        procedure PutInt64(const aKey: TPYEKVSKey; const aValue: Int64);
        procedure PutInt64VT(const aKey: TPYEKVSKey; const aValue: Int64; const aForceValueType: TPYEKVSValueType);
        function GetInt64(const aKey: TPYEKVSKey; const aDefault: Int64=0): Int64;
        procedure ChgInt64(const aKey: TPYEKVSKey; const aValue: Int64);

        // Int128
        procedure PutInt128(const aKey: TPYEKVSKey; const aValue: TPYEKVSInt128);
        function GetInt128(const aKey: TPYEKVSKey; const aDefault: TPYEKVSInt128): TPYEKVSInt128;

        // UInt (UInt8, UInt16, UInt32)
        procedure PutUInt(const aKey: TPYEKVSKey; const aValue: Cardinal);
        procedure PutUIntVT(const aKey: TPYEKVSKey; const aValue: Cardinal; const aForceValueType: TPYEKVSValueType);
        function GetUInt(const aKey: TPYEKVSKey; const aDefault: Cardinal=0): Cardinal;
        procedure ChgUInt(const aKey: TPYEKVSKey; const aValue: Cardinal);

        // UInt64
        procedure PutUInt64(const aKey: TPYEKVSKey; const aValue: UInt64);
        procedure PutUInt64VT(const aKey: TPYEKVSKey; const aValue: UInt64; const aForceValueType: TPYEKVSValueType);
        function GetUInt64(const aKey: TPYEKVSKey; const aDefault: UInt64=0): UInt64;
        procedure ChgUInt64(const aKey: TPYEKVSKey; const aValue: UInt64);

        // UInt128
        procedure PutUInt128(const aKey: TPYEKVSKey; const aValue: TPYEKVSUInt128);
        function GetUInt128(const aKey: TPYEKVSKey; const aDefault: TPYEKVSUInt128): TPYEKVSUInt128;

        // Float32
        procedure PutSingle(const aKey: TPYEKVSKey; const aValue: Single);
        function GetSingle(const aKey: TPYEKVSKey; const aDefault: Single=0): Single;

        // Float64
        procedure PutDouble(const aKey: TPYEKVSKey; const aValue: Double);
        function GetDouble(const aKey: TPYEKVSKey; const aDefault: Double=0): Double;

        // String
        procedure PutString(const aKey: TPYEKVSKey; const aValue: String);
        procedure PutStringVT(const aKey: TPYEKVSKey; const aValue: String; const aForceValueType: TPYEKVSValueType);
        function GetString(const aKey: TPYEKVSKey; const aDefault: String=''): String;
        procedure ChgString(const aKey: TPYEKVSKey; const aValue: String);

        procedure PutUTF8String(const aKey: TPYEKVSKey; const aValue: UTF8String);
        procedure PutUTF8StringVT(const aKey: TPYEKVSKey; const aValue: UTF8String; const aForceValueType: TPYEKVSValueType);
        function GetUTF8String(const aKey: TPYEKVSKey; const aDefault: UTF8String=''): UTF8String;
        procedure ChgUTF8String(const aKey: TPYEKVSKey; const aValue: UTF8String);

        // Memory
        procedure PutMemory(const aKey: TPYEKVSKey; const aValue: TStream); overload;
        function GetMemory(const aKey: TPYEKVSKey; aValue: TStream): Integer; overload;

        procedure PutMemory(const aKey: TPYEKVSKey; const aBuffer: Pointer; aSize: Cardinal); overload;
        function GetMemory(const aKey: TPYEKVSKey; const aBuffer: Pointer; aSize: Cardinal): Integer; overload;

        function PutStream(const aKey: TPYEKVSKey): TStream;
        function GetStream(const aKey: TPYEKVSKey): TStream;

        // container values ...

        // List
        function PutList(const aKey: TPYEKVSKey): TPYEKVSList;
        function GetList(const aKey: TPYEKVSKey): TPYEKVSList;

        // Array
        function PutArray(const aKey: TPYEKVSKey; const aValueType: TPYEKVSValueType): TPYEKVSArray;
        function GetArray(const aKey: TPYEKVSKey): TPYEKVSArray;

        // ArrayMap
        function PutArrayMap(const aKey: TPYEKVSKey; const aValueTypeMap: TPYEKVSValueTypeMap): TPYEKVSArrayMap;
        function GetArrayMap(const aKey: TPYEKVSKey): TPYEKVSArrayMap;
    end;


    // Default Stream class of a pyeKVS document; memory stream
    TPYEKVSStreamDefault=TBytesStream;

    // pyeKVS document is the master object
    // - holder of the pyeKVS root list
    // - holder of the pyeKVS stream (hold all values)
    TPYEKVSDocument=class(TPYEKVSObject)
    private
        RClassPYEKVSList    : RPYEKVSList;     // ClassReference of uses TPYEKVSList; possibility to use custom call
        RClassPYEKVSArray   : RPYEKVSArray;    // ClassReference of uses TPYEKVSArray; possibility to use custom call
        RClassPYEKVSArrayMap: RPYEKVSArrayMap; // ClassReference of uses TPYEKVSArrayMap; possibility to use custom call

        FRoot: TPYEKVSList; // Root List

        FFallbackEnabled : Boolean;
        FFallbackList    : TPYEKVSList;
        FFallbackArray   : TPYEKVSArray;
        FFallbackArrayMap: TPYEKVSArrayMap;

        FStream    : TStream; // Datastream; values with ValueType+Data
        FStreamType: TPYEKVSStreamType;
        FStreamOwns: Boolean;

        FStreamHeader: TPYEKVSStreamHeader;

        // PutBegin/End control
        FPutCounter : Integer;
        FWriteBranch: TPYEKVSBranch;

        function FallbackList: TPYEKVSList;
        function FallbackArray: TPYEKVSArray;
        function FallbackArrayMap: TPYEKVSArrayMap;

        procedure WriteDataHeader;
        procedure WriteRoot;

        // Standard Write; Result=Bytes written
        function _WriteInt8(const aValue: Int8): Integer;
        function _WriteUInt8(const aValue: UInt8): Integer;
        function _WriteInt16(const aValue: Int16): Integer;
        function _WriteUInt16(const aValue: UInt16): Integer;
        function _WriteInt32(const aValue: Int32): Integer;
        function _WriteUInt32(const aValue: UInt32): Integer;
        function _WriteInt64(const aValue: Int64): Integer;
        function _WriteUInt64(const aValue: UInt64): Integer;
        function _WriteInt128(const aValue: TPYEKVSInt128): Integer;
        function _WriteUInt128(const aValue: TPYEKVSUInt128): Integer;

        function _WriteFloat32(const aValue: Single): Integer;
        function _WriteFloat64(const aValue: Double): Integer;
        function _WriteStringUTF8S(const aValue: UTF8String): Integer;
        function _WriteStringUTF8L(const aValue: UTF8String): Integer;
        function _WriteMemoryStream(const aValue: TStream): Integer;
        function _WriteMemoryBuffer(const aValue: Pointer; aSize: Cardinal): Integer;

        // Standard Read; Result=Bytes read
        function _ReadInt8(out aValue: Int8): Integer;
        function _ReadUInt8(out aValue: UInt8): Integer;
        function _ReadInt16(out aValue: Int16): Integer;
        function _ReadUInt16(out aValue: UInt16): Integer;
        function _ReadInt32(out aValue: Int32): Integer;
        function _ReadUInt32(out aValue: UInt32): Integer;
        function _ReadInt64(out aValue: Int64): Integer;
        function _ReadUInt64(out aValue: UInt64): Integer;
        function _ReadInt128(out aValue: TPYEKVSInt128): Integer;
        function _ReadUInt128(out aValue: TPYEKVSUInt128): Integer;
        function _ReadFloat32(out aValue: Single): Integer;
        function _ReadFloat64(out aValue: Double): Integer;
        function _ReadFloat128(out aValue: TPYEKVSFloat128): Integer;
        function _ReadStringUTF8(out aValue: UTF8String; const aLen: Cardinal): Integer;
        function _ReadStringUTF8S(out aValue: UTF8String): Integer;
        function _ReadStringUTF8L(out aValue: UTF8String): Integer;
        function _ReadMemoryStream(aValue: TStream; out aByteRead: Integer): Integer;
        function _ReadMemoryBuffer(const aValue: Pointer; aSize: Cardinal; out aByteRead: Integer): Integer;

        // Standard Read; Result=Value
        function ReadInt8(): Int8;
        function ReadUInt8(): UInt8;
        function ReadInt16(): Int16;
        function ReadUInt16(): UInt16;
        function ReadInt32(): Int32;
        function ReadUInt32(): UInt32;
        function ReadInt64(): Int64;
        function ReadUInt64(): UInt64;
        function ReadFloat32(): Single;
        function ReadFloat64(): Double;
        function ReadStringUTF8S(): UTF8String;
        function ReadStringUTF8L(): UTF8String;

        // Write by ValueType; Result=Bytes written
        function WriteVTInt(const aValue: Integer; const aVT: TPYEKVSValueType): Integer;
        function WriteVTInt64(const aValue: Int64; const aVT: TPYEKVSValueType): Integer;
        function WriteVTUInt(const aValue: Cardinal; const aVT: TPYEKVSValueType): Integer;
        function WriteVTUInt64(const aValue: UInt64; const aVT: TPYEKVSValueType): Integer;
        function WriteVTSingle(const aValue: Single; const aVT: TPYEKVSValueType): Integer;
        function WriteVTDouble(const aValue: Double; const aVT: TPYEKVSValueType): Integer;
        function WriteVTStringUTF8(const aValue: UTF8String; const aVT: TPYEKVSValueType): Integer;
        function WriteVTMemoryStream(const aValue: TStream; const aVT: TPYEKVSValueType): Integer;
        function WriteVTMemoryBuffer(const aValue: Pointer; aSize: Cardinal; const aVT: TPYEKVSValueType): Integer;

        // Read by ValueType; Result=Bytes read
        function ReadVTBool(out aValue: Boolean; const aVT: TPYEKVSValueType; const aDefault: Boolean): Integer;
        function ReadVTInt32(out aValue: Int32; const aVT: TPYEKVSValueType; const aDefault: Int32): Integer;
        function ReadVTInt64(out aValue: Int64; const aVT: TPYEKVSValueType; const aDefault: Int64): Integer;
        function ReadVTInt128(out aValue: TPYEKVSInt128; const aVT: TPYEKVSValueType; const aDefault: TPYEKVSInt128): Integer;
        function ReadVTUInt32(out aValue: UInt32; const aVT: TPYEKVSValueType; const aDefault: UInt32): Integer;
        function ReadVTUInt64(out aValue: UInt64; const aVT: TPYEKVSValueType; const aDefault: UInt64): Integer;
        function ReadVTUInt128(out aValue: TPYEKVSUInt128; const aVT: TPYEKVSValueType; const aDefault: TPYEKVSUInt128): Integer;
        function ReadVTSingle(out aValue: Single; const aVT: TPYEKVSValueType; const aDefault: Single): Integer;
        function ReadVTDouble(out aValue: Double; const aVT: TPYEKVSValueType; const aDefault: Double): Integer;
        function ReadVTStringUTF8(const aVT: TPYEKVSValueType; const aDefault: UTF8String): UTF8String;
        function ReadVTMemoryStream(const aVT: TPYEKVSValueType; aValue: TStream): Integer;
        function ReadVTMemoryBuffer(const aVT: TPYEKVSValueType; const aValue: Pointer; aSize: Cardinal): Integer;

        function ReadKey(out aKey: TPYEKVSKey): Integer;
        function WriteKey(const aBranch: TPYEKVSBranch; const aKey: TPYEKVSKey): Integer;

        function ReadValueType(out aVT: TPYEKVSValueType): Integer;
        function WriteValueType(const aVT: TPYEKVSValueType): Integer;

        // Read/Write UInt8 value (non pyeKVS); used for ValueHeader size and count
        function ReadFixUInt8(out aValue: UInt8): Integer;
        function WriteFixUInt8(const aValue: UInt8; const aPosition: Int64=0): Integer;

        // Read/Write UInt32 value (non pyeKVS); used for ValueHeader size and count
        function ReadFixUInt32(out aValue: UInt32): Integer;
        function WriteFixUInt32(const aValue: UInt32; const aPosition: Int64=0): Integer;

        // activate a branch for writing; other branches must be finished
        procedure WriteBranchSet(const aBranch: TPYEKVSBranch);

        // Read fundamental values given by aVT to a string
        function ReadToString(const aVT: TPYEKVSValueType): string;

        function EncodeVT(const aValueType: TPYEKVSValueType): Integer;
        function EncodeFundamental(const aValueType: TPYEKVSValueType): Integer;
        function EncodeStringUTF8S(): Integer;
        function EncodeStringUTF8L(): Integer;
        function EncodeMemory(): Integer;

        function GetState: TPYEKVSBranchState;

        function GetStreamPosition: Int64; inline;
        procedure SetStreamPosition(const Value: Int64); inline;
        function GetStreamSize: Int64;
    protected
        procedure CreateInternal(aClassPYEKVSList: RPYEKVSList; aClassPYEKVSArray: RPYEKVSArray; aClassPYEKVSArrayMap: RPYEKVSArrayMap); virtual;
        procedure DoPutBegin(); virtual;
        procedure DoPutEnd(); virtual;
    public
        /// pyeKVS document with memory stream class; read/write to memory
        constructor Create(aClassPYEKVSList: RPYEKVSList=nil; aClassPYEKVSArray: RPYEKVSArray=nil; aClassPYEKVSArrayMap: RPYEKVSArrayMap=nil); overload;

        /// pyeKVS document with file stream; direct read/write to file
        constructor Create(const aFileName: string; aFileMode: Word; aFileRights: Cardinal=0; aClassPYEKVSList: RPYEKVSList=nil; aClassPYEKVSArray: RPYEKVSArray=nil; aClassPYEKVSArrayMap: RPYEKVSArrayMap=nil); overload;

        /// pyeKVS document with forign user stream; direct read/write to aUserStream
        constructor Create(const aUserStream: TStream; aOwnsStream: Boolean; aClassPYEKVSList: RPYEKVSList=nil; aClassPYEKVSArray: RPYEKVSArray=nil; aClassPYEKVSArrayMap: RPYEKVSArrayMap=nil); overload;

        destructor Destroy; override;

        procedure Clear(); virtual;
        procedure Reset(); virtual;

        // Output the values to aString; this will read the values from the stream
        procedure ToStringsJSON(const aString: TStrings; const aIndenting: string); virtual;
        procedure ToStringsSimple(const aString: TStrings; const aIndenting: string); virtual;

        procedure ToOutputCSV(aCSVOutput: TPYEKVSCSVOutput); virtual;

        // Show the state of the root list
        property State: TPYEKVSBranchState read GetState;

        // Control the writing of a branch
        procedure WriteBegin(const aNewWriteBranch: TPYEKVSBranch);
        procedure WriteEnd(const aNewWriteBranch: TPYEKVSBranch);

        // Put values is only allowed between PutBegin und PutEnd calls
        procedure PutBegin(); virtual;
        procedure PutEnd(); virtual;
        property PutCounter: Integer read FPutCounter;

        // Read a stream and find values; call before Get-function
        function Encode(): Integer; virtual;

        // Load the document from a file; this is a stream copy function
        procedure LoadFromFile(aFileName: string);
        // Save the document to a file; this is a stream copy function
        procedure SaveToFile(aFileName: string);

        // Load the document from the stream F; this is a stream copy function
        procedure ReadFromStream(const F: TStream);
        // Save the document to the stream F; this is a stream copy function
        procedure WriteToStream(const F: TStream);

        // Stream access; can be a MemoryStream, FileStream or UserStream
        property Stream: TStream read FStream;
        property StreamType: TPYEKVSStreamType read FStreamType;
        property StreamOwns: Boolean read FStreamOwns;

        // current size of the stream; after PutEnd functions
        property StreamSize: Int64 read GetStreamSize;

        // current stream position
        property StreamPosition: Int64 read GetStreamPosition write SetStreamPosition;

        // case reading FData from stream in blocks
        // return the numbers of Bytes that still pending to read; size of next block
        function DataSizePending: Int64;

        // access to the current StreamHeader; e.g. for TCP transmitions
        property StreamHeader: TPYEKVSStreamHeader read FStreamHeader;

        // check if a StreamHeader is valid by the version (true) or not (false); e.g. for TCP transmitions before access to StreamHeader.StreamSize
        function StreamHeaderUpdate: Boolean;

        // Root list; start of all Get functions
        property Root: TPYEKVSList read FRoot;

        // Control the result of GetList/Array/ArrayMap if key not exists; nil or Fallback object
        property FallbackEnabled: Boolean read FFallbackEnabled write FFallbackEnabled;

    end;

function IntToPyeKVSKey(const aValue: Integer): TPYEKVSKey;

implementation

const
    // predefined size of specific TPYEKVSValueType
    cPYEKVSValueTypeSize: array [TPYEKVSValueType] of Cardinal=(
        { pyeUnknown } 0,
        { pyeList } 0, // dynamic
        { pyeZero } 0,
        { pyeBool } 0,
        { pyeInt8 } 1,   // sizeof(Int8)
        { pyeUInt8 } 1,  // sizeof(UInt8)
        { pyeInt16 } 2,  // sizeof(Int16)
        { pyeUInt16 } 2, // sizeof(UInt16)
        { pyeInt32 } 4,  // sizeof(Int32)
        { pyeUInt32 } 4, // sizeof(UInt32)
        { pyeInt64 } 8,  // sizeof(Int64)
        { pyeUInt64 } 8, // sizeof(UInt64)
        { pyeInt128 } 16,
        { pyeUInt128 } 16,
        { pyeFloat32 } 4, // sizeof(Single)
        { pyeFloat64 } 8, // sizeof(Double)
        { pyeFloat128 } 16,
        { pyeStringUTF8S } 0, // dynamic
        { pyeStringUTF8L } 0, // dynamic
        { pyeMemory } 0,      // dynamic
        { pyeArray } 0,       // dynamic
        { pyeArrayMap } 0     // dynamic
    );

    // Value of TPYEKVSValueType in the stream
    cPYEKVSValueTypeID: array [TPYEKVSValueType] of TPYEKVSValueTypeSize=(
        { pyeUnknown } 000,
        { pyeList } 001,
        { pyeZero } 002,
        { pyeBool } 003,
        { pyeInt8 } 004,
        { pyeUInt8 } 005,
        { pyeInt16 } 006,
        { pyeUInt16 } 007,
        { pyeInt32 } 008,
        { pyeUInt32 } 009,
        { pyeInt64 } 010,
        { pyeUInt64 } 011,
        { pyeInt128 } 012,
        { pyeUInt128 } 013,
        { pyeFloat32 } 014,
        { pyeFloat64 } 015,
        { pyeFloat128 } 016,
        { pyeStringUTF8S } 017,
        { pyeStringUTF8L } 018,
        { pyeMemory } 019,
        { pyeArray } 020,
        { pyeArrayMap } 021
    );

    // Allowed ValueType of Array
    cPYEKVSValueTypeOfArray: TPYEKVSValueTypeSet=[
        pyeInt8,
        pyeUInt8,
        pyeInt16,
        pyeUInt16,
        pyeInt32,
        pyeUInt32,
        pyeInt64,
        pyeUInt64,
        pyeInt128,
        pyeUInt128,
        pyeFloat32,
        pyeFloat64,
        pyeFloat128,
        pyeStringUTF8S,
        pyeStringUTF8L,
        pyeMemory
    ];

    // pyeValueTypes with dynamic length; pyeArray need an encoding for this types
    cPYEKVSValueTypeOfArrayDynamicLen: TPYEKVSValueTypeSet=[
        pyeStringUTF8S,
        pyeStringUTF8L,
        pyeMemory
    ];

    // Exception strings
    cStrPYEKVSExcept_KeyNotFound='Key %s not found';
    cStrPYEKVSExcept_ValueNotCompatible='Value not compatible';

var
    // stream value conversation to TPYEKVSValueType
    GValueTypeValue: array [TPYEKVSValueTypeSize] of TPYEKVSValueType;

    // global english format settings; decimal separator="." for JSON compatibilty
    GFormatSettingsFloat: TFormatSettings;

function IntToPyeKVSKey(const aValue: Integer): TPYEKVSKey;
begin
    result:=TPYEKVSKey(IntToStr(aValue));
end;


{ TPYEKVSDocument }

constructor TPYEKVSDocument.Create(aClassPYEKVSList: RPYEKVSList; aClassPYEKVSArray: RPYEKVSArray; aClassPYEKVSArrayMap: RPYEKVSArrayMap);
begin
    inherited Create;

    FStreamType:=pyeStreamMemory;
    FStreamOwns:=true; // TPYEKVSDocument controls the free
    FStream:=TPYEKVSStreamDefault.Create();

    CreateInternal(aClassPYEKVSList, aClassPYEKVSArray, aClassPYEKVSArrayMap);
end;

/// pyeKVS document with file stream; direct read/write to file
constructor TPYEKVSDocument.Create(const aFileName: string; aFileMode: Word; aFileRights: Cardinal=0; aClassPYEKVSList: RPYEKVSList=nil; aClassPYEKVSArray: RPYEKVSArray=nil; aClassPYEKVSArrayMap: RPYEKVSArrayMap=nil);
begin
    inherited Create;

    FStreamType:=pyeStreamFile;
    FStreamOwns:=true; // TPYEKVSDocument controls the free
    FStream:=TFileStream.Create(aFileName, aFileMode, aFileRights);

    CreateInternal(aClassPYEKVSList, aClassPYEKVSArray, aClassPYEKVSArrayMap);
end;


constructor TPYEKVSDocument.Create(const aUserStream: TStream; aOwnsStream: Boolean; aClassPYEKVSList: RPYEKVSList=nil; aClassPYEKVSArray: RPYEKVSArray=nil; aClassPYEKVSArrayMap: RPYEKVSArrayMap=nil);
begin
    inherited Create;

    FStreamType:=pyeStreamUser;
    FStreamOwns:=aOwnsStream; // TPYEKVSDocument controls the free

    FStream:=aUserStream;

    CreateInternal(aClassPYEKVSList, aClassPYEKVSArray, aClassPYEKVSArrayMap);
end;


procedure TPYEKVSDocument.CreateInternal(aClassPYEKVSList: RPYEKVSList; aClassPYEKVSArray: RPYEKVSArray; aClassPYEKVSArrayMap: RPYEKVSArrayMap);
begin
    // Set default pyeKVS ClassReferences
    RClassPYEKVSList:=TPYEKVSList;
    RClassPYEKVSArray:=TPYEKVSArray;
    RClassPYEKVSArrayMap:=TPYEKVSArrayMap;

    // User define container classes (overide e.g. for interface usage)
    if (aClassPYEKVSList<>nil) then
        RClassPYEKVSList:=aClassPYEKVSList;
    if (aClassPYEKVSArray<>nil) then
        RClassPYEKVSArray:=aClassPYEKVSArray;
    if (aClassPYEKVSArrayMap<>nil) then
        RClassPYEKVSArrayMap:=aClassPYEKVSArrayMap;

    // Create root
    FRoot:=RClassPYEKVSList.Create(self, nil, '');

    FFallbackList:=nil;
    FFallbackArray:=nil;
    FFallbackArrayMap:=nil;

    // Default header
    FStreamHeader.StreamPrefix:=cPYEKVSStreamPrefix;
    FStreamHeader.StreamVersionH:=PYEKVSVersionH;
    FStreamHeader.StreamVersionL:=PYEKVSVersionL;
    FStreamHeader.StreamSize:=0;
end;

destructor TPYEKVSDocument.Destroy;
begin
    if (FStreamOwns) then
        FStream.Free();

    FRoot.Free();

    if (FFallbackList<>nil) then
        FFallbackList:=nil;
    if (FFallbackArray<>nil) then
        FFallbackArray:=nil;
    if (FFallbackArrayMap<>nil) then
        FFallbackArrayMap:=nil;

    inherited;
end;


function TPYEKVSDocument.FallbackList: TPYEKVSList;
begin
    if (FFallbackEnabled) then begin
        if (FFallbackList=nil) then
            FFallbackList:=RClassPYEKVSList.Create(self, nil, '');
        result:=FFallbackList;
    end
    else exit(nil);
end;

function TPYEKVSDocument.FallbackArray: TPYEKVSArray;
begin
    if (FFallbackEnabled) then begin
        if (FFallbackArray=nil) then
            FFallbackArray:=RClassPYEKVSArray.Create(self, nil, '');
        result:=FFallbackArray;
    end
    else exit(nil);
end;

function TPYEKVSDocument.FallbackArrayMap: TPYEKVSArrayMap;
begin
    if (FFallbackEnabled) then begin
        if (FFallbackArrayMap=nil) then
            FFallbackArrayMap:=RClassPYEKVSArrayMap.Create(self, nil, '');
        result:=FFallbackArrayMap;
    end
    else exit(nil);
end;


procedure TPYEKVSDocument.Clear;
begin
    FRoot.Clear();

    if (FStream is TMemoryStream) then begin
        TMemoryStream(FStream).Clear();
    end else begin
        FStream.Position:=0;
        FStream.Size:=0;
    end;

    FWriteBranch:=nil;
end;


procedure TPYEKVSDocument.WriteRoot;
begin
    WriteDataHeader();

    WriteKey(FRoot, ''); // no root key name

    FRoot.WriteHeader();
end;

procedure TPYEKVSDocument.WriteDataHeader;
begin
    if (FStream.Size>sizeof(TPYEKVSStreamHeader)) then
        FStreamHeader.StreamSize:=FStream.Size-sizeof(TPYEKVSStreamHeader)
    else
        FStreamHeader.StreamSize:=0;

    FStream.Position:=0;
    FStream.Write(FStreamHeader, sizeof(FStreamHeader));

    // Go back to end
    FStream.Seek(0, soEnd);
end;

procedure TPYEKVSDocument.PutBegin;
begin
    if (FPutCounter=0) then begin
        DoPutBegin();
    end;
    Inc(FPutCounter);
end;

procedure TPYEKVSDocument.PutEnd;
begin
    Dec(FPutCounter);
    if (FPutCounter=0) then begin
        DoPutEnd();
    end;
end;

procedure TPYEKVSDocument.DoPutBegin;
begin
    FRoot.SetPutValues();
    WriteRoot();
end;

procedure TPYEKVSDocument.DoPutEnd;
begin
    // close all write branches
    WriteEnd(nil);

    // Update size in header
    WriteDataHeader();
end;

procedure TPYEKVSDocument.LoadFromFile(aFileName: string);
var
    F: TFileStream;
begin
    F:=TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
    try
        ReadFromStream(F);
    finally
        F.Free;
    end;
end;

procedure TPYEKVSDocument.SaveToFile(aFileName: string);
var F: TFileStream;
begin
    F:=TFileStream.Create(FileCreate(aFileName));
    try
        WriteToStream(F);
    finally
        F.Free;
    end;
end;

procedure TPYEKVSDocument.ReadFromStream(const F: TStream);
begin
    Reset();

    // Load FData from stream F
    FStream.Size:=F.Size;

    if (FStream is TCustomMemoryStream) then begin
        // Write memory direct to F
        F.ReadBuffer(TCustomMemoryStream(FStream).Memory^, FStream.Size);
    end else begin
        // Use stream copy function
        FStream.CopyFrom(F, FStream.Size);
    end;

    // Encode keys and values
    Encode();
end;

procedure TPYEKVSDocument.WriteToStream(const F: TStream);
begin
    if (FRoot.State in [srzstate_PutValues]) then
        raise EPYEKVSException.Create('put values incomplete');
    try
        FStream.Position:=0;
        if (FStream is TCustomMemoryStream) then begin
            // Write memory direct to F
            F.WriteBuffer(TCustomMemoryStream(FStream).Memory^, FStream.Size);
        end else begin
            // Use stream copy function
            F.CopyFrom(FStream, FStream.Size);
        end;
    finally
    end;
end;



function TPYEKVSDocument.GetState: TPYEKVSBranchState;
begin
    result:=FRoot.State;
end;

function TPYEKVSDocument.GetStreamPosition: Int64;
begin
    result:=FStream.Position;
end;

function TPYEKVSDocument.GetStreamSize: Int64;
begin
    result:=FStream.Size;
end;

procedure TPYEKVSDocument.SetStreamPosition(const Value: Int64);
begin
    FStream.Position:=Value;
end;

procedure TPYEKVSDocument.ToStringsJSON(const aString: TStrings; const aIndenting: string);
begin
    aString.Add(Str_PYEKVSListSeperatorO);
    FRoot.ToStringsJSON(aString, aIndenting, '', true);
    aString.Add(Str_PYEKVSListSeperatorC);
end;


procedure TPYEKVSDocument.ToStringsSimple(const aString: TStrings; const aIndenting: string);
begin
    aString.Add(Str_PYEKVSListSeperatorO);
    FRoot.ToStringsSimple(aString, aIndenting, '');
    aString.Add(Str_PYEKVSListSeperatorC);
end;

procedure TPYEKVSDocument.ToOutputCSV(aCSVOutput: TPYEKVSCSVOutput);
begin
    FRoot.ToOutputCSV(aCSVOutput);
end;



procedure TPYEKVSDocument.Reset;
begin
    FPutCounter:=0;
    Clear();
end;


function TPYEKVSDocument.StreamHeaderUpdate: Boolean;
begin
    // result=true if DataHeader is valid
    result:=true;

    // encode data header
    FStream.Position:=0;
    FStream.Read(FStreamHeader, sizeof(FStreamHeader));

    if (FStreamHeader.StreamPrefix<>cPYEKVSStreamPrefix) then exit(false);

    if (FStreamHeader.StreamVersionH>PYEKVSVersionH)OR
    (FStreamHeader.StreamVersionH=PYEKVSVersionH)AND(FStreamHeader.StreamVersionL>PYEKVSVersionL) then exit(false);
end;

function TPYEKVSDocument.DataSizePending: Int64;
begin
    // case reading FData from stream in blocks
    // return the numbers of Bytes that still pending to read; size of next block
    result:=sizeof(TPYEKVSStreamHeader)+FStreamHeader.StreamSize-UInt64(FStream.Size);
end;


function TPYEKVSDocument.Encode(): Integer;
var iVT: TPYEKVSValueType;
begin
    // Encode keys and values after ReadFromStoreList
    // Result is the number of bytes

    if (FStream.Size<sizeof(FStreamHeader)) then
        raise EPYEKVSException.Create('Stream is empty');

    FStream.Position:=0;
    result:=0;

    // data header
    result:=result+FStream.Read(FStreamHeader, sizeof(FStreamHeader));

    // check DataHeader
    if (FStreamHeader.StreamPrefix<>cPYEKVSStreamPrefix) then raise EPYEKVSException.Create('Store header prefix error');
    if (FStreamHeader.StreamVersionH>PYEKVSVersionH)OR
    (FStreamHeader.StreamVersionH=PYEKVSVersionH)AND(FStreamHeader.StreamVersionL>PYEKVSVersionL) then
        raise EPYEKVSException.CreateFmt('Store protocol version error.'#10#13'Version in stream: %d.%d'#10#13'Version API: V%d.%d'#10#13, [FStreamHeader.StreamVersionH, FStreamHeader.StreamVersionL, PYEKVSVersionH, PYEKVSVersionL]);
    if (FStreamHeader.StreamSize<>FStream.Size-sizeof(TPYEKVSStreamHeader)) then
        raise EPYEKVSException.Create('Store size error');

    // root header
    result:=result+ReadKey(FRoot.FKey);
    result:=result+ReadValueType(iVT);
    if (iVT<>pyeList) then
        raise EPYEKVSException.Create('root list failed');

    result:=result+FRoot.Encode();

    if (result<>FStream.Size) then
        raise EPYEKVSException.Create('ReadStream incomplete');
end;



{ TPYEKVSObject }

function TPYEKVSObject._AddRef: Integer;
begin
    result:=-1;
end;

function TPYEKVSObject._Release: Integer;
begin
    result:=-1;
end;

function TPYEKVSObject.QueryInterface(const IID: TGUID; out Obj): HResult;
const E_NOINTERFACE=HResult($80004002);
begin
    if GetInterface(IID, Obj) then result:=0
    else result:=E_NOINTERFACE;
end;


{ TPYEKVSBranch }



class function TPYEKVSBranch.ValueTypeOf(const aValue: Boolean): TPYEKVSValueType;
begin
    // return the pyeValueType to store aValue
    if (aValue=false) then result:=pyeZero
    else result:=pyeBool;
end;

class function TPYEKVSBranch.ValueTypeOf(const aValue: Integer): TPYEKVSValueType;
begin
    // return the pyeValueType to store aValue
    if (aValue=0) then result:=pyeZero
    else if (aValue=1) then result:=pyeBool
    else if (aValue>=Low(Int8))AND(aValue<=High(Int8)) then result:=pyeInt8
    else if (aValue>=Low(UInt8))AND(aValue<=High(UInt8)) then result:=pyeUInt8
    else if (aValue>=Low(Int16))AND(aValue<=High(Int16)) then result:=pyeInt16
    else if (aValue>=Low(UInt16))AND(aValue<=High(UInt16)) then result:=pyeUInt16
    else result:=pyeInt32;
end;

class function TPYEKVSBranch.ValueTypeTst(const aValue: Integer; const aVT: TPYEKVSValueType): Boolean;
begin
    // Test if aValue can be coded as aVT
    case aVT of
    pyeZero: result:=(aValue=0);
    pyeBool: result:=(aValue=1);
    pyeInt8: result:=(aValue>=Low(Int8))AND(aValue<=High(Int8));
    pyeUInt8: result:=(aValue>=Low(UInt8))AND(aValue<=High(UInt8));
    pyeInt16: result:=(aValue>=Low(Int16))AND(aValue<=High(Int16));
    pyeUInt16: result:=(aValue>=Low(UInt16))AND(aValue<=High(UInt16));
    pyeInt32: result:=true;
else result:=false;
    end;
end;

class function TPYEKVSBranch.ValueTypeOf(const aValue: Int64): TPYEKVSValueType;
begin
    // return the pyeValueType to store aValue
    if (aValue=0) then result:=pyeZero
    else if (aValue=1) then result:=pyeBool
    else if (aValue>=Low(Int8))AND(aValue<=High(Int8)) then result:=pyeInt8
    else if (aValue>=Low(UInt8))AND(aValue<=High(UInt8)) then result:=pyeUInt8
    else if (aValue>=Low(Int16))AND(aValue<=High(Int16)) then result:=pyeInt16
    else if (aValue>=Low(UInt16))AND(aValue<=High(UInt16)) then result:=pyeUInt16
    else if (aValue>=Low(Int32))AND(aValue<=High(Int32)) then result:=pyeInt32
    else if (aValue>=Low(UInt32))AND(aValue<=High(UInt32)) then result:=pyeUInt32
    else result:=pyeInt64;
end;

class function TPYEKVSBranch.ValueTypeTst(const aValue: Int64; const aVT: TPYEKVSValueType): Boolean;
begin
    // Test if aValue can be coded as aVT
    case aVT of
    pyeZero: result:=(aValue=0);
    pyeBool: result:=(aValue=1);
    pyeInt8: result:=(aValue>=Low(Int8))AND(aValue<=High(Int8));
    pyeUInt8: result:=(aValue>=Low(UInt8))AND(aValue<=High(UInt8));
    pyeInt16: result:=(aValue>=Low(Int16))AND(aValue<=High(Int16));
    pyeUInt16: result:=(aValue>=Low(UInt16))AND(aValue<=High(UInt16));
    pyeInt32: result:=(aValue>=Low(UInt32))AND(aValue<=High(UInt32));
    pyeInt64: result:=true;
else result:=false;
    end;
end;

class function TPYEKVSBranch.ValueTypeOf(const aValue: Cardinal): TPYEKVSValueType;
begin
    // return the pyeValueType to store aValue
    if (aValue=0) then result:=pyeZero
    else if (aValue=1) then result:=pyeBool
    else if (aValue<=High(UInt8)) then result:=pyeUInt8
    else if (aValue<=High(UInt16)) then result:=pyeUInt16
    else result:=pyeUInt32;
end;

class function TPYEKVSBranch.ValueTypeTst(const aValue: Cardinal; const aVT: TPYEKVSValueType): Boolean;
begin
    // Test if aValue can be coded as aVT
    case aVT of
    pyeZero: result:=(aValue=0);
    pyeBool: result:=(aValue=1);
    pyeUInt8: result:=(aValue<=High(UInt8));
    pyeUInt16: result:=(aValue<=High(UInt16));
    pyeUInt32: result:=true;
else result:=false;
    end;
end;

class function TPYEKVSBranch.ValueTypeOf(const aValue: UInt64): TPYEKVSValueType;
begin
    // return the pyeValueType to store aValue
    if (aValue=0) then result:=pyeZero
    else if (aValue=1) then result:=pyeBool
    else if (aValue<=High(UInt8)) then result:=pyeUInt8
    else if (aValue<=High(UInt16)) then result:=pyeUInt16
    else if (aValue<=High(UInt32)) then result:=pyeUInt32
    else result:=pyeUInt64;
end;

class function TPYEKVSBranch.ValueTypeTst(const aValue: UInt64; const aVT: TPYEKVSValueType): Boolean;
begin
    // Test if aValue can be coded as aVT
    case aVT of
    pyeZero: result:=(aValue=0);
    pyeBool: result:=(aValue=1);
    pyeUInt8: result:=(aValue<=High(UInt8));
    pyeUInt16: result:=(aValue<=High(UInt16));
    pyeUInt32: result:=(aValue<=High(UInt32));
    pyeUInt64: result:=true;
else result:=false;
    end;
end;

class function TPYEKVSBranch.ValueTypeOf(const aValue: Single): TPYEKVSValueType;
begin
    // return the pyeValueType to store aValue
    if IsZero(aValue) then result:=pyeZero
    else result:=pyeFloat32;
end;

class function TPYEKVSBranch.ValueTypeOf(const aValue: Double): TPYEKVSValueType;
var S: Single;
begin
    // return the pyeValueType to store aValue
    if IsZero(aValue) then result:=pyeZero
    else begin
        S:=aValue;
        if (IsZero(aValue-S)) then result:=pyeFloat32
        else result:=pyeFloat64;
    end;
end;

class function TPYEKVSBranch.ValueTypeOf(const aValue: UTF8String): TPYEKVSValueType;
var Len: Integer;
begin
    // return the pyeValueType to store aValue
    Len:=Length(aValue);
    if Len=0 then result:=pyeZero
    else if (Len<=High(UInt8)) then result:=pyeStringUTF8S
    else result:=pyeStringUTF8L;
end;

class function TPYEKVSBranch.ValueTypeTst(const aValue: UTF8String; const aVT: TPYEKVSValueType): Boolean;
var Len: Integer;
begin
    // Test if aValue can be coded as aVT
    Len:=Length(aValue);
    case aVT of
    pyeZero: result:=(Len=0);
    pyeStringUTF8S: result:=(Len<=High(UInt8));
    pyeStringUTF8L: result:=true;
else result:=false;
    end;
end;


constructor TPYEKVSBranch.Create(const aStoreDocument: TPYEKVSDocument; const aStoreParent: TPYEKVSBranch; const aKey: TPYEKVSKey);
begin
    FStoreDocument:=aStoreDocument;
    FStoreParent:=aStoreParent;
    FKey:=aKey;
end;

destructor TPYEKVSBranch.Destroy;
begin
    inherited;
end;

procedure TPYEKVSBranch.Clear;
begin
    FState:=srzstate_Idle;
    FSizeData:=0;
end;

function TPYEKVSBranch.Encode(): Integer;
begin
    result:=0;
    FState:=srzstate_Closed;
end;


procedure TPYEKVSBranch.SetPutValues;
begin
    // start writing of this branch in the stream

    if (FState<>srzstate_Idle) then
        raise EPYEKVSException.CreateFmt('store branch %s cannot write again', [FKey]);

    FState:=srzstate_PutValues;
end;

procedure TPYEKVSBranch.SetPutClosed;
begin
    // finish the writing of this branch in the stream (change to an other branch)
    FState:=srzstate_Closed;

    if Assigned(FOnSetPutClosed) then
        FOnSetPutClosed(); // needed by TStoreListPrcEx

    if (Parent<>nil) then
        Parent.SizeChildBranch(FSizeData);
end;

procedure TPYEKVSBranch.SizeChildBranch(const aSize: Int64);
begin
    // child branch finished writing and inform parent about there size
    FSizeData:=FSizeData+aSize;
end;

procedure TPYEKVSBranch.ToStringsJSON(const aString: TStrings; const aIndenting, aActIndenting: string; const aOutName: Boolean);
begin
end;

procedure TPYEKVSBranch.ToStringsSimple(const aString: TStrings; const aIndenting, aActIndenting: string);
begin
end;

procedure TPYEKVSBranch.ToOutputCSV(aCSVOutput: TPYEKVSCSVOutput);
begin
end;

function TPYEKVSBranch.DetailsToStrings(): string;
begin
    result:='';
end;

procedure TPYEKVSBranch.KeyAndParent(var aKeyName: TPYEKVSKey);
var sParent:TPYEKVSKey;

    procedure ParseParent(const aBranch:TPYEKVSBranch);
    begin
        if (aBranch.FStoreParent<>nil)AND(aBranch.FStoreParent.FKey<>'') then
            ParseParent(aBranch.FStoreParent);
        if (sParent<>'') then
            sParent:=sParent+'.';
        sParent:=sParent+aBranch.FKey;
    end;

begin
    sParent:='';
    if (FKey<>'') then begin
        ParseParent(self);
        if (sParent<>'') then
            sParent:=sParent+'.';
    end;
    aKeyName:=sParent+aKeyName;
end;


{ TPYEKVSList }

constructor TPYEKVSList.Create(const aStoreDocument: TPYEKVSDocument; const aStoreParent: TPYEKVSBranch; const aKey: TPYEKVSKey);
begin
    inherited;
    FKeys:=TDictionary<TPYEKVSKey, Int64>.Create();
    FList:=TObjectDictionary<Int64, TPYEKVSBranch>.Create([doOwnsValues]);
end;

destructor TPYEKVSList.Destroy;
begin
    FList.Free();
    FKeys.Free();
    inherited;
end;

procedure TPYEKVSList.Clear;
begin
    inherited Clear;
    FKeys.Clear;
    FList.Clear;
end;

function TPYEKVSList.Encode(): Integer;
var uSize    : Cardinal;
    uCount   : Cardinal;
    iSize    : Int64;
    iCount   : Int64;
    iPos     : Int64;
    iVT      : TPYEKVSValueType;
    iKey     : TPYEKVSKey;
    iList    : TPYEKVSList;
    iArray   : TPYEKVSArray;
    iArrayMap: TPYEKVSArrayMap;
begin
    result:=inherited Encode();

    // Values ...
    // size
    result:=result+FStoreDocument.ReadFixUInt32(uSize);
    iSize:=uSize;

    // count
    result:=result+FStoreDocument.ReadFixUInt32(uCount);
    iCount:=uCount;

    result:=result+iSize;

    while (iSize>0) do begin

        iSize:=iSize-FStoreDocument.ReadKey(iKey);

        iPos:=FStoreDocument.StreamPosition;
        iSize:=iSize-FStoreDocument.ReadValueType(iVT);
        if iVT=pyeUnknown then
            raise EPYEKVSException.Create('read unknown data type');

        // register Key and FData stream position
        FKeys.Add(iKey, iPos);

        case iVT of
        pyeList: begin
                iList:=FStoreDocument.RClassPYEKVSList.Create(FStoreDocument, self, iKey);

                // register list at FData stream position
                FList.Add(iPos, iList);

                // read list itself
                iSize:=iSize-iList.Encode();
            end;
        pyeArray: begin
                iArray:=FStoreDocument.RClassPYEKVSArray.Create(FStoreDocument, self, iKey);

                // register list at FData stream position
                FList.Add(iPos, iArray);

                // read array itself
                iSize:=iSize-iArray.Encode();
            end;
        pyeArrayMap: begin
                iArrayMap:=FStoreDocument.RClassPYEKVSArrayMap.Create(FStoreDocument, self, iKey);

                // register list at FData stream position
                FList.Add(iPos, iArrayMap);

                // read array itself
                iSize:=iSize-iArrayMap.Encode();
            end;
    else begin
            // pyeFundamental
            // pyeStringUTF8S
            // pyeStringUTF8L
            // pyeMemory
            iSize:=iSize-FStoreDocument.EncodeVT(iVT);
        end;
        end;

        iCount:=iCount-1;

    end;
    if (iSize<0) then raise EPYEKVSException.Create('read failed because of size mismatch');
    if (iCount<>0) then raise EPYEKVSException.Create('read failed because of count mismatch');
end;


function TPYEKVSList.WriteHeader(): Integer;
begin
    // write list Header to Data
    result:=FStoreDocument.WriteValueType(pyeList);
    FStoreDocument.WriteBranchSet(self); // change to new branch

    FPosSize:=FStoreDocument.StreamPosition;
    result:=result+FStoreDocument.WriteFixUInt32(0); // Placeholder of Size-Value

    FPosCount:=FStoreDocument.StreamPosition;
    result:=result+FStoreDocument.WriteFixUInt32(0); // Placeholder of Count-Value
end;

procedure TPYEKVSList.SetPutClosed();
begin
    // Update SizeData
    FStoreDocument.WriteFixUInt32(FSizeData, FPosSize);
    // Update Count
    FStoreDocument.WriteFixUInt32(FKeys.Count, FPosCount);

    inherited SetPutClosed;
end;


function TPYEKVSList.DetailsToStrings(): string;
begin
    result:=Str_PYEKVSArrayCount+Str_PYEKVSSpace+IntToStr(FKeys.Count);
end;



procedure TPYEKVSList.ToStringsJSON(const aString: TStrings; const aIndenting: string; const aActIndenting: string; const aOutName: Boolean);
var i         : Integer;
    S         : string;
    sIndenting: string;
    iKeyArray : TArray<TPYEKVSKey>;
    iKey      : TPYEKVSKey;
    iValue    : Int64;
    iBranch   : TPYEKVSBranch;
    iVT       : TPYEKVSValueType;
begin
    inherited;
    sIndenting:=aActIndenting+aIndenting;

    iKeyArray:=KeySorted();

    for i:=0 to High(iKeyArray) do begin
        iKey:=iKeyArray[i];
        iValue:=FKeys.Items[iKeyArray[i]];

        FStoreDocument.StreamPosition:=iValue;
        FStoreDocument.ReadValueType(iVT);

        S:=sIndenting;

        if (aOutName) then
            S:=S+Str_PYEKVSStringQuote+string(iKey)+Str_PYEKVSStringQuote+Str_PYEKVSSpace+Str_PYEKVSValueSeperator+Str_PYEKVSSpace;

        case iVT of
        pyeList: begin
                iBranch:=FList.Items[iValue];
                S:=S+Str_PYEKVSListSeperatorO;
                aString.Add(S);
                if (iBranch<>nil) then
                    iBranch.ToStringsJSON(aString, aIndenting, sIndenting, aOutName);
                S:=sIndenting+Str_PYEKVSListSeperatorC;
            end;
        pyeArray, pyeArrayMap: begin
                iBranch:=FList.Items[iValue];
                S:=S+Str_PYEKVSArraySeperatorO;
                aString.Add(S);
                if (iBranch<>nil) then
                    iBranch.ToStringsJSON(aString, aIndenting, sIndenting, aOutName);
                S:=sIndenting+Str_PYEKVSArraySeperatorC;
            end;
    else begin
            // fundamental values
            S:=S+FStoreDocument.ReadToString(iVT);
        end;
        end;

        if (i<FKeys.Count-1) then
            S:=S+Str_PYEKVSComma;

        aString.Add(S);
    end;

end;

procedure TPYEKVSList.ToStringsSimple(const aString: TStrings; const aIndenting: string; const aActIndenting: string);
var sIndenting: string;
    S         : string;
    i         : Integer;
    iKeyArray : TArray<TPYEKVSKey>;
    iKey      : TPYEKVSKey;
    iValue    : Int64;
    iBranch   : TPYEKVSBranch;
    iVT       : TPYEKVSValueType;
begin
    inherited;

    // Simple StringList output:
    // without quotes, without comma
    // with typename, arrayindex as numbers

    sIndenting:=aActIndenting+aIndenting;

    iKeyArray:=KeySorted();

    for i:=0 to High(iKeyArray) do begin
        iKey:=iKeyArray[i];
        iValue:=FKeys.Items[iKeyArray[i]];

        FStoreDocument.StreamPosition:=iValue;
        FStoreDocument.ReadValueType(iVT);

        S:=sIndenting+string(iKey)+Str_PYEKVSSpace+Str_PYEKVSTypeSeperatorO+cPYEKVSValueTypeName[iVT];

        case iVT of
        pyeList: begin
                iBranch:=FList.Items[iValue];
                S:=S+Str_PYEKVSSpace+iBranch.DetailsToStrings()+Str_PYEKVSTypeSeperatorC+Str_PYEKVSSpace+Str_PYEKVSListSeperatorO;
                aString.Add(S);
                if (iBranch<>nil) then
                    iBranch.ToStringsSimple(aString, aIndenting, sIndenting);
                S:=sIndenting+Str_PYEKVSListSeperatorC;
            end;
        pyeArray, pyeArrayMap: begin
                iBranch:=FList.Items[iValue];
                S:=S+Str_PYEKVSSpace+iBranch.DetailsToStrings()+Str_PYEKVSTypeSeperatorC+Str_PYEKVSSpace+Str_PYEKVSArraySeperatorO;
                aString.Add(S);
                if (iBranch<>nil) then
                    iBranch.ToStringsSimple(aString, aIndenting, sIndenting);
                S:=sIndenting+Str_PYEKVSArraySeperatorC;
            end;
    else begin
            // fundamental values
            S:=S+Str_PYEKVSTypeSeperatorC+Str_PYEKVSSpace+Str_PYEKVSValueSeperator+Str_PYEKVSSpace+FStoreDocument.ReadToString(iVT);
        end;

        end;

        aString.Add(S);

    end;
end;

procedure TPYEKVSList.ToOutputCSV(aCSVOutput: TPYEKVSCSVOutput);
var sKey     : TPYEKVSKey;
    i        : Integer;
    iKeyArray: TArray<TPYEKVSKey>;
    iValue   : Int64;
    iBranch  : TPYEKVSBranch;
    iVT      : TPYEKVSValueType;
begin
    inherited;

    // CSV output

    iKeyArray:=KeySorted();

    for i:=0 to High(iKeyArray) do begin
        sKey:=iKeyArray[i];
        iValue:=FKeys.Items[iKeyArray[i]];

        FStoreDocument.StreamPosition:=iValue;
        FStoreDocument.ReadValueType(iVT);

        //Make the Key unique over all KVSLists
        KeyAndParent(sKey);

        case iVT of
        pyeList, pyeArray, pyeArrayMap: begin
                iBranch:=FList.Items[iValue];
                if (iBranch<>nil) then
                    iBranch.ToOutputCSV(aCSVOutput);
            end;
        else begin
            // fundamental values
            aCSVOutput.AddKV( aCSVOutput.RowCurrent, string(sKey), FStoreDocument.ReadToString(iVT));

        end;
        end;

    end;
end;



procedure TPYEKVSList.KeyAdd(const aKey: TPYEKVSKey; const aPosition: Int64);
begin
    FKeys.Add(aKey, aPosition);
end;

function TPYEKVSList.KeySorted: TArray<TPYEKVSKey>;
begin
    result:=FKeys.Keys.ToArray;
    // Sort erzeugt in XE5 Exception!
    // System.Generics.Collections.TArray.Sort<TPYEKVSKey>(result);
end;


function TPYEKVSList.GetKey(const aKey: TPYEKVSKey): TPYEKVSValueType;
var iPos: Int64;
begin
    if FKeys.TryGetValue(aKey, iPos) then begin
        FStoreDocument.StreamPosition:=iPos;
        FStoreDocument.ReadValueType(result);
    end else begin
        result:=pyeUnknown;
    end;
end;

function TPYEKVSList.KeyExists(const aKey: TPYEKVSKey): Boolean;
begin
    result:=FKeys.ContainsKey(aKey);
end;


procedure TPYEKVSList.PutZero(const aKey: TPYEKVSKey);
begin
    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition); // Position before WriteValueType
    FSizeData:=FSizeData+FStoreDocument.WriteValueType(pyeZero);
end;

function TPYEKVSList.GetZero(const aKey: TPYEKVSKey): Boolean;
var iVT: TPYEKVSValueType;
begin
    // result=false means Key is vtZero
    // result=true means Key <> vtZero
    iVT:=GetKey(aKey);
    result:=(iVT<>pyeZero);
end;


procedure TPYEKVSList.PutBool(const aKey: TPYEKVSKey; const aValue: Boolean);
var iVT: TPYEKVSValueType;
begin
    iVT:=ValueTypeOf(aValue);
    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition); // Position before WriteValueType
    FSizeData:=FSizeData+FStoreDocument.WriteValueType(iVT);
end;

function TPYEKVSList.GetBool(const aKey: TPYEKVSKey; const aDefault: Boolean): Boolean;
var iVT: TPYEKVSValueType;
begin
    iVT:=GetKey(aKey);
    FStoreDocument.ReadVTBool(result, iVT, aDefault);
end;


procedure TPYEKVSList.PutInt(const aKey: TPYEKVSKey; const aValue: Integer);
begin
    PutIntVT(aKey, aValue, pyeUnknown);
end;

procedure TPYEKVSList.PutIntVT(const aKey: TPYEKVSKey; const aValue: Integer; const aForceValueType: TPYEKVSValueType);
var iVT: TPYEKVSValueType;
begin
    if (aForceValueType=pyeUnknown) then begin
        iVT:=ValueTypeOf(aValue);
    end else begin
        // Try to use aForceValueType; maybe aValue should be Int32 (4Bytes) to make later the call "ChgInt" with a higher aValue
        iVT:=aForceValueType;
        if (ValueTypeTst(aValue, iVT)=false) then raise EPYEKVSException.Create(cStrPYEKVSExcept_ValueNotCompatible);
    end;
    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition); // Position before WriteValueType
    FSizeData:=FSizeData+FStoreDocument.WriteValueType(iVT);
    FSizeData:=FSizeData+FStoreDocument.WriteVTInt(aValue, iVT);
end;

function TPYEKVSList.GetInt(const aKey: TPYEKVSKey; const aDefault: Integer): Integer;
var iVT: TPYEKVSValueType;
begin
    iVT:=GetKey(aKey);
    FStoreDocument.ReadVTInt32(result, iVT, aDefault);
end;

procedure TPYEKVSList.ChgInt(const aKey: TPYEKVSKey; const aValue: Integer);
var iVT: TPYEKVSValueType;
begin
    // Change Value of existing Key; Value size must match
    iVT:=GetKey(aKey);
    if (iVT=pyeUnknown) then raise EPYEKVSException.CreateFmt(cStrPYEKVSExcept_KeyNotFound, [aKey]);
    if (ValueTypeTst(aValue, iVT)=false) then
        raise EPYEKVSException.Create(cStrPYEKVSExcept_ValueNotCompatible);
    FStoreDocument.WriteVTInt(aValue, iVT);
end;


procedure TPYEKVSList.PutInt64(const aKey: TPYEKVSKey; const aValue: Int64);
begin
    PutInt64VT(aKey, aValue, pyeUnknown);
end;

procedure TPYEKVSList.PutInt64VT(const aKey: TPYEKVSKey; const aValue: Int64; const aForceValueType: TPYEKVSValueType);
var iVT: TPYEKVSValueType;
begin
    if (aForceValueType=pyeUnknown) then begin
        iVT:=ValueTypeOf(aValue);
    end else begin
        // Try to use aForceValueType; maybe aValue should be UInt32 (4Bytes) to make later the call "ChgUInt" with a higher aValue
        iVT:=aForceValueType;
        if (ValueTypeTst(aValue, iVT)=false) then raise EPYEKVSException.Create(cStrPYEKVSExcept_ValueNotCompatible);
    end;
    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition); // Position before WriteValueType
    FSizeData:=FSizeData+FStoreDocument.WriteValueType(iVT);
    FSizeData:=FSizeData+FStoreDocument.WriteVTInt64(aValue, iVT);
end;

function TPYEKVSList.GetInt64(const aKey: TPYEKVSKey; const aDefault: Int64): Int64;
var iVT: TPYEKVSValueType;
begin
    iVT:=GetKey(aKey);
    FStoreDocument.ReadVTInt64(result, iVT, aDefault);
end;

procedure TPYEKVSList.ChgInt64(const aKey: TPYEKVSKey; const aValue: Int64);
var iVT: TPYEKVSValueType;
begin
    // Change Value of existing Key; Value size must match
    iVT:=GetKey(aKey);
    if (iVT=pyeUnknown) then raise EPYEKVSException.CreateFmt(cStrPYEKVSExcept_KeyNotFound, [aKey]);
    if (ValueTypeTst(aValue, iVT)=false) then
        raise EPYEKVSException.Create(cStrPYEKVSExcept_ValueNotCompatible);
    FStoreDocument.WriteVTInt64(aValue, iVT);
end;


procedure TPYEKVSList.PutInt128(const aKey: TPYEKVSKey; const aValue: TPYEKVSInt128);
var iVT: TPYEKVSValueType;
begin
    iVT:=pyeInt128;
    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition); // Position before WriteValueType
    FSizeData:=FSizeData+FStoreDocument.WriteValueType(iVT);
    FSizeData:=FSizeData+FStoreDocument._WriteInt128(aValue);
end;

function TPYEKVSList.GetInt128(const aKey: TPYEKVSKey; const aDefault: TPYEKVSInt128): TPYEKVSInt128;
var iVT: TPYEKVSValueType;
begin
    iVT:=GetKey(aKey);
    FStoreDocument.ReadVTInt128(result, iVT, aDefault);
end;

procedure TPYEKVSList.PutUInt(const aKey: TPYEKVSKey; const aValue: Cardinal);
begin
    PutUIntVT(aKey, aValue, pyeUnknown);
end;

procedure TPYEKVSList.PutUIntVT(const aKey: TPYEKVSKey; const aValue: Cardinal; const aForceValueType: TPYEKVSValueType);
var iVT: TPYEKVSValueType;
begin
    if (aForceValueType=pyeUnknown) then begin
        iVT:=ValueTypeOf(aValue);
    end else begin
        // Try to use aForceValueType; maybe aValue should be UInt32 (4Bytes) to make later the call "ChgUInt" with a higher aValue
        iVT:=aForceValueType;
        if (ValueTypeTst(aValue, iVT)=false) then raise EPYEKVSException.Create(cStrPYEKVSExcept_ValueNotCompatible);
    end;
    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition); // Position before WriteValueType
    FSizeData:=FSizeData+FStoreDocument.WriteValueType(iVT);
    FSizeData:=FSizeData+FStoreDocument.WriteVTUInt(aValue, iVT);
end;

function TPYEKVSList.GetUInt(const aKey: TPYEKVSKey; const aDefault: Cardinal): Cardinal;
var iVT: TPYEKVSValueType;
begin
    iVT:=GetKey(aKey);
    FStoreDocument.ReadVTUInt32(result, iVT, aDefault);
end;

procedure TPYEKVSList.ChgUInt(const aKey: TPYEKVSKey; const aValue: Cardinal);
var iVT: TPYEKVSValueType;
begin
    // Change Value of existing Key; Value size must match
    iVT:=GetKey(aKey);
    if (iVT=pyeUnknown) then raise EPYEKVSException.CreateFmt(cStrPYEKVSExcept_KeyNotFound, [aKey]);
    if (ValueTypeTst(aValue, iVT)=false) then
        raise EPYEKVSException.Create(cStrPYEKVSExcept_ValueNotCompatible);
    FStoreDocument.WriteVTUInt(aValue, iVT);
end;


procedure TPYEKVSList.PutUInt64(const aKey: TPYEKVSKey; const aValue: UInt64);
begin
    PutUInt64VT(aKey, aValue, pyeUnknown);
end;

procedure TPYEKVSList.PutUInt64VT(const aKey: TPYEKVSKey; const aValue: UInt64; const aForceValueType: TPYEKVSValueType);
var iVT: TPYEKVSValueType;
begin
    if (aForceValueType=pyeUnknown) then begin
        iVT:=ValueTypeOf(aValue);
    end else begin
        // Try to use aForceValueType; maybe aValue should be UInt32 (4Bytes) to make later the call "ChgUInt" with a higher aValue
        iVT:=aForceValueType;
        if (ValueTypeTst(aValue, iVT)=false) then raise EPYEKVSException.Create(cStrPYEKVSExcept_ValueNotCompatible);
    end;
    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition); // Position before WriteValueType
    FSizeData:=FSizeData+FStoreDocument.WriteValueType(iVT);
    FSizeData:=FSizeData+FStoreDocument.WriteVTUInt64(aValue, iVT);
end;

function TPYEKVSList.GetUInt64(const aKey: TPYEKVSKey; const aDefault: UInt64): UInt64;
var iVT: TPYEKVSValueType;
begin
    iVT:=GetKey(aKey);
    FStoreDocument.ReadVTUInt64(result, iVT, aDefault);
end;

procedure TPYEKVSList.ChgUInt64(const aKey: TPYEKVSKey; const aValue: UInt64);
var iVT: TPYEKVSValueType;
begin
    // Change Value of existing Key; Value size must match
    iVT:=GetKey(aKey);
    if (iVT=pyeUnknown) then raise EPYEKVSException.CreateFmt(cStrPYEKVSExcept_KeyNotFound, [aKey]);
    if (ValueTypeTst(aValue, iVT)=false) then
        raise EPYEKVSException.Create(cStrPYEKVSExcept_ValueNotCompatible);
    FStoreDocument.WriteVTUInt64(aValue, iVT);
end;


procedure TPYEKVSList.PutUInt128(const aKey: TPYEKVSKey; const aValue: TPYEKVSUInt128);
var iVT: TPYEKVSValueType;
begin
    iVT:=pyeUInt128;
    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition); // Position before WriteValueType
    FSizeData:=FSizeData+FStoreDocument.WriteValueType(iVT);
    FSizeData:=FSizeData+FStoreDocument._WriteUInt128(aValue);
end;

function TPYEKVSList.GetUInt128(const aKey: TPYEKVSKey; const aDefault: TPYEKVSUInt128): TPYEKVSUInt128;
var iVT: TPYEKVSValueType;
begin
    iVT:=GetKey(aKey);
    FStoreDocument.ReadVTUInt128(result, iVT, aDefault);
end;

procedure TPYEKVSList.PutSingle(const aKey: TPYEKVSKey; const aValue: Single);
var iVT: TPYEKVSValueType;
begin
    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition); // Position before WriteValueType
    iVT:=ValueTypeOf(aValue);
    FSizeData:=FSizeData+FStoreDocument.WriteValueType(iVT);
    FSizeData:=FSizeData+FStoreDocument.WriteVTSingle(aValue, iVT);
end;

function TPYEKVSList.GetSingle(const aKey: TPYEKVSKey; const aDefault: Single): Single;
var iVT: TPYEKVSValueType;
begin
    iVT:=GetKey(aKey);
    FStoreDocument.ReadVTSingle(result, iVT, aDefault);
end;

procedure TPYEKVSList.PutDouble(const aKey: TPYEKVSKey; const aValue: Double);
var iVT: TPYEKVSValueType;
begin
    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition); // Position before WriteValueType
    iVT:=ValueTypeOf(aValue);
    FSizeData:=FSizeData+FStoreDocument.WriteValueType(iVT);
    FSizeData:=FSizeData+FStoreDocument.WriteVTDouble(aValue, iVT);
end;

function TPYEKVSList.GetDouble(const aKey: TPYEKVSKey; const aDefault: Double): Double;
var iVT: TPYEKVSValueType;
begin
    iVT:=GetKey(aKey);
    FStoreDocument.ReadVTDouble(result, iVT, aDefault);
end;


procedure TPYEKVSList.PutString(const aKey: TPYEKVSKey; const aValue: String);
begin
    PutStringVT(aKey, aValue, pyeUnknown);
end;

procedure TPYEKVSList.PutStringVT(const aKey: TPYEKVSKey; const aValue: String; const aForceValueType: TPYEKVSValueType);
var iVT        : TPYEKVSValueType;
    iStringUTF8: UTF8String;
begin
    iStringUTF8:=UTF8Encode(aValue);
    if (aForceValueType=pyeUnknown) then begin
        iVT:=ValueTypeOf(iStringUTF8);
    end else begin
        // Try to use aForceValueType; maybe aValue should be UInt32 (4Bytes) to make later the call "ChgUInt" with a higher aValue
        iVT:=aForceValueType;
        if (ValueTypeTst(iStringUTF8, iVT)=false) then raise EPYEKVSException.Create(cStrPYEKVSExcept_ValueNotCompatible);
    end;
    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition); // Position before WriteValueType
    FSizeData:=FSizeData+FStoreDocument.WriteValueType(iVT);
    FSizeData:=FSizeData+FStoreDocument.WriteVTStringUTF8(iStringUTF8, iVT);
end;

function TPYEKVSList.GetString(const aKey: TPYEKVSKey; const aDefault: String): String;
var iVT: TPYEKVSValueType;
begin
    iVT:=GetKey(aKey);
    result:=UTF8ToString(FStoreDocument.ReadVTStringUTF8(iVT, UTF8Encode(aDefault)));
end;

procedure TPYEKVSList.ChgString(const aKey: TPYEKVSKey; const aValue: String);
var iVT        : TPYEKVSValueType;
    iStringUTF8: UTF8String;
begin
    // Change Value of existing Key; Value size must match
    iStringUTF8:=UTF8Encode(aValue);
    iVT:=GetKey(aKey);
    if (iVT=pyeUnknown) then raise EPYEKVSException.CreateFmt(cStrPYEKVSExcept_KeyNotFound, [aKey]);
    if (ValueTypeTst(iStringUTF8, iVT)=false) then
        raise EPYEKVSException.Create(cStrPYEKVSExcept_ValueNotCompatible);
    FSizeData:=FSizeData+FStoreDocument.WriteVTStringUTF8(iStringUTF8, iVT);
end;


procedure TPYEKVSList.PutUTF8String(const aKey: TPYEKVSKey; const aValue: UTF8String);
begin
    PutUTF8StringVT(aKey, aValue, pyeUnknown);
end;

procedure TPYEKVSList.PutUTF8StringVT(const aKey: TPYEKVSKey; const aValue: UTF8String; const aForceValueType: TPYEKVSValueType);
var iVT: TPYEKVSValueType;
begin
    if (aForceValueType=pyeUnknown) then begin
        iVT:=ValueTypeOf(aValue);
    end else begin
        // Try to use aForceValueType; maybe aValue should be UInt32 (4Bytes) to make later the call "ChgUInt" with a higher aValue
        iVT:=aForceValueType;
        if (ValueTypeTst(aValue, iVT)=false) then raise EPYEKVSException.Create(cStrPYEKVSExcept_ValueNotCompatible);
    end;
    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition); // Position before WriteValueType
    FSizeData:=FSizeData+FStoreDocument.WriteValueType(iVT);
    FSizeData:=FSizeData+FStoreDocument.WriteVTStringUTF8(aValue, iVT);
end;

function TPYEKVSList.GetUTF8String(const aKey: TPYEKVSKey; const aDefault: UTF8String): UTF8String;
var iVT: TPYEKVSValueType;
begin
    iVT:=GetKey(aKey);
    result:=FStoreDocument.ReadVTStringUTF8(iVT, aDefault);
end;

procedure TPYEKVSList.ChgUTF8String(const aKey: TPYEKVSKey; const aValue: UTF8String);
var iVT: TPYEKVSValueType;
begin
    // Change Value of existing Key; Value size must match
    iVT:=GetKey(aKey);
    if (iVT=pyeUnknown) then raise EPYEKVSException.CreateFmt(cStrPYEKVSExcept_KeyNotFound, [aKey]);
    if (ValueTypeTst(aValue, iVT)=false) then
        raise EPYEKVSException.Create(cStrPYEKVSExcept_ValueNotCompatible);
    FSizeData:=FSizeData+FStoreDocument.WriteVTStringUTF8(aValue, iVT);
end;


procedure TPYEKVSList.PutMemory(const aKey: TPYEKVSKey; const aValue: TStream);
var iVT: TPYEKVSValueType;
begin
    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition); // Position before WriteValueType
    if (aValue.Size>0) then iVT:=pyeMemory
    else iVT:=pyeZero;
    FSizeData:=FSizeData+FStoreDocument.WriteValueType(iVT);
    FSizeData:=FSizeData+FStoreDocument.WriteVTMemoryStream(aValue, iVT);
end;

function TPYEKVSList.GetMemory(const aKey: TPYEKVSKey; aValue: TStream): Integer;
var iVT: TPYEKVSValueType;
begin
    // result: Bytes read
    iVT:=GetKey(aKey);
    result:=FStoreDocument.ReadVTMemoryStream(iVT, aValue);
end;

procedure TPYEKVSList.PutMemory(const aKey: TPYEKVSKey; const aBuffer: Pointer; aSize: Cardinal);
var iVT: TPYEKVSValueType;
begin
    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition); // Position before WriteValueType
    if (aSize>0) then iVT:=pyeMemory
    else iVT:=pyeZero;
    FSizeData:=FSizeData+FStoreDocument.WriteValueType(iVT);
    FSizeData:=FSizeData+FStoreDocument.WriteVTMemoryBuffer(aBuffer, aSize, iVT);
end;

function TPYEKVSList.GetMemory(const aKey: TPYEKVSKey; const aBuffer: Pointer; aSize: Cardinal): Integer;
var iVT: TPYEKVSValueType;
begin
    // result: Bytes read
    iVT:=GetKey(aKey);
    result:=FStoreDocument.ReadVTMemoryBuffer(iVT, aBuffer, aSize);
end;

function TPYEKVSList.PutStream(const aKey: TPYEKVSKey): TStream;
var iMemStream: TPYEKVSMemoryStream;
begin
    iMemStream:=TPYEKVSMemoryStream.Create(FStoreDocument, self, aKey);

    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition);          // Position before WriteValueType
    FList.Add(FStoreDocument.StreamPosition, iMemStream); // Position after Key

    FSizeData:=FSizeData+iMemStream.WriteHeader;
    result:=iMemStream.GetStream;
end;

function TPYEKVSList.GetStream(const aKey: TPYEKVSKey): TStream;
var iPos      : Int64;
    iPosHead  : Int64;
    iVT       : TPYEKVSValueType;
    iBranch   : TPYEKVSBranch;
    iMemStream: TPYEKVSMemoryStream;
begin
    result:=nil;
    if FKeys.TryGetValue(aKey, iPos) then begin
        FStoreDocument.StreamPosition:=iPos;
        iPosHead:=iPos+FStoreDocument.ReadValueType(iVT);
        if (iVT=pyeMemory) then begin
            if FList.TryGetValue(iPos, iBranch) then begin
                if (iBranch is TPYEKVSMemoryStream) then begin
                    result:=TPYEKVSMemoryStream(iBranch).GetStream; // Warning: user read dircet from KVSstream
                end;
            end else begin
                // First access of GetStream: need dynamic TPYEKVSMemoryStream class creation
                iMemStream:=TPYEKVSMemoryStream.Create(FStoreDocument, self, aKey, iPosHead);
                FList.Add(iPos, iMemStream);
                result:=iMemStream.GetStream; // Warning: user read dircet from KVSstream
            end;
        end;
    end;
end;


function TPYEKVSList.PutList(const aKey: TPYEKVSKey): TPYEKVSList;
begin
    result:=FStoreDocument.RClassPYEKVSList.Create(FStoreDocument, self, aKey);

    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition);      // Position before WriteValueType
    FList.Add(FStoreDocument.StreamPosition, result); // Position after Key

    FSizeData:=FSizeData+result.WriteHeader;
end;


function TPYEKVSList.GetList(const aKey: TPYEKVSKey): TPYEKVSList;
var iPos   : Int64;
    iVT    : TPYEKVSValueType;
    iBranch: TPYEKVSBranch;
begin
    result:=nil;
    if FKeys.TryGetValue(aKey, iPos) then begin
        FStoreDocument.StreamPosition:=iPos;
        FStoreDocument.ReadValueType(iVT);
        if (iVT=pyeList) then begin
            if FList.TryGetValue(iPos, iBranch) then begin
                result:=TPYEKVSList(iBranch);
            end;
        end;
    end;
    if (result=nil) then begin
        // create a empty list to read default values ...
        result:=FStoreDocument.FallbackList();
    end;
end;


function TPYEKVSList.PutArray(const aKey: TPYEKVSKey; const aValueType: TPYEKVSValueType): TPYEKVSArray;
begin
    result:=FStoreDocument.RClassPYEKVSArray.Create(FStoreDocument, self, aKey, aValueType);

    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition);      // Position before WriteValueType
    FList.Add(FStoreDocument.StreamPosition, result); // Position after Key

    FSizeData:=FSizeData+result.WriteHeader(aValueType);
end;

function TPYEKVSList.GetArray(const aKey: TPYEKVSKey): TPYEKVSArray;
var iPos   : Int64;
    iVT    : TPYEKVSValueType;
    iBranch: TPYEKVSBranch;
begin
    result:=nil;
    if FKeys.TryGetValue(aKey, iPos) then begin
        FStoreDocument.StreamPosition:=iPos;
        FStoreDocument.ReadValueType(iVT);
        if (iVT=pyeArray) then begin
            if FList.TryGetValue(iPos, iBranch) then begin
                result:=TPYEKVSArray(iBranch);
            end;
        end;
    end;
    if (result=nil) then begin
        // create a empty array to read default values ...
        result:=FStoreDocument.FallbackArray();
    end;
end;

function TPYEKVSList.PutArrayMap(const aKey: TPYEKVSKey; const aValueTypeMap: TPYEKVSValueTypeMap): TPYEKVSArrayMap;
begin
    result:=FStoreDocument.RClassPYEKVSArrayMap.Create(FStoreDocument, self, aKey, aValueTypeMap);

    FSizeData:=FSizeData+FStoreDocument.WriteKey(self, aKey);
    KeyAdd(aKey, FStoreDocument.StreamPosition);      // Position before WriteValueType
    FList.Add(FStoreDocument.StreamPosition, result); // Position after Key

    FSizeData:=FSizeData+result.WriteHeader();
end;

function TPYEKVSList.GetArrayMap(const aKey: TPYEKVSKey): TPYEKVSArrayMap;
var iPos   : Int64;
    iVT    : TPYEKVSValueType;
    iBranch: TPYEKVSBranch;
begin
    result:=nil;
    if FKeys.TryGetValue(aKey, iPos) then begin
        FStoreDocument.StreamPosition:=iPos;
        FStoreDocument.ReadValueType(iVT);
        if (iVT=pyeArrayMap) then begin
            if FList.TryGetValue(iPos, iBranch) then begin
                result:=TPYEKVSArrayMap(iBranch);
            end;
        end;
    end;
    if (result=nil) then begin
        // create a empty array to read default values ...
        result:=FStoreDocument.FallbackArrayMap();
    end;
end;


procedure InitValueType();
var iDT: TPYEKVSValueType;
begin
    FillChar(GValueTypeValue, Length(GValueTypeValue), Ord(pyeUnknown));
    for iDT:=Low(TPYEKVSValueType) to High(TPYEKVSValueType) do begin
        GValueTypeValue[cPYEKVSValueTypeID[iDT]]:=iDT;
    end;
end;



function TPYEKVSDocument.ReadKey(out aKey: TPYEKVSKey): Integer;
var Len: TPYEKVSKeySize;
begin
    result:=FStream.Read(Len, sizeof(TPYEKVSKeySize));
    if (Len>0) then begin
        SetLength(aKey, Len);
        result:=result+FStream.Read(aKey[1], Len);
    end;
end;

function TPYEKVSDocument.WriteKey(const aBranch: TPYEKVSBranch; const aKey: TPYEKVSKey): Integer;
var Len: Integer;
begin
    WriteBranchSet(aBranch);

    Len:=Length(aKey);
    if (Len>High(TPYEKVSKeySize)) then begin
        raise EPYEKVSException.Create('Key too long');
    end;
    result:=FStream.Write(Len, sizeof(TPYEKVSKeySize));
    if (Len>0) then
        result:=result+FStream.Write(aKey[1], Len);
end;

procedure TPYEKVSDocument.WriteBranchSet(const aBranch: TPYEKVSBranch);
begin
    if (aBranch<>FWriteBranch) then begin
        WriteEnd(aBranch);
        WriteBegin(aBranch);
    end;
end;

procedure TPYEKVSDocument.WriteBegin(const aNewWriteBranch: TPYEKVSBranch);
begin
    if (aNewWriteBranch<>nil) then begin

        // Root branch must be in state srzstate_writing; otherwise PutBegin was missing
        if aNewWriteBranch.FStoreDocument.FRoot.State<>srzstate_PutValues then raise EPYEKVSException.Create('store branch root is not ready for writing. call PutBegin ... PutEnd');

        FWriteBranch:=aNewWriteBranch;
        while (FWriteBranch<>nil)AND(FWriteBranch.State<>srzstate_PutValues) do begin
            FWriteBranch.SetPutValues();
            FWriteBranch:=FWriteBranch.Parent;
        end;
        FWriteBranch:=aNewWriteBranch;
    end;
end;

procedure TPYEKVSDocument.WriteEnd(const aNewWriteBranch: TPYEKVSBranch);
begin
    if (FWriteBranch<>nil) then begin
        if (aNewWriteBranch<>nil)AND(aNewWriteBranch.Parent=FWriteBranch) then exit;

        while (FWriteBranch<>aNewWriteBranch)AND(FWriteBranch<>nil) do begin
            FWriteBranch.SetPutClosed();
            FWriteBranch:=FWriteBranch.Parent;
        end;
    end;
end;


function TPYEKVSDocument.ReadValueType(out aVT: TPYEKVSValueType): Integer;
var iVT: TPYEKVSValueTypeSize;
begin
    result:=FStream.Read(iVT, sizeof(TPYEKVSValueTypeSize));
    aVT:=GValueTypeValue[iVT];
end;

function TPYEKVSDocument.WriteValueType(const aVT: TPYEKVSValueType): Integer;
begin
    result:=FStream.Write(cPYEKVSValueTypeID[aVT], sizeof(TPYEKVSValueTypeSize));
end;


function TPYEKVSDocument.ReadFixUInt8(out aValue: UInt8): Integer;
begin
    // special function for list size
    result:=FStream.Read(aValue, 1);
end;

function TPYEKVSDocument.WriteFixUInt8(const aValue: UInt8; const aPosition: Int64=0): Integer;
begin
    if (aPosition<>0) then begin
        // set stream position
        FStream.Position:=aPosition;
    end;
    result:=FStream.Write(aValue, 1);
    // set stream position end
    FStream.Seek(0, soEnd);
end;

function TPYEKVSDocument.ReadFixUInt32(out aValue: UInt32): Integer;
begin
    // special function for list size
    result:=FStream.Read(aValue, 4);
end;

function TPYEKVSDocument.WriteFixUInt32(const aValue: UInt32; const aPosition: Int64=0): Integer;
begin
    if (aPosition<>0) then begin
        // set stream position
        FStream.Position:=aPosition;
    end;
    result:=FStream.Write(aValue, 4);
    // set stream position end
    FStream.Seek(0, soEnd);
end;


function TPYEKVSDocument.ReadVTBool(out aValue: Boolean; const aVT: TPYEKVSValueType; const aDefault: Boolean): Integer;
begin
    result:=0;
    case aVT of
    pyeZero: aValue:=false;
    pyeBool: aValue:=true;
else aValue:=aDefault;
    end;
end;


function TPYEKVSDocument._WriteInt8(const aValue: Int8): Integer;
begin
    result:=FStream.Write(aValue, cPYEKVSValueTypeSize[pyeInt8]);
end;

function TPYEKVSDocument._WriteUInt8(const aValue: UInt8): Integer;
begin
    result:=FStream.Write(aValue, cPYEKVSValueTypeSize[pyeUInt8]);
end;

function TPYEKVSDocument._WriteInt16(const aValue: Int16): Integer;
begin
    result:=FStream.Write(aValue, cPYEKVSValueTypeSize[pyeInt16]);
end;

function TPYEKVSDocument._WriteUInt16(const aValue: UInt16): Integer;
begin
    result:=FStream.Write(aValue, cPYEKVSValueTypeSize[pyeUInt16]);
end;

function TPYEKVSDocument._WriteInt32(const aValue: Int32): Integer;
begin
    result:=FStream.Write(aValue, cPYEKVSValueTypeSize[pyeInt32]);
end;

function TPYEKVSDocument._WriteUInt32(const aValue: UInt32): Integer;
begin
    result:=FStream.Write(aValue, cPYEKVSValueTypeSize[pyeUInt32]);
end;

function TPYEKVSDocument._WriteInt64(const aValue: Int64): Integer;
begin
    result:=FStream.Write(aValue, cPYEKVSValueTypeSize[pyeInt64]);
end;

function TPYEKVSDocument._WriteUInt64(const aValue: UInt64): Integer;
begin
    result:=FStream.Write(aValue, cPYEKVSValueTypeSize[pyeUInt64]);
end;

function TPYEKVSDocument._WriteInt128(const aValue: TPYEKVSInt128): Integer;
begin
    result:=FStream.Write(aValue, cPYEKVSValueTypeSize[pyeInt128]);
end;

function TPYEKVSDocument._WriteUInt128(const aValue: TPYEKVSUInt128): Integer;
begin
    result:=FStream.Write(aValue, cPYEKVSValueTypeSize[pyeUInt128]);
end;

function TPYEKVSDocument._WriteFloat32(const aValue: Single): Integer;
begin
    result:=FStream.Write(aValue, cPYEKVSValueTypeSize[pyeFloat32]);
end;

function TPYEKVSDocument._WriteFloat64(const aValue: Double): Integer;
begin
    result:=FStream.Write(aValue, cPYEKVSValueTypeSize[pyeFloat64]);
end;

function TPYEKVSDocument._WriteStringUTF8S(const aValue: UTF8String): Integer;
var iLen: Cardinal;
begin
    // Small String; Char lenght UInt8
    iLen:=Length(aValue);
    if (iLen>High(UInt8)) then
        raise EPYEKVSException.Create('_WriteStringUTF8S string too long');
    // UTF8 head (length)
    result:=WriteFixUInt8(iLen);
    // UTF8 value
    if (iLen>0) then
        result:=result+FStream.Write(aValue[1], iLen);
end;

function TPYEKVSDocument._WriteStringUTF8L(const aValue: UTF8String): Integer;
var iLen: Cardinal;
begin
    // Large String; Char lenght UInt32
    iLen:=Length(aValue);
    // UTF8 head (length)
    result:=WriteFixUInt32(iLen);
    // UTF8 value
    if (iLen>0) then
        result:=result+FStream.Write(aValue[1], iLen);
end;


function TPYEKVSDocument._WriteMemoryStream(const aValue: TStream): Integer;
var iData: array [0..31] of Byte;
    iSize: Int64;
    iLen : Integer;
begin
    iSize:=aValue.Size;
    // Memory head (size)
    result:=WriteFixUInt32(iSize);
    // Memory data copy
    aValue.Position:=0; // all bytes in stream
    while iSize>0 do begin
        iLen:=Min(iSize, sizeof(iData));
        aValue.Read(iData, iLen);
        result:=result+FStream.Write(iData[0], iLen);
        iSize:=iSize-iLen;
    end;
end;

function TPYEKVSDocument._WriteMemoryBuffer(const aValue: Pointer; aSize: Cardinal): Integer;
begin
    // Memory head (size)
    result:=WriteFixUInt32(aSize);
    // Memory data copy
    if (aSize>0) then begin
        result:=result+FStream.Write(aValue^, aSize);
    end;
end;


function TPYEKVSDocument.WriteVTInt(const aValue: Integer; const aVT: TPYEKVSValueType): Integer;
begin
    case aVT of
    pyeZero: result:=0;
    pyeBool: result:=0;
    pyeInt8: result:=_WriteInt8(aValue);
    pyeInt16: result:=_WriteInt16(aValue);
    pyeInt32: result:=_WriteInt32(aValue);
    pyeInt64: result:=_WriteInt64(aValue);
    pyeUInt8: result:=_WriteUInt8(aValue);
    pyeUInt16: result:=_WriteUInt16(aValue);
    pyeUInt32: result:=_WriteUInt32(aValue);
    pyeUInt64: result:=_WriteUInt64(aValue);
    pyeFloat32: result:=_WriteFloat32(aValue);
    pyeFloat64: result:=_WriteFloat64(aValue);
else
    raise EPYEKVSException.Create('WriteVTInt wrong ValueType');
    end;
end;

function TPYEKVSDocument.WriteVTUInt(const aValue: Cardinal; const aVT: TPYEKVSValueType): Integer;
begin
    case aVT of
    pyeZero: result:=0;
    pyeBool: result:=0;
    pyeInt8: result:=_WriteInt8(aValue);
    pyeInt16: result:=_WriteInt16(aValue);
    pyeInt32: result:=_WriteInt32(aValue);
    pyeInt64: result:=_WriteInt64(aValue);
    pyeUInt8: result:=_WriteUInt8(aValue);
    pyeUInt16: result:=_WriteUInt16(aValue);
    pyeUInt32: result:=_WriteUInt32(aValue);
    pyeUInt64: result:=_WriteUInt64(aValue);
    pyeFloat32: result:=_WriteFloat32(aValue);
    pyeFloat64: result:=_WriteFloat64(aValue);
else
    raise EPYEKVSException.Create('WriteVTUInt wrong ValueType');
    end;
end;

function TPYEKVSDocument.WriteVTInt64(const aValue: Int64; const aVT: TPYEKVSValueType): Integer;
begin
    case aVT of
    pyeZero: result:=0;
    pyeBool: result:=0;
    pyeInt8: result:=_WriteInt8(aValue);
    pyeInt16: result:=_WriteInt16(aValue);
    pyeInt32: result:=_WriteInt32(aValue);
    pyeInt64: result:=_WriteInt64(aValue);
    pyeUInt8: result:=_WriteUInt8(aValue);
    pyeUInt16: result:=_WriteUInt16(aValue);
    pyeUInt32: result:=_WriteUInt32(aValue);
    pyeUInt64: result:=_WriteUInt64(aValue);
    pyeFloat32: result:=_WriteFloat32(aValue);
    pyeFloat64: result:=_WriteFloat64(aValue);
else
    raise EPYEKVSException.Create('WriteVTInt64 wrong ValueType');
    end;
end;

function TPYEKVSDocument.WriteVTUInt64(const aValue: UInt64; const aVT: TPYEKVSValueType): Integer;
begin
    case aVT of
    pyeZero: result:=0;
    pyeBool: result:=0;
    pyeInt8: result:=_WriteInt8(aValue);
    pyeInt16: result:=_WriteInt16(aValue);
    pyeInt32: result:=_WriteInt32(aValue);
    pyeInt64: result:=_WriteInt64(aValue);
    pyeUInt8: result:=_WriteUInt8(aValue);
    pyeUInt16: result:=_WriteUInt16(aValue);
    pyeUInt32: result:=_WriteUInt32(aValue);
    pyeUInt64: result:=_WriteUInt64(aValue);
    pyeFloat32: result:=_WriteFloat32(aValue);
    pyeFloat64: result:=_WriteFloat64(aValue);
else
    raise EPYEKVSException.Create('WriteVTUInt64 wrong ValueType');
    end;
end;

function TPYEKVSDocument.WriteVTSingle(const aValue: Single; const aVT: TPYEKVSValueType): Integer;
begin
    case aVT of
    pyeZero: result:=0;
    pyeBool: result:=0;
    pyeFloat32: result:=_WriteFloat32(aValue);
    pyeFloat64: result:=_WriteFloat64(aValue);
else
    raise EPYEKVSException.Create('WriteVTSingle wrong ValueType');
    end;
end;

function TPYEKVSDocument.WriteVTDouble(const aValue: Double; const aVT: TPYEKVSValueType): Integer;
begin
    case aVT of
    pyeZero: result:=0;
    pyeBool: result:=0;
    pyeFloat32: result:=_WriteFloat32(aValue);
    pyeFloat64: result:=_WriteFloat64(aValue);
else
    raise EPYEKVSException.Create('WriteVTDouble wrong ValueType');
    end;
end;

function TPYEKVSDocument.WriteVTStringUTF8(const aValue: UTF8String; const aVT: TPYEKVSValueType): Integer;
begin
    case aVT of
    pyeZero: result:=0;
    pyeStringUTF8S: result:=_WriteStringUTF8S(aValue);
    pyeStringUTF8L: result:=_WriteStringUTF8L(aValue);
else
    raise EPYEKVSException.Create('WriteVTStringUTF8 wrong ValueType');
    end;
end;

function TPYEKVSDocument.WriteVTMemoryStream(const aValue: TStream; const aVT: TPYEKVSValueType): Integer;
begin
    case aVT of
    pyeZero: result:=0;
    pyeMemory: result:=_WriteMemoryStream(aValue);
else
    raise EPYEKVSException.Create('WriteVTMemoryStream wrong ValueType');
    end;
end;

function TPYEKVSDocument.WriteVTMemoryBuffer(const aValue: Pointer; aSize: Cardinal; const aVT: TPYEKVSValueType): Integer;
begin
    case aVT of
    pyeZero: result:=0;
    pyeMemory: result:=_WriteMemoryBuffer(aValue, aSize);
else
    raise EPYEKVSException.Create('WriteVTMemoryBuffer wrong ValueType');
    end;
end;



function TPYEKVSDocument._ReadInt8(out aValue: Int8): Integer;
begin
    result:=FStream.Read(aValue, cPYEKVSValueTypeSize[pyeInt8]);
end;

function TPYEKVSDocument._ReadUInt8(out aValue: UInt8): Integer;
begin
    result:=FStream.Read(aValue, cPYEKVSValueTypeSize[pyeUInt8]);
end;

function TPYEKVSDocument._ReadInt16(out aValue: Int16): Integer;
begin
    result:=FStream.Read(aValue, cPYEKVSValueTypeSize[pyeInt16]);
end;

function TPYEKVSDocument._ReadUInt16(out aValue: UInt16): Integer;
begin
    result:=FStream.Read(aValue, cPYEKVSValueTypeSize[pyeUInt16]);
end;

function TPYEKVSDocument._ReadInt32(out aValue: Int32): Integer;
begin
    result:=FStream.Read(aValue, cPYEKVSValueTypeSize[pyeInt32]);
end;

function TPYEKVSDocument._ReadUInt32(out aValue: UInt32): Integer;
begin
    result:=FStream.Read(aValue, cPYEKVSValueTypeSize[pyeUInt32]);
end;

function TPYEKVSDocument._ReadInt64(out aValue: Int64): Integer;
begin
    result:=FStream.Read(aValue, cPYEKVSValueTypeSize[pyeInt64]);
end;

function TPYEKVSDocument._ReadUInt64(out aValue: UInt64): Integer;
begin
    result:=FStream.Read(aValue, cPYEKVSValueTypeSize[pyeUInt64]);
end;

function TPYEKVSDocument._ReadInt128(out aValue: TPYEKVSInt128): Integer;
begin
    result:=FStream.Read(aValue, cPYEKVSValueTypeSize[pyeInt128]);
end;

function TPYEKVSDocument._ReadUInt128(out aValue: TPYEKVSUInt128): Integer;
begin
    result:=FStream.Read(aValue, cPYEKVSValueTypeSize[pyeUInt128]);
end;

function TPYEKVSDocument._ReadFloat32(out aValue: Single): Integer;
begin
    result:=FStream.Read(aValue, cPYEKVSValueTypeSize[pyeFloat32]);
end;

function TPYEKVSDocument._ReadFloat64(out aValue: Double): Integer;
begin
    result:=FStream.Read(aValue, cPYEKVSValueTypeSize[pyeFloat64]);
end;

function TPYEKVSDocument._ReadFloat128(out aValue: TPYEKVSFloat128): Integer;
begin
    result:=FStream.Read(aValue, cPYEKVSValueTypeSize[pyeFloat128]);
end;


function TPYEKVSDocument._ReadStringUTF8(out aValue: UTF8String; const aLen: Cardinal): Integer;
begin
    SetLength(aValue, aLen);
    result:=FStream.Read(aValue[1], aLen);
end;

function TPYEKVSDocument._ReadStringUTF8S(out aValue: UTF8String): Integer;
var uLen: UInt8;
begin
    // UTF8 head
    result:=ReadFixUInt8(uLen);
    if (uLen>0) then result:=result+_ReadStringUTF8(aValue, uLen)
    else aValue:='';
end;

function TPYEKVSDocument._ReadStringUTF8L(out aValue: UTF8String): Integer;
var uLen: UInt32;
begin
    // UTF8 head
    result:=ReadFixUInt32(uLen);
    if (uLen>0) then result:=result+_ReadStringUTF8(aValue, uLen)
    else aValue:='';
end;

function TPYEKVSDocument._ReadMemoryStream(aValue: TStream; out aByteRead: Integer): Integer;
var iData: array [0..31] of Byte;
    uSize: Cardinal;
    iSize: Int64;
    iLen : Integer;
begin
    aByteRead:=0;
    // Memory head (size)
    result:=ReadFixUInt32(uSize);
    // Memory data copy
    iSize:=uSize;
    while iSize>0 do begin
        iLen:=Min(iSize, sizeof(iData));
        result:=result+FStream.Read(iData, iLen);
        aByteRead:=aByteRead+aValue.Write(iData, iLen);
        iSize:=iSize-iLen;
    end;
end;

function TPYEKVSDocument._ReadMemoryBuffer(const aValue: Pointer; aSize: Cardinal; out aByteRead: Integer): Integer;
var uSize: Cardinal;
begin
    aByteRead:=0;
    // Memory head (size)
    result:=ReadFixUInt32(uSize);
    if (uSize>0) then begin
        // Memory data copy
        aByteRead:=Min(aSize, uSize);
        result:=result+FStream.Read(aValue^, aByteRead);
    end;
end;


function TPYEKVSDocument.ReadInt8(): Int8;
begin
    _ReadInt8(result);
end;

function TPYEKVSDocument.ReadUInt8(): UInt8;
begin
    _ReadUInt8(result);
end;

function TPYEKVSDocument.ReadInt16(): Int16;
begin
    _ReadInt16(result);
end;

function TPYEKVSDocument.ReadUInt16(): UInt16;
begin
    _ReadUInt16(result);
end;

function TPYEKVSDocument.ReadInt32(): Int32;
begin
    _ReadInt32(result);
end;

function TPYEKVSDocument.ReadUInt32(): UInt32;
begin
    _ReadUInt32(result);
end;

function TPYEKVSDocument.ReadInt64(): Int64;
begin
    _ReadInt64(result);
end;

function TPYEKVSDocument.ReadUInt64(): UInt64;
begin
    _ReadUInt64(result);
end;

function TPYEKVSDocument.ReadFloat32(): Single;
begin
    _ReadFloat32(result);
end;

function TPYEKVSDocument.ReadFloat64(): Double;
begin
    _ReadFloat64(result);
end;

function TPYEKVSDocument.ReadStringUTF8S(): UTF8String;
begin
    _ReadStringUTF8S(result);
end;

function TPYEKVSDocument.ReadStringUTF8L(): UTF8String;
begin
    _ReadStringUTF8L(result);
end;



function TPYEKVSDocument.ReadVTInt32(out aValue: Int32; const aVT: TPYEKVSValueType; const aDefault: Int32): Integer;

    function __ReadInt8(): Integer;
    var i: Int8;
    begin
        result:=_ReadInt8(i);
        aValue:=i;
    end;
    function __ReadInt16(): Integer;
    var i: Int16;
    begin
        result:=_ReadInt16(i);
        aValue:=i;
    end;
    function __ReadInt64(): Integer;
    var i: Int64;
    begin
        i:=0;
        result:=_ReadInt64(i);
        if (i>=Low(Integer))AND(i<=High(Integer)) then aValue:=i
        else aValue:=aDefault;
    end;

    function __ReadUInt8(): Integer;
    var U: UInt8;
    begin
        result:=_ReadUInt8(U);
        aValue:=U;
    end;
    function __ReadUInt16(): Integer;
    var U: UInt16;
    begin
        result:=_ReadUInt16(U);
        aValue:=U;
    end;
    function __ReadUInt32(): Integer;
    var U: UInt32;
    begin
        result:=_ReadUInt32(U);
        if (Int64(U)<=High(Integer)) then aValue:=U
        else aValue:=aDefault;
    end;
    function __ReadUInt64(): Integer;
    var U: UInt64;
    begin
        result:=_ReadUInt64(U);
        if (U<=High(Integer)) then aValue:=U
        else aValue:=aDefault;
    end;

begin
    case aVT of
    pyeZero: begin
            aValue:=0;
            result:=0;
        end;
    pyeBool: begin
            aValue:=1;
            result:=0;
        end;
    pyeInt8: result:=__ReadInt8();
    pyeInt16: result:=__ReadInt16();
    pyeInt32: result:=_ReadInt32(aValue);
    pyeInt64: result:=__ReadInt64();
    pyeUInt8: result:=__ReadUInt8();
    pyeUInt16: result:=__ReadUInt16();
    pyeUInt32: result:=__ReadUInt32();
    pyeUInt64: result:=__ReadUInt64();
else begin
        aValue:=aDefault;
        result:=0;
    end;
    end;
end;

function TPYEKVSDocument.ReadVTInt64(out aValue: Int64; const aVT: TPYEKVSValueType; const aDefault: Int64): Integer;

    function __ReadInt8(): Integer;
    var i: Int8;
    begin
        result:=_ReadInt8(i);
        aValue:=i;
    end;
    function __ReadInt16(): Integer;
    var i: Int16;
    begin
        result:=_ReadInt16(i);
        aValue:=i;
    end;
    function __ReadInt32(): Integer;
    var i: Int32;
    begin
        result:=_ReadInt32(i);
        aValue:=i;
    end;

    function __ReadUInt8(): Integer;
    var U: UInt8;
    begin
        result:=_ReadUInt8(U);
        aValue:=U;
    end;
    function __ReadUInt16(): Integer;
    var U: UInt16;
    begin
        result:=_ReadUInt16(U);
        aValue:=U;
    end;
    function __ReadUInt32(): Integer;
    var U: UInt32;
    begin
        result:=_ReadUInt32(U);
        aValue:=U;
    end;
    function __ReadUInt64(): Integer;
    var U: UInt64;
    begin
        result:=_ReadUInt64(U);
        if (U<=High(Int64)) then aValue:=U
        else aValue:=aDefault;
    end;

begin
    case aVT of
    pyeZero: begin
            aValue:=0;
            result:=0;
        end;
    pyeBool: begin
            aValue:=1;
            result:=0;
        end;
    pyeInt8: result:=__ReadInt8();
    pyeInt16: result:=__ReadInt16();
    pyeInt32: result:=__ReadInt32();
    pyeInt64: result:=_ReadInt64(aValue);
    pyeUInt8: result:=__ReadUInt8();
    pyeUInt16: result:=__ReadUInt16();
    pyeUInt32: result:=__ReadUInt32();
    pyeUInt64: result:=__ReadUInt64();
else begin
        aValue:=aDefault;
        result:=0;
    end;
    end;
end;

function TPYEKVSDocument.ReadVTInt128(out aValue: TPYEKVSInt128; const aVT: TPYEKVSValueType; const aDefault: TPYEKVSInt128): Integer;
begin
    result:=0;
    case aVT of
    pyeInt128: result:=_ReadInt128(aValue);
else begin
        aValue:=aDefault;
    end;
    end;
end;


function TPYEKVSDocument.ReadVTUInt32(out aValue: UInt32; const aVT: TPYEKVSValueType; const aDefault: UInt32): Integer;

    function __ReadInt8(): Integer;
    var i: Int8;
    begin
        result:=_ReadInt8(i);
        if (i>=0) then aValue:=i
        else aValue:=aDefault;
    end;
    function __ReadInt16(): Integer;
    var i: Int16;
    begin
        result:=_ReadInt16(i);
        if (i>=0) then aValue:=i
        else aValue:=aDefault;
    end;
    function __ReadInt32(): Integer;
    var i: Int32;
    begin
        result:=_ReadInt32(i);
        if (i>=0) then aValue:=i
        else aValue:=aDefault;
    end;
    function __ReadInt64(): Integer;
    var i: Int64;
    begin
        i:=0;
        result:=_ReadInt64(i);
        if (i>=0)AND(i<=High(UInt32)) then aValue:=i
        else aValue:=aDefault;
    end;

    function __ReadUInt8(): Integer;
    var U: UInt8;
    begin
        result:=_ReadUInt8(U);
        aValue:=U;
    end;
    function __ReadUInt16(): Integer;
    var U: UInt16;
    begin
        result:=_ReadUInt16(U);
        aValue:=U;
    end;
    function __ReadUInt64(): Integer;
    var U: UInt64;
    begin
        result:=_ReadUInt64(U);
        if (U>=Low(UInt32))AND(U<=High(UInt32)) then aValue:=U
        else aValue:=aDefault;
    end;

begin
    case aVT of
    pyeZero: begin
            aValue:=0;
            result:=0;
        end;
    pyeBool: begin
            aValue:=1;
            result:=0;
        end;
    pyeInt8: result:=__ReadInt8();
    pyeInt16: result:=__ReadInt16();
    pyeInt32: result:=__ReadInt32();
    pyeInt64: result:=__ReadInt64();
    pyeUInt8: result:=__ReadUInt8();
    pyeUInt16: result:=__ReadUInt16();
    pyeUInt32: result:=_ReadUInt32(aValue);
    pyeUInt64: result:=__ReadUInt64();
else begin
        aValue:=aDefault;
        result:=0;
    end;
    end;
end;

function TPYEKVSDocument.ReadVTUInt64(out aValue: UInt64; const aVT: TPYEKVSValueType; const aDefault: UInt64): Integer;

    function __ReadInt8(): Integer;
    var i: Int8;
    begin
        result:=_ReadInt8(i);
        if (i>=0) then aValue:=i
        else aValue:=aDefault;
    end;
    function __ReadInt16(): Integer;
    var i: Int16;
    begin
        result:=_ReadInt16(i);
        if (i>=0) then aValue:=i
        else aValue:=aDefault;
    end;
    function __ReadInt32(): Integer;
    var i: Int32;
    begin
        result:=_ReadInt32(i);
        if (i>=0) then aValue:=i
        else aValue:=aDefault;
    end;
    function __ReadInt64(): Integer;
    var i: Int64;
    begin
        i:=0;
        result:=_ReadInt64(i);
        if (i>=0) then aValue:=i
        else aValue:=aDefault;
    end;

    function __ReadUInt8(): Integer;
    var U: UInt8;
    begin
        result:=_ReadUInt8(U);
        aValue:=U;
    end;
    function __ReadUInt16(): Integer;
    var U: UInt16;
    begin
        result:=_ReadUInt16(U);
        aValue:=U;
    end;
    function __ReadUInt32(): Integer;
    var U: UInt32;
    begin
        result:=_ReadUInt32(U);
        aValue:=U;
    end;

begin
    case aVT of
    pyeZero: begin
            aValue:=0;
            result:=0;
        end;
    pyeBool: begin
            aValue:=1;
            result:=0;
        end;
    pyeInt8: result:=__ReadInt8();
    pyeInt16: result:=__ReadInt16();
    pyeInt32: result:=__ReadInt32();
    pyeInt64: result:=__ReadInt64();
    pyeUInt8: result:=__ReadUInt8();
    pyeUInt16: result:=__ReadUInt16();
    pyeUInt32: result:=__ReadUInt32();
    pyeUInt64: result:=_ReadUInt64(aValue);
else begin
        aValue:=aDefault;
        result:=0;
    end;
    end;
end;

function TPYEKVSDocument.ReadVTUInt128(out aValue: TPYEKVSUInt128; const aVT: TPYEKVSValueType; const aDefault: TPYEKVSUInt128): Integer;
begin
    result:=0;
    case aVT of
    pyeUInt128: result:=_ReadUInt128(aValue);
else begin
        aValue:=aDefault;
    end;
    end;
end;

function TPYEKVSDocument.ReadVTSingle(out aValue: Single; const aVT: TPYEKVSValueType; const aDefault: Single): Integer;

    function __ReadInt8(): Integer;
    var i: Int8;
    begin
        result:=_ReadInt8(i);
        aValue:=i;
    end;
    function __ReadInt16(): Integer;
    var i: Int16;
    begin
        result:=_ReadInt16(i);
        aValue:=i;
    end;
    function __ReadInt32(): Integer;
    var i: Int32;
    begin
        result:=_ReadInt32(i);
        aValue:=i;
    end;
    function __ReadInt64(): Integer;
    var i: Int64;
    begin
        result:=_ReadInt64(i);
        aValue:=i;
    end;
    function __ReadUInt8(): Integer;
    var i: UInt8;
    begin
        result:=_ReadUInt8(i);
        aValue:=i;
    end;
    function __ReadUInt16(): Integer;
    var i: UInt16;
    begin
        result:=_ReadUInt16(i);
        aValue:=i;
    end;
    function __ReadUInt32(): Integer;
    var i: UInt32;
    begin
        result:=_ReadUInt32(i);
        aValue:=i;
    end;
    function __ReadUInt64(): Integer;
    var i: UInt64;
    begin
        result:=_ReadUInt64(i);
        aValue:=i;
    end;
    function __ReadFloat64(): Integer;
    var D: Double;
    begin
        result:=_ReadFloat64(D);
        aValue:=D;
    end;

begin
    result:=0;
    case aVT of
    pyeZero: aValue:=0;
    pyeBool: aValue:=1;
    pyeInt8: result:=__ReadInt8();
    pyeInt16: result:=__ReadInt16();
    pyeInt32: result:=__ReadInt32();
    pyeInt64: result:=__ReadInt64();
    pyeUInt8: result:=__ReadUInt8();
    pyeUInt16: result:=__ReadUInt16();
    pyeUInt32: result:=__ReadUInt32();
    pyeUInt64: result:=__ReadUInt64();
    pyeFloat32: result:=_ReadFloat32(aValue);
    pyeFloat64: result:=__ReadFloat64();
else aValue:=aDefault;
    end;
end;


function TPYEKVSDocument.ReadVTDouble(out aValue: Double; const aVT: TPYEKVSValueType; const aDefault: Double): Integer;

    function __ReadInt8(): Integer;
    var i: Int8;
    begin
        result:=_ReadInt8(i);
        aValue:=i;
    end;
    function __ReadInt16(): Integer;
    var i: Int16;
    begin
        result:=_ReadInt16(i);
        aValue:=i;
    end;
    function __ReadInt32(): Integer;
    var i: Int32;
    begin
        result:=_ReadInt32(i);
        aValue:=i;
    end;
    function __ReadInt64(): Integer;
    var i: Int64;
    begin
        result:=_ReadInt64(i);
        aValue:=i;
    end;
    function __ReadUInt8(): Integer;
    var i: UInt8;
    begin
        result:=_ReadUInt8(i);
        aValue:=i;
    end;
    function __ReadUInt16(): Integer;
    var i: UInt16;
    begin
        result:=_ReadUInt16(i);
        aValue:=i;
    end;
    function __ReadUInt32(): Integer;
    var i: UInt32;
    begin
        result:=_ReadUInt32(i);
        aValue:=i;
    end;
    function __ReadUInt64(): Integer;
    var i: UInt64;
    begin
        result:=_ReadUInt64(i);
        aValue:=i;
    end;
    function __ReadFloat32(): Integer;
    var S: Single;
    begin
        result:=_ReadFloat32(S);
        aValue:=S;
    end;

begin
    result:=0;
    case aVT of
    pyeZero: aValue:=0;
    pyeBool: aValue:=1;
    pyeInt8: result:=__ReadInt8();
    pyeInt16: result:=__ReadInt16();
    pyeInt32: result:=__ReadInt32();
    pyeInt64: result:=__ReadInt64();
    pyeUInt8: result:=__ReadUInt8();
    pyeUInt16: result:=__ReadUInt16();
    pyeUInt32: result:=__ReadUInt32();
    pyeUInt64: result:=__ReadUInt64();
    pyeFloat32: result:=__ReadFloat32();
    pyeFloat64: result:=_ReadFloat64(aValue);
else aValue:=aDefault;
    end;
end;


function TPYEKVSDocument.ReadVTStringUTF8(const aVT: TPYEKVSValueType; const aDefault: UTF8String): UTF8String;
begin
    case aVT of
    pyeZero: result:='';
    pyeStringUTF8S: _ReadStringUTF8S(result);
    pyeStringUTF8L: _ReadStringUTF8L(result);
else result:=aDefault;
    end;
end;

function TPYEKVSDocument.ReadVTMemoryStream(const aVT: TPYEKVSValueType; aValue: TStream): Integer;
begin
    case aVT of
    pyeMemory: _ReadMemoryStream(aValue, result);
else result:=0;
    end;
end;

function TPYEKVSDocument.ReadVTMemoryBuffer(const aVT: TPYEKVSValueType; const aValue: Pointer; aSize: Cardinal): Integer;
begin
    case aVT of
    pyeMemory: _ReadMemoryBuffer(aValue, aSize, result);
else result:=0;
    end;
end;



function TPYEKVSDocument.ReadToString(const aVT: TPYEKVSValueType): string;
// FKVSStream.Read fundamental values given by aVT to a string

    procedure __FmtInt8();
    var iValue: Int8;
    begin
        _ReadInt8(iValue);
        result:=IntToStr(iValue);
    end;
    procedure __FmtInt16();
    var iValue: Int16;
    begin
        _ReadInt16(iValue);
        result:=IntToStr(iValue);
    end;
    procedure __FmtInt32();
    var iValue: Int32;
    begin
        _ReadInt32(iValue);
        result:=IntToStr(iValue);
    end;
    procedure __FmtInt64();
    var iValue: Int64;
    begin
        _ReadInt64(iValue);
        result:=IntToStr(iValue);
    end;
    procedure __FmtInt128();
    var iValue: TPYEKVSInt128;
    var P     : PWideChar;
    begin
        _ReadInt128(iValue);
        SetLength(result, sizeof(iValue)*2);
        P:=@(result[1]);
        BinToHex(@(iValue[0]), P, sizeof(iValue));
        // result:='$'+result;
    end;
    procedure __FmtUInt8();
    var iValue: UInt8;
    begin
        _ReadUInt8(iValue);
        result:=UIntToStr(iValue);
    end;
    procedure __FmtUInt16();
    var iValue: UInt16;
    begin
        _ReadUInt16(iValue);
        result:=UIntToStr(iValue);
    end;
    procedure __FmtUInt32();
    var iValue: UInt32;
    begin
        _ReadUInt32(iValue);
        result:=UIntToStr(iValue);
    end;
    procedure __FmtUInt64();
    var iValue: UInt64;
    begin
        _ReadUInt64(iValue);
        result:=UIntToStr(iValue);
    end;
    procedure __FmtUInt128();
    var iValue: TPYEKVSUInt128;
    var P     : PWideChar;
    begin
        _ReadUInt128(iValue);
        SetLength(result, sizeof(iValue)*2);
        P:=@(result[1]);
        BinToHex(@(iValue[0]), P, sizeof(iValue));
        // result:='$'+result;
    end;
    procedure __FmtFloat32();
    var iValue: Single;
    begin
        _ReadFloat32(iValue);
        result:=FloatToStr(iValue, GFormatSettingsFloat);
    end;
    procedure __FmtFloat64();
    var iValue: Double;
    begin
        _ReadFloat64(iValue);
        result:=FloatToStr(iValue, GFormatSettingsFloat);
    end;
    procedure __FmtFloat128();
    var iValue: TPYEKVSFloat128;
    var P     : PWideChar;
    begin
        _ReadFloat128(iValue);
        SetLength(result, sizeof(iValue)*2);
        P:=@(result[1]);
        BinToHex(@(iValue[0]), P, sizeof(iValue));
        // result:='$'+result;
    end;
    procedure __FmtStringUTF8S();
    var iValue: UTF8String;
    begin
        _ReadStringUTF8S(iValue);
        result:=Str_PYEKVSStringQuote+UTF8ToString(iValue)+Str_PYEKVSStringQuote;
    end;
    procedure __FmtStringUTF8L();
    var iValue: UTF8String;
        iLen  : Integer;
    begin
        _ReadStringUTF8L(iValue);
        result:=Str_PYEKVSStringQuote+UTF8ToString(iValue)+Str_PYEKVSStringQuote;
        iLen:=Length(result);
        if (iLen>255) then begin
            Delete(result, 255, iLen);
            result:=result+Format('... %dChars', [iLen]);
        end;
        result:=result+Str_PYEKVSStringQuote;
    end;
    procedure __FmtMemory();
    var uSize : Cardinal;
        uLen  : UInt64;
        iValue: TBytes;
        P     : PWideChar;
    begin
        // Memory head (size)
        ReadFixUInt32(uSize);
        // Memory data copy
        if (uSize=0) then begin
            result:=Str_PYEKVSZero;
            exit;
        end;
        uLen:=uSize;
        if (uLen>32) then
            uLen:=32;
        SetLength(iValue, uLen);
        FStream.Read(iValue, uLen);
        // Hex-Output
        SetLength(result, Length(iValue)*2);
        P:=@(result[1]);
        BinToHex(iValue, P, Length(iValue));
        if (uLen<>uSize) then
            result:=result+Format('... %dBytes', [uSize]);
        result:=Str_PYEKVSStringQuote+result+Str_PYEKVSStringQuote;
    end;

begin
    case aVT of
    pyeUnknown: begin
            result:=Str_PYEKVSUnknown;
        end;
    pyeZero: begin
            result:=Str_PYEKVSZero;
        end;
    pyeBool: begin
            result:=Str_PYEKVSBool;
        end;
    pyeInt8: __FmtInt8();
    pyeInt16: __FmtInt16();
    pyeInt32: __FmtInt32();
    pyeInt64: __FmtInt64();
    pyeInt128: __FmtInt128();
    pyeUInt8: __FmtUInt8();
    pyeUInt16: __FmtUInt16();
    pyeUInt32: __FmtUInt32();
    pyeUInt64: __FmtUInt64();
    pyeUInt128: __FmtUInt128();
    pyeFloat32: __FmtFloat32();
    pyeFloat64: __FmtFloat64();
    pyeFloat128: __FmtFloat128();
    pyeStringUTF8S: __FmtStringUTF8S();
    pyeStringUTF8L: __FmtStringUTF8L();
    pyeMemory: __FmtMemory();
else raise EPYEKVSException.Create('ReadVTTo String wrong ValueType');
    end;
end;

function TPYEKVSDocument.EncodeVT(const aValueType: TPYEKVSValueType): Integer;
begin
    case aValueType of
    pyeStringUTF8S: result:=EncodeStringUTF8S();
    pyeStringUTF8L: result:=EncodeStringUTF8L();
    pyeMemory: result:=EncodeMemory();
else result:=EncodeFundamental(aValueType);
    end;
end;


function TPYEKVSDocument.EncodeFundamental(const aValueType: TPYEKVSValueType): Integer;
begin
    result:=cPYEKVSValueTypeSize[aValueType];
    // skip value bytes
    FStream.Position:=FStream.Position+result;
end;

function TPYEKVSDocument.EncodeStringUTF8S(): Integer;
var uLen: UInt8;
begin
    // UTF8 head
    result:=ReadFixUInt8(uLen);
    if (uLen>0) then begin
        // skip UTF8 chars
        result:=result+Integer(uLen);
        FStream.Position:=FStream.Position+uLen;
    end;
end;

function TPYEKVSDocument.EncodeStringUTF8L(): Integer;
var uLen: UInt32;
begin
    // UTF8 head
    result:=ReadFixUInt32(uLen);
    if (uLen>0) then begin
        // skip UTF8 chars
        result:=result+Integer(uLen);
        FStream.Position:=FStream.Position+uLen;
    end;
end;

function TPYEKVSDocument.EncodeMemory(): Integer;
var uSize: UInt32;
begin
    // Memory head
    result:=ReadFixUInt32(uSize);
    if (uSize>0) then begin
        // skip memory bytes
        result:=result+Integer(uSize);
        FStream.Position:=FStream.Position+uSize;
    end;
end;


{ TPYEKVSArray }

constructor TPYEKVSArray.Create(const aStoreDocument: TPYEKVSDocument; const aStoreParent: TPYEKVSBranch; const aKey: TPYEKVSKey);
begin
    inherited;
end;

constructor TPYEKVSArray.Create(const aStoreDocument: TPYEKVSDocument; const aStoreParent: TPYEKVSBranch; const aKey: TPYEKVSKey; const aValueType: TPYEKVSValueType);
begin
    Create(aStoreDocument, aStoreParent, aKey);
    FValueType:=aValueType;
    SetValueTypeAllowed(FValueType);

    if (FValueTypeAllowed*cPYEKVSValueTypeOfArrayDynamicLen<>[]) then begin
        FPosList:=TDictionary<Cardinal, Int64>.Create();
    end;
end;

destructor TPYEKVSArray.Destroy;
begin
    if (FPosList<>nil) then FPosList.Free();
    inherited;
end;

procedure TPYEKVSArray.Clear;
begin
    if (FPosList<>nil) then FPosList.Free();
    inherited;
end;

function TPYEKVSArray.GetCount: Int64;
begin
    result:=FCount;
end;


procedure TPYEKVSArray.SetValueTypeAllowed(const aValueType: TPYEKVSValueType);
begin
    // Hint: pyeZero is here allowed because Integer=0 will be pyeZero (but stored as Array-ValueType)
    case aValueType of
    pyeInt8: FValueTypeAllowed:=[pyeZero, pyeBool, pyeInt8];
    pyeInt16: FValueTypeAllowed:=[pyeZero, pyeBool, pyeInt8, pyeUInt8, pyeInt16];
    pyeInt32: FValueTypeAllowed:=[pyeZero, pyeBool, pyeInt8, pyeUInt8, pyeInt16, pyeUInt16, pyeInt32];
    pyeInt64: FValueTypeAllowed:=[pyeZero, pyeBool, pyeInt8, pyeUInt8, pyeInt16, pyeUInt16, pyeInt32, pyeUInt32, pyeInt64];
    pyeInt128: FValueTypeAllowed:=[ { pyeZero, pyeBool, pyeInt8, pyeUInt8, pyeInt16, pyeUInt16, pyeInt32, pyeUInt32, pyeInt64, } pyeInt128];

    pyeUInt8: FValueTypeAllowed:=[pyeZero, pyeBool, pyeUInt8];
    pyeUInt16: FValueTypeAllowed:=[pyeZero, pyeBool, pyeUInt8, pyeUInt16];
    pyeUInt32: FValueTypeAllowed:=[pyeZero, pyeBool, pyeUInt8, pyeUInt16, pyeUInt32];
    pyeUInt64: FValueTypeAllowed:=[pyeZero, pyeBool, pyeUInt8, pyeUInt16, pyeUInt32, pyeUInt64];
    pyeUInt128: FValueTypeAllowed:=[ { pyeZero, pyeBool, pyeUInt8, pyeUInt16, pyeUInt32, pyeUInt64, } pyeUInt128];

    pyeFloat32: FValueTypeAllowed:=[pyeZero, pyeFloat32];
    pyeFloat64: FValueTypeAllowed:=[pyeZero, pyeFloat32, pyeFloat64];
    pyeFloat128: FValueTypeAllowed:=[ { pyeZero, pyeFloat32, pyeFloat64, } pyeFloat128];

    pyeStringUTF8S: FValueTypeAllowed:=[pyeZero, pyeStringUTF8S];
    pyeStringUTF8L: FValueTypeAllowed:=[pyeZero, pyeStringUTF8S, pyeStringUTF8L];

    pyeMemory: FValueTypeAllowed:=[pyeMemory];

else raise EPYEKVSException.CreateFmt('value type %s is not allowed for Array', [cPYEKVSValueTypeName[aValueType]]);
    end;
end;

function TPYEKVSArray.Encode(): Integer;
var uSize : Cardinal;
    uCount: Cardinal;
    iIndex: Cardinal;
    iSize : Int64;
begin
    result:=inherited Encode();

    result:=result+FStoreDocument.ReadValueType(FValueType);

    SetValueTypeAllowed(FValueType);

    // size
    FPosSize:=FStoreDocument.StreamPosition;
    result:=result+FStoreDocument.ReadFixUInt32(uSize);
    FSizeData:=uSize;
    iSize:=uSize;

    // count
    FPosCount:=FStoreDocument.StreamPosition;
    result:=result+FStoreDocument.ReadFixUInt32(uCount);
    FCount:=uCount;

    // save values start posittion
    FPosValues:=FStoreDocument.StreamPosition;


    if (FValueType in cPYEKVSValueTypeOfArrayDynamicLen) then begin
        if (FPosList=nil) then
            FPosList:=TDictionary<Cardinal, Int64>.Create();

        // Encode StringUFT8 Positions (dynamic item size in the array)

        iIndex:=0;
        while iSize>0 do begin
            FPosList.Add(iIndex, FStoreDocument.StreamPosition);
            iSize:=iSize-FStoreDocument.EncodeVT(FValueType);
            Inc(iIndex);
        end;
        if (iSize<>0) then raise EPYEKVSException.Create('encode dynamic array failes by size mismatch');
        if (iIndex<>uCount) then raise EPYEKVSException.Create('encode dynamic array failes by count mismatch');

        result:=result+Integer(uSize);

    end else begin
        if (FPosList<>nil) then FreeAndNil(FPosList);

        // skip values
        result:=result+Integer(uSize);
        FStoreDocument.StreamPosition:=FStoreDocument.StreamPosition+uSize;
    end;
end;


function TPYEKVSArray.WriteHeader(const aValueType: TPYEKVSValueType): Integer;
begin
    // write list Header to Data
    FValueType:=aValueType;
    if not(FValueType in cPYEKVSValueTypeOfArray) then raise EPYEKVSException.CreateFmt('requested array ValueType %s not supported', [cPYEKVSValueTypeName[FValueType]]);


    result:=FStoreDocument.WriteValueType(pyeArray);
    FStoreDocument.WriteBranchSet(self); // change to new branch

    result:=result+FStoreDocument.WriteValueType(FValueType);

    FPosSize:=FStoreDocument.StreamPosition;
    result:=result+FStoreDocument.WriteFixUInt32(0); // Placeholder of Size-Value

    FPosCount:=FStoreDocument.StreamPosition;
    result:=result+FStoreDocument.WriteFixUInt32(0); // Placeholder of Count-Value

    FPosValues:=FStoreDocument.StreamPosition;
end;

procedure TPYEKVSArray.SetPutClosed();
begin
    // Update SizeData
    FStoreDocument.WriteFixUInt32(FSizeData, FPosSize);
    // Update Count
    FStoreDocument.WriteFixUInt32(FCount, FPosCount);

    inherited SetPutClosed;
end;

procedure TPYEKVSArray.GetIndexPosCalc(const aIndex: Cardinal);
begin
    if (aIndex>=FCount) then raise EPYEKVSException.CreateFmt('get array item at index %d is out of count %d', [aIndex, FCount]);
    FStoreDocument.StreamPosition:=FPosValues+cPYEKVSValueTypeSize[FValueType]*aIndex;
end;

procedure TPYEKVSArray.GetIndexPosList(const aIndex: Cardinal);
var iPos: Int64;
begin
    if FPosList=nil then raise EPYEKVSException.Create('array PosList not in use');
    if (aIndex>=FCount)OR(FPosList.TryGetValue(aIndex, iPos)=false) then raise EPYEKVSException.CreateFmt('get array item at index %d is out of count %d', [aIndex, FCount]);
    FStoreDocument.StreamPosition:=iPos;
end;

procedure TPYEKVSArray.PutInt(const aValue: Integer);
begin
    if not(ValueTypeOf(aValue) in FValueTypeAllowed) then raise EPYEKVSException.CreateFmt('array value with ValueType %s cannot filled', [cPYEKVSValueTypeName[ValueTypeOf(aValue)]]);
    FStoreDocument.WriteBranchSet(self);
    FSizeData:=FSizeData+FStoreDocument.WriteVTInt(aValue, FValueType);
    FCount:=FCount+1;
end;

function TPYEKVSArray.GetInt(const aIndex: Cardinal; const aDefault: Integer): Integer;
begin
    GetIndexPosCalc(aIndex);
    FStoreDocument.ReadVTInt32(result, FValueType, aDefault);
end;

procedure TPYEKVSArray.PutInt64(const aValue: Int64);
begin
    if not(ValueTypeOf(aValue) in FValueTypeAllowed) then raise EPYEKVSException.CreateFmt('array value with ValueType %s cannot filled', [cPYEKVSValueTypeName[ValueTypeOf(aValue)]]);
    FStoreDocument.WriteBranchSet(self);
    FSizeData:=FSizeData+FStoreDocument.WriteVTInt64(aValue, FValueType);
    FCount:=FCount+1;
end;

function TPYEKVSArray.GetInt64(const aIndex: Cardinal; const aDefault: Int64): Int64;
begin
    GetIndexPosCalc(aIndex);
    FStoreDocument.ReadVTInt64(result, FValueType, aDefault);
end;


procedure TPYEKVSArray.PutUInt(const aValue: UInt32);
begin
    if not(ValueTypeOf(aValue) in FValueTypeAllowed) then raise EPYEKVSException.CreateFmt('array value with ValueType %s cannot filled', [cPYEKVSValueTypeName[ValueTypeOf(aValue)]]);
    FStoreDocument.WriteBranchSet(self);
    FSizeData:=FSizeData+FStoreDocument.WriteVTUInt(aValue, FValueType);
    FCount:=FCount+1;
end;

function TPYEKVSArray.GetUInt(const aIndex: Cardinal; const aDefault: Cardinal=0): Cardinal;
begin
    GetIndexPosCalc(aIndex);
    FStoreDocument.ReadVTUInt32(result, FValueType, aDefault);
end;

procedure TPYEKVSArray.PutUInt64(const aValue: UInt64);
begin
    if not(ValueTypeOf(aValue) in FValueTypeAllowed) then raise EPYEKVSException.CreateFmt('array value with ValueType %s cannot filled', [cPYEKVSValueTypeName[ValueTypeOf(aValue)]]);
    FStoreDocument.WriteBranchSet(self);
    FSizeData:=FSizeData+FStoreDocument.WriteVTUInt64(aValue, FValueType);
    FCount:=FCount+1;
end;

function TPYEKVSArray.GetUInt64(const aIndex: Cardinal; const aDefault: UInt64=0): UInt64;
begin
    GetIndexPosCalc(aIndex);
    FStoreDocument.ReadVTUInt64(result, FValueType, aDefault);
end;


procedure TPYEKVSArray.PutSingle(const aValue: Single);
begin
    if not(pyeFloat32 in FValueTypeAllowed) then raise EPYEKVSException.CreateFmt('array value with ValueType %s cannot filled', [cPYEKVSValueTypeName[pyeFloat32]]);
    FStoreDocument.WriteBranchSet(self);
    FSizeData:=FSizeData+FStoreDocument.WriteVTSingle(aValue, FValueType);
    FCount:=FCount+1;
end;

function TPYEKVSArray.GetSingle(const aIndex: Cardinal; const aDefault: Single=0): Single;
begin
    GetIndexPosCalc(aIndex);
    FStoreDocument.ReadVTSingle(result, FValueType, aDefault);
end;

procedure TPYEKVSArray.PosListAdd(const aIndex: Cardinal; const aPos: Int64);
begin
    if (FPosList=nil) then FPosList:=TDictionary<Cardinal, Int64>.Create();
    FPosList.Add(aIndex, aPos);
end;

procedure TPYEKVSArray.PutDouble(const aValue: Double);
begin
    if not(pyeFloat64 in FValueTypeAllowed) then raise EPYEKVSException.CreateFmt('array value with ValueType %s cannot filled', [cPYEKVSValueTypeName[pyeFloat64]]);
    FStoreDocument.WriteBranchSet(self);
    FSizeData:=FSizeData+FStoreDocument.WriteVTDouble(aValue, FValueType);
    FCount:=FCount+1;
end;

function TPYEKVSArray.GetDouble(const aIndex: Cardinal; const aDefault: Double=0): Double;
begin
    GetIndexPosCalc(aIndex);
    FStoreDocument.ReadVTDouble(result, FValueType, aDefault);
end;


procedure TPYEKVSArray.PutString(const aValue: String);
var iVT        : TPYEKVSValueType;
    iStringUTF8: UTF8String;
begin
    iStringUTF8:=UTF8Encode(aValue);
    iVT:=ValueTypeOf(iStringUTF8);
    if not(iVT in FValueTypeAllowed) then raise EPYEKVSException.CreateFmt('array value with ValueType %s cannot filled', [cPYEKVSValueTypeName[iVT]]);
    FStoreDocument.WriteBranchSet(self);
    PosListAdd(FCount, FStoreDocument.StreamPosition);
    FSizeData:=FSizeData+FStoreDocument.WriteVTStringUTF8(iStringUTF8, FValueType);
    FCount:=FCount+1;
end;

function TPYEKVSArray.GetString(const aIndex: Cardinal; const aDefault: String=''): String;
begin
    GetIndexPosList(aIndex);
    result:=UTF8ToString(FStoreDocument.ReadVTStringUTF8(FValueType, UTF8Encode(aDefault)));
end;

procedure TPYEKVSArray.PutUTF8String(const aValue: UTF8String);
var iVT: TPYEKVSValueType;
begin
    iVT:=ValueTypeOf(aValue);
    if not(iVT in FValueTypeAllowed) then raise EPYEKVSException.CreateFmt('array value with ValueType %s cannot filled', [cPYEKVSValueTypeName[iVT]]);
    FStoreDocument.WriteBranchSet(self);
    PosListAdd(FCount, FStoreDocument.StreamPosition);
    FSizeData:=FSizeData+FStoreDocument.WriteVTStringUTF8(aValue, FValueType);
    FCount:=FCount+1;
end;

function TPYEKVSArray.GetUTF8String(const aIndex: Cardinal; const aDefault: UTF8String=''): UTF8String;
begin
    GetIndexPosList(aIndex);
    result:=FStoreDocument.ReadVTStringUTF8(FValueType, aDefault);
end;


procedure TPYEKVSArray.PutMemory(const aValue: TStream);
begin
    if not(pyeMemory in FValueTypeAllowed) then raise EPYEKVSException.CreateFmt('array value with ValueType %s cannot filled', [cPYEKVSValueTypeName[pyeMemory]]);
    FStoreDocument.WriteBranchSet(self);
    PosListAdd(FCount, FStoreDocument.StreamPosition);
    FSizeData:=FSizeData+FStoreDocument._WriteMemoryStream(aValue);
    FCount:=FCount+1;
end;

function TPYEKVSArray.GetMemory(const aIndex: Cardinal; aValue: TStream): Integer;
begin
    GetIndexPosList(aIndex);
    result:=FStoreDocument.ReadVTMemoryStream(FValueType, aValue);
end;

procedure TPYEKVSArray.PutMemory(const aBuffer: Pointer; aSize: Cardinal);
begin
    if not(pyeMemory in FValueTypeAllowed) then raise EPYEKVSException.CreateFmt('array value with ValueType %s cannot filled', [cPYEKVSValueTypeName[pyeMemory]]);
    FStoreDocument.WriteBranchSet(self);
    PosListAdd(FCount, FStoreDocument.StreamPosition);
    FSizeData:=FSizeData+FStoreDocument._WriteMemoryBuffer(aBuffer, aSize);
    FCount:=FCount+1;
end;

function TPYEKVSArray.GetMemory(const aIndex: Cardinal; const aBuffer: Pointer; aSize: Cardinal): Integer;
begin
    GetIndexPosList(aIndex);
    result:=FStoreDocument.ReadVTMemoryBuffer(FValueType, aBuffer, aSize);
end;


procedure TPYEKVSArray.ToStringsJSON(const aString: TStrings; const aIndenting: string; const aActIndenting: string; const aOutName: Boolean);
var sIndenting: string;
    S         : string;
    i         : Cardinal;
begin
    inherited;
    sIndenting:=aActIndenting+aIndenting;

    FStoreDocument.StreamPosition:=FPosValues;

    if (FCount>0) then begin // FCount is Cardinal; FCount-1 not allowed

        for i:=0 to FCount-1 do begin

            S:=sIndenting+FStoreDocument.ReadToString(FValueType);

            if i<FCount-1 then
                S:=S+Str_PYEKVSComma;

            aString.Add(S);
        end;
    end;
end;

procedure TPYEKVSArray.ToStringsSimple(const aString: TStrings; const aIndenting: string; const aActIndenting: string);
var sIndenting: string;
    i         : Cardinal;
begin
    inherited;
    sIndenting:=aActIndenting+aIndenting;

    FStoreDocument.StreamPosition:=FPosValues;
    if (FCount>0) then begin // FCount is Cardinal; FCount-1 not allowed
        for i:=0 to FCount-1 do begin
            aString.Add(sIndenting+FStoreDocument.ReadToString(FValueType));
        end;
    end;
end;

procedure TPYEKVSArray.ToOutputCSV(aCSVOutput: TPYEKVSCSVOutput);
var i : Cardinal;
    sKey: TPYEKVSKey;
begin
    inherited;

    FStoreDocument.StreamPosition:=FPosValues;
    if (FCount>0) then begin // FCount is Cardinal; FCount-1 not allowed
        for i:=0 to FCount-1 do begin
            //Every index of the array goes to a key in CSV
            sKey:=IntToPyeKVSKey(i);
            KeyAndParent(sKey);     //unique key name
            aCSVOutput.AddKV(aCSVOutput.RowCurrent, string(sKey), FStoreDocument.ReadToString(FValueType));
        end;
    end;
end;


function TPYEKVSArray.DetailsToStrings(): string;
begin
    result:=Str_PYEKVSArrayTypeOf+Str_PYEKVSSpace+cPYEKVSValueTypeName[FValueType]+Str_PYEKVSSpace+Str_PYEKVSArrayCount+Str_PYEKVSSpace+IntToStr(FCount);
end;


{ TPYEKVSArrayMap }

constructor TPYEKVSArrayMap.Create(const aStoreDocument: TPYEKVSDocument; const aStoreParent: TPYEKVSBranch; const aKey: TPYEKVSKey);
begin
    inherited;
    FPosList:=TDictionary<Cardinal, Int64>.Create();
end;

constructor TPYEKVSArrayMap.Create(const aStoreDocument: TPYEKVSDocument; const aStoreParent: TPYEKVSBranch; const aKey: TPYEKVSKey; const aValueTypeMap: TPYEKVSValueTypeMap);
begin
    Create(aStoreDocument, aStoreParent, aKey);
    FValueTypeMap:=aValueTypeMap;
end;

destructor TPYEKVSArrayMap.Destroy;
begin
    FPosList.Free();
    inherited;
end;

procedure TPYEKVSArrayMap.Clear;
begin
    FPosList.Clear();
    inherited;
end;

function TPYEKVSArrayMap.GetCount: Int64;
begin
    result:=FCount;
end;

function TPYEKVSArrayMap.ValueTypeMapEqual(const aMap: TPYEKVSValueTypeMap): Boolean;
var i, k: Integer;
begin
    // Compare aMap with internal ValueTypeMap; result=true if match
    k:=High(FValueTypeMap);
    if (High(aMap)<>k) then
        exit(false); // Length different
    for i:=0 to k do begin
        if (FValueTypeMap[i]<>aMap[i]) then
            exit(false); // ValueType different
    end;
    result:=true;
end;


function TPYEKVSArrayMap.Encode(): Integer;
var uSize : Cardinal;
    uCount: Cardinal;
    iIndex: Cardinal;
    iSize : Int64;
    MapLen: Word;
    iMap  : Integer;
begin
    // Encode keys and values after ReadFromStoreList

    result:=inherited Encode();

    // Read Map header ..

    // Read ValueTypeMap Length (2 Byte)
    result:=result+FStoreDocument._ReadUInt16(MapLen);


    // Write ValueTypeMap
    SetLength(FValueTypeMap, MapLen);

    for iMap:=0 to MapLen-1 do begin
        result:=result+FStoreDocument.ReadValueType(FValueTypeMap[iMap]);
    end;

    // size
    FPosSize:=FStoreDocument.StreamPosition;
    result:=result+FStoreDocument.ReadFixUInt32(uSize);
    FSizeData:=uSize;
    iSize:=uSize;

    // count
    FPosCount:=FStoreDocument.StreamPosition;
    result:=result+FStoreDocument.ReadFixUInt32(uCount);
    FCount:=uCount;

    // save values start posittion
    FPosValues:=FStoreDocument.StreamPosition;

    // Encode the item positions (maybe dynamic item size in the array)
    iIndex:=0;
    while iSize>0 do begin
        FPosList.Add(iIndex, FStoreDocument.StreamPosition);

        for iMap:=0 to High(FValueTypeMap) do begin
            iSize:=iSize-FStoreDocument.EncodeVT(FValueTypeMap[iMap]);
        end;

        Inc(iIndex);
    end;

    if (iSize<>0) then raise EPYEKVSException.Create('encode arraymap failes by size mismatch');
    if (iIndex<>uCount) then raise EPYEKVSException.Create('encode arraymap failes by count mismatch');

    result:=result+Integer(uSize);
end;


function TPYEKVSArrayMap.WriteHeader(): Integer;
var MapLen: Integer;
    i     : Integer;
begin
    // write list Header to Data

    MapLen:=Length(FValueTypeMap);

    if (MapLen>High(Word)) then
        raise EPYEKVSException.CreateFmt('ArrayMap length too long', []);


    result:=FStoreDocument.WriteValueType(pyeArrayMap);
    FStoreDocument.WriteBranchSet(self); // change to new branch

    // Write ValueTypeMap Length (2 Byte)
    result:=result+FStoreDocument._WriteUInt16(MapLen);

    // Write ValueTypeMap
    for i:=0 to MapLen-1 do begin
        if not(FValueTypeMap[i] in cPYEKVSValueTypeOfArray) then
            raise EPYEKVSException.CreateFmt('requested arraymap ValueType %s not supported', [cPYEKVSValueTypeName[FValueTypeMap[i]]]);

        result:=result+FStoreDocument.WriteValueType(FValueTypeMap[i]);

    end;

    FPosSize:=FStoreDocument.StreamPosition;
    result:=result+FStoreDocument.WriteFixUInt32(0); // Placeholder of Size-Value

    FPosCount:=FStoreDocument.StreamPosition;
    result:=result+FStoreDocument.WriteFixUInt32(0); // Placeholder of Count-Value

    FPosValues:=FStoreDocument.StreamPosition;
end;

procedure TPYEKVSArrayMap.SetPutClosed();
begin
    // Update SizeData
    FStoreDocument.WriteFixUInt32(FSizeData, FPosSize);
    // Update Count
    FStoreDocument.WriteFixUInt32(FCount, FPosCount);

    inherited SetPutClosed;
end;

procedure TPYEKVSArrayMap.GetIndexPosList(const aIndex: Cardinal);
var iPos: Int64;
begin
    if (aIndex>=FCount)OR(FPosList.TryGetValue(aIndex, iPos)=false) then raise EPYEKVSException.CreateFmt('get array item at index %d is out of count %d', [aIndex, FCount]);
    FStoreDocument.StreamPosition:=iPos;
end;

procedure TPYEKVSArrayMap.PosListAdd(const aIndex: Cardinal; const aPos: Int64);
begin
    FPosList.Add(aIndex, aPos);
end;

procedure TPYEKVSArrayMap.PutItem(const aValueMap: array of Variant);
var iMap: Integer;
begin
    if Length(aValueMap)<>Length(FValueTypeMap) then
        raise EPYEKVSException.CreateFmt('arraymap put failed because given values dont match with map', []);

    FStoreDocument.WriteBranchSet(self);
    PosListAdd(FCount, FStoreDocument.StreamPosition);

    for iMap:=0 to High(FValueTypeMap) do begin

        case FValueTypeMap[iMap] of
        pyeInt8: FSizeData:=FSizeData+FStoreDocument._WriteInt8(aValueMap[iMap]);
        pyeUInt8: FSizeData:=FSizeData+FStoreDocument._WriteUInt8(aValueMap[iMap]);
        pyeInt16: FSizeData:=FSizeData+FStoreDocument._WriteInt16(aValueMap[iMap]);
        pyeUInt16: FSizeData:=FSizeData+FStoreDocument._WriteUInt16(aValueMap[iMap]);
        pyeInt32: FSizeData:=FSizeData+FStoreDocument._WriteInt32(aValueMap[iMap]);
        pyeUInt32: FSizeData:=FSizeData+FStoreDocument._WriteUInt32(aValueMap[iMap]);
        pyeInt64: FSizeData:=FSizeData+FStoreDocument._WriteInt64(aValueMap[iMap]);
        pyeUInt64: FSizeData:=FSizeData+FStoreDocument._WriteUInt64(aValueMap[iMap]);
        pyeFloat32: FSizeData:=FSizeData+FStoreDocument._WriteFloat32(aValueMap[iMap]);
        pyeFloat64: FSizeData:=FSizeData+FStoreDocument._WriteFloat64(aValueMap[iMap]);
        pyeStringUTF8S: FSizeData:=FSizeData+FStoreDocument._WriteStringUTF8S(UTF8Encode(aValueMap[iMap]));
        pyeStringUTF8L: FSizeData:=FSizeData+FStoreDocument._WriteStringUTF8L(UTF8Encode(aValueMap[iMap]));
        end;

    end;

    FCount:=FCount+1;
end;

function TPYEKVSArrayMap.GetItem(const aIndex: Cardinal): TPYEKVSArrayMapValues;
var iMap: Integer;
begin
    GetIndexPosList(aIndex);

    // read Variant result array
    SetLength(result, Length(FValueTypeMap));

    for iMap:=0 to High(FValueTypeMap) do begin

        case FValueTypeMap[iMap] of
        pyeInt8: result[iMap]:=FStoreDocument.ReadInt8();
        pyeUInt8: result[iMap]:=FStoreDocument.ReadUInt8();
        pyeInt16: result[iMap]:=FStoreDocument.ReadInt16();
        pyeUInt16: result[iMap]:=FStoreDocument.ReadUInt16();
        pyeInt32: result[iMap]:=FStoreDocument.ReadInt32();
        pyeUInt32: result[iMap]:=FStoreDocument.ReadUInt32();
        pyeInt64: result[iMap]:=FStoreDocument.ReadInt64();
        pyeUInt64: result[iMap]:=FStoreDocument.ReadUInt64();
        pyeFloat32: result[iMap]:=FStoreDocument.ReadFloat32();
        pyeFloat64: result[iMap]:=FStoreDocument.ReadFloat64();
        pyeStringUTF8S: result[iMap]:=UTF8ToString(FStoreDocument.ReadStringUTF8S());
        pyeStringUTF8L: result[iMap]:=UTF8ToString(FStoreDocument.ReadStringUTF8L());
        end;

    end;

end;


procedure TPYEKVSArrayMap.ToStringsJSON(const aString: TStrings; const aIndenting: string; const aActIndenting: string; const aOutName: Boolean);
var sIndenting: string;
    S         : string;
    i         : Cardinal;
    iMap      : Integer;
begin
    inherited;
    sIndenting:=aActIndenting+aIndenting;

    FStoreDocument.StreamPosition:=FPosValues;

    if (FCount>0) then begin // FCount is Cardinal; FCount-1 not allowed
        for i:=0 to FCount-1 do begin

            S:='';
            for iMap:=0 to High(FValueTypeMap) do begin

                if (S<>'') then S:=S+Str_PYEKVSComma+Str_PYEKVSSpace;
                S:=S+FStoreDocument.ReadToString(FValueTypeMap[iMap]);

            end;

            if (i<FCount-1) then
                S:=S+Str_PYEKVSComma;

            aString.Add(sIndenting+S);
        end;
    end;
end;

procedure TPYEKVSArrayMap.ToStringsSimple(const aString: TStrings; const aIndenting: string; const aActIndenting: string);
var sIndenting: string;
    S         : string;
    i         : Cardinal;
    iMap      : Integer;
begin
    inherited;
    sIndenting:=aActIndenting+aIndenting;

    FStoreDocument.StreamPosition:=FPosValues;
    if (FCount>0) then begin // FCount is Cardinal; FCount-1 not allowed
        for i:=0 to FCount-1 do begin

            S:='';
            for iMap:=0 to High(FValueTypeMap) do begin

                if (S<>'') then S:=S+Str_PYEKVSComma+Str_PYEKVSSpace;
                S:=S+FStoreDocument.ReadToString(FValueTypeMap[iMap]);

            end;

            aString.Add(sIndenting+S);
        end;
    end;
end;

procedure TPYEKVSArrayMap.ToOutputCSV(aCSVOutput: TPYEKVSCSVOutput);
var i   : Cardinal;
    sKeyIdx: TPYEKVSKey;
    sKeyMap: TPYEKVSKey;
    iMap: Integer;
begin
    inherited;

    FStoreDocument.StreamPosition:=FPosValues;
    if (FCount>0) then begin // FCount is Cardinal; FCount-1 not allowed
        for i:=0 to FCount-1 do begin

            //Every index of the array goes to a key in CSV
            sKeyIdx:=IntToPyeKVSKey(i);

            for iMap:=0 to High(FValueTypeMap) do begin

                sKeyMap:=sKeyIdx+'.Map'+IntToPyeKVSKey(iMap);
                KeyAndParent(sKeyMap);     //unique key name

                aCSVOutput.AddKV(aCSVOutput.RowCurrent, string(sKeyMap), FStoreDocument.ReadToString(FValueTypeMap[iMap]));
            end;
        end;
    end;
end;



function TPYEKVSArrayMap.DetailsToStrings(): string;
var iMap: Integer;
    sMap: string;
begin
    sMap:='';
    for iMap:=0 to High(FValueTypeMap) do begin
        if (sMap<>'') then sMap:=sMap+Str_PYEKVSComma;
        sMap:=sMap+cPYEKVSValueTypeName[FValueTypeMap[iMap]];
    end;
    result:=Str_PYEKVSArrayTypeOf+Str_PYEKVSSpace+sMap+Str_PYEKVSSpace+Str_PYEKVSArrayCount+Str_PYEKVSSpace+IntToStr(FCount);
end;


{ TPYEKVSMemoryStream }

constructor TPYEKVSMemoryStream.Create(const aStoreDocument: TPYEKVSDocument; const aStoreParent: TPYEKVSBranch; const aKey: TPYEKVSKey);
begin
    inherited Create(aStoreDocument, aStoreParent, aKey);
    FValue:=TPYEKVSMemoryScope.Create(FStoreDocument.Stream);
end;

constructor TPYEKVSMemoryStream.Create(const aStoreDocument: TPYEKVSDocument; const aStoreParent: TPYEKVSBranch; const aKey: TPYEKVSKey; const aPos: Int64);
var uSize: Cardinal;
begin
    Create(aStoreDocument, aStoreParent, aKey);

    FStoreDocument.StreamPosition:=aPos;

    // Memory head
    FStoreDocument.ReadFixUInt32(uSize);

    // Pimp the Scope ...
    FValue.FStartPos:=FStoreDocument.StreamPosition;
    FValue.FCurrentPos:=0;
    FValue.FMaxSize:=uSize;
end;

destructor TPYEKVSMemoryStream.Destroy;
begin
    FValue.Free();
    inherited;
end;

function TPYEKVSMemoryStream.GetStream: TStream;
begin
    if (FValue.FStartPos=0) then
        raise EPYEKVSException.Create('TPYEKVSMemoryStream stream position failed');

    FValue.Position:=0;
    result:=FValue;
end;

procedure TPYEKVSMemoryStream.Clear;
begin
    inherited;
end;

procedure TPYEKVSMemoryStream.SetPutClosed;
begin
    // Update SizeData

    FSizeData:=FValue.Size; // add length of written bytes
    if (FSizeData<0) then
        raise EPYEKVSException.Create('TPYEKVSMemoryStream stream size failed');

    FStoreDocument.WriteFixUInt32(FSizeData, FPosSize);

    inherited SetPutClosed;
end;

function TPYEKVSMemoryStream.WriteHeader: Integer;
begin
    // write Memory Header to Data
    result:=FStoreDocument.WriteValueType(pyeMemory);
    FStoreDocument.WriteBranchSet(self); // change to new branch

    FPosSize:=FStoreDocument.StreamPosition;
    result:=result+FStoreDocument.WriteFixUInt32(0); // Placeholder of Size-Value; Update size after memory colleted: TPYEKVSMemoryStream.SetPutClosed

    // Pimp the Scope ...
    FValue.FStartPos:=FStoreDocument.StreamPosition;
    FValue.FCurrentPos:=0;
end;


{ TPYEKVSMemoryScope }

constructor TPYEKVSMemoryScope.Create(const aParentStream: TStream);
begin
    inherited Create;

    FParentStream:=aParentStream;
    FStartPos:=ParentStream.Position;
    FCurrentPos:=0;
    FMaxSize:=-1;
end;

function TPYEKVSMemoryScope.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
    case Origin of
    soBeginning:
        begin
            if (Offset<0)or((FMaxSize>=0)and(Offset>FMaxSize)) then
                result:=-1 // low and high bound check
            else
                result:=ParentStream.Seek(FStartPos+Offset, soBeginning)-FStartPos;
        end;
    soCurrent:
        begin
            if Offset=0 then
                result:=FCurrentPos // speeding the Position property up
            else if ((FCurrentPos+Offset)<0)or((FMaxSize>=0)and((FCurrentPos+Offset)>FMaxSize)) then
                result:=-1 // low and high bound check
            else
                result:=ParentStream.Seek(Offset, soCurrent)-FStartPos;
        end;
    soEnd:
        begin
            if (FMaxSize>=0) then
            begin
                if (Offset>0)or(FMaxSize<-Offset) then // low and high bound check
                    result:=-1
                else
                    result:=ParentStream.Seek(FStartPos+FMaxSize+Offset, soBeginning)-FStartPos;
            end else begin
                result:=ParentStream.Seek(Offset, soEnd)-FStartPos;
                if (result<>-1)and(result<0) then // low bound check
                begin
                    result:=-1;
                    ParentStream.Seek(FStartPos+FCurrentPos, soBeginning);
                end;
            end;
        end;
else
    result:=-1;
    end;
    if result<>-1 then
        FCurrentPos:=result;
end;

procedure TPYEKVSMemoryScope.SetSize(const NewSize: Int64);
var ScopedNewSize: Int64;
begin
    if (FMaxSize>=0)and(NewSize>=(FStartPos+FMaxSize)) then
        ScopedNewSize:=FMaxSize+FStartPos
    else
        ScopedNewSize:=NewSize;
    inherited SetSize(ScopedNewSize);
end;

function TPYEKVSMemoryScope.Read(var Buffer; Count: Longint): Longint;
begin
    if (MaxSize>=0)and((FCurrentPos+Count)>MaxSize) then
        Count:=MaxSize-FCurrentPos;

    if (Count>0)and Assigned(ParentStream) then begin
        result:=ParentStream.Read(Buffer, Count);
        Inc(FCurrentPos, result);
    end
    else
        result:=0;
end;

function TPYEKVSMemoryScope.Write(const Buffer; Count: Longint): Longint;
begin
    if (FMaxSize>=0)and((FCurrentPos+Count)>FMaxSize) then
        Count:=FMaxSize-FCurrentPos;

    if (Count>0)and Assigned(ParentStream) then begin
        result:=ParentStream.Write(Buffer, Count);
        Inc(FCurrentPos, result);
    end
    else
        result:=0;
end;


{ TPYEKVSValueTypeMap_Maker }

procedure TPYEKVSValueTypeMap_Maker.CountSet(const aCount: Word);
begin
    SetLength(Map, aCount);
    Index:=0;
end;

procedure TPYEKVSValueTypeMap_Maker.Add(const aVT: TPYEKVSValueType);
begin
    Map[Index]:=aVT;
    Index:=Index+1;
end;

class function TPYEKVSValueTypeMap_Maker.Map1(const aVT1: TPYEKVSValueType): TPYEKVSValueTypeMap;
begin
    SetLength(result, 1);
    result[0]:=aVT1;
end;

class function TPYEKVSValueTypeMap_Maker.Map2(const aVT1, aVT2: TPYEKVSValueType): TPYEKVSValueTypeMap;
begin
    SetLength(result, 2);
    result[0]:=aVT1;
    result[1]:=aVT2;
end;

class function TPYEKVSValueTypeMap_Maker.Map3(const aVT1, aVT2, aVT3: TPYEKVSValueType): TPYEKVSValueTypeMap;
begin
    SetLength(result, 3);
    result[0]:=aVT1;
    result[1]:=aVT2;
    result[2]:=aVT3;
end;

class function TPYEKVSValueTypeMap_Maker.Map4(const aVT1, aVT2, aVT3, aVT4: TPYEKVSValueType): TPYEKVSValueTypeMap;
begin
    SetLength(result, 4);
    result[0]:=aVT1;
    result[1]:=aVT2;
    result[2]:=aVT3;
    result[3]:=aVT4;
end;

class function TPYEKVSValueTypeMap_Maker.Map5(const aVT1, aVT2, aVT3, aVT4, aVT5: TPYEKVSValueType): TPYEKVSValueTypeMap;
begin
    SetLength(result, 5);
    result[0]:=aVT1;
    result[1]:=aVT2;
    result[2]:=aVT3;
    result[3]:=aVT4;
    result[4]:=aVT5;
end;

class function TPYEKVSValueTypeMap_Maker.Map6(const aVT1, aVT2, aVT3, aVT4, aVT5, aVT6: TPYEKVSValueType): TPYEKVSValueTypeMap;
begin
    SetLength(result, 6);
    result[0]:=aVT1;
    result[1]:=aVT2;
    result[2]:=aVT3;
    result[3]:=aVT4;
    result[4]:=aVT5;
    result[5]:=aVT6;
end;

class function TPYEKVSValueTypeMap_Maker.Map7(const aVT1, aVT2, aVT3, aVT4, aVT5, aVT6, aVT7: TPYEKVSValueType): TPYEKVSValueTypeMap;
begin
    SetLength(result, 7);
    result[0]:=aVT1;
    result[1]:=aVT2;
    result[2]:=aVT3;
    result[3]:=aVT4;
    result[4]:=aVT5;
    result[5]:=aVT6;
    result[6]:=aVT7;
end;

class function TPYEKVSValueTypeMap_Maker.Map8(const aVT1, aVT2, aVT3, aVT4, aVT5, aVT6, aVT7, aVT8: TPYEKVSValueType): TPYEKVSValueTypeMap;
begin
    SetLength(result, 8);
    result[0]:=aVT1;
    result[1]:=aVT2;
    result[2]:=aVT3;
    result[3]:=aVT4;
    result[4]:=aVT5;
    result[5]:=aVT6;
    result[6]:=aVT7;
    result[7]:=aVT8;
end;

class function TPYEKVSValueTypeMap_Maker.Map9(const aVT1, aVT2, aVT3, aVT4, aVT5, aVT6, aVT7, aVT8, aVT9: TPYEKVSValueType): TPYEKVSValueTypeMap;
begin
    SetLength(result, 9);
    result[0]:=aVT1;
    result[1]:=aVT2;
    result[2]:=aVT3;
    result[3]:=aVT4;
    result[4]:=aVT5;
    result[5]:=aVT6;
    result[6]:=aVT7;
    result[7]:=aVT8;
    result[8]:=aVT9;
end;

class function TPYEKVSValueTypeMap_Maker.Map10(const aVT1, aVT2, aVT3, aVT4, aVT5, aVT6, aVT7, aVT8, aVT9, aVT10: TPYEKVSValueType): TPYEKVSValueTypeMap;
begin
    SetLength(result, 10);
    result[0]:=aVT1;
    result[1]:=aVT2;
    result[2]:=aVT3;
    result[3]:=aVT4;
    result[4]:=aVT5;
    result[5]:=aVT6;
    result[6]:=aVT7;
    result[7]:=aVT8;
    result[8]:=aVT9;
    result[9]:=aVT10;
end;

initialization

InitValueType();

GFormatSettingsFloat:=FormatSettings;
GFormatSettingsFloat.ThousandSeparator:=#0;
GFormatSettingsFloat.DecimalSeparator:='.'

end.

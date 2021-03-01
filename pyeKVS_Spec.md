# pyeKVS format
pyeKVS is a Key Value Storage format.
The name pyeKVS initially stands only for a format description.  
The format is equivalent to a binary form of the [JSON](https://www.json.org) format.

## Features
- Reduction to necessary value types
- Storage of an object as a set of key and value
- Key representation as AnsiString
- Value representation as value value type + optional value header [e.g. size] + optional value data
- Value data are stored in binary representation
- Container objects are List, Array and ArrayMap
- Array objects without additional keys for the items
- ArrayMap is an optimal variant for arrays with a fixed structure, without having to store keys all the time. This is unique.
- Specification for a header of the entire document including format version and size
- Low effort for coding and decoding

## State
Version: 1.0  
Date: 01.03.2021  
License:  [MIT](http://opensource.org/licenses/MIT)  
[pyeKVS home](https://www.kxtec.de/projects/pyekvs/specification)  

# Format Specification

## pyeObject
An pyeObject here represents a storage item consisting of a key and a value.  
An pyeObject is stored in the following sequence.  
|pyeKey|pyeValue|
|--|--|

## pyeKey
The key should be passed as an AnsiString. There is no conversation to store the key in the stream. The key data are the AnsiChars of the string.
Definition|Length of key data|key data|
|--|--|--|
pyeKey|1 Byte |0..255 Bytes|

The key can be an empty string. Then there is no key data in the stream.

***Attention:*** All keys in a list should occur only once. The API that collects the values should check for this.

## pyeValue
The general representation of a value is showing by the following sequence.
|Definition|pyeValueTypes|value header|value data|
|--|--|--|--|
pyeValue|1 Byte |optionally|optionally

### pyeValueTypes
All values must be mapped into one of the following data formats.
The pyeValueType is coded with 1Byte.

|Name of value type|DEC value of value type|value header|value data|usage|
|--|--|--|--|--|
pyeUnknown|0|no|no|reserved for unknown types
pyeList|1|UInt32 Size + UInt32 Count|yes (if size>0)|see notes
pyeZero|2|no|no|null, nil, Int=0, Float=0, string=''
pyeBool|3|no|no|BOOL=true; Int=1
pyeInt8|4|no|1 Byte|little endian; -128 - 127
pyeUInt8|5|no|1 Byte|little endian;   0 - 255
pyeInt16|6|no|2 Byte|little endian; -32.768 - 32.767
pyeUInt16|7|no|2 Byte|little endian;   0 - 65.535
pyeInt32|8|no|4 Byte|little endian; -2.147.483.648 - 2.147.483.647
pyeUInt32|9|no|4 Byte|little endian;  0 - 4.294.967.295
pyeInt64|10|no|8 Byte|little endian; -9.223.372.036.854.775.808 -  9.223.372.036.854.775.807
pyeUInt64|11|no|8 Byte|little endian;  0 - 18.446.744.073.709.551.615
pyeInt128|12|no|16 Byte|little endian; -1,70141 E38 - 1,70141E38
pyeUInt128|13|no|16 Byte|little endian;  0 - 3,40282 E38
pyeFloat32|14|no|4 Byte|little endian; single
pyeFloat64|15|no|8 Byte|little endian; double
pyeFloat128|16|no|16 Byte|little endian; reserve
pyeStringUTF8S|17|UInt8 as char count|UTF8 chars|small; max. 255 chars
pyeStringUTF8L|18|UInt32 as char count|UTF8 chars|long; max. 4GB chars
pyeMemory|19|UInt32 size of mem|mem data bytes|see notes
pyeArray|20|pyeValueType + UInt32 Size + UInt32 Count|values|see notes
pyeArrayMap|21|UInt16 map length + map + UInt32 Size + UInt32 Count|values|see notes

### pyeList

The pyeList is a container for further values given by the pyeObject definition. A pyeList can also contain further sub-lists.
The pyeList value type has a fix value header with 4 bytes size in bytes and 4 bytes count of items. This information is strictly speaking duplicated. But it is useful in decoding the stream to perform an internal validation.
The size and the count represents only the value data information.
The list ends when the number of bytes given by the ‘size’ value have been read/writed.

Name|type|size in byte|usage
|--|--|--|--|
pyeList|pyeValueType|1|Value=1
Size|UInt32|4|Size of the list in bytes; start after header
Count|UInt32|4|count of items in the list
Item0|Object||
Item1|Object||
Item
|Object||

### pyeStringUTF8S

To encode a small string with maximum 255 chars. The value header has information about the string length. This length is an native UInt8 value. 

Name|type|size in byte|usage
|--|--|--|--|
pyeStringUTF8S|pyeValueType|1|Value=17
Length|UInt8|1|count of chars
CharUTF8|char|0..Length|chars

The API implementation must ensure that the string is encoded as UTF8.

### pyeStringUTF8L

To encode a long string with maximum 4GB chars. The value header has information about the string length. This length is an native UInt32 value. 

Name|type|size in byte|usage
|--|--|--|--|
pyeStringUTF8L|pyeValueType|1|Value=18
Length|UInt32|4|count of chars
CharUTF8|char|0..Length|chars

The API implementation must ensure that the string is encoded as UTF8.

### pyeMemory

To encode a memory block the value header has information about the length of the memory. This length is an native UInt32 value. 

Name|type|size in byte|usage
|--|--|--|--|
pyeMemory|pyeValueType|1|Value=19
Length|UInt32|4|size of memory
memory|Bytes|0..Length|values

### pyeArray

The pyeArray is a container for further values given by the pyeValue definition. The individual items do not have an individual key.
The array header starts with 1 byte for the value type information of the array items. Then follow 4 bytes size in bytes and 4 bytes count of items. This information is strictly speaking duplicated. But it is useful in decoding the stream to perform an internal validation.

Name|type|size in byte|usage
|--|--|--|--|
pyeArray|pyeValueType|1|Value=20
ArrayType|pyeValueType|1|value type of the items
Size|UInt32|4|Size of the array in bytes; start after header
Count|UInt32|4|count of items in the array
Item0|Value||
Item1|Value||
Item|Value||

The pyeArray can contain the following pyeValueTypes:
- pyeInt8
- pyeUInt8
- pyeInt16
- pyeUInt16
- pyeInt32
- pyeUInt32
- pyeInt64
- pyeUInt64
- pyeInt128
- pyeUInt128
- pyeFloat32
- pyeFloat64
- pyeFloat128
- pyeStringUTF8S (dynamic length!)
- pyeStringUTF8L (dynamic length!)
- pyeMemory (dynamic length!)

In value data the items of the array are stored consecutively.
The position of an array item in the stream is given by

StreamPos[array value data start] + Index * SizeOf(array value type)

However, this calculation only works for value types with constant value lengths. If the array stores values of type pyeStringUTF8S, pyeStringUTF8L or pyeMemory, the stream position of a value specified by an index is dynamic. It depends on the previous element sizes. The API must encode the complete array by parsing the stream.

### pyeArrayMap

The pyeArrayMap is a container to store items as a map. Map here describes a structure or record of different value types. The value types and their order are defined in the header of the pyeArrayMap object. A structure can contain a maximum of 65535 elements. All array items use the same map definition.

The header of an pyeArrayMap has a dynamic length, as it depends on the length of the map.

Name|type|size in byte|usage
|--|--|--|--|
pyeArrayMap|pyeValueType|1|Value=21
MapLength|UInt16|2|Length of the MapStruct; Number of elements of the structure; Max. 65535
MapStruct|pyeValueTypes|MapLength|pyeValueType and their order of the structure; 1 Byte per value type
Size|UInt32|4|sizeof value data of complete pyeArrayMap
Count|UInt32|4|count of items
Item0|structure||Item with structure of values
Item1|structure||Item with structure of values
Item|structure||Item with structure of values

The pyeArrayMap can contain the following pyeValueTypes:
- pyeInt8
- pyeUInt8
- pyeInt16
- pyeUInt16
- pyeInt32
- pyeUInt32
- pyeInt64
- pyeUInt64
- pyeInt128
- pyeUInt128
- pyeFloat32
- pyeFloat64
- pyeFloat128
- pyeStringUTF8S (dynamic length!)
- pyeStringUTF8L (dynamic length!)
- pyeMemory (dynamic length!)

In value data the items of the array are stored consecutively.
The position of an array item in the stream is given by

StreamPos[array value data start] + Index * SizeOf(MapStruct)

However, this calculation only works for value types with constant value lengths. If the MapStruct stores values of type pyeStringUTF8S, pyeStringUTF8L or pyeMemory, the stream position of a value specified by an index is dynamic. It depends on the previous element sizes. The API must encode the complete array by parsing the stream.


### Value conversation
The API implementation is free to convert the values into the shortest possible data format. 

Examples:
- Integer is a typical value in any programming language. In pyeKVS the storage would be as pyeInt32 with 1Byte value type + 4Byte value = 5Bytes in total. If the value is very small enough then it can be stored as pyeInt16 (3Bytes) or pyeUInt8 (2Bytes) or as pyeZero (1Byte) for example.
- Source pyeFloat64 (Double) Value can be stored as value type pyeFloat32, if no loss of accuracy or pyeZero if value==0.0.
- String value=” (empty) can be stored as pyeZero.

Conversely, when reading out the values, it must also be possible to return a pyeUInt8 value type as an Int32 value. It must be also possible to return a value type pyeZero as an empty string.


## Document
A document describes the encapsulation of a pyeKVS data set. Usually it is a data stream for a file or a socket.
The document starts with a fixed header definition (structure or record).

### Document header
Name|type|size in byte|usage
|--|--|--|--|
StreamPrefix|UInt32|4|constant: $53455950 (PYES)
StreamVersionH|UInt16|2|version high of pyeKVS protocol
StreamVersionL|UInt16|2|version low of pyeKVS protocol
StreamSize|UInt64|8|size of data after the StoreDataHeader; exluded header 

*StreamPrefix*
This constant value is just to identify the stream as a pyeKVS stream definition. It's also useful to find the start of a pyeKVS document in a data stream.
*StreamVersionH*
The StreamVersion high gives the version if the encoding protocol. Current number:1
*StreamVersionL*
The StreamVersion low gives the version if the encoding protocol.  Current number:0
*StreamSize*
The StreamSize gives the numbers of bytes for the complete document but excluded document header.

### Document data
The document start allways with an pyeList object, called root list. The Key of the root list is empty. The entry key length is 0.


## Appendix

### StreamPrefix
$53455950 (Longword)

### Root List
Key of root list: '' (empty AnsiString)

## Example
### Example 1: Root list with two values; Integer and String
```
{
    MyValue1 (pyeInt16) : 256
    MyString1 (pyeStringUTF8S) : Hello PYES.
}
```
```
stream data [HEX]       comment
50594553                StreamPrefix (PYES)
0100                    StreamVersionH (1)
0000                    StreamVersionL (0)
2D00000000000000        StreamSize (45 bytes)
00                      key length (0); no key for root list
01                      value type pyeList
23000000                size of list (35 bytes)
02000000                count of list (2 objects)
08                      key length (8 chars)
4D7956616C756531        key (MyValue1)
06                      value type pyeInt16
0001                    value (256)
09                      key length (9 chars)
4D79537472696E6731      key (MyString1)
11                      value type pyeStringUTF8S
0B                      string length (11 UTF8 chars)
48656C6C6F20505945532E  UTF8 string (Hello PYES.)
``` 

## Implementation recommendations
How to implement this format is generally not defined. 

The following procedure would be recommended:
### Create pyeKVS stream to store values

 1. Create pyeDocument
 2. Start of value recording
 3. Collect the values with different keys and value types; store direct in a stream (decoding on-the-fly)
 4. End the value recording; this can finish the stream and update the data sizes
 5. Use the pyeDocument to save the stream somewhere or transmit to a socket

### Receive a pyeKVS stream

 1. Create pyeDocument
 2. Read the pyeDocument header from a socket into the PyeKVS stream
 3. Validate the document header; version check, size check
 4. Continue reading until the specified size in the document header is complete
 5. Encode the pyeDocument; validation of the stream; this call can also capture and store all stream positions of an value (e.g. as Directory<Key, StreamPos>); consider dynamic data lengths
 6. Value request via key name; on-the-fly encoding: find the key in the directory and get the stream position; read the value from the stream
 7. Value request via count of a list or of an array; request all existing keys of a list


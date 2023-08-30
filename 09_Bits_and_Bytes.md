# Excursion Down to Bits and Bytes

This sheet goes a bit into the technical background of data types and
data objects. It may be helpful for a better understanding of how to
handle data in ABAP including a glance on casting and conversions.

After its declaration, a [data
object](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abendata_object_glosry.htm "Glossary Entry")
is usable in its context (procedure, class, program) according to its
type. For example, a numeric data object can be assigned the result of a
calculation:


> **💡 Note**<br>
> For checking out the code snippets in an SAP BTP ABAP environment, you can use the interface `if_oo_adt_classrun` in a class by implementing the method `if_oo_adt_classrun~main`.

``` abap
DATA num TYPE i.

num = 2 * 3 * 5 * 53 * 2267.

cl_demo_output=>display( num ).
```

After this assignment, the data object `num` contains the
calculated value 3604530, which is also displayed accordingly with a
type-compliant output as, for example,
`cl_demo_output=>display`. And of course the same can be seen
in the display of the variable in the ABAP Debugger when setting a
breakpoint at the last statement.

The ABAP Debugger also shows the hexadecimal value 32003700 of the data
object. This directly represents the binary value `0011 0010 0000 0000
0011 0111 0000 0000` stored in the 4 bytes allocated to the 4-byte
integer number in the memory. This value is platform dependent and for
numeric types is defined by the [byte
order](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenbyte_order_glosry.htm "Glossary Entry")
order, where either the most significant (big endian) or least
significant (little endian) byte is written to the first memory
location. The decimal value of the hexadecimal value `32003700` shown here
would be `838874880` and is not the integer value
`3604530` that ABAP deals with. This shows the meaning of
data types. A data object is a sequence of bytes stored in memory at its
address, which is interpreted by the ABAP runtime framework according to
the data type. The hexadecimal value is in most cases irrelevant to the
programmer.

Let us now consider a character-like field text with a length of two
characters:

``` abap
DATA text TYPE c LENGTH 2.

text = '2' && '7'.

cl_demo_output=>display( text ).
```
This field can be assigned the result of a string operation as shown and
the result shown by `cl_demo_output=>display` as well as in
the ABAP Debugger is the character string `27`. Again, it is
the data type, that derives the value `27` from the actual hexadecimal
content, which is `32003700` as in the previous example! In
this case, `32003700` is the encoding of the string
`27` in the Unicode character representation
[UCS-2](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenucs2_glosry.htm "Glossary Entry"),
which is supported by ABAP in Unicode systems. The Unicode character
representation also depends on the platform-dependent byte order.

Only for a byte-type data type does the value as interpreted by ABAP
directly correspond to the hexadecimal content. The following lines
modify the bits of a byte string with a bit-operation:

``` abap
DATA hex TYPE x LENGTH 4 VALUE 'CDFFC8FF'.

hex = BIT-NOT hex.

cl_demo_output=>display( hex ).
```

Here, the output with `cl_demo_output=>display`, the value
display of the ABAP Debugger as well as the hexadecimal value are the
same, namely `32003700`.

In the above examples, we presented three data objects that all occupy 4
bytes in memory that have the same binary values, but are handled
differently by ABAP due to their data type. The data type is also
responsible for the fact, that different kind of operations (numeric
calculation, string concatenation, bit-operation) can be applied to the
respective data objects. Using these examples we can have now look at
the basic concepts of
[casting](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abencast_casting_glosry.htm "Glossary Entry")
and [type
conversion](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abentype_conversion_glosry.htm "Glossary Entry")
and their relation to bits and bytes.

In ABAP, the term casting means nothing more than treating a data object
according to a different data type than the one that is permanently
assigned to it. This can be done using [field
symbols](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenfield_symbol_glosry.htm "Glossary Entry"),
with which a new (symbolic) name and a new type can be defined for the
memory area of a data object. When the memory area is accessed using a
field symbol, it is handled according to the type of the field symbol.
The following lines show an example.

``` abap
DATA hex TYPE x LENGTH 4 VALUE '32003700'.
FIELD-SYMBOLS: <num>  TYPE i,
               <text> TYPE c.
ASSIGN hex TO <num>  CASTING.
ASSIGN hex TO <text> CASTING.

cl_demo_output=>new(
  )->write_data( hex
  )->write_data( <num>
  )->write_data( <text> )->display( ).
```

The bit string in `hex` is cast to a numeric field when accessed
using the name `<num>` and to a text field when accessed using
the name `<text>`. The outputs are `32003700`,
`3604530` and `27`, clearly showing the effect of
the data type on handling one and the same hexadecimal content.

In contrast, in a type conversion (or conversion for short), the actual
binary content of a data object is converted so that it fits another
data type. Type conversions usually occur in assignments between data
objects of different data types. The goal of such a conversion is to
preserve the type-specific meaning of the content in the source field as
far as possible for the data type of the target field. For this purpose,
ABAP contains a large set of [conversion
rules](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenconversion_rules.htm).
A simple example is shown here:

``` abap
TYPES hex TYPE x LENGTH 4.
FIELD-SYMBOLS:
  <hex_text> TYPE hex,
  <hex_num>  TYPE hex.

DATA: text TYPE c LENGTH 2 VALUE '27',
      num  TYPE i.

num = text.

ASSIGN text TO <hex_text> CASTING.
ASSIGN num  TO <hex_num>  CASTING.

cl_demo_output=>new(
  )->write_data( text
  )->write_data( <hex_text>
  )->write_data( num
  )->write_data( <hex_num> )->display( ).
```

We are assigning the character-like field `text` to the numeric
field `num` and display the result that can also be checked in
the ABAP Debugger. The ABAP runtime framework recognizes that the
character string `27` in text can be interpreted as the
integer number `27`, generates the hexadecimal value 1B000000
in which this number is encoded for the numeric type of `num`,
and assigns it to the memory location of `num`. Thus, the actual
conversion takes place for the original hexadecimal content
`32003700` of `text` to the new hexadecimal content
`1B000000` of `num`. For character strings in text
fields, for which no such meaningful conversion is possible, an
exception occurs. The field symbols `<hex_text>` and
`<hex_num>` are used to show the hexadecimal content of the
fields `text` and `num` by casting them to a byte-like
type.

> **✔️ Hint**<br>
> For reasons of simplicity this sheet is restricted to named elementary
variables. Note that in particular the same holds for
[literals](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/index.htm?file=abenabap_literal_glosry.htm "Glossary Entry")
that are handled internally in such a way as if they were constants of
the data type assigned to the literal. In the preceding example,
`text` can be replaced by a literal `'27'` yielding
the same results.



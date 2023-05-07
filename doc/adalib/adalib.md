-----------------------------------------------------------------------------------------------
# ECLAT Implementation of the Ada Standard Library 

The Ada standard specifies an accompanying library, generally known as the _Ada standard library_, 
but it does not specify the implementation. 

The implementation of the standard library that ECLAT comes with is called the _ECLAT Ada 
Standard Library_, or just _AdaLib_. 

.....

Unlike other Ada implementations, and (strictly speaking) contrary to the requirements of the 
Ada standard, there is nothing special about the library units of the ECLAT ASL. They are made available by depending on a [stub library](../eclat/building.md#stublibs) named:

    adaos.adalib.stub

They are implemented by a corresponding full library that is configured a [module product]



..... _extension packages_ ......









-----------------------------------------------------------------------------------------------
## The Package `System`

With ECLAT, the package `System` is a normal package, but it is not considered a part of the 
ASL. Instead it is considered part of the [run time system](../rts/rts.md). 

However, units that are children of the 
package `System` are normal packages and are usually considered part of the ASL. 








-----------------------------------------------------------------------------------------------
## Standard Streams {#stdstrm}

.....

A [predefined stream](../adaos/streams.md) is one of the ten text streams that a
[compartment](../adaos/compart.md#strm) can normally expect to be already set up and
immediately usable. 

Two of these streams are for text input, and the other eight are for text output. 

 * standard output
 * standard input
 * standard error
 * standard printer
 * standard auxiliary output
 * standard auxiliary input
 * standard log message output
 * standard automation output
 * standard debugging output
 * standard system log message output

The first three are defined by the Ada standard, the remaining seven are AdaOS specific. 

.....

The extension package `Ada.Text_IO.Extended_Streams` contains declarations that facilitate use
of the extra predefined streams. 

.....





    procedure Set_Current_Printer (File : in File_Type);
    function Current_Printer return File_Type;
    function Current_Printer return File_Access;
    function Standard_Printer return File_Type;
    function Standard_Printer return File_Access;

These subprograms pertain to the standard printer. 

    procedure Set_ (File : in File_Type);
    function Standard_Auxiliary_Output return File_Type;
    function Standard_Auxiliary_Output return File_Access;
    function Standard_Auxiliary_Output return File_Type;
    function Standard_Auxiliary_Output return File_Access;

These subprograms pertain to the standard auxiliary output. 

    procedure Set_ (File : in File_Type);
    function Standard_Auxiliary_Input return File_Type;
    function Standard_Auxiliary_Input return File_Access;
    function Standard_Auxiliary_Input return File_Type;
    function Standard_Auxiliary_Input return File_Access;

These subprograms pertain to the standard auxiliary input. 

    procedure Set_ (File : in File_Type);
    function Standard_Log return File_Type;
    function Standard_Log return File_Access;
    function Standard_Log return File_Type;
    function Standard_Log return File_Access;

These subprograms pertain to the standard log message output. 

    procedure Set_ (File : in File_Type);
    function Standard_Automation return File_Type;
    function Standard_Automation return File_Access;
    function Standard_Automation return File_Type;
    function Standard_Automation return File_Access;

These subprograms pertain to the standard automation output. 

    procedure Set_ (File : in File_Type);
    function Standard_Debug return File_Type;
    function Standard_Debug return File_Access;
    function Standard_Debug return File_Type;
    function Standard_Debug return File_Access;

These subprograms pertain to the standard debugging output. 

    procedure Set_ (File : in File_Type);
    function Standard_System_Log return File_Type;
    function Standard_System_Log return File_Access;
    function Standard_System_Log return File_Type;
    function Standard_System_Log return File_Access;

These subprograms pertain to the standard system log message output. 

.....


-----------------------------------------------------------------------------------------------
## File I/O

.....

..... [](../objects/files.md) .....

In all of the Ada standard library I/O packages, there are procedures named `Create` and `Open` 
that have a `Name` parameter and a `Form` parameter.


### The Name Parameter

.....


### The Form Parameter

In all the `Open` and `Create` procedures, the `Form` parameter, of type `String`, can be used 
to qualify the required access to the file, but the recognised values of this parameter, and 
their meaning, is implementation defined.

The ECLAT ASL interprets the `Form` parameter in a unified manner. 

The value of the `Form` parameter is the name of a [SONG](#song). The SONG is a conceptual 
entity that provides all the information needed to customise access to a file. 

.....

The default value of the `Form` parameter is always the empty string. This value will always be
interpreted as specifying the SONG named `default`. 









-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 







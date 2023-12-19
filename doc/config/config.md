-----------------------------------------------------------------------------------------------
# Configuration

When a traditional (old-style) program is executed, it is typically given a set of inputs and
produces a set of outputs. It may read the inputs from a file and write its outputs into
another file. It may read its inputs from a database and write its outputs into the same
database. It may simply get its inputs from the user's keyboard and write its outputs to the
user's display. 

A [service program](../services/servprog.md), or a program of a similar ilk, may receive its
inputs as the (input) parameters of one of its service routines and return its outputs as the
output (parameters) of the routine. 

However, very often, there will be some kinds of extra input value that are likely to remain
unchanged most times the program is executed or one of its service routines called, but which
might need to be changed occasionally. 

Instead of having to add these extra values every time the program or one of its service
routines is invoked, it is more convenient for the program to have a separate mechanism to
obtain these extra values. 

These extra input values are termed _configuration values_ (in the ECLAT/AdaOS universe, at
least), and the mechanism for obtaining them is broadly termed _configuration_ of the program. 

ECLAT defines and uses a set of conventions and utilities that enable a program to get
configuration values. In fact, the conventions and utilities are designed in terms of
configuring [system objects](..objects/objects.md), rather than programs directly, but this is
only a minor difference to traditional configuration. Typically, a service program only has one
service, and so configuring the program is very nearly the same as configuring the service.

All of the software supplied with ECLAT, including ECLAT itself, use these conventions and
utilities, but they are very general-purpose, and could be used by your own programs as well. 



-----------------------------------------------------------------------------------------------
## Introduction

A [service](../services/services.md), or any other system object, persists its state in a
[saved state file](../objects/objects.md#state). 

It is assumed that, generally, configuring a service or other system object will be done by
updating its saved state file using a utility that is separate from the software implementing
the system object. 

A system object must be dormant to have its saved state changed externally like this; typically
this means that the program implementing the system object needs to be shut down. The saved
state file is updated, and then when the system object is next awakened (when the program is
restarted, or at some time after the program has been restarted) the updated saved state file
will be loaded.

In order to update a specific saved state file, a specialised
[saved state update procedure](#stup) can be used. 

Alternatively, a specialised [Allegra plugin](#plugins) adds commands that can be used to
interrogate and update a specific saved state file. 

.....



-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Saved State Files {#state}

The [saved state](../objects/objects.md#state) of a system object can be stored in a file,
whilst the object is dormant. This file is called a _saved state file_. 



.....



By convention, a saved state file for a service whose name is `S` has the name: 

    S.dat

The `dat` is short for 'data'. 

However, it is possible for a saved state file to pertain to a program, a group of services, or
some other entity, so this naming convention is not strict. 







-----------------------------------------------------------------------------------------------
## Saved State Update Files {#stuf}

A _saved state update file_ is an [XML][1] file used to specify values that are to update the
data stored in a specific [saved state file](#state).

.....


### File Name

By convention, a saved state update file, for a special configuration (mode) named `C`, is
named: 

    C,X-stup.xml

or for the main/default/normal/only configuration: 

    X-stup.xml

In both cases, `X` is:

 * `P` for a saved state update file that updates the state (the service or services of) a
   program named `P`; or

 * `P-L` for a saved state update file that updates the state of the library named `L` of a
   program named `P`. 

This naming convention is *only* a convention, but it is strongly recommended, to help avoid
confusion. Nevertheless, it is only a convention, and any file names can actually be used. 

The special configuration (mode) `C` can be anything, but it is typical for it to be one of the
[echelon](../adaos/envvars.md#ech) values (but in lower case), e.g.: 

    dev
    stg
    uat
    live

One might have the common configuration items in the main/default/normal file, and only those
items that differ in special configuration files. 


### File Format

Saved state update files are in the [XML][1] format, in a schema that is specific to the saved
state file being updated. 

.....


### File Location

Where the saved state update files are located depends on what they are configuring. The saved
state file is also located in the same directory as the corresponding update files. 

 * For all of the ECLAT libraries on your user (principal, on the AdaOS Native platform)
   account, there is a separate library configuration directory. All of the `eclat` library
   saved state update files, as well as the ECLAT service saved state file itself, are located
   in this directory. 

 * For a PXCR (Realizor) image, there is a special image configuration directory. All the
   `pxcr` image saved state update files, as well as the PXCR service saved state file itself,
   are located in this directory. 

 * For a program that is part of a [Kantan](../kantan/kantan.md) package, .....






..... [User-Specific Data, within Package Installation](../objects/paths.md#work) .....

This directory might be used, for example to keep [saved state update files](#stuf) which
configure a user's preferences for the program, such as which level of help the user requires
for the program. 




..... [User-Specific Data, within Package Installation and System](../objects/paths.md#local) .....

This directory might be used, for example to keep [saved state update files](#stuf) which
configure a user's preferences for the program that are specific to the computer system, such
as settings for how things are to be printed by the program onto a local printer. 





..... [Package Installation Data](../objects/paths.md#data) .....

This directory might be used, for example to keep [saved state update files](#stuf) which
configure settings that are specific to how the program is to operate for the installation
package, such as the name of an external web service that can be used to look up postal
addresses. 






-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Library-Specific Configuration {#libconfig}

A [library](../eclat/libraries.md) is intended to be a 'black box'. Whichever other libraries
use it (by depending on it) do not wish to know how it works internally. This goes for the
configuration data a library needs. 

Very often, a library will need to be able to obtain its own configuration data, without any
library that is depending on it wanting to, or being able to, know about it. 

All libraries that might be depended upon should adopt the convention of declaring a 



[limited controlled](?????) tagged type, 



and declaring a single object of that type. Both the type and the
object of the type will usually be private. The type is called the library's _configuration
type_ and the object is the library's _configuration object_. 

The type's procedures `Initialize` and `Finalize` (inherited from
`Ada.Finalization.Limited_Controlled`) should be overridden. 

The overriding of `Initialize` should read (or otherwise obtain) the library's configuration
data, so that those values are subsequently used by the library as appropriate. This procedure
will be called once during the initialisation of the library's configuration object. 

If the library might change the value of its configuration (saved state), then the overriding
of `Finalize` should save (or otherwise persist) the configuration data. 

In every case, the configuration data and its effects should be documented for users of the
library.

.....

Typically, a library will have its own separate 
[saved state file](../objects/objects.md#state), and the configuration type will hold the 
saved state. 

It should be possible to update this saved state file as described in this document. 

.....

It is likely to be very important that a library's configuration has sensible default values
for everything, so that the library is as likely as possible to function usefully in the
absence of any explicit configuration. Of course, there will be limits; in some circumstances
something will need to be explicitly configured in order to make the library function as
required. 

.....



Usually, a separate saved state update file will be used to update the parts of the saved state
file that are relevant to the library. The saved state file pertains to the program as a whole,
but it can be updated by multiple update files. Most programs will, in practice, be updated by
multiple update files. 


### Example

.....




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
## Saved State Update Programs {#stup}

A special [command-line tool](../tools/tools.md), called a _saved state update program_, can
read a particular schema of [saved state update file](#stuf) and use the data read from it to
update the corresponding data in a [saved state file](../objects/objects.md#state).

These programs may perform some validation of the data in the update file at the same time, and
can perform a variety of other related functions as well. 


The saved state update program for a service or object named `P` is named `P-stup.A.B` (or
`P-stup.A.B.exe` on Windows) where `A.B` is the [compatibility version](versions.md#comp) of
the saved state update program (*not* the program `P`).

For example, for the [stock service](?????) programs, `P` is replaced by one of:

 * `adaos-eclat` (for the ECLAT Service)

 * `adaos-pxcr` (for the Realizor Service)

 * `adaos-rts` (for the Run Time System) ?????service?

 * `adaos-tethys` (for the Tethys Service)

 * `adaos-kronos` (for the Kronos service)

.....


### Normal Arguments

Each normal argument must be the path name of a saved state update file, and may contain
wildcards.

Each update file will be read, in the order given, and used to update the saved state file.

.....

If the saved state file does not exist, then an empty (default) saved state file is created,
but the user is asked first to confirm unless the force option is specified.


### Force Option

If the saved state file does not exist, then an empty (default) saved state file is created,
but the user is asked first to confirm unless the `--force` (or `-f`) option is specified.


### Quiet Option

For every simple item in the saved state whose value is changed (to a different value), the
path of the item, its previous value, and its new value are output, unless the `--quiet` (or
`-q`) option is specified.


### Dump Option

The option `--dump` (or `-d`) must have parameter which gives the full path (including full
file name) of the file into which the state is to be [dumped](#dump). 

.....


### Consolidate Option

The option `--merge` (or `-m`) must have parameter which gives the full path (including full
file name) of the file into which the saved state update files, specified as normal arguments,
are to be [consolidated](#merge). 

.....



-----------------------------------------------------------------------------------------------
## State Dumping {#dump}

A [saved state update procedure](#stup) can perform essentially the reverse of its normal
function. 

If the `--dump` option is specified, the program will generate a saved state update (XML) file
containing all the data from a [saved state file](../objects/objects.md#state). 

.....








-----------------------------------------------------------------------------------------------
## Consolidation {#merge}

It is entirely possible that two or more [saved state update files](#stuf) will update the same
item of data in a [saved state file](../objects/objects.md#state). 

The effect of applying first one file and then the other (to the same saved state file) is
called _consolidation_, and will depend on: 

 * the defined [behaviour](?????) of the item; 

 * which file is processed first.

.....

A [saved state update procedure](#stup) can be made to report on this consolidation, and also to
generate a single new saved state update (XML) file from the consolidation of two (or more)
saved state update files. 

.....




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Allegra Plugins {#plugins}

A specialised [Allegra](../allegra/allegra.md) plugin adds commands that can be used to
interrogate and update a saved state file. 




```allegra




```










-----------------------------------------------------------------------------------------------
##



-----------------------------------------------------------------------------------------------
## Making a Saved State Update Program {#mkstup}

The `mkstup` [command-line tool](../tools/tools.md) aids in generating:

 * the Ada source text that compiles into a package that can be used to read and write a
   specific [saved state file](../objects/objects.md#state); 

 * the Ada source text that compiles into a corresponding stand-alone command-line tool; 

 * the Ada source text that compiles into a corresponding [saved state update
   procedure](#stup); 

 * the Ada source text that compiles into a corresponding [Allegra plugin](#plugins). 

The `mkstup` tool reads a [state specification file](#ssf) and generates the following Ada
source text files:

 * ;

 * ;

 * ;

 * ;

 * ;

 * ;

 * ;

 * ;

 * . 











-----------------------------------------------------------------------------------------------
## Format of a State Specification File {#ssf}

.....



The format of the state specification file is very simple. 

It is a plain text file, divided into lines (but not pages). It should:

 * use either LF (code 10) or CRLF (codes 13 and 10) to end each line; 

 * not have *any* other control characters in it (including HT, code 9); 
 
 * only use spaces for indentation. 

Trailing spaces in any line are ignored. 

Blank lines, and lines beginning with  `#` hash sign, are ignored.

Lines beginning with an `@` at-sign are _directives_. 

Lines ending with a `:` are _structural items_.

All other lines are _data items_. 

The first line of the file should be a directive with the word `mkstup` followed by a space and
a number. The number refers to the major version number of the `mkstup` tool, and is currently
just `1`. 

A _key_ is 

A structural item comprises:

 1. The key of the item; 

 2. Either a pair of parentheses (with nothing between them), or a pair of parentheses with a
    key between them; 

 3. 


```
@mkstup 1
ECLAT_Service_State <eclat>:
   Builds(Name) <build>:
      Name <name>: Unbounded_Wide_String
      Modules() <module>:
         Name <name>: Unbounded_Wide_String

```


```yaml
eclat:
   ada_name: ECLAT_Service_State
   ada_type: record
   builds:
      ada_name:
```







-----------------------------------------------------------------------------------------------
## Generated Saved State Types {#gen}

The data written into a stream in order to preserve the saved state of a system object is not 
set in stone. Different system objects can write out the data in any way that works. 

However, some recommendations are presented here, for guidance.

The `mkstup` [tool](#mkstup) .....


### Type Names

It is recommended that the name of a type which contains the saved state of a system object has
two numbers that represent the type's [compatibility version](../intro/versions.md#comp).  

For example:

```ada

type ACME_Weather_Predictor_State_2_7 is new Saved_State 
with
   record
      OID: Object_Id;
      Name: Unbounded_String;
      -- other components that store the state
   end record;
```

The compatibility version of this type is is 2.7 (major version 2, minor version 7). 


### Multi-Version Read-Back

As well as declaring the latest version of the saved type, some or all of the previous versions
should also be declared. 

When saving the state of a system object into a stream, the first thing that should be written
into the stream is the [external tag][2] of the saved state type. 

When reading a stream in order to load the saved state back into a system object, the external
tag should be read, and used to select which version of the saved state is being read. If the
saved state is a previous version, it should be read from the stream into an object of the
appropriate saved state type, and then an object of the latest version should be populated from
that previous version. 

For example, supposing we wanted to introduce a new version, 2.8.0.0, .....: 

```ada

type ACME_Weather_Predictor_State_2_8 is new Saved_State 
with
   record
      OID: Object_Id;
      Name: Wide_Unbounded_String;
      -- other components that store the state
   end record;

type ACME_Weather_Predictor_State_2_7 is new Saved_State 
with
   record
      OID: Object_Id;
      Name: Unbounded_String;
      -- other components that store the state
   end record;

function To_Version_2_8 (Old: in ACME_Weather_Predictor_State_2_7)
return 
   ACME_Weather_Predictor_State_2_8
is
   Result: ACME_Weather_Predictor_State_2_8;
begin
   Result.OID := Old.OID;
   Result.Name := To_Wide_Unbounded_String(To_String(Old.Name));
   -- copy or translate other components
   return Result;
end;
```

.....

```ada

procedure Load (State:  not null access ACME_Weather_Predictor_2_8;
                Stream: not null access Root_Stream_Type'Class) 
is
   Version: Compatibility_Version;
begin
   Version := Compatibility_Version'Input(Stream);
   case Version.Major
   is
      when 2 =>
         case Version.Minor
         is
            when 7 => 
               State := To_Version_2_8(ACME_Weather_Predictor_State_2_7'Input(Stream));
               return;
            when 8 => 
               State := ACME_Weather_Predictor_State_2_8'Input(Stream);
               return;
         end case;
   end case;
   raise Loading_Error with .....;
end;
```
















-----------------------------------------------------------------------------------------------
## State Specification Type Descriptor Files {#tdf}

An assumption is made by the `mkstup` [command-line tool](#mkstup) that any saved state can be
modelled as an Ada record which contains only components of types that are described by _type
descriptor files_. 

.....

A set of ready-made type descriptor files is supplied which cover a basic set of types: simple
values; other records; sequences; mappings; sets.

A simple value type is one of:

 * `Boolean`;

 * `Integer`;

 * `Float`;

 * `Wide_String`;

 * `Ada.Calendar.Time`;

 * `Duration`;

 * ``;

 * ``;

 * ``;

 * ``;

 * ``;

 * ``.

All the other types supported out-of-the-box are composite value types: records; sequences;
mappings; sets.

A record type is a plain Ada record type.

A sequence type is an indefinite vector, as implemented by an instantiation of the standard
container package `Ada.Containers.Indefinite_Vectors`. The index type is assumed to be
`Positive`. 

A mapping type is an indefinite map, as implemented by an instantiation of the standard
container package `Ada.Containers.Indefinite_Maps`. The key type is assumed to be
`Wide_String`. 

A set type is an ordered set of strings, as implemented by an instantiation of the standard
container package `Ada.Containers.Ordered_Sets`. The key type is assumed to be `Wide_String`. 

However, you can support further types in saved states by adding your own type description
files, and you are generally encouraged to do so. 







-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Example: Accounts

.....

The following .....


```xml
<accounts 
      xmlns="urn:adaos:ns:accounts.stup.1.0" 
      xmlns:db=" http://docbook.org/ns/docbook">

   <unit name="" ada-id="" uuid="">
      <label></label>
      <abbrev></abbrev>
   </unit>

   <principal-account name="" ada-id="" uuid="">
      <label></label>
      <abbrev></abbrev>
      <unit name=""/>
      <description>
         <db:para>
         </db:para>
      </description>
   </principal-account>

   ...

</accounts>
```




-----------------------------------------------------------------------------------------------
## Example: Top Compartment Model

```xml
<compartment-model
      xmlns="urn:adaos:ns:adaos.stup.1.0"
      domain="acme.geosciences.analab"
      principal="top"
      clock-offset="0d0h0m0s">

   <event-channels>
      <event-channel>

      </event-channel>
   </event-channels>

   <environment-variables>
      <environment-variable merge-priority="0">
         <name>LANG</name>
         <value>en-GB</value>
      </environment-variable>
      <environment-variable merge-priority="0">
         <name>DEFAUTH</name>
         <value>top</value>
      </environment-variable>
   </environment-variables>

   <services>
      <service name="" merge-priority="0">

      </service>
   </services>

   <programs>
      <program name="" merge-priority="0">
      </program>
   </programs>

   <service-program-controllers>
      <service-program-controller>
         
      </service-program-controller>
   </service-program-controllers>


</compartment-model>
```


-----------------------------------------------------------------------------------------------
## Example: Service

.....

```xml
<?xml version="1.0"?>
<service 
      xmlns="urn:adaos:ns:adaos.stup.1.0" 
      name="" 
      merge-priority="-3">


</service>
```

-----------------------------------------------------------------------------------------------
## Example: Program

.....

```xml




```



-----------------------------------------------------------------------------------------------
## Example: ECLAT Library

.....

The following .....

```xml
<eclat-service 
      xmlns="urn:adaos:ns:eclat.stup.1.0" 
      xmlns:db="">

   <file-type name="ada">
      <pattern>*.ada</pattern>
      <type>ada</type>
   </file-type>

   <source-root name="foobar-client">
      <path tree="true" type="ada">${ECLAT_SRC}/foobar/client</path>
      <control type="git"/>
   </source-root>

   <source-root name="foobar-ext1">
      <path tree="true" type="ada">${ECLAT_SRC}/foobar/ext1</path>
      <control type="git"/>
   </source-root>

   <source-root name="foobar-acme">
      <path tree="true" type="ada">${ECLAT_SRC}/acmecfg</path>
      <control type="git"/>
   </source-root>

   <library name="foobar-client" uuid="B1F36B1D-6AD9-4549-9F9B-37236CEA85EF">
      <source name="foobar-client"/>
      <source name="foobar-ext1"/>
      <source name="foobar-acme"/>

      <product name="default">



      </product>
   </library>

   ...
 
</eclat-service>
```







-----------------------------------------------------------------------------------------------
## Example: PXCR Image

.....

The following .....


```xml
<pxcr xmlns="urn:adaos:ns:pxcr.stup.1.0" xmlns:db="">

   <image name="" uuid="">






   </image>
   ...

</pxcr>
```




.....



-----------------------------------------------------------------------------------------------
## Example: State Specification File

.....

```xml
<saved-state xmlns="urn:adaos:ns:mkstup.1.0">



</saved-state>
```
.....







-----------------------------------------------------------------------------------------------
## Example: State Specification Type Descriptor File

.....





```xml
<saved-state xmlns="urn:adaos:ns:mkstup.1.0">
   <state-member-type type="simple">
      <name>Boolean</name>

      <ada-declaration><![CDATA[
{name}: Boolean;
      ]]></ada-declaration>





   </state-member-type>
</saved-state>
```

.....







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
## References

[1]: <https://www.w3.org/XML/> "Extensible Markup Language (XML)"

[2]: <http://www.ada-auth.org/standards/22aarm/html/AA-3-9.html#I2405> 
     "Annotated Ada Reference Manual (Ada 2022): 3.9 Tagged Types and Type Extensions"



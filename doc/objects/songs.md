-----------------------------------------------------------------------------------------------
# Specification of Name Garnering {#song}

AdaOS implements [file](../rts/files.md) I/O in a very flexible manner, based on
[services](../services/services.md) of a special kind, called General Resource Name Resolvers,
or GRNRs, pronounced [garnerers](../services/garnerers.md). 

The ECLAT implementations of the [Ada Standard Library](../adalib/adalib.md) and [C Standard
Library](../clib/clib.md) make use of this mechanism to allow files to be created and opened by
any of the available GRNR services. 

.....






In practice, a GRNR may merely provide a (quite thin) interface to a/the host platform 
(operating system) filesystem, but it could partially or fully implement a filesystem itself. 

Although the term 'filesystem' is used, its implementation could be anything that obeys the 
(meaning and protocol of the) interface. For example, an implementation might be a connection 
to a remote filesystem (protocol). 



A _specification of name garnering _, or _SONG_, .....

Incidentally, I apologise for acronyms such as 'GRNR' and 'SONG'. I appreciate that they are
annoying, but I think the alternatives are worse. I have at least tried to invent a friendlier
way to pronounce 'GRNR'---NJR

.....



There must always be a SONG defined with the name `default`. .....




-----------------------------------------------------------------------------------------------
## Modes {#mode}

The _mode_ of a song is one of: 

| `read`    | Read
| `write`   | Write 


Both modes can be specified for a SONG, in which case either mode can be used, .....

In the case of Write mode, there are two other aspects of the mode that can be specified: 

 * The _initial position_, which may be the beginning of the file or the end of the file; 

 * Whether the file is to be _auto-created_ or not. 

If the initial position is the beginning of the file, this means that the next stream element
to be read or written will be the first one in the file, at offset 0. 

If the initial position is the end of the file, this means that the next stream element to be
written will be one past the last one in the file, at offset `L` where `L` is the number of
elements in the file. If the file is empty, this will be the beginning of the file anyway. 

In read mode, the initial position is always the beginning of the file. 

If the file, as identified by the given `Name` (Ada) or `filename` (C) parameter, does not
exist, then:

 * if the mode is 'read' or auto-create is false, an error occurs (in Ada, the exception `Name_Error` is propagated, in C ?????); 
 
 * if the mode is 'write' and auto-create is true, an empty file with the given `Name` or
   `filename` will be created, which can then be written as normal. 

According to the programming language, the mode, initial position, and auto-create will be
_required_. 

In Ada, which mode is required depends on the `Mode` parameter (I appreciate that this is
confusing): 

| `Mode`          | Mode   | Initial position   | Auto-Create? |
| --------------- | ------ | ------------------ | ------------ |
| `Read_File`     | Read   | N/A                | No           |
| `Write_File`    | Write  | Beginning of file  | Yes          |
| `Append_File`   | Write  | End of file        | Yes          |

Therefore, in Ada, a SONG specified in the `Form` parameter may have multiple modes configured. 

In C, the mode, initial position, and auto-create are not required (by the `fopen` function),
but instead must be determined by the SONG. 

Therefore, in C, a SONG specified in the `mode` parameter (I appreciate that this is confusing)
must have exactly one mode configured. 

The mode of a SONG can be [configured](../intro/config.md) as follows:

```xml
<eclat ...>
   <adalib>
      ...
      <song name="S">
         <use-mode name="read"/>
         <use-mode name="write" position="end" auto-create="true"/>
         ...
      </song>
      ...
   </adalib>
</eclat>
```

where `S` is the name of the SONG. 

The `position` property must have the value `start` or `end` and the `auto-create` property
must have the value `false` or `true`. 



-----------------------------------------------------------------------------------------------
## Serialisation {#ser}

When reading a file, ...........


......... [](../services/text.md#ser) .......






The serialiser of a SONG can be [configured](../intro/config.md) as follows:

```xml
<eclat ...>
   <adalib>
      ...
      <song name="S">
         <use-serializer name="adaos.slzr.text.utf-8"/>
         ...
      </song>
      ...
   </adalib>
</eclat>
```

where `S` is the name of the SONG. 






-----------------------------------------------------------------------------------------------
## GRNR {#grnr}

A SONG specifies the [GRNR](../services/garnerers.md) to be used to resolve the path (name) 
given as an actual of a `Name` parameter. 

The GRNR of a SONG can be [configured](../intro/config.md) with the following:

```xml
<eclat ...>
   <adalib>
      ...
      <song name="S" mode="M">
         <use-grnr name="G">
            <use-scheme name="">
         </use-grnr>
      </song>
      ...
   </adalib>
</eclat>
```

where `S` is the name of the SONG, `M` is the mode, and `G` is the name of the GRNR (service) name. 


-----------------------------------------------------------------------------------------------
## Rules

.....



-----------------------------------------------------------------------------------------------
## Stock GRNRs

..... _stock garnerers_ .....

.....

| Name      | See                                                             |
| --------- | --------------------------------------------------------------- |
| `host`    | [Host Filesystem GRNR](../services/garnerers.md#host)           |
| `url`     | [Uniform Resource Location GRNR](../services/garnerers.md#url)  |


### Example

For example, if:

 * the SONG is named `url`
 
 * you want to use the GRNR named `adaos.grnr.url` 
 
 * you want to restrict the schemes that would be recognised for this SONG to just `file` and 
   `ftp`
 
 * you want names .....

 * you want the default SONG to have the GRNR named `adaos.grnr.host`. 
 
To [configure](../intro/config.md) this, you could use the following: 

```xml
<eclat ...>
   <adalib>
      ...
      <song name="url">
         <use-mode name="read"/>
         <use-mode name="write" position="end" auto-create="true"/>
         <use-serializer name="adaos.slzr.text.utf-8"/>
         <use-grnr name="adaos.grnr.url">
            <use-scheme name="file"/>
            <use-scheme name="ftp"/>
         </grnr>
      </song>
      <song name="default">
         <use-mode name="read"/>
         <use-mode name="write" position="end" auto-create="true"/>
         <use-serializer name="adaos.slzr.text.utf-8"/>
         <use-grnr name="adaos.grnr.host"/>
      </song>
   </adalib>
</eclat>
```


-----------------------------------------------------------------------------------------------
## 


    r w a r+ w+ a+ rb wb ab rb+ wb+ ab+ r+b w+b a+b 


```xml
<eclat ...>
   <adalib>
      ...
      <song name="r">
         <use-mode name="read"/>
         <use-serializer name="adaos.slzr.text.utf-8"/>
         <use-grnr name="adaos.grnr.host"/>
      </song>
      <song name="w">
         <use-mode mode="write" position="start" auto-create="false"/>
         <use-serializer name="adaos.slzr.text.utf-8"/>
         <use-grnr name="adaos.grnr.host"/>
      </song>
      .....
      ...
   </adalib>
</eclat>
```




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 


















????? URL GRNR:




### Advice

It is recommended to use the URL GRNR and the `file` scheme for local file names whenever
feasible, because these file names are uniform regardless of the host platform (operating
system) or filesystem. This is likely to help make your program more portable. 


.....







## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 





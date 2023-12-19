-----------------------------------------------------------------------------------------------
# General Resource Name Resolvers

A __General Resource Name Resolver__ (or __GRNR__, pronounced 'garnerer') is a kind of service
that can be given a [GRL](#grl) string, and will then return a 
[system object](../objects/objects.md) that represents a [general resource](#genrsc). 

.....

The four GRNRs currently supplied with ECLAT are:

| GRNR Service Name     | Kind of Path                                          |
| --------------------- | ----------------------------------------------------- |
| `AdaOS.GRNR.Native`   | [AdaOS named objects path](../rts/compart.md#namobj)       |
| `AdaOS.GRNR.Linux`    | [Linux system root path](?????)        |
| `AdaOS.GRNR.Windows`  | [Microsoft Windows local or UNC path](?????)        |
| `AdaOS.GRNR.URL`      | [universal resource locator](#url)   |

In the case of the Host Filesystem GRNR, the only kinds of system object returned are
[files](../rts/files.md). 



-----------------------------------------------------------------------------------------------
## General Resource Locators {#grl}

In AdaOS terminology, a _general resource locator_ (or _GRL_) is a string that can be used to
uniquely identify a [general resource](#genrsc). 





.....



-----------------------------------------------------------------------------------------------
## General Resources {#genrsc}

In AdaOS parlance, a _general resource_ is any [system object](../objects/objects.md) that can
be identified by a [GRL](#grl). 



.....



-----------------------------------------------------------------------------------------------
## Internet Resources {#url}

In Internet Protocol nomenclature, a _resource_ is a specific piece of data that can be fetched
(downloaded) from a network or internet using a protocol such as HTTP or FTP (or any of
numerous others). 

The data of a resource is called the _resource content_, and is usually assumed to be a
sequence of 8-bit bytes. The length of the sequence can be anything from 0 upwards (with no
specified limit). 

Various means exist for a specific resource to be identified. Broadly, strings called _Uniform
Resource Locators_ (or _URLs_) can be used to identify resources. The syntax of URLs is defined
in various [RFCs][3]. 

It needs to be noted that a URL is one kind of _Uniform Resource Indicator_, or _URI_. A URI is
an identifier, but it identifies other things as well as resources. Often the term URI is used
where URL might be expected, causing confusion sometimes, but where it is actually appropriate
to use the broader term URI. See the [URI RFC](https://www.rfc-editor.org/rfc/rfc3986). 

But returning to AdaOS parlance, a URL is one kind of [GRL](#grl). 

Correspondingly, an internet resource is considered to be one kind of [general
resource](#genrsc). It will also be represented as a [system object](../objects/objects.md),
and will typically be a file object that can be opened so that its data (the resource content)
can be read from it. 



.....



-----------------------------------------------------------------------------------------------
## Resource Seeking

A GRNR can, in fact, be given two or more GRLs. 

If it is, the GRNR attempts to fetch the resource from each GRL in turn; as soon as it 
succeeds, it does not try any further GRLs. If all GRLs fail, the fetch fails. 

.....



-----------------------------------------------------------------------------------------------
## AdaOS Native GRNR {#natgrnr}

The __AdaOS Native Resolver__ service is a General Resource Name Resolver that 

simply provides 
direct access to the [objects](../pxcr/targets#plat)  

The name of this service is: 

    AdaOS.GRNR.Native
    
.....



-----------------------------------------------------------------------------------------------
## Linux Filesystem GRNR {#lingrnr}

.....

    AdaOS.GRNR.Linux

.....



-----------------------------------------------------------------------------------------------
## Windows Filesystem GRNR {#wingrnr}

.....

    AdaOS.GRNR.Windows

.....



-----------------------------------------------------------------------------------------------
## Uniform Resource Location GRNR

The __Uniform Resource Location GRNR__ (or __URL GRNR__) service is a General Resource Name 
Resolver that can be given a _uniform resource locator_ (URL) and returns a [system 
object](../objects/objects.md) that represents a [general resource](#genrsc). 

The name of this service is: 

    AdaOS.GRNR.URL




????? Must or usually does?

The URL must resolve to a file (of a type derived from `AdaOS.Files.Binary_File`); if it is 
not, the exception `Ada.IO_Exceptions.Name_Error` is propagated. 

.....




### Plugins

The URL GRNR can be augmented by [plugins](../pxcr/plugins.md), of the plugin class: 

    AdaOS.GRNR.URL

.....


### URI Schemes (#schemes)

A __URI scheme__ is .....

..... _name_ .....

.....

Any URL GRNR plugin can _register_ one or more URI schemes, .....

.....



### Scheme Registration

.....





### IANA

.....

The __Internet Assigned Numbers Association__ (IANA) maintains a list of [URI schemes][1]. 
Listed against each scheme is its scheme identifier and references to further information about 
the scheme. 

It is intended that the service scheme names used are fully aligned with the IANA list, now and 
in the future.

IANA allows the private use of a scheme identifier not in the list, provided it begins with the 
two characters `x-` (lower case Roman X followed by hyphen). 


### Host Names

The resolver maintains a name of the computer on which it is running. 

?????network, multiple computers, multiple clients?





-----------------------------------------------------------------------------------------------
# 




-----------------------------------------------------------------------------------------------
## .....

.....

The value of the `Name` parameter either:

 * contains a scheme identifier; or 
 
 * begins with a scheme identifier, followed by the `:` colon character which is termed the 
   _scheme delimiter character_. 

The scheme identifier is used to select a service. 

The remainder of the name---the part after the scheme delimiter character, or an empty string 
if it has no scheme delimiter character---is passed to the resolver, as well as the scheme 
identifier, to help the resolver construct or select a general resource object. 



.....



-----------------------------------------------------------------------------------------------
## .....

.....

The _scheme delimiter character_, if there is one, is the first `:` colon character in the 
name. 

The name is considered *not* to have a scheme identifier if any of the following is true:

 * there is no scheme delimiter character in the name; 
 
 * the scheme delimiter character is the first character in the name; 
 
 * there is only one character before the scheme delimiter character. 

If the name has a scheme identifier, 

If the name does not have a scheme identifier:

 * on a [hosted platform](../rts/rts.md#hosted), the host filesystem is used to resolve the 
   name; 
 
 * on the AdaOS platform, an exception is propagated.
 
????? (Or should there be a default filesystem? Should the RTS be involved at all?)
 
.....




-----------------------------------------------------------------------------------------------
# The 'file' URI Scheme

The URI scheme with identifier `file` .....

...... [......][2]

See also: [The "file" URI Scheme](https://tools.ietf.org/html/rfc8089)

The associated service program is also named `adaos.files` and .....




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Filesystem Services

A _filesystem service_ is simply any service which registers one or more schemes with the [URL
GRNR](#url). 













-----------------------------------------------------------------------------------------------
# References

[1]: <https://www.iana.org/assignments/uri-schemes/uri-schemes.xhtml> "Uniform Resource 
     Identifier (URI) Schemes"

[2]: <https://datatracker.ietf.org/doc/html/rfc8089> "RFC 8089: The 'file' URI Scheme"

[3]: <https://www.ietf.org/standards/rfcs/> "RFCs"






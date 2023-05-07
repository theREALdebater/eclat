-----------------------------------------------------------------------------------------------
# NyLan

The Nyota Interface Definition Language (NyLan) is an Interface Definition Language (IDL)
suitable for defining service interfaces. 

NyLan is based on XML, with the schema ????? and the namespace `?????`. 

NyLan takes its inspiration from [OpenAPI][1], but it is based on XML (rather than JSON), and
is aimed at being agnostic in regard to programming language and also to the API mechanisms and
protocols (rather than specifically RESTful web API). 

......




-----------------------------------------------------------------------------------------------
## Interface Technologies (Protocols) {#prot}

Nyota is based on [remote procedure call]

supports multiple different interface technologies 








-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}








-----------------------------------------------------------------------------------------------
## Types

An item of data has a _type_, which defines the set of values that can be passed as that item.
For example, if the type were `traffic_light`, the possible values might be `red`, `amber`,
`green`, and `red_and_amber`. 

In NyLan, a type is identified by a name, and can be one of the following _kinds_: 

 * _mapped_
 
 * _structure_

 * _sequence_

 * _lookup_

 * _enumeration_

 * __

 * __

Elements that define both mapped types and structured types must be located within a `types`
element, which must be located within the root (`nylan`) element. It is permitted to have
multiple `types` elements within a document, but the names of all defined mapped and structured
types must all be distinct. 



### Programming Languages

Every `mapping` and `mapped-name` element must have a `proglang`
attribute which specifies the language. Currently, the only languages recognised are:

 * `json`

 * `ada`

 * `c`

A mapping for any other language will be ignored at the moment. 


### JSON

[JSON][2] (the JavaScript Object Notation) is considered an honorary programming language, even
though it is only a data language really (albeit derived from the JavaScript programming
language). 

For every `mapped`, `structure`, `lookup`, `enum`, `member` or `value` element, a `mapping` or
`mapped-name` element for JSON must be specified. 

This is because the RESTful web API .....


### Mapped Types

A `mapped` element is used to specify a mapped type to be used elsewhere in the document, and
the mappings to the corresponding types of whichever programming languages are to be supported. 

A set of example mapped types is given in the table below. This table's mappings are
suggestions: use them if you have no strong reason to use a different mapping (but ultimately
the choice is yours). 

| NyLan              | Ada                            | C                        |
| ------------------ | ------------------------------ | ------------------------ |
| `string`           | `Wide_Wide_Unbounded_String`   | `char[]`                 |
| `int32`            | `Interfaces.Integer_32`        | `int32_t`                |
| `int64`            | `Interfaces.Integer_64`        | `int64_t`                |
| `float`            | `Interfaces.IEEE_Float_32`     | `float`                  |
| `double`           | `Interfaces.IEEE_Float_64`     | `double`                 |
| `` | `` | `` |
| `` | `` | `` |
| `` | `` | `` |
| `` | `` | `` |
| `` | `` | `` |
| `` | `` | `` |
| `password`         | `` | `` |

However, note that all mapped must be explicitly defined; none is implicit. 

The type mappings provided are simply inserted into the generated source text directly, without
any checking by Nyota. If there is a mistake in any of the mapped types, only compilation (or
linting or similar) of the generated source text will find it. 

Ada types must all be a subtype name, and must be definite, because they will be used as the
type of a component in a record declaration. Ada imposes these rules. 

For C, the type mapping can be anything that is acceptable as the type of a field in a struct. 

The `type` element contains a `mapping` element for each different programming language for
which the type is to have a mapping. The content of the `mapping` element must be text and will
be the mapping of the type to that language. The `mapping` element must have a `proglang`
attribute. 


### Structure Types

A `structure` element is used to specify a structured type to be used elsewhere in the
document, and the members of the structure. For Ada, this will be a record type. For C, it will
be a struct. 

Each member of the structure .....









### Sequence Types





### Lookup Types







### Enumeration Types

An `enum` element is used to specify an enumeration type to be used elsewhere in the document,
and the defined values of the enumeration. 

The `enum` element must contain a `value` element defining each (permissible) value of the
enumeration. 

The content of the `value` element must contain a `mapped-name` element......




### 








### Example

As a very brief example:

```xml
      <types>

         <mapped name="int32">
            <mapping proglang="json">number</mapping>
            <mapping proglang="ada">Interfaces.C.Integer_32</mapping>
            <mapping proglang="c">int</mapping>
         </mapped>

         <structure name="">
            <mapped-name proglang=""></mapped-name>
            <mapped-name proglang=""></mapped-name>
            <member name="" type="">
               <mapped-name proglang="ada"></mapped-name>
               <mapped-name proglang="c"></mapped-name>
            </member>
            <member name="" type="">
               <mapped-name proglang="ada"></mapped-name>
               <mapped-name proglang="c"></mapped-name>
            </member>
         </structure>

         <lookup name="" type="">
            <key-function proglang="ada"></key-function>


         </lookup>

         <enum name="">
            <mapped-name proglang=""></mapped-name>
            <mapped-name proglang=""></mapped-name>
            <value name="">
               <mapped-name proglang=""></mapped-name>
               <mapped-name proglang=""></mapped-name>
            </value>
            <value name="">
               <mapped-name proglang=""></mapped-name>
               <mapped-name proglang=""></mapped-name>
            </value>
            <value name="">
               <mapped-name proglang=""></mapped-name>
               <mapped-name proglang=""></mapped-name>
            </value>
         </enum>



      </types>
```






-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Endpoints {#}

An _endpoint_ is a method, procedure, subroutine, etc. that is _invoked_ by the client and is
_implemented_ (actually executed) by the service (server). 

Within a service, all of its endpoints have a different name to distinguish them. 

The kind of name, and the rules applied to the naming, depend on the 









-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## References

[1]:
    <https://www.openapis.org/blog/2017/07/26/the-oai-announces-the-openapi-specification-3-0-0>
    "The OAI Announces the OpenAPI Specification 3.0.0"

[2]: <https://www.json.org/json-en.html> "Introducing JSON"

[3]: <> ""
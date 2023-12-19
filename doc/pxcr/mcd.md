-----------------------------------------------------------------------------------------------
# Module Class Definition Files

Everything that anyone may need to know about a module class must be encapsulated in a _module 
class definition_, or _MCD_, which is an XML file of a specific schema, .....

A module _conforms_ to an MCD if it fulfils everything that the MCD promises. 


.....





-----------------------------------------------------------------------------------------------
## .....




The schema ..... 




The elements and properties of a module class definition file are all in the namespace `?????` .....

The root element is `pxcr`, which .....


-----------------------------------------------------------------------------------------------
## Namespaces

Within this root element, a `namespace` element encloses other elements whose names will all be 
implicitly prefixed by the name of the namespace. 

```xml
   <namespace name="acme.fruit">
      <!-- other elements -->
   </namespace>
```

This kind of namespace is completely distinct from XML namespaces. 

This element has one mandatory property, `name`, which must be set to the name of the 
namespace. Within the namespace element, any `name` property whose value begins with a `.` dot 
will have the namespace's name implicitly prepended. 

Within a namespace:

 * a `module-class` element defines a module class; 
 
 * a `structure` element defines a named data structure; 

 * .....



-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
## .....

A module _conforms_ to an MCD if it fulfils everything that the MCD promises. 

This generally means that the module must contain all the elements declared in the MCD, and for
each subroutine every parameter has the same type as declared in the MCD. This is termed
_structural conformity_. 

However, conformity also requires that every element obeys the *meaning* of its definition in
the MCD. This is termed _semantic conformity_. 

If, for example, there is an element defined in the MCD that is a subroutine which is supposed
to output a number, then a corresponding element in the module which has exactly the right
parameters but which outputs some text instead means that the module fails semantic conformity,
even though it passes structural conformity. 

When ECLAT builds a module from a module library, it automatically checks that the module
conforms structurally to the MCD. It cannot check semantic conformity. 

In fact, if ECLAT cannot find the MCD, it automatically generates a skeletal MCD. 



-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
## Example {#example}

.....

```yaml
pxcr:
   module-classes:
      - 
         id: acme.fgs.gat
         exports:
            - 
               id: acme.fgs.geo.normalize
               .....
            - 
               id: acme.fgs.geo.constrain
               .....




```





The following example .....

```xml
<?xml version="1.0" encoding="UTF-8"?>
<pxcr
   xmlns="?????"
   xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
   xsi:schemaLocation="?????"
   xmlns:db="http://docbook.org/ns/docbook"
   xmlns:pxc="?????">
   
   <namespace name="acme.farming">
      <module-class 
         name="acme.fruit.management"
         uuid="E729-7551-984E-4A3E-A727-4477-DC91-568D">

         <description>
            <db:para>
               This module .....
            </db:para>
         </description>

         <inherit name=".general"/>

         <fulfils>
            <module-class name="" uuid=""/>
         </fulfils>

         <requirement>
            <module-class name="" uuid=""/>
            <module name="" uuid=""/>
         </requirement>

         <exports>
         
            <constant name=".number_of_bottles">
               <structure>string<structure/>
               <value>10<value/>
            </constant>
            
            <array-structure name=".ten_green_bottles">
               <lower-bound>1</lower-bound>
               <upper-bound>.number_of_bottles</upper-bound>
               <element-structure>.green_bottle</element-structure>
            </array-structure>

            <constant name="acme.maths.general.pi" initialized="true" 
               access="read-only" 
               structure="float64" 
            </constant>
            
            ?????<array-structure 
               name="acme.fruit.fruit-class.method-lookup"
               initialized="true" 
               access="read-only" 
               element-structure="method-lookup-table" 
            />

            ?????<data
               name="acme.fruit.fruit-class.methods"
               initialized="true" 
               access="read-only" 
               structure="acme.fruit.fruit-class.method-lookup" 
            />
            
            ?????<pointer-structure 
               name="acme.fruit.fruit-class.methods-pointer"
               target-structure="method-lookup-table"
               constraint="acme.fruit.fruit-class"
            />

            <subroutine
               name=".register_fruit"
               uuid="A42E-1C8A-347E-44F0-A369-3ECD-1ACD-716F"
               protocol="eclat-ada">
               <description>
                  <db:para>
                     Registers a fruit with the fruit registry. It is 
                     <db:emphasis>essential</db:emphasis> that the fruit is ripe enough to be 
                     eaten. 
                  </db:para>
               </description>
               <input 
                  name="Food"
                  uuid="1D11-A55B-F249-47AB-A60D-34D6-1A69-9BB3">
                  <description>
                     <db:para>                     
                        The item of food .....
                     </db:para>
                  </description>
                  <structure>
                     <pxc:pointer type="heap">
                        ?????
                     </pxc:pointer>
                  </structure>
               </input>
            </subroutine>

         </exports>

      </module-class>
   </namespace>
</pxcr>
```






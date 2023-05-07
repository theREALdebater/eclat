-----------------------------------------------------------------------------------------------
# Plugins

A _plugin_ is a [module](modules.md) that can be added to an [executable image](realizor.md#images) 
in order to provide optional additional functionality to other modules in the image. 

A plugin _extends_ one or more other modules, which are called the _base modules_ of the plugin. 

Note however the following situation is possible (and frequently occurs):

 * there are three modules, `A`, `B`, and `C`; 
 
 * modules `A` and `B` are both plugins; 
 
 * `A` extends `B`, so that `B` is a base module of `A`; 
 
 * `B` extends `C`, so that `C` is a base module of `B`. 
 
Of course, this transivity could involve four modules or more. 

.....

In general, multiple plugins can be added to an image, provided all the plugins .....

.....



-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## Base Module Classes {#bmc}

?????For a plugin (module) to extend a base module, the base module must fulfil a [module 
class](modules.md#class), which is called the _base module class_, and the plugin must 
require that class. 

?????For any 
plugin, its base module will fulfil a module class that the plugin will require. The module 
class will define what the plugin can do in respect of its base module. 

[Module classes](modules.md#classes) are not to be confused with [plugin classes](#classes). 

.....



-----------------------------------------------------------------------------------------------
## Plugin Classes {#class}

A _plugin class_ is an [ECLAT entity name](../intro/names.md). 

When a plugin is included in an [executable image](realizor.md#images), it is also associated 
with one or more plugin classes. 

Plugin classes are not to be confused with [module classes](modules.md#classes). 

.....

A plugin class represents what kind of functionality the plugin adds to a base module .....

.....





-----------------------------------------------------------------------------------------------
## Plugin Dependencies {#deps}

Plugins are not normally required by an executable image. A plugin is added to the image in 
order to provide *additional* functionality; if that functionality is not required, the plugin 
can be omitted. However, plugins have their own dependencies which may dictate that if a 
certain plugin is added, another plugin (which it depends on) is also required, and should be 
initialised before it. 

Plugin dependencies are not to be confused with [module dependencies](modules.md#deps). For any 
plugin, its base module will fulfil a module class that the plugin will require. The module 
class will define what the plugin can do in respect of its base module. 

.....

Plugin classes enable the Realizor to select which plugins should be added to an image, and to 
add the calls to the initialisation procedures of the added plugins in an order that accords 
with the dependencies between them. 




Each plugin class can have a dependency on one or more other plugin classes .....



When the Realizor has discovered a set of [eligible plugins](?????), it then sets about 
determining: 

 1. which plugins to include and which to _reject_; 
 
 2. of the included plugins, for each base module, the order in which its plugins should be 
    initialised. 

The Realizor uses the configured associations of plugin classes with plugins and base modules 
to determine the result of (1), and the configured dependencies between the plugin classes to 
determine (2). 


.....




-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## Plugin Configuration

The Realizor must be told that a module is a plugin, and given details. 

.....

?????If ECLAT is being used, a plugin library should be built in [plugin 
mode](../eclat/building.md#modes). 

If ECLAT is being used, a plugin library should be configured to require the appropriate 
[module class](#modules.md#classes). Normally, this module class will be fulfilled by the 
plugin's base module. 



Commands of the [`pxcr`](../tools/pxcr.md) tool can be used to configure the ..... 

The current image can be set to `X` by the command:

    image X
    
The modules to be included in the current image as plugins in a particular plugin class `C` can 
be configured by executing the command: 

    plugin C search P

where `P` is a directory 
path. The directory will be searched by the Realizor for modules (files ending with `.pxc`). 


The value of this item must be set to a [file path pattern list](?????); 
module files matching any of the path patterns will be included in the image as plugins in the 
plugin class `C`. The default is an empty list, meaning that no plugin modules will be added 
implicitly. 



The base module for all the plugins of a specific plugin class 

?????The base modules for all the plugins in a particular plugin class can be configured by setting 
the following state update item: 

?????    pxcr/images(X)/plugins(C)/base_modules

?????where `X` is the name of the image and `C` is the name of the plugin class. 

?????The value must a list of module names, separated by a `|` vertical bar character. In practice, 
it will be unusual for one plugin class to have more than one base module, but it is perfectly 
possible. 





?????dependencies






?????The name of each plugin will be the name of the module. 

?????is the name embedded in the module file?

.....



-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## Automated Testing

Plugins facilitate a very powerful technique for the automated testing of software. 

The idea is that all the plugins of a particular [base module class](#bmc), possibly confined
to a [plugin class](#class) or a set of plugin classes, can be subjected to a battery of tests
by a special _test base module_ that is of the same base module class. 

Instead of, or in addition to, [configuring](?????) the normal _production base module_, the test base module is
configured. Whichever matching plugins end up being included in the image will be
automatically tested. 

.....



-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## 



-----------------------------------------------------------------------------------------------
## Example

For each [base module class](#bmc), each base module fulfilling that class must provide some 
means by which each plugin of that class can, in some way, register itself or something about 
itself with the base. The nature of this registration will depend on what plugins in that 
class are meant to do, and will form a part (or all) of the definition of the base module 
class. 

The base module class for the plugin in this example will be `acme.farming.fruit`. Plugins 
requiring this module class are intended to implement one or more fruits, which have the 
ability to be eaten. To this end, an interface named `Fruit` is declared, and an operation for 
it, named `Eat`, causing the fruit to be eaten. Each different kind of fruit will be derived 
from `Fruit` and will need to provide its own implementation of `Eat`. 


### Common Library

First, we create a _common library_ containing just the following library-level package:

```ada
package Fructacious 
   with 
      Pure;
is
   type Fruit is interface 
      with 
         Preelaborable_Initialization;

   procedure Eat (Food: in out Fruit) is abstract;
end;
```

It is vital that the package is declared pure, and that the `Fruit` interface is declared 
`Preelaborable_Initialization`. 

The common library is never built, but is only ever used as a foundation for other libraries. 


### Base Module

Then we create a library for the base module 

?????of the plugin class `acme.farming.fruit`. 

This 
library will depend on the common library, and it will include the following package:

```ada
with Ada.Objects;
with Fructacious;

package Fruit_Registration
with 
   Remote_Types
is
   procedure Register_Fruit (Food: not null access all Fructacious.Fruit'Class)
   with 
      Export, 
      External_Name => "acme.farming.register-fruit";      
end;
```

We'll leave the corresponding package body to the imagination, but it will involve making a 
copy of the fruit access value passed in the parameter `Food` into the `Register_Fruit` 
procedure, and at some point calling the `Eat` procedure for some or all of the registered 
fruits. 

?????We will configure a build for the base module, but in general there are many possibilities: the 
base module may be an assembly, in which case we would probably build it in `assembly` 
mode; the base module might, on the other hand, itself be a plugin (probably of a different 
plugin class), in which case, of course, it would be built in `plugin` mode; the base module 
might even be the start module of the image. 


### Plugin Libraries

Finally, we create a library for each plugin. Each plugin library must also depend on the 
common library, and might include a package such as the following (a plugin for plums): 

```ada
with Fructacious;

package Prunae
is
   type Plum is new Fructacious.Fruit with private;
   
   procedure Eat (Food: in out Plum);

private
   type Plum is new Fructacious.Fruit 
   with
      record
         -- whatever is inside a plum
      end record;
end;

package body Prunae
is
   procedure Register_Fruit (Food: not null access all Fructacious.Fruit'Class)
   with 
      Import, 
      External_Name => "acme.farming.register_fruit";
   
   procedure Eat (Food: access Plum)
   is 
   begin
      -- eat the plum
   end;
   
   Pie: array (1..15) of aliased Plum;

begin
   for P of Pie loop
      Register_Fruit (P'Access);
   end loop;
end;
```

We will configure a build for each plugin. Naturally, the mode of these builds will be 
`plugin`. 


### Configuration

.....

We imagine three libraries, named:

    acme.fruit.common
    acme.fruit.management
    acme.fruit.prunae
    
The `.common` library contains the package `Fructacious`. The `.management` library contains 
the package `Fruit_Registration`. The `.prunae` library contains the package `Prunae`. 

Both the management and the prunae libraries depend on the common library:  

?????    eclat/library(acme.fruit.management.lib)/dependencies(acme.fruit.common.lib)/versions=1.0
?????    eclat/library(acme.fruit.prunae.lib)/dependencies(acme.fruit.common.lib)/versions=1.0
    
    eclat lib acme.fruit.management
    eclat dep acme.fruit.common ver 1.0
    
    eclat lib acme.fruit.prunae
    eclat dep acme.fruit.common ver 1.0

For this reason, the common library must be [wrapped](../eclat/libraries.md), and we are 
assuming that its [compatibility version](../intro/versions.md) is 1.0. 

For the management and prunae libraries, there is a build of the matching name:

    eclat/builds(acme.fruit.management.bld)/mode=plugin
    eclat/builds(acme.fruit.management.bld)/modules(acme.fruit.management.mod)/file=c:\fruit\mgmt.pxc

    eclat/builds(acme.fruit.prunae.bld)/mode=plugin
    eclat/builds(acme.fruit.prunae.bld)/modules(acme.fruit.prunae.mod)/file=c:\fruit\plugins\prunae.pxc
    
As previously noted, the common library is only to be depended on, and has no builds. 

.....

    pxcr/images(acme.farm.exe)/file=c:\fruit\bin\farm.exe

.....

    pxcr/images(acme.farm.exe)/plugins(acme.farming.fruit.plg)/search_paths=c:\fruit\plugins\*.pxc
    
.....

    pxcr/images(acme.farm.exe)/plugins(acme.farming.fruit.plg)/base_modules=acme.fruit.management.mod

.....


### Summary

The essence of this approach is the export and import of the `Register_Food` procedure, and the 
abstract type `Fruit` that can be safely shared by the host and all the plugins. 



-----------------------------------------------------------------------------------------------
## 






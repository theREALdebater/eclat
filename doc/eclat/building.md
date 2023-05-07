-----------------------------------------------------------------------------------------------
# Building

One of the fundamental functions of ECLAT is _building_ something

from a [library](libraries.md)

.....




-----------------------------------------------------------------------------------------------
## Products {#prod}

..... _product_, that gives the information needed to enable ECLAT to build .....

There are three types of product:

 * [module products](#modprod)

 * [plugin products](plugprod)

 * [program products](progprod)

Each different product is identified by a _product name_, which is a [ECLAT Entity
Name](../intro/names.md) .....






-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
## Module Products {#modprod}

The purpose of a _module product_ is to generate one or more [modules](../pxcr/modules.md)
which have [exports](../pxcr/modules.md#elem). The exports are declared in the source text.

A module product has no assemblies, and no main procedure.

Each module being generated includes all the PXC code corresponding to all the subroutines
explicitly exported by that module, together with all the code corresponding to the downward
closure of the subprograms called by those subroutines, plus all the data explicitly exported.

......





### Module Class Checking

Each generated module is checked against the [module class definition](../pxcr/mcd.md) file,
for every module class that the module is in. Every export in the module must match a
corresponding export definition in the MCD, and there must be no remaining unmatched exports
in the MCD.

.....





### Initialisation

For any module being generated, there will be a set of library-level non-generic packages with
initialisation code.

?????in which at least
one subprogram is included in the set of all the subroutines explicitly exported by that
module, together with the downward closure of the subprograms called by those subroutines.

All of the initialisation logic---the sequence of statements in the initialisation part of
all those package's bodies, in an order that accords with the dependencies between the
packages---is gathered into a _module initialisation subroutine_ which is implicitly exported
under the name:

    M.init

where `M` is the name of the module.

Nothing automatically calls any of these initialisation subroutines; something somewhere will
need to call them at the appropriate time.

......


### Stub Libraries {#stublibs}

?????The `eclat` [command-line tool](../tools/eclat.md) has a command `create-stub-library` (or
`mkstub`) which generates the Ada and C++ source text for a _stub library_ of a module of a
module product.

A stub library contains declarations which import suitable entities for all of the exports of
the module.

This source text can be used as a way for other Ada and C++ libraries to import and use the
module's exports.

?????The generated Ada stub library contains just one package, a non-generic, library-level
package, which contains all the declarations of constants, variables, types, and subprograms
necessary to meaningfully import all of the module's exports.

?????The generated C++ stub library contains one file. In this, there is a namespace which contains
all the declarations of constants, variables, typedefs, and functions necessary to meaningfully
import all of the module's exports.

?????The names of the library, namespace, and package can be configured, but by default they are
derived from the module's name (each `-` hyphen is converted into an `_` underscore).

?????It will be usual for a stub library, having been generated, to have hand-made modifications
which add comments and generally change the library into something good for someone wanting to
use the module.

.......


### Configuration

To configure building a module product, .....

For example:



```xml
<?xml version="1.0"?>
<eclat-state-update>
   <product>
      <name> gat </name>
      <module>
         <name> acme.fgs.gat </name>
         <version> 1.7.0 </version>
         <class> acme.fgs.gat </class>
      </module>
   </product>
</eclat-state-update>
```

You might place this file in the root directory of the source text for this library, and you
would run the .....


```sh
eclat-stup

```




In this example, the module has the name `acme.fgs.gat` (which, in the fictional universe of
our examples, stands for a company named Acme, Faculty of Geological Sciences, Geological
Analysis Tools).

.....

The version number for the module, when it is built, is explicitly given as 1.7.0. It is not
normal for the version number to be specified like this. Instead, it is normally omitted, and
then the version number of the module will be automatically taken from that of the library. 

?????If
the version has been explicitly set, the command `modver -a` (or `module-version --auto`)
will set it back to automatic.

The module is set to belong to one [module class](../pxcr/modules.md#classes), also named
`acme.fgs.gat`. 

?????If the module belongs to just one module class, whose ID is the same as the
module ID, then the class doesn't need to be explicitly set.

It can be imagined there is a corresponding [module class definition](../pxcr/mcd.md#example).

?????Finally, the module is set to contain all the exports (in the CWL) whose names begin with
`acme.fgs.geo.` Note the use of the `*` wildcard. An asterisk must always be next to a `.`
dot: there must be a dot to its left unless the asterisk is at the beginning of the name;
there must be a dot to its right unless the asterisk is at the end of the name.











?????

### Initialisation Configuration

All the units which are needed---as explained in [10.2 Program Execution][1] of the Ada
Reference Manual---by at least one of the exports in a module are automatically included in
the module, in no specified order except that no unit will come before another unit that needs
it.

A list of units to be included can be explicitly set using the `module-units` (or `modunits`)
command. For example:

```sh
module-units Geology.Monitoring Geology.Utilities Printer.Control
```

```sh
modunit Geology.Monitoring Geology.Utilities Printer.Control
```

In this example, the three packages:

 1. `Geology.Monitoring`

 2. `Geology.Utilities`

 3. `Printer.Control`

are included. The order in which they are specified will be the order in which their
initialisation code is executed. It is an error if any explicit unit needs a unit that comes
after it in the list.

If there is any unit that is needed by an export but is not included in the explicit list, it
is included automatically.

The units should all be non-generic library-level packages with initialisation code; any others
are (harmlessly) ignored.

?????The explicitly named library units, together with the downward closure of the library units
they need---as explained in [10.2 Program Execution][1] of the Ada Reference Manual---are
the library units associated with the module. The module's [module initialisation procedure](?????) will
contain code that corresponds to the initialisation code of each of these library units.

.....


### .....

The downward closure of the semantic dependencies of a set, denoted by T, of library units is
defined as the set of library units, denoted by G, which have semantic dependency on a unit in
T or another unit in G.

......










### Default Configuration

If a module product has no module (explicitly added) when the product is built, then one
module is automatically created as follows:

 * the implicitly created module is given the same name as the product and the same ID as the
   CWL;

 * all exports in the [effective library](?????) of the CWL are added to the module;

 * no units are explicitly added (so it has the implicit units as explained above);

 * the new module's version is automatic (taken from the CWL);

 * the new module is put into one module class, whose ID is the same as new module (which is,
   in turn, the same as the name of the CWL).


### Example

In this example, we will configure a module product, named `wee`, which contains two modules
named `wee.bon` and `wee.frew`.

Two procedures are exported as follows:

```ada
package Footlage
is
   procedure Footle (Foot: out String);
end;

with Bartlage;
package body Footlage
is
   The_Foot: String;

   procedure Footle (Foot: out String)
   with
      Export,
      External_Name => "wee.bon.footle"
   is
   begin
?????      Foot := Bartlage.Grobble & The_Foot;
   end;
begin
   The_Foot := "Down below";
end;

package Bartlage
is
    procedure Bartle (Bart: out Integer);
    function Grobble return String;
end;

package body Bartlage
is
   The_Bart: Integer;

?????   function Grobble return String is ("Whereabouts: ");

   procedure Bartle (Bart: out Integer)
   with
      Export,
      External_Name => "wee.frew.bartle"
   is
   begin
      Bart := The_Bart;
   end;
begin
   The_Bart := 123;
end;
```

These could be exported from modules named `wee.bon` and `wee.frew`, in a build named
`wee.live`, with the following ECLAT AdaShell commands:





```xml
<?xml version="1.0"?>
<eclat-state-update>
   <product>
      <name>  </name>
      <module>
         <name> wee </name>
         <class>  </class>
      </module>
   </product>
</eclat-state-update>
```




?????

```sh
create-product --module wee
create-module wee.bon
module-exports wee.bon.footle
module-class wee.bon
create-module wee.frew
module-exports wee.frew.bartle
module-class wee.frew
```

```sh
mkprod --module wee
mkmod wee.bon
modexp wee.bon.footle
modcls wee.bon
mkmod wee.frew
modexp wee.frew.bartle
modcls wee.frew
```










.....

The module named `wee.bon` would have two export subroutines:

    wee.bon.init
    wee.bon.footle

The `wee.bon.init` subroutine would contain the PXC code corresponding to the
initialisation code (`The_Foot := "Down below";`) for the package `Footlage`.

Similarly, the module named `wee.frew` would have two export subroutines:

    wee.frew.init
    wee.frew.bartle

The `wee.frew.init` subroutine would contain the PXC code corresponding to the
initialisation code (`The_Bart := 123;`) for the package `Bartlage`.

?????Because `wee.bon` needs `wee.frew' (it calls ) initialisation



-----------------------------------------------------------------------------------------------
## Plugin Products {#plugprod}

The purpose of a _plugin product_ is to generate a module which is a
[plugin](../pxcr/plugins.md). The module is implicit (it is never explicitly configured) and
always has the same name as the product.

A plugin product has no assemblies, and no main procedure.

It is configured with a set of [plugin classes](../pxcr/plugins.md#classes).

An export, named

    M.plugin_classes

where `M` is the product's (and module's) name, is implicitly configured for the module. This
export corresponds to a constant array of constant (references to) wide strings; each string
holds the name of a plugin class, in upper case,

The Realizor's [Plugin Helper](../pxcr/helpers.md#plugin) uses this array to select plugins to
add to an image.


### Example

 This example shows that the plugin
belongs to two plugin classes:

    acme.greenhouse.device_controller
    acme.greenhouse.temperature_probe






The PXC for this export would look like this:

```pxc

char16 => {0 ~ 65535};
string16(n) => char16 # n;
acme.greenhouse.service.plugin_classes.1: @ "ACME.GREENHOUSE.DEVICE_CONTROLLER";
acme.greenhouse.service.plugin_classes.2: @ "ACME.GREENHOUSE.TEMPERATURE_PROBE";
acme.greenhouse.service.plugin_classes: @ {
   M.plugin_classes.1 -> @ string16,
   M.plugin_classes.2 -> @ string16,
};
```






as if it were declared like this example:

```ada

   Plugin_Classes: constant array (Positive range <>)
      of not null access constant Wide_String :=
         (new Wide_String'("ACME.GREENHOUSE.DEVICE_CONTROLLER",
          new Wide_String'("ACME.GREENHOUSE.TEMPERATURE_PROBE"))
   with
      Export,
      External_Name => "acme.greenhouse.service.plugin_classes";
```

In this example, the Ada identifier `Plugin_Classes` is arbitrary; only the export name
(`acme.greenhouse.service.plugin_classes`) matters.
.....


### Configuration


......

















?????

A product is configured to build a plugin module with either the following ECLAT AdaShell
commands:

    mkprod -p P

where `P` is the name of the product.

This needs to be followed by a command to create the (one and only) plugin module:

    mkmod M

where `M` is the name of the module

The plugin classes must be defined. For each plugin class:

    plugcls C

where `C`,  is the names of the plugin class.

However, the module class, which will be its [base module class](../pxcr/plugins.md#bmc), must
be configured:

    modcls B

where `B` is the base module class.

Extra exports can be added to the module, although this is unlikely in practice.


### Example

For example, the following commands would put the plugin module `acme.greenhouse.service` into
the plugin class `acme.greenhouse.device_controller`.






```xml
<?xml version="1.0"?>
<eclat-state-update>
   <product>
      <name>  </name>
      <plugin>
         <name> acme.greenhouse.service </name>
         <class> acme.greenhouse.plugin </class>
         <plugin-class> acme.greenhouse.device_controller </plugin-class>
      </module>
   </product>
</eclat-state-update>
```








?????

```sh
mkprod --plugin service
plugcls acme.greenhouse.device_controller
mkmod acme.greenhouse.service
```








.....



-----------------------------------------------------------------------------------------------
## Partitions {#part}

An Ada program is divided into partitions (see [Ada Reference Manual, 10.2 Program
Execution][1]). Note that, in practice, most programs will comprise just one assembly.

Partitions can be executed separately---possibly on different computers in a network---but
nevertheless form part of a single coherent [program](../adaos/programs.md).

.....









-----------------------------------------------------------------------------------------------
## Main Partition {#main}

The _main subprogram_ of a partition must be a library unit that is a non-generic parameterless
procedure. (The standard allows any subprogram, but ECLAT requires a parameterless procedure.)

In a program, one partition (in a build) must have a main subprogram, and only one partition
may have a main subprogram. If more or less than one partition has a main subprogram,
explicitly configured or by default, building the product [fails](#fail).

The partition with a main subprogram is termed the _main partition_, and its module is termed
the _main partition module_.

.....

The main subprogram of a partition can be configured with an ECLAT state update file that looks
like the following:

```xml
<?xml version="1.0"?>
<eclat-state-update>
   <product>
      ...
      <program>
         ...
         <partition>
            <name> N </name>
            ...
            <main-unit> U </main-unit>
            ...
         </partition>
      </program>
   </product>
</eclat-state-update>
```

where `N` is the name of the partition, and `U` is the name of the main procedure. 

It is usually not necessary to explicitly configure the main subprogram of a partition. 

If the main subprogram is not explicitly configured, a default is chosen as follows:

 * if the partition includes a library unit that is a parameterless procedure whose fully
   qualified name is `MAIN` (case insensitive), the main subprogram of the partition will be
   that procedure; 

 * otherwise, if the partition includes exactly one library unit that is a parameterless
   procedure, the main subprogram of the partition will be that procedure; 

 * otherwise, the partition will have no main subprogram. 

.....

In fact, any units that are not explicitly allocated to another partition are implicitly
allocated to a partition named `main`, which is implicitly created if it doesn't already exist. 

If it so happens that a main subprogram is configured for the `main` partition, then that
partition will be the main partition. It is recommended to use the name `main` for the main
partition unless there is a strong reason not to. 

Thus, most often, the `main` partition does not need to be explicitly configured. If the
program is a single-partition program, usually no partitions need to be explicitly configured
at all. 



-----------------------------------------------------------------------------------------------
## .....









-----------------------------------------------------------------------------------------------
## Building a Program {#progprod}

A _program product_ builds a set of modules from the [partitions](#part) of a
[program](../rts/programs.md).

........

When a program is built, ECLAT generates (or regenerates) a separate module for each
partition.

.....

 * for each partition in the program, one module is generated, called the
   _partition module_

 * the one and only partition module that contains the main subprogram is
   called the _main partition module_);

 * one main partition is required, it is optional as to whether there are any other partition;

 * for every partition module, ECLAT adds code to the beginning of the [module initialisation
   procedure](../pxcr/modules.md#init) which sets `AdaOS.RPC.Partitions.Me` to the value of
   the partition's identifier;

 * for every partition module, ECLAT adds a [partition module table](#pmt) export;

 * ?????for the main assembly module, adds a [assembly identifier table](#pit) export.

 * for the main assembly module, ECLAT adds a main subprogram export;

 * for the main assembly module, ECLAT adds code to the end of the module initialisation procedure
   which registers the corresponding [program](../rts/programs.md).


.....

It should be noted that, in this mode, there only needs to be one assembly---which will
necessarily be the main assembly---and this is how a non-assemblyed program is built.

For the overall program,

.....



-----------------------------------------------------------------------------------------------
## Program Helper {#proghelp}

The Program Helper [Realizor Helper plugin](../pxcr/helpers.md)
[generates](../pxcr/modules.md#genmod) the configuration data for the assemblies and the
program in the [run-time support](../rts/rts.md) configuration module.






### Assemblies

One or more [fixed assemblies](../rts/assemblies.md#fixed) can be configured for a program.

Each fixed assembly must be configured with the partitions that will go to make it up.

..... pointing at the module initialisation procedure ......

......








### Configuration

.....

For example:








```xml
<?xml version="1.0"?>
<eclat-state-update>
   <product>
      <previous-name> qm </previous-name>
      <name> quakemon </name>
      <program>
         <name> acme.fgs.earthquake-monitor </name>
         <partition>
            <name> printer-controller </name>
            <module-name> acme.fgs.earthquake-monitor.printer-controller </module-name>
            <assembly-name> acme.fgs.earthquake-monitor.printer-controller </assembly-name>
            <unit> HP4114P </unit>
            <unit> HP4114P.Monitoring </unit>
            <unit> Print_Formatting </unit>
         </partition>
         <partition>
            <name> main </name>
            <module-name> acme.fgs.earthquake-monitor.main </module-name>
            <assembly-name> acme.fgs.earthquake-monitor.main </assembly-name>
            <main-unit> Main </main-unit>
         </partition>
         <partition>
            <name> datatape-controller </name>
            <module-name> acme.fgs.earthquake-monitor.datatape-controller </module-name>
            <assembly-name> acme.fgs.earthquake-monitor.datatape-controller </assembly-name>
            <unit> HP7012 </unit>
            <unit> HP7012.Control </unit>
         </partition>
         <assembly>
            <name> acme.fgs.earthquake-monitor.main </name>
         </assembly>
         <assembly>
            <name> acme.fgs.earthquake-monitor.datatape-controller </name>
         </assembly>
         <assembly>
            <name> acme.fgs.earthquake-monitor.printer-controller </name>
         </assembly>
      </program>
   </product>
</eclat-state-update>
```









This example creates a program divided into three partitions. Note that the module names and
assembly names have been explicitly specified, with the values that they would have been given
by default. Also, the whole main partition has been set to values it would have taken by
default, and the assemblies have been explicitly configured but with no properties other than
the defaults. 

Therefore, the exact same configuration would be more succinctly provided by:

```xml
<?xml version="1.0"?>
<eclat-state-update>
   <product>
      <previous-name> qm </previous-name>
      <name> quakemon </name>
      <program>
         <name> acme.fgs.earthquake-monitor </name>
         <partition>
            <name> printer-controller </name>
            <unit> HP4114P </unit>
            <unit> HP4114P.Monitoring </unit>
            <unit> Print_Formatting </unit>
         </partition>
         <partition>
            <name> datatape-controller </name>
            <unit>HP7012 </unit>
            <unit> HP7012.Control </unit>
         </partition>
      </program>
   </product>
</eclat-state-update>
```

The main partition didn't need to be explicitly specified at all, since its values are all the
default values. 

Note that this product, now named `quakemon`, used to be named `qm`. By adding the
`previous-name` element, the ECLAT state update program will delete any product named `qm` if
it finds one. Anything with a `name` can have any number of `previous-name` elements alongside. 

At the same time, if the update program finds a product named `quakemon`, it will delete it
first. In other words, the properties defined in the `product` element (with name `quakemon`)
will replace any existing product named `quakemon`. To avoid this effect, add the attribute
`update="add"` to the `product` element. 

I the attribute `update="add"` is added, then `previous-name` changes behaviour: instead of
just deleting a product with the `qm` previous name, that product's properties will be added to
`quakemon`, and *then* the `qm` product will be deleted. Thus, the old product (`qm`) will
update the new one (`quakemon`). 

.....






















-----------------------------------------------------------------------------------------------
## Partition Module Table {#pmt}

..........







-----------------------------------------------------------------------------------------------
## Configuring Partitions and their Units




????? This can go somewhere else.




......

It is an error to try to add a partition to a build whose ?????mode is not program

To add a new partition to a build, use the following command:

    eclat build B add-partition P

where `B` is the name of the build, and `P` is the name of the new partition.

To change the name of an existing partition, use the following command:

    eclat build B rename-partition P1 as P2

where `P1` is the existing name and `P2` is the new name.

To remove an existing partition from a build, use the following command:

    eclat build B delete-partition P

To explicitly associate a unit with a partition, use the following command:

    eclat build B partition P add-unit U

where `U` is the fully qualified name of the library unit to be added to the partition `P`.


### Example

For example, we may have a build:

    acme.greenhouse.prod

It is typical to end the name of a build with `.prod` for a production build, or `.test` for a
(end-user) testing build, or `.debug` for a (developer) testing and debugging build.

Suppose this build has four assemblies, named:

    acme.greenhouse.aircon
    acme.greenhouse.salinity
    acme.greenhouse.security
    acme.greenhouse.main

It is conventional to give all the partitions a name with the same prefix (in this case
`acme.greenhouse`) and to have the name of the main partition end with `.main`.

To configure this build, we might use the following command:

```
eclat read <<.
build acme.greenhouse.prod add
assembly acme.greenhouse.aircon add
unit Acme.Greenhouse.Aircon add
assembly acme.greenhouse.salinity add
unit Acme.Greenhouse.Salinity add
assembly acme.greenhouse.security add
unit Acme.Greenhouse.Security add
assembly acme.greenhouse.main add
unit Acme.Greenhouse.Main add
main Main
.
```















    [eclat/builds(acme.greenhouse.service)/modules(acme.greenhouse.aircon)/units]
    ACME.Greenhouse.Aircon
    ACME.Calculations.Temperature
    ACME.Calculations.Fluid

    [eclat/builds(acme.greenhouse.service)/modules(acme.greenhouse.salinity)/units]
    ACME.Greenhouse.Salinity
    ACME.Calculations.Fluid

    [eclat/builds(acme.greenhouse.service)/modules(acme.greenhouse.security)/units]
    ACME.Greenhouse.Security
    ACME.Windows_And_Doors

    [eclat/builds(acme.greenhouse.service)/modules(acme.greenhouse.main)/units]
    Main

It is typical that the main partition is configured with just the main subprogram, and it is
conventional for the main subprogram to be named `Main`.

.....



-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
## Partition Identifier Table {#pit}

The _partition identifier table_ .......

An export, named

    system.partitions

is added to the main partition module. This export corresponds to .....:

```ada
type Partition_Identifier_Table
is
   aliased constant array (AdaOS.RPC.Partiton_ID) of
      constant not null access AdaOS.RPC.?????_Receiver;

``` 


......







-----------------------------------------------------------------------------------------------
## Partition Initialisation

When an [assembly](../rts/assemblies.md) is executed, it is expected that the [run time
system](../rts/rts.md) will call the [module initialisation procedure](../pxcr/modules.md#init)
of each assembly module.

......

.





???????

To initialise the module and register the partition, PXC code is automatically generated by
ECLAT which is equivalent to the following Ada source text:

```ada
declare
   A: AdaOS.Execution.Assembly_Access;
begin
   -- other initialisation
   AdaOS.Execution.Registration.Register_Assembly (N, P, A);
   -- other initialisation
end;
```

In the above,

 * `P` is replaced by the memory address of the initialisation procedure of the assembly;

 * `N` is replaced by a static string value which is the configured name of the assembly;

 * `A` represents an assembly object, of a type derived from
   `AdaOS.Execution.Program_Assembly`.

The procedure `AdaOS.Execution.Registration.Register_Assembly` is imported with the external
name `system.register_assembly`. The RTS exports this procedure.











-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
## Build Failure {#fail}

.....




-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
## Reproducible Build Environment

..... _reproducible build environment_ .....

as a specialisation of the concept of the [Reproducible Execution Environment
Management](../reem/reem.md)




-----------------------------------------------------------------------------------------------
## ECLAT Plugins

.....

ECLAT [plugins](#plugins) can

?????add further modes, and they can extend the actions of existing
modes.

.....



-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
## .....




-----------------------------------------------------------------------------------------------
## References

[1]: <http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-10-2.html> "?????"












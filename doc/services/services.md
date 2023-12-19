-----------------------------------------------------------------------------------------------
# Services

[System objects](../objects/objects.md) are essentially just a way to enable objects (in the
sense of object-oriented programming) to be shared between [programs](../adaos/programs.md). 

A _service_ is simply a special kind of system object that is available ab initio to programs,
without having to be created by some other (factory) object. Very often, of course, services
will be [factory objects](../objects/objects.md#fact) that allow other objects, possibly system
objects, to be created. 

Most services will be either added statically to programs or they will be implemented in their
own [service programs](servprog.md). Services can execute remotely. 

A service, like any system object, obeys an interface that defines its methods (primitive
operations) and the signature (parameters) of each of those methods. In terms of the Ada
programming language, the interface is defined by a type (which may be an interface or a tagged
type). 

In general, there can be many system objects of a particular (Ada) type, but it is very typical
that there is, within the [service directory](#svcdir) of a particular
[compartment](../adaos/compart.md), only one service of a particular type. 

The [Nyota](../nyota/nyota.md) tool will usually be used to generate the skeleton (wrapper
code) for a service. Additionally, this tool can generate the proxy code which allows services
to be remote (programs can use services running on different computers in a network). 

In AdaOS, services serve a variety of [purposes](purposes.md). 



-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Service Interfaces

Each different service must belong to a _service interface_ (or possibly two or more service
interfaces). 

The service interface of a service defines the methods (procedures and functions that are
primitive operations) of the services that belong to it, and the meanings of those methods. 

In practice, a service interface is represented by an Ada limited interface type derived
(directly or indirectly) from the type `System_Service`, which is declared in the package
`AdaOS.Services`. 

```ada

type System_Service is limited interface and Objects.System_Object;
```

Note how a service interface is a [system object](../objects/objects.md). 

For each service interface, there will be a remote types package in which the service interface
type, will be declared. 

For example, for a service named `Foobar`:

```ada

type Foobar_Service is limited interface and AdaOS.Services.System_Service;
```


### Service Interface Versions

Like all interfaces that are shared (usually an interface is shared if it is declared in a
separate library), service interfaces need to be [versioned](../intro/versions.md). 

It is important to distinguish the versions of a service interface from the versions of the
(one or many) [implementations](#imp) of it. Those implementations will have their own
versions, but these are separate to the versions of the interface. 

.....

The recommended way to name a service interface is to append its compatibility version to the
base name, in the form `_A_B` where `A` is the major version number, and `B` is the minor
version number. 

For example, if the service interface has the base name `Foobar`, and the compatibility version
is 1.0, then the name used for the service interface would be: 

    Foobar_1_0


### Stub Library

A service interface will usually be declared in a (library level) package (an Ada package, that
is) and typically that package will have the same name as the service interface, but without
the version affix, and very often the package will be a child of an appropriate package
hierarchy.

This package will then very likely need to be compiled into a
[stub library](../eclat/building.md#stublibs), so that it can be included by both the libraries
of [implementations](#imp) and the libraries of [clients](#clients) of the service. 

The use of the stub library like this helps to _decouple_ clients from the implementations,
which is one of the vital objectives of services. 



-----------------------------------------------------------------------------------------------
## Service Implementations {#imp}

A _service implementation_ is a class (an Ada non-abstract limited tagged type) derived from a
service interface (or possibly two or more service implementations).

Objects of this type will be services that implement the service interface (or service
interfaces, if there are several). 

An implementation will usually be a package (an Ada package) that is categorised as a remote
types package. 

This remote types package will typically contain the declaration of a service implementation
type together with the declarations of its methods (primary operations), as well whichever
associated types are required, plus *their* methods and so on. 



-----------------------------------------------------------------------------------------------
## {#}














????? The service object will have task components, which will all inherit the [identity and
authority](../security/security.md#ca) and [transaction](../database/transactions.md) of the
task which calls `Create_Foobar_Service` to create the service object. 

?????This is very significant to the task calling `Create_Foobar_Service`, since it will be the
master of these component tasks, and it will not be able to complete (normally) until those of
the service object have. 

?????That, in turn, is dependent on the service object being disengaged and then the tasks will all
wait on a terminate alternative, enabling them to terminate with the master. 














-----------------------------------------------------------------------------------------------
## Service Clients {#client}

......



-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## Service Links {#link}

A _service link_ is a [link](../objects/objects.md#links) whose target is a service.

Each different service (object) is identified by name, but will also have, as part of its 
metadata, 
the service identifier and a compatibility version. 

.....




-----------------------------------------------------------------------------------------------
## Service Directories {#dir}

A _service directory_ is a [directory](../objects/containers.md#dir) that is used solely or
primarily to contain [service links](#link). 

The type `Service_Directory`, declared in the package `AdaOS.Services`, is derived from 
`AdaOS.Objects.Object_Directory`, but adds some operations of its own.

As well as allowing a service to be found using its OID or name, a service directory allows a
service to be found using its type. Specifically, there is an overloading of the function `Find` 
declared in the package `AdaOS.Services:

```ada

function Find (Directory:    access Service_Directory; 
               External_Tag: in String) return Objects.Object_Sets.Set is abstract;
```

This function returns the set of services whose type has an external tag that matches the given
`External_Tag` or which is a descendant (at the same level) of that type. (I'm saying 'at the
same level', but it is highly recommended that a service interface type is only ever declared
at library level.)

The result is in the form of a set of object IDs (OIDs). If more than one OID is returned, the
program making the inquiry might examine the [metadata](#meta) of each service to decide which
one to use. 


.......







-----------------------------------------------------------------------------------------------
## Service Metadata {#meta}

Every service in a [service directory](#dir) can be associated with
[metadata](../objects/containers.md#meta) that gives information about the service. 

The package `AdaOS.Services` declares the abstract tagged non-limited type `Service_Metadata`,
which is derived from `AdaOS.Objects.Member_Metadata`. 

The metadata produced by a service directory must be derived from `Service_Metadata`, which
adds the following properties. 


### Program

The `Service_Program` property is a link to the [service program](../services/servprog.md)
which implements the service. 


### Mode

The `Mode` property id the [mode](servprog.md#mode) of the service program (at the time the
metadata was extracted). 

.....


### Canonical Name

.....


### Title

.....


### Version {#ver}

Any one particular service (implementation of a service interface) could have multiple
versions. 

.....

Every service implementation 

[version](../intro/versions.md)

.....



-----------------------------------------------------------------------------------------------
## Service Events {#}

A service can be expected to produce a variety of [events](../events/events.md). 

.....

The _lifetime events_ of a service are all represented by the type `Service_Lifetime_Event`, 
declared in the package `AdaOS.Services`: 

```ada

type Service_Lifetime_Event_Kind is (Start, Stop, )

type Service_Lifetime_Event (Kind: Service_Lifetime_Event_Kind)
is
   record;

   end record;
```

Other events are specific to the service interface.

.....







-----------------------------------------------------------------------------------------------
## Service Directory Events {#}

A service directory can be expected to produce a variety of [events](../events/events.md). 

.....

The _lifetime events_ of a service directory: 

 * .....








-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## Finding a Service {#find}



????? Review



..... [service directory](#dir) by name:

```ada

function Find (
   Directory:   not null access Service_Directory;
   Name:        in Name_String;
   Version:     in Compatibility_Level)
return 
   access System_Service'Class is abstract;

overriding
function Find (
   Directory:   not null access Service_Directory;
   Name:        in Name_String)
return 
   access System_Service'Class is abstract;
```

The overloading with no `Version` parameter find the service with the highest (latest) version.
This overloading overrides the [`Find`](../objects/containers.md#findname) function of the base
type `Object_Directory`. 

There is also, for convenience, the same set of functions that use the [stock services](#stock) of a
specific compartment: 

```ada

function Find_Service (
   Name:        in Name_String;
   Version:     in Compatibility_Level;
   Compartment: not null access Program_Compartment'Class := Task_Instance.Compartment)
return 
   access System_Service'Class;

function Find_Service (
   Name:        in Name_String;
   Compartment: not null access Program_Compartment'Class := Task_Instance.Compartment)
return 
   access System_Service'Class;
```

For both overloadings, the `Compartment` parameter defaults to the compartment of the instance
of the calling task. 

.....















-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Stock Services {#stock}

Every [compartment](../programs/compart.md) provides a set of _stock services_ .....

[.....](../native/dirhier.md)

Stock services can provide a lot of the essential functionality that most programs rely on,
including:

 * event brokers, including logging providers
 
 * file systems

 * a [service shepherd](shepherds.md), `shepherd`; 
 
 * network connectivity (this will be supported in the future)

 * .....




Compartments have a [property](../intro/intro.md#prop) for each of the stock services. For
example, the following function and procedure are declared for the property `Event_Channels`: 

```ada

function Event_Channels (Compartment: not null access Program_Compartment)
return
   access Events.Event_Broker'Class is abstract;

procedure Set_Event_Channels (
   Compartment: not null access Program_Compartment;
   Broker:      access Events.Event_Broker'Class)
is
   abstract;
```

.....





### Convenience Functions

????? Overkill?

For convenience, the package `AdaOS.Services.Stock` contains the declarations of functions for
all of the stock services, with a `Compartment` parameter that defaults to the compartment of
the current executional instance of the calling task. 

For example: 

```ada

function Event_Channels (
   Compartment: not null access Program_Compartment'Class := Task_Instance.Compartment
) 
return access Events.Event_Broker'Class is abstract;
```


### Stock Service Directory

In AdaOS Native, the directory `/adaos/serv` is a [service directory](#dir) whose members
comprise the stock services. 

The name of each member is the fully qualified name of the service's interface type. 

For example, the full path of the system event broker is: 

    /adaos/serv/chan


### List of Stock Services

The stock services are currently: 

| Property              | Name         | Description                                |
| --------------------- | ------------ | ------------------------------------------ |
| `Event_Channels`      | `chan`       | System Event Broker                        |
| ` ` | ` ` |  |
| ` ` | ` ` |  |
| ` ` | ` ` |  |
| ` ` | ` ` |  |
| ` ` | ` ` |  |
| ` ` | ` ` |  |
| ` ` | ` ` |  |
| ` ` | ` ` |  |
| ` ` | ` ` |  |
| ` ` | ` ` |  |
| ` ` | ` ` |  |
| ` ` | ` ` |  |
| ` ` | ` ` |  |
| ` ` | ` ` |  |
| ` ` | ` ` |  |
| ` ` | ` ` |  |

The Name in the above table is the name the service has within the stock service directory
(`/adaos/serv`). 






-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Service Catalogues {#cat}

A formal declaration and description of one or more services can be put into an XML file, 
of schema ?????
called a _service catalogue_.

The ????? command-line tool can be used to add one or more service catalogues into a 
binary data file called a _service database_. 



The description of a service can include some or all of its [metadata](#meta), in textual form. 

[Plugins](../pxcr/plugins.md) for the ????? command-line tool are able to add parsers that can
interpret textual data that ????? cannot interpret innately, and which can construct the
metadata objects from the XML. 

.....



-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Service Proxies {#prox}

A _proxy_ service is a service that can take the place of another service.

Clients of the service are still able to interact with the service exactly as they did before,
but the proxy service works in a different way to the one it replaces. 

.....


### Changeling Proxies {#changeling}

A _changeling_ proxy service is a service that not only mimics another service but also
stealthily replaces it. A program that seeks the original service, by [service
identifier](#id), or by most means (but not by OID), will be given the changeling instead. 

This might seem a little underhand, but in fact it is a very useful technique. 

.....







-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Example 1: Providing a Service {#ex1}

.....

For example, supposing the ACME division Foo has a service interface Bar, and its current
compatibility version is 2.7.

This pacakge might be declared in a stub library for the Bar service: 

```ada
with AdaOS.Services;
package ACME.Foo.Bar_Service
is
   type Bar_2_5 is limited interface and AdaOS.Services.System_Service; 
   -- DEPRECATED, SUPERSEDED
   -- public operations of Bar_2_5

   type Bar_2_6 is limited interface and AdaOS.Services.System_Service; 
   -- DEPRECATED, SUPERSEDED
   -- public operations of Bar_2_6

   type Bar_2_7 is limited interface and AdaOS.Services.System_Service;
   -- public operations of Bar_2_7
private
   -- private stuff
end;
```

The two preceding compatibility versions of the service interface are also declared, but are
marked as deprecated and superseded. (Previous versions have already been removed altogether.)

Some declarations have been omitted, but it is assumed here the package has no body. In
general, packages declaring service interfaces could have a body. 

.....


Maybe the same division of ACME has an implementation of this service interface (Bar 2.7). The
implementation is named Central Bar, and is currently at compatibility version 1.3. 

This implementation may be in a library with the following package specification: 

```ada
with ACME.Foo;
package ACME.Foo.Central_Bar
is
   type Central_Bar_1_3 is new ACME.Foo.Bar_Service.Bar_2_7 with private;
   -- declare the overridings of the operations of Bar_2_7
private
   -- private stuff
end;
```

In the same library would be the body of this package. 

.....

```ada
package body ACME.Foo.Central_Bar
is
   -- implementations of the public operations of Central_Bar_1_3
   Unibar: aliased constant Central_Bar_1_3; -- singleton object of type Central_Bar_1_3
begin
   Task_Instance.Compartment.Services.Activate ("acme.unibar.1.3", Unibar'Access);
   -- add the Unibar service to the stock services of the current compartment
end;
```

.....



-----------------------------------------------------------------------------------------------
## Example 2: Using a Service {#ex2}

The following procedure illustrates the skeleton of a [client](#client) making use of the
service provided in [Example 1: Providing a Service](#ex1). 

The client in this example is a separate library that includes the stub library of the Bar
service interface. 

The procedure is named `Frobnify` and performs a totally ficticious activity involving the Bar
service. 

```ada

with AdaOS.Instances, AdaOS.Compartments, AdaOS.Services;
use  AdaOS.Instances, AdaOS.Compartments, AdaOS.Services;

with Foo.Bar_Service;
use  Foo.Bar_Service;

procedure Frobnify (Frobs: in out Frob_List)
is
   Bar: access Bar_2_7'Class;
   Frob_Ops: access Frob_Operations_Controller'Class;

begin
   Bar := Find_Service ("acme.unibar.1.3");
   if Bar = null raise Bar_Not_Available; end if;

   begin
      select
         delay 0.001; -- one millisecond
         if Bar /= null then Bar.Disengage; end if;
         raise Bar_Engagement_Tiemd_Out;
      then abort
         Bar.Engage;
         Frob_Ops := Bar.Create_Frob_Ops;
         Bar.Disengage;
      end select;
   exception
      when others =>
         if Bar /= null then Bar.Disengage; end if;
         raise;
   end;

   if Frob_Ops = null  then raise Frob_Ops_Not_Obtained; end if;

   for Item of Frobs
   loop
      Frob_Ops.Frobnify (Item);
   end loop;

end;
```

This procedure is whimsically based on the notion that an ephemeral object called a 'frob
operations controller' can be used to 'frobnify' an equally whimsical kind of thing called a
'frob'. 

.....

The only interaction with the Bar service itself is simply to obtain (an access value
referencing) a frob operations controller for the service. The service only needs to be engaged
for the period of time needed to obtain a controller. 

Engaging `Bar` and obtaining (an access value referencing) a frob operations controller must
complete within one millisecond, otherwise this procedure is timed out. Note how careful we are
to ensure that `Bar` is disengaged, as soon as possible, no matter what happens. This is vital. 

Then, the obtained controller, `Frob_Ops`, is used to frobnify the given list of frobs. 

The frob operations controller object will be remote object, and it will be tying up resources
of which program implements the Bar service while it exists. The local variable `Frob_Ops` is
the only reference to the object. Thus when `Frob_Ops` goes out of scope the controller object
will be finalised. It can be presumed that finalising it will cause it to release all the
resurces it is using, except for memory. The memory will be released upon
[garbage collection](../memory/garbcoll.md).




-----------------------------------------------------------------------------------------------
## Example 3: Predicting the Weather {#ex3}

.....

```ada
with Ada.Calendar, Ada.Strings.Unbounded;
use  Ada.Calendar, Ada.Strings.Unbounded;

with AdaOS.Instances, AdaOS.Compartments, AdaOS.Services;
use  AdaOS.Instances, AdaOS.Compartments, AdaOS.Services;

with ACME.FGS.Weather_Prediction;
use  ACME.FGS.Weather_Prediction;

function The_Weather_Tomorrow (Location: in Weather_Location;
                               Timeout:  in Duration := 5.0) return String
with
   Pre => Timeout > 0.0
is
   Day: constant Duration := 24 * 60 * 60.0;
   Predictor: Weather_Predictor_Service_Access;
   Result: Weather_Forecast;
begin
   select
      delay Timeout;
      if Predictor /= null then Predictor.Disengage; end if;
      raise Prediction_Failure with "Could not predict weather: service timed out.";
   then abort
      Predictor := Nearest_Weather_Predictor_Service;
      if Predictor = null 
      then 
         raise Prediction_Failure with "Could not find weather prediction service."; 
      end if;
      Predictor.Engage;
      Result := Predictor.Forecast (Location, Clock + 1 * Day);
      Predictor.Disengage;
      return Result.Human_Readable;
exception
   when others =>
      if Predictor /= null then Predictor.Disengage; end if;
      raise;
end;
```

......



-----------------------------------------------------------------------------------------------
## References

[1]: <https://www.neff-home.com> "Neff"










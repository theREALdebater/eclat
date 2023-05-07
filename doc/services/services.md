-----------------------------------------------------------------------------------------------
# Services

[System objects](../objects/objects.md) are essentially just a way to enable objects (in the
sense of object-oriented programming) to be shared between programs. 

A _service_ is simply a special kind of [system object](../objects/objects.md) that is
available ab initio to programs, without having to be created by some other (factory) object.
Very often, of course, services will be factory objects that allow other system objects to be
created. 

Most services will be either added statically to programs or they will be implemented in their
own [service programs](servprog.md). 

A service, like any system object, obeys an interface that defines its methods (primitive
operations) and the signature (parameters) of each of those methods. In terms of the Ada
programming language, the interface is defined by a type (which may be an interface or a tagged
type). 

In general, there can be many system objects of a particular (Ada) type, but it is very typical
that there is, within the [service directory](#svcdir) of a particular
[compartment](../adaos/compart.md), only one service of a particular type. 

.....




-----------------------------------------------------------------------------------------------
## Purpose of Services

Services create a decoupling of the parts of an overall large piece of software, so that .....

Whilst (object-oriented) classes, (Ada) packages, and (compilation) libraries provide static 
decoupling of software, services provide dynamic decoupling. 

.....



### Software Construction

One of the essential consequences of this approach is that services will tend to depend on
other services, and there will be a tendency for a whole solution to be built up as a large
number of services in a complicated dependency tree. 

This is likely to set the pattern of how software is constructed and structured under AdaOS. 

Of course, the Ada programming language was always designed to enable a large piece of software
to be built up from smaller pieces called packages. The assemblage of packages is entirely
static, and tends to have a certain amount of brittleness, in that a change in one package can
often have a lot of knock-on effects, requiring further changes elsewhere, and this can cascade
somewhat at times. The saving grace is that almost all of these knock-on effects are detected
at compile time, and, to a large extent, resolving them to the compiler's satisfaction is
enough to properly resolve them. 

Nevertheless, the purpose of the object-oriented features introduced to the language in 1995 is
to reduce this brittleness. The visible methods (primitive operations) of a class can be
changed without actually changing the class; instead, a new class can be derived from the old
one, with the necessary changes added to the new one. New software uses the new class, but old
software can continue to use the old class and remain unaffected. 

System objects add to this the ability for objects to be dynamically shared between running
programs, so helping to enable the construction of systems whose execution comprises the
execution of multiple programs in a relatively dynamic way. 

Services provide the starting points for programs to obtain all the system objects they need to
be able to co-operate with the other programs of the system. 


### Dynamicity 

The structure of implementing services in AdaOS has been designed so that any service can be
made available to programs either dynamically or statically. 

To add in a set of services statically to program, functions that construct the services, and
return access to them, are added to the program. The [library](../eclat/libraries.md) that
builds the program is configured to depend on the libraries that implement the services, so
that they end up included in the build. 

To access the services dynamically, there are mechanisms that allow each service to be
implemented by its own [service program](servprog.md) and then to be accessed by name. 

One of the advantages of using the static method is that the 
[?????](?????) plugin for the [Realizor](../pxcr/realizor.md)
performs a dependency analysis, detecting any circularities that would be very likely
to cause software problems during execution. 

.....


### Dependency Injection {#di}

For the Ada programming language, services---together with a good set of service choice
managers---can be used to achieve an effect similar to the various 'dependency injection' (DI)
technologies available for other programming languages. 

The essential idea of DI is that a piece of software can say what kinds of other pieces it
needs (or wants) to use (i.e. has a dependency on), and some manager-type component is
responsible for providing what it needs. 

Different DI management technologies have all sorts of sophisticated mechanisms for matching up
what is required to what is available and resolving the dependency tree. 

In AdaOS, each [compartment](../adaos/compart.md) has its own [service directory](#svcdir) which is 
used (by the compartment's program) to find services either by [id](#svcid), or by name, or by class. 

Other service directories could be used, but it is the compartment's specific service directory
that is used by default. 

By configuring (or otherwise setting up) a compartment's service directory, the services used
by a program can be controlled, so that the overall execution of the program can be modified in
a dynamic manner. 


### Inversion of Control

The principle of _inversion of control_, or _IoC_, in computing is based on the idea that it is
often more appropriate for a piece of software A, which calls upon another piece B as a part of
its execution, to be told what B is rather than dictating what B is for itself. 

To illustrate this as succinctly as possible, supposing we have a procedure `Make_a_Cake`, and
this procedure calls, at the appropriate point in its execution, a procedure `Turn_On_the_Oven`. 

We might have:

```ada
with Neff;
procedure Make_a_Cake
is
   ...
begin
   ...
   Neff.Turn_On_the_Oven (Temperature => 125);
   ...
end;
```

This formulation suggests that procedure `Make_a_Cake` assumes we have a [Neff][1] oven. If we
wanted to change the oven we are using, we must change the procedure `Make_a_Cake`. This is
unfortunate, because the algorithm for making a cake doesn't really have anything to do with a
choice of oven. 

However, if we were to somehow pass the oven-related procedures into procedure `Make_a_Cake`,
we could then change the oven without having to change procedure `Make_a_Cake`. 

For example: 

```ada
with Ovens;
procedure Make_a_Cake (Cooker: not null access Ovens.Oven'Class)
is
   ...
begin
   ...
   Cooker.Turn_On_the_Oven (Temperature => 125);
   ...
end;
```

Do not fear that you are being sucked into the depths of academic computer science. The essence
of IoC is simple and effective. 

Because of the way that a whole system will tend to assemble itself out of services, an IoC
pattern will often be appropriate. 

Basically, what this means is that, if a program (which might be a service program) wants to
make use of method `M` of a service, but wants to customise how `M` works in some way, it can
do so by passing `M` either a remote subprogram or a remote object (by means of a remote access
value). 

When `M` executes, it calls the remote subprogram, or a method of the remote object, in order
to get the custom functionality at the right time (and with the right parameters). 

.....

Of course, the principles and techniques of IoC also apply to other program structures, not
just services. 


### Middleware

The basic model of the relationship between services is of 'client' and 'server': the 'server'
service provides some functionality (along with some state, i.e. a database, in many cases),
and the 'client' (which may be another service or it may be any kind of program) calls upon
that functionality. 

However, quite often it is useful to be able to configure a service to be inserted, like the
filling in a sandwich, between a client and a server. This kind of inserted service is called
_middleware_. 

In fact, one may wish two or more middleware services to be inserted between a client and a
server. 

The way that a middleware service works (in AdaOS) is that it is set up to be a [proxy](#prox),
or indeed a changeling, for the original server, and at the same time it retains a pointer to
the original server. 

In this way, the middleware service implements all the same methods as the original service. In
general, it will implement each method by simply calling the corresponding method of the
original service. For some, or maybe all, of the methods it will reimplement the method in some
way, which might involve calling the original method, or maybe some combination of it and other
original methods. 

.....

If the middleware service is a changeling then the client will get it, instead of the original
service, automatically. If it is a proxy (but not a changeling), then typically the client will
need to insert the middleware itself (or request its insertion by some other software). 

.....





### Network Computing

One kind of middleware for services are _networking proxies_. 

A pair of networking proxies for a service allows the service, running on a particular
computer, to be called from any other computer that has a suitable communications mechanism. 

To enable a _base service_ to be called from other computers, it must have a pair of proxy
services: an _outbound proxy_ and an _inbound proxy_. 

The outbound proxy is a changeling for the base service, and must run on every computer which
needs to enable its programs to call the base service. The outbound proxy's job is to
communicate calls to the base service, using whatever communication mechanisms it has been
configured to use. It sends messages, representing the calls, to the computer on which the base
service is actually running. It receives messages representing the responses being communicated
in the reverse direction, and pass the responses back to the original callers. 

The inbound proxy (which is not a changeling), runs on the same computer as the base service.
It receives the call messages being communicated, by outbound proxies on other computers, and
passes those on as calls to the base service. It wraps up the responses into messages and
communicates those back to the originating outbound proxies. 

As a convenience, the [Nyota](../tools/nyota.md) toolset can generate the Ada source text that
implements a pair of networking proxies for a service. 

The ability to run programs and services on different computers, and yet have them interact as
if they were all running in the same computer, is sometimes termed _network computing_, and can
have many important advantages, including [redundancy and scalability](replic.md). 

The composition of software in an AdaOS [effective system](../intro/intro.md#effsys) is usually
completely static. On the [AdaOS Native platform](../pxcr/targets.md#plat), the use of
[segmentation](../memory/segments.md) can make things a little more dynamic. 

However, it is assumed that the composition of software of all the computers within a [local
area network](?????) will generally be very dynamic. In effect, as computers within the LAN are
reconfigured and rebooted (or just switched on and off), the overall software picture within
the LAN cannot remain purely static. 

This is where the dynamic nature of services shines. 

.....








### 







### Small Services

It will be typical for some services to be small. What a small service does may be very modest.
It may, for example, simply have a few parameterless functions that return certain key values. 

There may be a feeling that such small services are too small to be legitimate, and need to be
eliminated or perhaps rolled into some other bigger service. However, this is not necessarily
the best approach. Amalgamating small services can have the effect of complicating dependency
relationships. Ultimately, this could actually cause a serious problem, because some systems
cannot be configured without there being a dependency dilemma (where there is a circularity
that cannot be resolved by adjusting the configuration). 

Thus, small services may seem odd but they have their place. Let them be. 


### Stock Services

AdaOS comes with a set of _stock services_.

Stock services can provide a lot of the essential functionality that most programs rely on,
including:

 * event brokers, including logging providers
 
 * file systems
 
 * network connectivity (this will be supported in the future)

 * .....
 
.....





-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Service Interfaces

Each different service must belong to a single _service interface_. 

The service interface of a service defines the methods (procedures and functions that are
primitive operations) of the services that belong to it, and the meanings of and relationships
between those methods. 

In practice, a service interface is represented by an Ada type derived (directly or indirectly)
from the task interface type `System_Service`, which is declared in the package
`AdaOS.Services`. 

```ada

type System_Service is task interface and Objects.System_Object;
```

Note how a service interface is a [system object](../objects/objects.md). 





?????A service is itself a system object, of a type derived from the abstract limited controlled
type `System_Service`, which is declared in the package `AdaOS.Services`. 

?????The type `Service_Access` is declared in the package `AdaOS.Services`, which is declared as a
remote types package, so the access type is a remote access type. 

.....

For each service interface, there will be a remote types package in which the service interface
type, 

?????and an access type for it, 

will be declared. 



????? There is usually a parameterless function that creates a new
service object. 



For example, for a service named `Foobar`:

```ada

type Foobar_Service is task interface and AdaOS.Services.System_Service;

type Foobar_Access is access all Foobar_Service'Class;

?????function Create_Foobar_Service return Foobar_Access;
```







????? The service object will have task components, which will all inherit the [identity and
authority](../security/security.md#ca) and [transaction](../database/transactions.md) of the
task which calls `Create_Foobar_Service` to create the service object. 

?????This is very significant to the task calling `Create_Foobar_Service`, since it will be the
master of these component tasks, and it will not be able to complete (normally) until those of
the service object have. 

?????That, in turn, is dependent on the service object being disengaged and then the tasks will all
wait on a terminate alternative, enabling them to terminate with the master. 














Normally, the declaration of a service interface type, together with the declarations of its
methods and associated types and *their* methods and so on, is put into a separate [shared library](?????).

This allows an implementation of the service interface to be put into a separate library which
does not need to be depended upon by libraries that use the service. This is the essence of the
decoupling provided by services. 

Instead, somewhere within a separate program, an implementation of the service can execute and
.....



### Service Interface Versions

Like all interfaces that are shared (usually an interface is shared if it is declared in a
separate library), service interfaces need to be [versioned](../intro/versions.md). 

.....

The recommended way to version an interface, e.g. `Foobar`, is to append the compatibility
version to the name, in the form `_A_B` where `A` is the major version number, and `B` is the
minor version number. For example: `Foobar_1_0`



.....




-----------------------------------------------------------------------------------------------
## Service Links {#link}

A _service link_ is a [link](../objects/objects.md#links) whose target is a service, as
pinpointed by the combination of a service identifier and a compatibility version. 

.....




-----------------------------------------------------------------------------------------------
## Service Directories {#dir}

A _service directory_ is a [directory](../objects/containers.md#dir) that is used solely or
primarily to contain [service links](#svclnk). 

The type `Service_Directory`, declared in the package `AdaOS.Services`, is derived from 
`AdaOS.Objects.Object_Directory`, but adds some operations of its own.

As well as allowing a service to be found using its OID or name, a service directory allows a
service to be found using its type. Specifically, there is an overloading of the function `Find` 
declared in the package `AdaOS.Services:

```ada

function Find (Directory:    access Service_Directory; 
               External_Tag: in String) return Objects.OID_Sets.Set is abstract;
```

This function returns the set of services whose type has an external that matches the given
`External_Tag` or which is a descendant (at the same level) of that type. (I'm saying 'at the
same level', but it is highly recommended that a service is only ever declared at library
level.)

The result is in the form of a set of object IDs (OIDs). If more than one OID is returned, the
program making the inquiry might examine the [metadata](#meta) of each service to decide which
one to use. 





????? totally internal?

A service directory allows members to be added which are _service placeholders_. 

A service placeholder is a special entity, stored in some opaque way within a service
directory. Every placeholder is associated with a 
[program controller](../services/servprog.md). Whenever a request is made to `Find` a placeholder, the
program controller is notified. The program controller replaces the placeholder with the real
service (when it becomes available), and so it is the real service that is engaged. 





.......




-----------------------------------------------------------------------------------------------
## Service Metadata {#meta}

Every service in a [service directory](#dir) can be associated with [metadata](../objects/containers.md#meta) that 
gives information about the service. 






The following components of a metadata object are typical. 



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


### 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 






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
##




-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
##






-----------------------------------------------------------------------------------------------
## Compartment Services {#cmpt}


A [compartment](../rts/compart.md) has a set of services that can be (engaged and) used by
[executional instances](instances.md) of the compartment. 

Every compartment has a method, a function named `Services`, which returns a [service directory](#dir) containing 
the services available to the compartment. 

```ada

function Services (
   Compartment: not null access Program_Compartment'Class := Task_Instance.Compartment
) 
return Service_Directory'Class is abstract;
```
There is also, for convenience, a function `Find_Service` that finds the service of a
compartment by ID or by name: 

```ada

function Find_Service (
   Id:          in Service_Id;
   Version:     in Compatibility_Level;
   Compartment: not null access Program_Compartment'Class := Task_Instance.Compartment
)
return Service_Access;

function Find_Service (
   Name:        in Name_String;
   Version:     in Compatibility_Level;
   Compartment: not null access Program_Compartment'Class := Task_Instance.Compartment
)
return Service_Access;

function Find_Service (
   Id:          in Service_Id;
   Compartment: not null access Program_Compartment'Class := Task_Instance.Compartment
)
return Service_Access;

function Find_Service (
   Name:        in Name_String;
   Compartment: not null access Program_Compartment'Class := Task_Instance.Compartment
)
return Service_Access;
```

In all cases, the `Compartment` parameter defaults to the compartment of the instance of the
calling task. 

The two overloadings of `Find_Service` without a `version` parameter return the latest version
of the identified service. ?????necessary



-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##






-----------------------------------------------------------------------------------------------
## Example: Using a Service

.....

The following procedure illustrates the skeleton of the usage of a service named
`Footle_Bartle`: 

```ada

with AdaOS.Instances, AdaOS.Compartments, AdaOS.Services;
use  AdaOS.Instances, AdaOS.Compartments, AdaOS.Services;

with Footle_Bartle_Services;
use  Footle_Bartle_Services;

procedure Example_1
is
   Foobar: Footle_Bartle_Service_Access;

begin
   select
      delay 10.0; -- seconds
      if Foobar /= null then Foobar.Disengage; end if;
      -- inform the user that the operation timed out

   then abort
      Foobar := Footle_Bartle_Service.Find;
      --Foobar := Footle_Bartle_Service_Access (Task_Instance.Compartment.Services.Find (Footle_Bartle_Service_Id));

      if Foobar = null
      then
         -- Foobar not available

      else
         Foobar.Engage;
         --- use Foobar
         Foobar.Disengage;
      end if;
   end select;

exception
   when others =>
      if Foobar /= null then Foobar.Disengage; end if;
      raise;
end;
```

The whole operation must complete within 10 seconds, otherwise it will be timed out. Note how
careful we are to ensure that `Foobar` is disengaged, as soon as possible, no matter what
happens. This is vital. 

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
      Predictor := Weather_Predictor_Service_Access (Find_Service (Weather_Predictor_Service_Id, (1, 0)));
      if Predictor = null 
      then 
         raise Prediction_Failure with "Could not find weather prediction service."; 
      end if;
      Predictor.Engage;
      Predictor.Make_Forecast (Location, Clock + 1 * Day, Result));
      Predictor.Disengage;
      return Weather_Forecast'Image (Result);
exception
   when others =>
      if Predictor /= null then Predictor.Disengage; end if;
      raise;
end;
```






-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Service Dependencies

A _dependency circularity_ is where, for example, service `A` depends on service `B`, `B`
depends on `C`, and `C` in turn depends on `A`. 

Most services are implemented as [service programs](servprog.md), they are (mostly) started up
on demand. 

.....





-----------------------------------------------------------------------------------------------
## Service Proxies {#prox}

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
## Example






.....

For example, supposing we have a service `Foo.Bar`:



.....

```ada
package Foo
is
   type Bar is new AdaOS.Services.System_Service with private;
   -- public operations of Bar
private
   -- private stuff
end;
```

.....

```ada
package body Foo
is
   -- implementations of the public operations of Bar   
   Unibar: aliased constant Bar; -- singleton object of type Bar
begin
   AdaOS.Services.Register_Service ("acme.unibar", Unibar'Access);
end;
```

.....



-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
## Reference

[1]: <https://www.neff-home.com> "Neff"










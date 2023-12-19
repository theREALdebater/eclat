-----------------------------------------------------------------------------------------------
# Purpose of Services

Services create a decoupling of the parts of an overall large piece of software, so that .....

Whilst (object-oriented) classes, (Ada) packages, and (compilation) libraries provide static 
decoupling of software, services provide dynamic decoupling. 

.....



-----------------------------------------------------------------------------------------------
## Software Structuring

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



-----------------------------------------------------------------------------------------------
## Dynamicity 

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



-----------------------------------------------------------------------------------------------
## Dependency Injection {#di}

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

By configuring (or otherwise setting up or modifying) a compartment's service directory, the
services used by a program can be controlled, so that the overall execution of the program can
be modified in a dynamic manner. 



-----------------------------------------------------------------------------------------------
## Inversion of Control

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



-----------------------------------------------------------------------------------------------
## Middleware

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





-----------------------------------------------------------------------------------------------
## Network Computing

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


-----------------------------------------------------------------------------------------------
## Shims

The services of the host operating system, if there is one, can be made available to ECLAT 
programs by creating _shims_.

A shim is a service that acts as intermediary for one or more services, daemons, or other 
functions of the host operating system. 

.....








-----------------------------------------------------------------------------------------------
## Small Services

It will be typical for some services to be small. What a small service does may be very modest.
It may, for example, simply have a few parameterless functions that return certain key values. 

There may be a feeling that such small services are too small to be legitimate, and need to be
eliminated or perhaps rolled into some other bigger service. However, this is not necessarily
the best approach. Amalgamating small services can have the effect of complicating dependency
relationships. Ultimately, this could actually cause a serious problem, because some systems
cannot be configured without there being a dependency dilemma (where there is a circularity
that cannot be resolved by adjusting the configuration). 

Thus, small services may seem odd but they have their place. Let them be. 





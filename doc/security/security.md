-----------------------------------------------------------------------------------------------
# Security

.....




........

ECLAT supports the execution of programs on hosted [platforms](../pxcr/targets.md#plat), as 
well as on top of the AdaOS Native platform. 

In order to provide a consistent execution environment for all programs compiled using ECLAT, a 
single model of users and other security concepts---based on AdaOS---is presented to those 
programs; on hosted platforms, this model is mapped to the equivalent features of the 
underlying operating system, but any extra features of the hosted platform are also made 
available. 






..........



-----------------------------------------------------------------------------------------------
## Compartments

A [compartment](../rts/compart.md) is a security boundary, which conceptually contains the
system resources which are accessible to the execution of a [program](../rts/programs.md). It
also represents many (or all) aspects of the 'outside world' to the execution of the program. 

.....


-----------------------------------------------------------------------------------------------
## Principals (Users and Roles) {#princ}

In AdaOS, a _user_ is a single, discrete, real human being (not a bot, agent, or 
representative program of any kind). A user can 'own' other users, who are termed their 
_sub-users_. Any user has complete control over their sub-users. This, therefore, forms a 
hierarchy of users. 

The principals of a computer system form a [hierarchy](../intro/hier.md). 

In a similar way, any user can have _roles_. A role is exactly the same as a user, except that 
it does not represent an actual person, and there is no way to log in as a role. 

Users and roles are collectively known as _principals_. A principal has a certain set of 
_permissions_. How those permissions are decided could be very complex. 

When a [assembly](../rts/assemblies.md) is executed, it must be within a specific 
[compartment](#cmpts). 

.....

Although principals---both users and roles---are represented as [system 
objects](../objects/objects.md), they are special because each principal is also represented by 
an [identity](#ident), which has meaning at a lower level than system objects. 

Identities and authorities and are very fundamental entities in AdaOS. 



The abstract limited controlled private tagged types `Security_Principal`, `Security_Role`, and
`Security_User` are declared in the package `AdaOS.Security`.

`Security_Principal` is derived from `AdaOS.Objects.System_Object`. `Security_Role` and
`Security_User` are in turn derived from `Security_Principal`. 

When a [vicarious program](programs.md#vic) is started for a particular principal, the new 
program compartment that is created will be copied from the principal compartment of that 
principal. 

A principal compartment can be considered, in a sense, 'dead', because there is no program to 
be making changes to it. Of course, certain configuration programs (with the necessary 
permissions) can make changes to a principal compartment, and indeed they can create and 
delete principal compartments (normally, as a part of creating and deleting principals), but 
these changes will, relatively speaking, be very infrequent. 


### Top User {#topuser}

.....



-----------------------------------------------------------------------------------------------
## Identities {#ident}

An _identity_ is a token (a small opaque value, for example implemented as a 64-bit value). 

Within a single computer system, there is a one-to-one relationship between a principal and an
identity. 

In other words, an identity identifies a principal. 

An identity is a way of pinpointing who (perhaps a legal entity, such as a person or an 
organisation) did something (especially something of significance to security). 

Identities are used in [accounting and limiting](accounting.md). 

Note that identities do *not* form a hierarchy, even though principals do. 



-----------------------------------------------------------------------------------------------
## Authorities {#auth}

An _authority_ is, just like an [identity](#ident), a token (a small opaque value, for example 
implemented as a 64-bit value). 

An authority effectively imposes a set of [permissions](#perm) on a [compartment](#cmpts). 

Every call that a task makes to a [secure object](#secobj) must _cite_ an authority (and an
identity), and the object uses that authority to check that the caller has permission to
perform the operation. 

Authorities form a [hierarchy](../intro/hier.md). 

Any permission granted to a particular authority will always also be granted to any of its
superiors. 

The top authority is never denied any permission; indeed all secure objects should simply
bypass all permission checks if the top authority is cited. 



-----------------------------------------------------------------------------------------------
## Compartments {#cmpts}

Every [compartment](../rts/compart.md) is associated with a [principal](#princ). The principal
is called the compartment's _owner_. 

When a principal compartment is created, by a task whose compartment's owner is `P1`, the new
compartment is given an owner `P2`. Normally `P2` is a new and different principal to `P2`, but
`P2` must be either the same as `P1` or an inferior of `P1`. 

Whenever a [program](../rts/programs.md) is executed, by a task whose compartment's owner is
`P`, this results in the creation of a new program compartment. The owner of this new
compartment is also `P`. 


### Ambit {#amb}

Every compartment has an explicit set of [authorities](#auth), called the _primary ambit_. The
downward closure of the primary ambit and their inferiors is called the compartment's _ambit_. 

The compartment's ambit may not be the same as the owner's ambit, but it will never include an
authority which is not in the owner's ambit. 


### Base Authority {#baseauth}


One of the authorities in the compartment's ambit is configured to be the _base authority_ of the 
compartment. If there is only one authority in the ambit, that is automatically the default, 
otherwise a default authority must be explicitly configured. 

......

When the system boots, a compartment is created called the _top compartment_ ......

......

The procedure `Start` and whose first parameter is of type `System_Program`, declared in the package `AdaOS.Execution`, creates a new program
compartment as a sub-compartment of a specific compartment (whose default is the compartment of 
the calling executional instance). 

The procedures `Create_User` and `Create_Role` create a new principal and return an object 
that is of a type derived from both `Security_Principal` and `Principal_Compartment`. 

.....

The function `Top_Compartment`, declared in the package `AdaOS.Security`, returns (a remote access value 
referencing) the top compartment. 



-----------------------------------------------------------------------------------------------
## Security Modes {#secmode}

All of the system configuration that is of significance to security is converted into a 
[module](../pxcr/modules.md) ............




In general, whenever anything of significance to security is modified in a system, the system must be re-
realised in order to ........





However, it is possible to configure two or more different _security modes_. 

The whole system can be dynamically (and rapidly) switched into a different security mode ........

On a hosted platform, .....


..... security modes supported, .....


The package `System.Security` contains .....

```ada
   type Security_Mode is (Default, Preliminary_Lockdown, Full_Lockdown);

   function Current_Security return Security_Mode;

   procedure Set_Security (Mode: in Security_Mode);
```

At the moment the three security modes supported are:

 * normal (`Default`); 

 * preliminary lockdown; 

 * full lockdown. 

The intention of these modes is explained in the following subsections. 

However, it is anticipated that different users will have a need for a different set of modes. 

.....


### Normal Security Mode

This is the mode that the system will operate in if there is no security threat pending. 

In this mode, permissions strike a balance between:

 * allowing principals to carry out legitimate activities unhampered; 

 * preventing principals carrying out illegitimate, threatening actions. 


### Preliminary Lockdown 

Enter this mode when a security threat (e.g. a hacking attempt) is suspected but not confirmed. 

In this mode, permissions are rescinded that allow activities which are not vital or which can
readily wait until the preliminary lockdown is lifted. 

Investigate the potential threat, and exit this mode (back to normal) if the threat remains
unconfirmed for a certain period of time (e.g. one hour). 


### Full Lockdown

Enter this mode when a security threat is confirmed to be underway. 

In this mode, all permissions are rescinded except those permissions which allow counteraction
of the threat: 

 * Detect malware. Remember that more than once form of malware may be a part of the current
   threat. Beware, there may be red herrings left by the remnants of previous malware or
   attacks. 

 * Detect illegitimate users who are part of the threat. Usually, any user detected should be
   [disabled](?????) and [evicted](?????) immediately. 

 * Gather and preserve evidence suitable for presentation to police and in court. .....

Users will not be able to go about their daily activities during this mode, so it should be
exited (back to preliminary lockdown or normal) as soon as the threat is no longer pending. 






-----------------------------------------------------------------------------------------------
## Secure Objects {#secobj}

Any remote type that is marked with the aspect `Secure_Object` is a _secure object type_, as is
any type derived from it. 

This aspect is non-standard and currently only supported by the ECLAT compiler. Nevertheless,
the entire security architecture of AdaOS has been designed so that it will compile and work on
any standard Ada compiler, but without effective [enforcement](#enf). 





Any access type (which must be a remote access type) that designates a secure object type is a
_secure access type_. 



The type `System_Object` is marked with the aspect `Secure_Object`, and is therefore a secure
object type. All system objects are secure objects. 













-----------------------------------------------------------------------------------------------
## Identity and Authority {#ca}

Every task and every secure object is associated with an _identity_ and an _authority_. 


### Tasks

When a task is created, its identity and authority are both automatically initialised to the
identity and authority, respectively, of the task creating it. 

A task can change its the identity and/or the authority of itself or another task at any time
during its execution, but these changes may be subject to [enforcement](#enf). 

The package `AdaOS.Security` contains declarations of the packages `Task_Principal` and
`Task_Authority`. 

Calling the function `Task_Principal.Value` returns the identity of the calling task. This
function can be passed one parameter, of type `Ada.Task_Identification.Task_Id`, to retrieve
the value for any task. 

Calling the function `Task_Authority.Value` returns the authority of the calling task. This
function, again, can be passed one parameter, of type `Ada.Task_Identification.Task_Id`, to
retrieve the value for any task. 

The procedure `Task_Principal.Set_Value` can be called to set the identity of the calling task.
Its first parameter is the new value. It can be passed a second parameter to set the value for
any task.

The procedure `Task_Authority.Set_Value` can be called to set the authority of the calling
task. Its first parameter is the new value. It can be passed a second parameter to set the
value for any task.


### Secure Objects

The ECLAT compiler implements two attributes for secure object types that are functions. These
attributes are non-standard, and are currently (as of April 2022) not implemented by any other
Ada compiler. Therefore, their use should be encapsulated in program units (e.g. packages) that
will reduce the effort of porting programs to and from ECLAT. 

Every non-abstract secure object type `T` implements the following primitive operations: 

For every object `X` of a non-abstract secure object type `T`, the following primitive
operations are defined, as if declared: 

    function T'Principal (Object: in T) return Boolean;

This function returns the identity of `Object`. The default implementation of this function
returns the result of calling `Task_Principal` (with no actual parameter, so that it applies to
the calling task). 

    function T'Authority (Object: in T) return Boolean;

This function returns the authority of `Object`. The default implementation of this function
returns the result of calling `Task_Authority` (with no actual parameter, so that it applies to
the calling task). 

These subprograms can be overridden, and typically will be. 


### System Objects

The type `AdaOS.Objects.System_Object` is a secure object type, and overrides `T'Principal` and
`T'Authority`. 

System objects store an identity and authority. The overridden `T'Principal` returns the stored
identity. The overridden `T'Authority` returns the stored authority. 

The following primitive operations are defined: 

    procedure Set_Principal (Object: in out System_Object; 
                             Id:     in     Principal_Id);

The procedure `Set_Principal` sets the identity of `Object`. 

.....


    procedure Set_Authority (Object:    in out System_Object; 
                             Authority: in     System_Authority);

The procedure `Set_Authority` sets the stored authority of `Object`

If the given `Authority` is not in the ambit of the compartment of the instance of the given
`Task`, the exception `Security_Violation` is propagated. 



.....






-----------------------------------------------------------------------------------------------
## Enforcement {#enf}

The ECLAT compiler automatically performs certain extra actions the enforce security. These
extra actions are all triggered by the non-standard aspect `Secure_Object`, and they are
modified by the aspect `Endorse`. Currently (as at April 2022), all other Ada compilers will
ignore these aspects, and therefore not perform the actions to enforce security; however, for a
system that does nothing to violate security, everything will compile and behave exactly the
same way. 



A primitive operation of a secure object can have the Boolean aspect `Endorse` applied to it.
In this case, the operation is said to be _endorsed_, otherwise it is _non-endorsed_. 



When a task makes a call to a primitive operation of a secure object, the task automatically
_cites_ its identity and authority. 

If the primitive operation is non-endorsed, a check is automatically made that the identity and
authority of the secure object are equal, respectively, to the cited identity and authority. If
they are not the exception `Security_Violation` is propagated to the caller (and the call is
not executed). 

If the primitive operation is endorsed, no automatic check is made. The operation is expected
to explicitly perform [checks of it own](endorse.md). 




The object might use the cited identity in an [audit log](../events/logging.md#aud) relating 
to the call, for example. 
















.....


### Example

```ada





```








......



-----------------------------------------------------------------------------------------------
## Permissions {#perm}

A fundamental concept in security is of _permissions_. 




...... denial by default ......

........









..... `AdaOS.Security` .....



-----------------------------------------------------------------------------------------------
## Security Violations {#viol}

The exception `Security_Violation`, declared in the package `AdaOS.Security`, is propagated
whenever one of the AdaOS security checks encounters one of the following situations: 

 * .....
 
 * 

 * 

 * 

 * 





In practice, the ambit of a compartment can change dynamically and unpredictably. 



-----------------------------------------------------------------------------------------------
##






-----------------------------------------------------------------------------------------------
##






-----------------------------------------------------------------------------------------------
## References

[1]: <https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=xacml> "OASIS eXtensible 
     Access Control Markup Language (XACML) TC"










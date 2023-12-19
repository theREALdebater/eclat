-----------------------------------------------------------------------------------------------
# Security

ECLAT supports the execution of programs on hosted [platforms](../pxcr/targets.md#plat), as 
well as on top of the AdaOS Native platform. 

In order to provide a consistent execution environment for all programs compiled using ECLAT, a 
single model of users and other security concepts---based on AdaOS---is presented to those 
programs; on hosted platforms, this model is mapped to the equivalent features of the 
underlying operating system, but any extra features of the hosted platform are also made 
available. 

The non-standard package `System.Security` is currently only supported by the ECLAT compiler.
Nevertheless, the entire security architecture of AdaOS has been designed so that, if any Ada
program has no direct dependency on the non-standard package `System.Security`, it will compile
and work correctly on any standard Ada compiler, but without any effective security
[enforcement](#enf). 

In addition to the package `System.Security`, which contains the declarations of a low-level
set of security-related entities, there is also the package `AdaOS.Security`, which contains
higher-level entities, such as security principals, for example. Be careful not to confuse
these two packages. 



-----------------------------------------------------------------------------------------------
## Principals (Users and Roles) {#princ}

In AdaOS, a _user_ is a single, discrete, real human being (not a bot, agent, automaton, or
autonomous program of any kind). 

However, there are also _roles_. A role is exactly the same as a user, except that it does not
represent an actual person, and there is no way to [log in](login.md) as a role. 

Users and roles are collectively known as _principals_. A principal has a certain set of 
_permissions_. How those permissions are decided could be very complex. 

The limited interface type `Security_Principal` is declared in the package `AdaOS.Security`: 

```ada
type Security_Principal is limited interface and AdaOS.Objects.System_Object;
```

Although principals---both users and roles---are [system objects](../objects/objects.md), they
are special because each principal is also represented by an [identity](#ident), which has
meaning at a lower level than system objects. 

Identities and authorities and are very fundamental entities in AdaOS. 


### Roles

The limited interface type `Security_Role` is declared in the package `AdaOS.Security.Roles`: 

```ada

type Security_Role is limited interface and Security_Principal;
```

### Users

The limited interface type `Security_User` is declared in the package `AdaOS.Security.Users`: 

```ada

type Security_User is limited interface and Security_Role;
```

The type `Security_User` is derived from `Security_Role` because a user will have all the same
operations as a role, but it will have some additional operations too. 


### Ownership {#owner}

Every principal except one has an _owner_, a read-only [property](../intro/intro.md#prop) named
`Owner`. The owner is also a principal. The owner _owns_ the principal.

Note that this 'ownership' of users is not intended to infer anything regarding the
corresponding relationship between actual people. It serves a vital purpose in enabling the
management of the users of one system to be distributed among multiple users, rather than
requiring one user to manage all the users of a system. 

Principals form a [hierarchy](../intro/hierarchy.md). A principal can _own_ other principals,
who/which are termed their _sub-principals_. A sub-principal that is a user can also be termed
a _sub-user_, and a sub-principal that is a role can also be termed a _sub-role_. 

The package `AdaOS.Security` contains the following visible declaration: 

```ada

function Owner (Principal: not null access Security_Principal) 
return
   access Security_Principal'Class is abstract;
```

The function `Owner` returns the owner of a principal, or it returns null if the principal has
no owner. (There is only one principal, the [top user](#topuser), that has no owner.) 

Any principal has complete control over its inferior principals. For any permission that a
principal has, all of its superior principals also implicitly have that permission. 



-----------------------------------------------------------------------------------------------
## Top User {#topuser}

Every [effective system](../intro/intro.md#effsys) has exactly one principal, a user, called
the _top user_, which is very special. The top user is the one and only principal in the
effective system which does not have an owner. 

The top user is therefore at the top (hence the name) of the hierarchy of principals. It is of
a type derived from `Security_User`, and therefore has the function `Owner`, but, for the top
user only, `Owner` returns null. 

.....







-----------------------------------------------------------------------------------------------
## Secure Objects {#secobj}

Any remote type that is derived from the limited interface type `Secure_Object`, declared in
the package `System.Security`, is a _secure object type_. Objects (Ada objects) that are of a
secure object type are _secure objects_. 

The package `System.Security` contains the following declarations:

```ada

type Secure_Object is limited interface;
```

Any [remote access type][2] that designates a secure object type is a _secure access type_. 

[System objects](../objects/objects.md) are secure objects, because the type `System_Object` is
derived from the type `Secure_Object`, and is therefore a secure object type. 



-----------------------------------------------------------------------------------------------
## Authorities {#auth}

An _authority_ is a token (a small opaque value, for example implemented as a 64-bit value). 

An authority indirectly imposes [permissions](#perm) on a [task](../pxcr/tasks.md). In
principle (pardon the pun), the task is allowed to do something if its authority permits it. 

Every call that a task makes to a [secure object](#secobj), if built by the [ECLAT
compiler](../eclat/eclat.md), implicitly _cites_ an authority (as well as an identity, q.v.),
and the object uses that authority to check that the caller has permission to perform the
operation, and possibly also uses the authority to qualify its execution of the operation. 

Authorities form a [hierarchy](../intro/hier.md). 

The package `System.Security` contains the following declarations: 

```ada

Security_Authority_Last: constant Natural with Import;

type Security_Authority is new Natural range 0 .. Security_Authority_Last;

No_Authority:  constant Security_Authority := 0;
Top_Authority: constant Security_Authority := 1;

function Super (Authority: in Security_Authority) return Security_Authority with Import;

function Is_Superior (Authority, Other: in Security_Authority) return Boolean with Import;

function Is_Inferior (Authority, Other: in Security_Authority) return Boolean with Import;
```

The value of `Security_Authority_Last`, as well as the implementations of the functions
`Super`, `Is_Superior`, and `Is_Inferior`, are imported from the 
[system configuration pseudo-module](../config/sysconfig.md). 

Therefore, changing the authorities and their relationships requires a
[re-realisation](../pxcr/realizor.md). 

The top authority is represented by the constant `Top_Authority`. The constant `No_Authority`
is used to indicate that there is no authority in a certain situation. It can be assumed that
`No_Authority` will never be granted permission to do anything. 

The function `Super` returns the super-authority of a given `Authority`. If the given
`Authority` is the top authority, this function returns `No_Authority`. 

Any permission, in principle, granted to a particular authority will always also be implicitly
granted to any of its superiors. There is no sensible way to ensure that this principle is
always adhered to; it is up to the programmers of secure objects to do their best to ensure
this principle is always upheld. 

The top authority is never denied any permission; indeed all secure objects should simply
bypass all permission checks if the top authority is cited. Any [guardian](guardians.md) will
itself automatically bypass all permission checks if the top authority is cited. 

.....





-----------------------------------------------------------------------------------------------
## Ambits {#amb}

An _ambit_ is a set of [authorities](#auth). If an authority is in an ambit, it is a _member_
of that ambit. 

An ambit is specified by a set of _explicit members_. The full set of members of the ambit
comprises (the downward closure of): 

 * the explicit members and 
 
 * the inferior authorities of all the explicit members. 

In other words, an authority is a member of an ambit if it is an explicit member of the ambit
or if it is an inferior authority of an explicit member of the ambit. 

The package `System.Security` contains the following declarations: 

```ada

type Security_Ambit is private;

Empty_Ambit: aliased constant Security_Ambit;
Top_Ambit:   aliased constant Security_Ambit

function "=" (Left, Right: in Security_Authority) return Boolean;
function "in" (Authority: in Security_Authority; Ambit: in Security_Ambit) return Boolean;

function Subordinates (Authority: in Security_Authority) return Security_Ambit;
function Inferiors    (Authority: in Security_Authority) return Security_Ambit;

Ambit_Identifier_Last: constant Natural with Import;

type Ambit_Identifier is new Natural range 0 .. Ambit_Identifier_Last;

function Ambit (Id: in Ambit_Identifier) return Security_Ambit;
```

An ambit (a set of authorities) is represented by the private type `Security_Ambit`. A set of
ambits is available, each of which is identified by a value of the type
`Security_Ambit_Identifier`.

The `Empty_Ambit` has no members, and can therefore be assumed to not imply any permission to
do anything. The `Top_Ambit` has one member, the `Top_Authority`, and can therefore be assumed
to have permission to do anything. 

The value of `Security_Ambit_Identifier_Last`, as well as the implementation of the function
`Ambit` are imported from the [system configuration pseudo-module](../config/sysconfig.md). 

Therefore, changing the ambits (the actual sets of authorities) available for use requires a
[re-realisation](../pxcr/realizor.md). 

The ambits never have any redundant members. An authority that is an inferior of another member
is redundant. `No_Authority` cannot be a member of any ambit (it wouldn't make any sense). 


### Ambit of a Principal

......


### Ambit of a Compartment

............


### Default Authority {#defauth}

One member of a compartment's ambit is specified as the _default authority_. 

.........



-----------------------------------------------------------------------------------------------
## Identities {#ident}

An _identity_ is, just like an [authority](#auth), a token (a small opaque value, for example
implemented as a 64-bit value). 

Within a single [effective system](../intro/intro.md#effsys), there is a one-to-one
relationship between principals and identities. There is exactly one identity for each
principal, and exactly one principal for each identity. 

In other words, an identity identifies a principal. However, identities operate at a lower
level than principles (indeed a lower level than system objects). 

An identity is a way of pinpointing who (perhaps a legal entity, such as a person or an
organisation) did something (especially something of significance to security or accounting). 

Identities are used in [accounting and limiting](accounting.md) and
[auditing](../events/auditing.md). 

Identities form a [hierarchy](../intro/hier.md). There is one identity, the _top identity_, at
the top of this hierarchy. Every other identity has one identity that is its _super-identity_.
The top identity is the only identity that does not have a super-identity. 

The identity hierarchy exactly matches the principal hierarchy: if any identity I has a
super-identity S, then the principal of S will always be the owner of the principal of I. 

Every identity is associated with an [ambit](#amb). 

The package `System.Security` contains the following declarations: 

```ada

Security_Identity_Last: constant Natural with Import;

type Security_Identity is Natural range 0 .. Security_Identity_Last;

Top_Identity: constant Security_Identity := 0;
```

The value of `Security_Identity_Last`is imported from the 
[system configuration pseudo-module](../config/sysconfig.md). 

Therefore, changing the identities requires a [re-realisation](../pxcr/realizor.md). 

The `Top_Identity` will be the identity of the [top user](#topuser). 


### Ambit of an Identity

The package `System.Security` contains the following declarations: 

```ada
function Ambit_Id (Identity: in Security_Identity) return Ambit_Identity
with 
   Import;

function Ambit (Identity: in Security_Identity) return Security_Ambit 
with 
   Import;
```

One of the (authorities that are the) members of an identity's ambit is designated the _default
authority_ of the identity. 

```ada
function Default_Authority (Identity: in Security_Identity) return Security_Authority
with 
   Import;
```

The functions `Ambit_Id`, `Ambit` and `Default_Authority` are imported from the 
[system configuration pseudo-module](../config/sysconfig.md). 

Therefore, changing the ambit of each identity, and the default authority of each identity,
requires a [re-realisation](../pxcr/realizor.md). 



-----------------------------------------------------------------------------------------------
## Engagement Identity and Authority {#eiea}

Every task is associated with an [identity](#ident) and a [current authority](#auth). 

When a [secure object](#secobj) is [engaged](engaging.md), it has two
[properties](../intro/intro.md#prop) named `Engagement_Identity` and `Engagement_Authority`,
which are the engagement identity and engagement authority of the object during the engagement. 

Every task and every secure object is indirectly associated with an [ambit](#amb), which is the
ambit of its (engagement) identity. 


### Tasks

The package `System.Security` contains declarations of the 
[task attributes](../adaos/taskattr.md) `Task_Authority` and `Task_Identity`. 

When a task is created, its identity and current authority are both automatically initialised
to the identity and authority of the task creating it. 

A task can change the identity and/or the current authority of itself or another task at any
time, but if a task A changes either of these of another task B, then B must be dependent on A;
if it is not, the exception `System.Security.Security_Violation` is propagated. 


### Secure Objects

A [secure object](#secobj) can be [engaged](engaging.md). 

......












-----------------------------------------------------------------------------------------------
## Enforcement {#enf}

When it builds a module (of any kind), the ECLAT compiler automatically generates code that
performs certain extra actions that enforce security during the execution of (any machine code
generated from) the module. These extra actions are all triggered by any call of a primitive
operation of a secure object (of a type derived from `Secure_Object`), but the actions are
modified by the aspect `Endorse`. 

Currently (as at 2023), all other Ada compilers will ignore the aspect `Endorse` and do not
implement the package `System.Security`, and will therefore not perform any actions to enforce
security; however, for a system that does nothing to violate security, everything will compile
and behave exactly the same way. 





A primitive operation of a secure object can have the Boolean aspect `Endorse` applied to it.
In this case, if the `Endorse` aspect is `True`, the operation is said to be _endorsed_,
otherwise it is _non-endorsed_. 



When a task makes a call to a primitive operation of a secure object, the task automatically
_cites_ its identity and current authority. 

If the primitive operation is non-endorsed, a check is automatically made that the identity and
authority of the secure object are equal, respectively, to the cited identity and authority. If
they are not, the exception `Security_Violation` is propagated to the caller (and the call is
not executed). 

If the primitive operation is endorsed, no automatic check is made. The operation is expected
to explicitly perform [checks of it own](endorse.md). 







.....


### Example

```ada





```



-----------------------------------------------------------------------------------------------
## Vicarious Execution {#vic}

Generally, a new task's inheritance of its identity and current authority from the task
allocating it is the behaviour required. Quite often the authority of a task is changed, as the
task performs activities that represent different 

.....




The package `System.Security` contains declarations of subprograms to set the
[authority](../adaos/taskattr.md#auth) and [identity](../adaos/taskattr.md#ident) of a task. 




### Advice

If a piece of software is going to change both the identity and authority of a task, it should
(probably) change the identity first, since this has consequences for the ambit available for
the setting of the authority. 





......

A [compartment](../adaos/compart.md) is a security boundary, which conceptually contains the
system resources which are accessible to the execution of a [program](../adaos/programs.md). It
also represents many (or all) aspects of the 'outside world' to the execution of the program. 

.....


Every compartment is a [system object](../objects/objects.md), so it is a [secure
object](#secobj), associated with a [principal](#princ) that is the compartment's owner. 




When a [vicarious program](programs.md#vic) is started for a particular principal, the new 
program compartment that is created 
is normally initialised by calling the `Initialize_Compartment` method of the principal. 






### Ambit {#amb}

Every compartment and principal has an explicit set of [authorities](#auth), called the
_primary ambit_ of the compartment or principal. The downward closure of the primary ambit and
their inferiors is called the compartment's or principal's _ambit_. 

A compartment's primary ambit may not be the same as its owner's ambit, but it will never
include an authority which is not in the owner's primary ambit. 

The method `Initialize_Compartment` of a principal can be expected to add all the authorities
in the principal's primary ambit to the new compartment's primary ambit, so giving it the same
ambit as the principal. 



????? Or are ambits fixed?  Could a compartment or principal have a 'current ambit'?



Authorities could always be removed. But note that a compartment with an empty ambit (no
authorities) will not have permission to do anything. 


### Default authority {#defauth}

One of the authorities in the compartment's ambit is configured to be the _default authority_ of the 
compartment. If there is only one authority in the ambit, that is automatically the default, 
otherwise a default authority must be explicitly configured. 

......

When the effective system boots, a compartment is created called the _top compartment_ ......

......






????? They are fixed, so no

The procedure `Create_User` creates a new user and returns (a remote access value referencing)
an object that is of a type derived from `Security_User`. The procedure `Create_Role` creates a
new role and returns (a remote access value referencing) an object that is of a type derived
from `Security_Role`. 

.....








?????

The function `Top_Compartment`, declared in the package `AdaOS.Security`, returns (a remote
access value referencing) the top compartment. 













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





In practice, the ambit of a compartment can change dynamically and unpredictably. ????? is that true? No, don't think so



-----------------------------------------------------------------------------------------------
##






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


### Default Security Mode

This is the mode that the system will operate in if there is no security threat pending. 

In this mode, permissions strike a balance between:

 * allowing principals to carry out legitimate activities unhampered; 

 * preventing principals carrying out illegitimate, threatening actions. 


### Preliminary Lockdown 

Enter this mode when a security threat (e.g. a hacking attempt) is suspected but not confirmed. 

In this mode, permissions are rescinded that allow activities which are not vital or which can
readily wait until the preliminary lockdown is lifted. 

Investigate the potential threat, and exit this mode (back to default) if the threat remains
unconfirmed for a certain period of time (e.g. one hour). 


### Full Lockdown

Enter this mode when a security threat is confirmed to be underway. 

In this mode, all permissions are rescinded except those permissions which allow counteraction
of the threat: 

 * Detect malware. Remember that more than one form of malware may be a part of the current
   threat. Beware, there may be red herrings left by the remnants of previous malware or
   attacks. 

 * Detect illegitimate users who are part of the threat. Usually, any user detected should be
   [disabled](?????) and [evicted](?????) immediately. 

 * Gather and preserve evidence suitable for presentation to police and in court. .....

 * Be careful not to end up making the situation worse during your investigations and ensuing
   actions. A good doctor does not harm the patient. 

Users will not be able to go about their daily activities during this mode, so it should be
exited (back to preliminary lockdown or normal) as soon as the threat is no longer pending. 

Keep all the information and evidence you have gathered, but securely. You do not want to give
away information to hackers that will aid them making another attack. 




-----------------------------------------------------------------------------------------------
##




The service named [Quis](../services/quis.md) provides a means for users to log in. 



-----------------------------------------------------------------------------------------------
## References

[1]: 
   <https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=xacml> 
   "OASIS eXtensible Access Control Markup Language (XACML) TC"

[2]: 
   <http://ada-auth.org/standards/22rm/html/RM-E-2-2.html> 
   "Ada Reference Manual: E.2.2 Remote Types Library Units"








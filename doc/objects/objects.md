-----------------------------------------------------------------------------------------------
# System Objects

A _system object_ is an AdaOS entity that has state and behaviour. These are the very objects
of the 'AdaOS' semi-acronym, short for 'Ada Object System'. 

A system object is represented by an object (an Ada object) of a type derived from the limited
interface type `System_Object`, which is declared in the package `AdaOS.Objects`. 

```ada

type System_Object is limited interface and System.Security.Secure_Object;
```

The package `AdaOS.Objects` is categorised as a [remote types][1] library unit. The type
`Object_Access`, declared in this package, designates `System_Object'Class`, and is therefore a
remote access type. 

Before a system object can be used, it generally has to be [engaged](../security/engaging.md). 

Collections of system objects can be kept in [object containers](objcont.md). 

This section describes how the majority of system objects behave. Those types of system object
that deviate from this norm must carefully document how they differ, and how they should be
used. 



-----------------------------------------------------------------------------------------------
## System Object Owner {#own}

System objects are [secure objects](../security/security.md#secobj). 

Every system object therefore has a read-only property `Owner`. 

```ada
function Owner (Object: not null access System_Object) 
return
   access Security_Principal'Class is asbtract;
```



-----------------------------------------------------------------------------------------------
## Ephemeral and Non-System Objects {#eph}

An _ephemeral object_ is a remote object that does not persist: when it .....

Therefore, an ephemeral object can *never* be a system object, and a system object can never be
an ephemeral object. 

It is possible for a remote object that is not a system object to be persistent, by some 
mechanism (it really doesn't matter what). These are termed _non-system objects_, so as to 
distinguish them in any discourse that requires a distinction to be made. 

...........



-----------------------------------------------------------------------------------------------
## Factory Objects {#fact}

A  _factory object_ is a system object which is able to create or give access to other objects,
called _product objects_ of the factory. 

.....

It is typical for product objects to be [ephemeral](#eph), but not a requirement. 





It is a particularly useful technique for system objects to act as factories, because system
objects become exclusive when engaged. In general, only one task can access a system object at
any one time (having engaged it). 

The typical pattern of usage is to engage a factory object for a very short period of time,
just enough to request it to create a product object. The factory object is then quickly
disengaged, allowing other tasks to engage and use it with minimal delay. The product object,
on the other hand, may be (held engaged and) used for as long as needed. 

.....





Many system objects will be hybrids, that serve as factory objects but also serve other 
significant purposes as well. 

.....


### Example

For example, the type `System_File` (declared in the package `AdaOS.Objects.Files` and derived
from `System_Object`) represents a file and itself provides primitive operations for the
reading and writing of the file's data. 

This object and its operations are suitable for exclusive read/write access to the file. 

However, the object is also a factory. It provides a primitive operation which returns an 
ephemeral product object, called a _reader_, that provides operations to read the file. The 
reader is suitable for non-exclusive read-only access to the file. 

For more information see .....







-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Object Identifiers {#oid}

Every [saved state](#state) is permanently associated with one _object identifier_, or _OID_,
that uniquely identifies the state within a [domain](../security/domains.md), permanently and
forever (in effect). 

An OID is a small opaque token (value) of a fixed size. 

The type `Object_Id` is declared in the package `AdaOS.Objects` ......

The constant `No_Object` is a value of type `Object_Id` that does not identify any saved state,
and is used to signify various situations where an `Object_Id` is expected but no state can be
identified. 



.....

A [vacant](#state) system object has no OID associated with it by default, but it can be set.
When a vacant system object has an OID set, it is intended to mean that the system object will
have that OID when it is loaded (i.e. when a saved state is loaded, its OID will be that
value). 

System objects have an `OID` property: 

```ada

function OID (Object: not null access System_Object) return Object_Id is asbtract;

function Set_OID (Object: not null access System_Object; 
                  OID:    in Object_Id) is asbtract;
```

.....

The function `OID` returns:

 * if the object has a state loaded, the OID of the loaded state; 

 * if the object is vacant and has no OID set, `No_Object`; 

 * if the object is vacant and has an OID set, that OID. 

A system object's OID can only be set when the system object is vacant. Attempting to do so
when the system object is loaded propagates the exception `Status_Error`. 

Setting the OID of a vacant system object when its loader has (already) been set causes an
immediate automatic load of the system object. 

..... [Object Identifier Allocation](alloc.md#oid) .....



........

An _object set_ is a set of object identifiers, and is represented by the type
`Object_Sets.Set`. The package `Object_Sets` is declared in the package `AdaOS.Objects`: 

```ada

package Object_Sets is new Ada.Containers.Sets (Object_Id);
```




-----------------------------------------------------------------------------------------------
## Persistence and Object State {#state}

A system object has _state_ and _behaviour_: 

 * the actions (if any) and results (if any) of any method of the system object depends on the
   system object's behaviour (and nothing else); 

 * the behaviour of a system object depends on its type (Ada type) and its state (and nothing
   else); 

 * the state of a system object can change from time to time. 

System objects are _persistent_. The state of a system object is able to survive the end of the
existence of the system object. A system object (the same one, or a different one of the same
type) can take that same state again. 

.....

How the persistence of a particular type of system object is achieved is not carved in stone,
but it will normally involve its state being stored somewhere. In this stored form, the state
is called a _saved state_. The saved state can be read, at a later time, in order to restore a
system object to that state. 

Very often the state is saved into a file, in which case the file is called a
[saved state file](../config/config.md#state). 

......

When a saved state is not being kept and updated by any system object, the saved state is said
to be _dormant_. When a saved state is restored into some system object, the saved state is
said to be _awake_. 

Concomitantly, when a system object has no state, the system object is said to be _vacant_.
When some saved state is restored into a system object, or if a new state has been created, the
system object is said to be _loaded_. 

Normally, an awake saved state should only ever be restored into (so being kept and updated by)
one system object at any one point in time. Certainly, a loaded system object can only
represent a single saved state at any one point in time. 

......

```ada
function Is_Loaded (Object: not null access System_Object) return Boolean is abstract;
```

The function `Is_Loaded` returns `True` if a given system object, `Object`, is loaded, or
`False` if it is vacant. 

Beware of race conditions when using this function .....



A system object that is vacant cannot be [engaged](#eng). A call to `Engage` of a vacant system
object is blocked until the system object becomes loaded. 


.....



All system objects implement procedures `Save`, `Load`, `Create` and `Delete`. 

```ada
procedure Save (Object: not null access System_Object;
                Stream: not null access Root_Stream_Type'Class) is abstract;
```

The procedure `Save` persists the state of a loaded system object, making the saved state
change from awake to dormant, and making the system object change from loaded to vacant. 

If the system object is already vacant (function `Is_Loaded` returns `False`), the exception
`Status_Error` is propagated. Otherwise, the current state of the system object is written into
the given [stream][4], and is then changed to being vacant (function `Is_Loaded` will return
`False`). 

See [Configuration](../intro/config.md) for more information as to how to write a saved state
into a stream, as well as how to be able to create and update saved state files. 

```ada
procedure Load (Object: not null access System_Object;
                Stream: not null access Root_Stream_Type'Class) is abstract;
```

The procedure `Load` awakens a dormant saved state, and makes a vacant system object loaded. If
the system object is already loaded (function `Is_Loaded` returns `True`), the exception
`Status_Error` is propagated. Otherwise, the state of the system object is updated by being
read from a given stream. Then the object is changed to being loaded (function `Is_Loaded` will
return `True`).

```ada
procedure Create (Object: not null access System_Object;
                  Owner:  not null access Security_Principal'Class) is abstract;
```

The procedure `Create` causes a vacant system object to become loaded, initialised with a
default state. The new state can be considered to represent an awake saved state, even though
it has never (yet) been saved. If the system object is already loaded (function `Is_Loaded`
returns `True`), the exception `Status_Error` is propagated. Otherwise, the object's state
becomes a default state, and the object is changed to being loaded (function `Is_Loaded` will
return `True`). 

The default state will depend on the type of the system object. 

The owner of the new saved state will be `Owner`. If the given `Owner` is not the same as or an
inferior principal of the calling task's owner, then the exception `Security_Violation` is
propagated (and a new saved state is not created). 

```ada
procedure Delete (Object: not null access System_Object) is abstract;
```

The procedure `Delete` causes an awake saved state to cease to exist (in whatever way makes
sense for that saved state), making the system object change from being loaded to being vacant.
If the object is already vacant (function `Is_Loaded` returns `False`), the exception
`Status_Error` is propagated. Otherwise, the system object is changed to being vacant (function
`Is_Loaded` will return `False`). 

If the saved state is stored in a file, deleting it will normally cause the file to be deleted. 

.....

When a system object is allocated, it is initially vacant. In order to become loaded, it must
either be loaded (by a call to procedure `Load`) or created (by a call to procedure `Create`).
For a loaded system object to become vacant, it must either be saved (by a call to procedure
`Save`) or deleted (by a call to procedure `Delete`). 

.....


### Loaders and Savers

A system object has, as properties, a _loader_ and/or a _saver_; their values are access-to-
subprogram values, so each can be null to indicate that it is not set; this (being not set) is
the default. 

Each loader and saver is a function that returns (an access value that references) a stream,
given an object identifier. For a saver, the stream is an output stream that can be written
into to save the object. For a loader, the stream is an input stream that can be read to load
the object. 

The access-to-subprogram types `Object_Loader` and `Object_Saver` are declared in the package
`AdaOS.Objects`. 

```ada

type Object_Saver is access function (OID: in Object_Id) 
return 
   access Root_Stream_Type'Class;

type Object_Loader is access function (OID: in Object_Id) 
return 
   access Root_Stream_Type'Class;
```

Because `AdaOS.Objects` is a remote types package, these types are both
remote-access-to-subprogram types. 

When called, if the loader or saver is unable to find the given object identifier, or cannot
return a stream for the identified object, it returns null. 

There are subprograms of `System_Object` that allow its loader and saver to be set and
retrieved. 

```ada

function Saver (Object: not null access System_Object) return Object_Saver is abstract;

procedure Set_Saver (Object: not null access System_Object;
                     Saver:  in Object_Saver) is abstract;

function Loader (Object: not null access System_Object) return Object_Loader is abstract;

procedure Set_Loader (Object: not null access System_Object;
                      Loader: in Object_Loader) is abstract;
```

Setting the loader of a vacant system object's when its OID property has (already) been set
causes an immediate automatic load of the system object. 

Calling the loader of an awake saved state, as well as calling the saver of a dormant saved
state, propagates the exception `Status_Error`. 


### Parallelism

For all of the six procedures `Save`, `Load`, `Create`, `Delete`, `Set_Saver`, `Set_Loader`: 

 * The procedure may (and generally will) return before all the actions of the procedure (for
   example, for `Save`, all the state data actually being written into the stream) are
   complete. If any other method of the system object is called before this is complete, the
   call will be blocked until it is. 

 * If the system object is (loaded and) engaged (function `Is_Engaged` returns `True`), the
   procedure is blocked until the system object becomes disengaged. 

The functions `Saver` and `Loader` are oblivious to whether the object is engaged, but will
only return when the object is not busy. 


### Finalisation

A system object should be of a [limited controlled][6] type (except in the unlikely case that
the system object is actually stateless). If the object is not vacant when it is finalised, it
should do the following:

 * If its save controller has been set, the object should call its save controller and, if the
   result is not null, use the returned stream to save itself; 

 * In all other cases, the object should propagate the exception `Status_Error` (the ECLAT
   implementation will catch and handle these exceptions). 

If the object is vacant, finalisation need not do anything. 

.....



-----------------------------------------------------------------------------------------------
## Engagement {#eng}

A system object is a [secure object](../security/security.md#secobj), so it is
[engaged](../security/engaging.md) the same way that a security object is engaged, but .....

Calling the procedure `Engage` should:

 1. If the system object is vacant and its load controller has been set, the load controller
    should be called, and, if the result is not null, the object should use the returned stream
    to load the system object and then proceed as below; 

 2. If the system object is vacant but its load controller has not been set, propagate the
    exception `Status_Error`; 

 3. In all cases, proceed as for a secure object. 

.....





-----------------------------------------------------------------------------------------------
## Current Transaction {#tran}

Every [engagement](../security/engaging.md#engagement) of a system object is associated with a
reference to a [transaction](../intro/trans.md). This reference is called the _current
transaction_ of the system object. It can be null. If the current transaction is null, there is
_no current transaction_. 

All actions performed by the system object on behalf of the calling task within that engagement
are performed in the context of the (transaction referenced by the) current transaction, if
there is one. 

In other words, any change made to the system state by the system object on behalf of the
calling task within that engagement is linked to the current transaction, if there is one. 

.....

Every system object has a [property](../intro/intro.md#prop) named `Transaction` which
represents the current transaction of the system object. 

When a system object is created or loaded, the current transaction is initially `null`. 

The current transaction is associated with an *engagement*, and so when the engagement is
terminated, the current transaction automatically becomes `null`. If this happens, it does not
mean that the transaction controller object itself is finalised (or affected in any way), and
so it does not trigger an automatic abort of the transaction (nor does it trigger the
transaction to perform any other kind of action). 

Thus, the current transaction of a system object is always `null` whenever the system object is
not engaged. To be pedantic, the current transaction of a system object is always `null`
whenever the system object is vacant. 

The package `AdaOS.Objects` contains the following declarations: 

```ada
function Transaction (Object: in out System_Object) 
is
   access Transaction_Controller'Class is asbtract;

procedure Set_Transaction (Object:      in out System_Object; 
                           Transaction: access Transaction_Controller'Class)
is asbtract;
```

The function `Transaction` returns `null` if there is no current transaction for the given
`Object`. Otherwise, it returns (an access value referencing) the current transaction. 

.....



????? Is a transaction a system object? If so, what does its current transaction mean? Its super-transaction?




-----------------------------------------------------------------------------------------------
## References

[1]: <http://ada-auth.org/standards/12rm/html/RM-E-2-2.html> "Ada Reference Manual, E.2.2 
     Remote Types Library Units"

[2]: <?????> "Remote Subprogram Calls"

[3]: <?????> "External Tag"

[4]: <?????> "Streams"

[5]: <> ""

[6]: <> "Controlled Types"




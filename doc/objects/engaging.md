-----------------------------------------------------------------------------------------------
# Engaging a System Object

For all types of [system object](objects.md), a mechanism is implemented by the object to
control its usage within a multi-tasking system. This section describes how the majority of
system objects behave. Those types of system object that deviate from this norm must carefully
document how they should be used. 

In a multi-tasking system, in general, it is possible that more than one task will attempt to
interact with (make calls to) a system object at the same time. Typically this would be a
chaotic situation, and some form of control must be used to prevent it happening. 

To ensure that a system object is accessed in a controlled manner, it is (normally) necessary
for a task to _engage_ the object before being able to interact with the object in any other
way. The task should _disengage_ the object when it has (temporarily) finished using it. 

Whilst a system object is engaged by a particular task, it is locked, so that no other task can
(inadvertently) interact with the object at the same time. 



-----------------------------------------------------------------------------------------------
## Operations

The following subprograms are primitive operations of the type `System_Object`: 

```ada
procedure Engage (Object:     not null access System_Object;
                  Controller: access Transaction_Controller'Class := Task_Transaction.Value;
                  Authority:  in     Security_Authority           := Task_Authority.Value;
                  Identity:   in     Security_Identity            := Task_Identity.Value)
is
   abstract
with 
   Endorse,
   Synchronization => By_Entry;

procedure Disengage (Object: access System_Object)
with 
   Endorse,
   Synchronization => By_Entry;

function Is_Engaged (Object: in System_Object) return Boolean
with 
   Endorse,
   Synchronization => By_Entry;

function Engagement_Transaction (Object: in System_Object) 
return 
   access Transaction_Controller'Class
with 
   Endorse;

function Engagement_Authority (Object: in System_Object) return Security_Authority
with 
   Endorse;

function Engagement_Identity (Object: in System_Object) return Security_Identity
with 
   Endorse;
```

A system object must be implemented as a task, and all these operations should be implemented
as entries. 

Normally, a system object type should implement `Engage` as follows`: 

 1. Call the parent type's `Engage`; 
 
 2. [Endorse](../security/endorse.md#eng) the call; 

 3. If the endorsement failed, call `Disengage` to disengage the object and then propagate an
    exception, usually `Security_Violation`. 



-----------------------------------------------------------------------------------------------
## Engagement

Calling the procedure `Engage` should:

 * If the system object is vacant and its load controller has been set, the load controller
   should be called, and, if the result is not null, the object should use the returned stream
   to load the system object and then proceed as below; 

 * If the system object is vacant but its load controller has not been set, propagate the
   exception `Status_Error`; 

 * Otherwise, proceed as below.

Calling the procedure `Engage` should create a notional entity called an _engagement_.

The engagement is characterised by: 

 * The system object, passed in as the `Object` parameter; 

 * The [identity and current authority](../security/security.md#ca) of the calling task, passed
   in as the `Identity` and `Authority` parameters; 
 
 * Optionally, the [transaction](../database/transactions.md) passed in via the `Controller`
   parameter. 
 
The engagement is _terminated_ when the system object is disengaged. When it is terminated, it
ceases to exist. 

If `Object` is already engaged, the calling task is blocked until the existing engagement is
terminated. Then, the calling task engages the system object as normal. 

Specifying a transaction in the parameter `Controller` implies that all changes of state made
by interacting with the system object will be under the control of the transaction. Any
deviation from this behaviour must be carefully documented. 

The `Controller` parameter defaults to the result of calling the function
`Task_Transaction.Value` (declared in the package `AdaOS.Transactions`), so if the calling task
has a current transaction, the engagement will have that transaction unless another transaction
or a null is explicitly passed in as the `Controller` parameter. 

The parameters `Authority` and `Identity` allow the engagement to be given a different
authority and/or identity. These parameters will only very rarely be given a value other than
their defaults. 

If the calling task changes its current authority during an engagement of a system object and
then attempts to make a call to one of the system object's non-endorsed operations, the
exception `Security_Violation` will be propagated. 



-----------------------------------------------------------------------------------------------
## Testing for Engagement

The function `Is_Engaged` returns `True` if the system object `Object` is engaged at the time
the function is called, or `False` if not. 

When using this function, care must be taken to avoid a race condition. In essence, it is safe
to use this function to decide to interact with the system object based on the function
returning `True`, but attempting to engage a system object based on this function returning
`False` may not prevent the caller being blocked (another task might engage the object in
between the call to `Is_Engaged` and the call to `Engage`). 

The functions `Engagement_Identity`, `Engagement_Authority`, and `Engagement_Transaction`
return the engagement's identity, authority, and transaction respectively. 

If the system object `Object` is not engaged at the time the function is called, these
functions return `No_Identity`, `No_Authority`, or `null` respectively. 

A similar caution about the function `Is_Engaged` and race conditions applies to these
functions as well. 



-----------------------------------------------------------------------------------------------
## Licensee

A _licensee_ of an engagement is any task whose identity and current authority are both equal
to those of the engagement, and is therefore (normally) permitted to interact with the system
object (until it is disengaged). Normally, other tasks do not have this permission. 



-----------------------------------------------------------------------------------------------
## Status Error

The exception `Status_Error` is declared in the package `AdaOS.Objects`. 

While a system object is engaged, it cannot be used by any task other than a licensee task.
Generally, if any task other than a licensee task attempts to call any operation of the system
object, except for its [endorsed operations](../security/endorse.md), the exception
`Status_Error` is propagated. 

If any operation deviates from this restriction, it must be clearly documented. 

The endorsed operations of a system object will include `Engage`, `Is_Engaged`,
`Engagement_Identity`, `Engagement_Authority`, and `Engagement_Transaction`. 



-----------------------------------------------------------------------------------------------
## Disengaging

The procedure `Disengage` disengages the system object `Object`. The associated engagement is
terminated. 

If `Object` is not engaged or the caller is not a licensee, the call to `Disengage` harmlessly
does nothing. 

This makes it possible to safely call `Disengage` on a system object at any point in the code,
including points where it may not be clear whether the object is currently engaged or not, or,
if it is engaged, who has engaged it. 

Beware that this can mean, in some circumstances, a system object might not necessarily be
disengaged immediately after a call to `Disengage`. 



-----------------------------------------------------------------------------------------------
## Endorsement of the Engagement

The call to `Engage` usually needs to be [endorsed](../security/endorse.md#eng). 



-----------------------------------------------------------------------------------------------
## Example

The Ada construct called an Asynchronous Transfer of Control (ATC) makes it possible to impose
a timeout on using a system object. 

For example, the following code attempts to make use of a system object named `Thingie`, but
aborts this effort if it is not all done within 5 seconds: 

```ada
   declare
      Thingie: Thingie_Access := Thingie_Access (Find (Some_Directory, "name_of_thingie"));

   begin
      if Thingie = null
      then
         -- Thingie not found
         return;
      end if;

      select
         delay 5.0; -- 5 seconds
         Thingie.Disengage; -- immediately disengages it if it is engaged
         Log ("Unable to do things with Thingie within 5 seconds. ");

      then abort
         Thingie.Engage;

         -- do things with Thingie

         Thingie.Disengage;

      end select;

   exception
      -- handle specific exceptions here, but disengage Thingie if necessary

      when others =>
         Thingie.Disengage; -- disengages it if it is engaged
         raise; -- re-raise the same exception
   end;
```

It is important to always catch any exceptions and disengage any possibly engaged system
objects (and then either remedy or re-raise the exception). 

It is, in most cases, also important to ensure that a system object cannot be held engaged for
more than a short period of time. 

The above example demonstrates this. `Thingie` is disengaged, if it needs to be, if any
exception occurs or if the timeout is triggered. 

An alternative formulation uses a timed call to impose a timeout on the call to `Engage`: 

```ada
   declare
      Thingie: Thingie_Access := Thingie_Access (Find (Some_Directory, "name_of_thingie"));

   begin
      if Thingie = null
      then
         -- Thingie not found
         return;
      end if;

      select
         Thingie.Engage;
      or
         delay 3.75; -- 3.75 seconds
         Thingie.Disengage; -- immediately disengages it if it is engaged
         Log ("Unable to engage Thingie within 3.75 seconds. ");
         return;
      end select;

      -- do things with Thingie

      Thingie.Disengage;

   exception
      -- handle specific exceptions here, but disengage Thingie if necessary

      when others =>
         Thingie.Disengage; -- disengages it if it is engaged
         raise; -- re-raise the same exception
   end;
```

If it is not desired to have a timeout apply to the `do things with Thingie` part, this
formulation is preferable. 




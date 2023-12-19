-----------------------------------------------------------------------------------------------
# Engaging a Secure Object

For all types of [secure object](security.md#secobj), a mechanism is implemented by the object
to control its usage within a multi-tasking system. 

In a multi-tasking system, in general, it is possible that more than one task will attempt to
interact with (make calls to) a secure object at the same time. Typically this would be a
chaotic situation, and some form of control must be used to prevent it happening. 

To ensure that a secure object is accessed in a controlled manner, it is generally necessary
for a task to _engage_ the object before being able to interact with the object in any other
way. The task should _disengage_ the object when it has (temporarily) finished using it. If it
needs the object again after that, it re-engages it, and so on. 

Whilst a secure object is engaged by a particular task, it is locked, so that no other task can
(inadvertently) interact with the object at the same time. 



-----------------------------------------------------------------------------------------------
## Timeout and Accounting

The period of time of any one engagement (between it being engaged and then disengaged) should
be as short as possible. Every secure object is able to, and generally will, abort an
engagement if it stretches on for too long. Each secure object type's behaviour in this regard
should be well documented. 

.....

The package `AdaOS` contains the following declarations: 

```ada
type Timeout_Controlled is limited interface;

function Timeout (Object: in Timeout_Controlled) 
return
   Duration is abstract;

procedure Set_Timeout (Object:  in out Timeout_Controlled; 
                       Timeout: in     Duration) 
is abstract;
```

Any type that is derived from `Timeout_Controlled` has the ......

During a period of engagement, the owner of the task that is engaging the secure object will
generally be [charged](accounting.md) for the amount of time that the engagement lasts, and if
the account's limit is reached, the engagement will be aborted. 

It is therefore in the interest of the task engaging a secure object to reduce the timeout to a
workable minimum, so as to protect itself from the possibility that a bug causes it to
accidentally hold an engagement for a long period of time, so incurring a big charge. 



-----------------------------------------------------------------------------------------------
## Operations

The following subprograms are primitive operations of the type `Secure_Object`: 

```ada
procedure Engage (Object:     not null access Secure_Object;
                  Authority:  in Security_Authority := Task_Authority;
                  Identity:   in Security_Identity  := Task_Identity)
is
   abstract with Endorse;

procedure Disengage (Object: access Secure_Object)
is
   abstract with Endorse;

function Is_Engaged (Object: in Secure_Object) return Boolean
is
   abstract with Endorse;

function Engagement_Transaction (Object: in Secure_Object) 
return 
   access Transaction_Controller'Class
is
   abstract with Endorse;

function Engagement_Authority (Object: in Secure_Object) return Security_Authority
is
   abstract with Endorse;

function Engagement_Identity (Object: in Secure_Object) return Security_Identity
is
   abstract with Endorse;
```

Normally, a secure object type should implement `Engage` as follows`: 

 1. Call the parent type's `Engage`; 
 
 2. [Endorse](../security/endorse.md#eng) the call; 

 3. If the endorsement failed, call `Disengage` to disengage the object and then propagate an
    exception, usually `System.Security.Security_Violation`. 



-----------------------------------------------------------------------------------------------
## Engagement

Calling the procedure `Engage` creates a notional entity called an _engagement_.

The engagement is _terminated_ when the secure object is disengaged. When it is terminated, it
ceases to exist. 

The engagement is characterised by: 

 * The secure object, passed in as the `Object` parameter; 

 * The [identity and current authority](../security/security.md#ca) of the calling task, passed
   in as the `Identity` and `Authority` parameters; 

 * The duration of time between the calls to `Engage` and `Disengage` at the start and end of
   the engagement. 
 
If `Object` is already engaged, the calling task is blocked until the existing engagement is
terminated. Then, the calling task engages the secure object as normal. The calling task is not
charged for the time it is blocked. 

Specifying a transaction in the parameter `Controller` implies that all changes of state made
by interacting with the secure object will be under the control of the transaction. Any
deviation from this behaviour must be carefully documented. 

The `Controller` parameter defaults to the result of calling the function `Task_Transaction`
(declared in the package `AdaOS.Transactions`), so if the calling task has a current
transaction, the engagement will have that transaction unless another transaction or a null is
explicitly passed in as the `Controller` parameter. 

The parameters `Authority` and `Identity` allow the engagement to be given a different
authority and/or identity. These parameters will only very rarely be given a value other than
their defaults. 



-----------------------------------------------------------------------------------------------
## Testing for Engagement

The function `Is_Engaged` returns `True` if the secure object `Object` is engaged at the time
the function is called, or `False` if not. 

When using this function, care must be taken to avoid a race condition. In essence, it is safe
to use this function to decide to interact with the secure object based on the function
returning `True`, but attempting to engage a secure object based on this function returning
`False` may not prevent the caller being blocked (another task might engage the object in
between the call to `Is_Engaged` and the call to `Engage`). 

The functions `Engagement_Identity`, `Engagement_Authority`, and `Engagement_Transaction`
return the engagement's identity, authority, and transaction respectively. 

If the secure object `Object` is not engaged at the time the function is called, these
functions return `No_Identity`, `No_Authority`, or `null` respectively. 

A similar caution about the function `Is_Engaged` and race conditions applies to these
functions as well. 



-----------------------------------------------------------------------------------------------
## Licensee

A _licensee_ of an engagement is any task whose identity and current authority are both equal
to those of the engagement, and is therefore (normally) permitted to interact with the system
object (until it is disengaged). Normally, other tasks do not have this permission. 

.....

In simple terms, if the calling task changes its current authority during an engagement of a
secure object and then attempts to make a call to one of the secure object's non-endorsed
operations, the exception `Security_Violation` will be propagated. 



-----------------------------------------------------------------------------------------------
## Status Error

The exception `Status_Error` is declared in the package `AdaOS.Objects`. 

While a secure object is engaged, it cannot be used by any task other than a licensee task or
(occasionally) a different task if the call is endorsed and the endorsement permits it. 

Generally, if any task other than a licensee task attempts to call any operation of the secure
object, except for its [endorsed operations](../security/endorse.md), the exception
`Security_Violation` is propagated. 

If any operation deviates from this restriction, it must be clearly documented. 

The endorsed operations of a secure object will include `Engage`, `Is_Engaged`,
`Engagement_Identity`, `Engagement_Authority`, and `Engagement_Transaction`. 



-----------------------------------------------------------------------------------------------
## Disengaging

The procedure `Disengage` disengages the secure object `Object`. The associated engagement is
terminated. 

If `Object` is not engaged or the caller is not a licensee, the call to `Disengage` harmlessly
does nothing. 

This makes it possible to safely call `Disengage` on a secure object at any point in the code,
including points where it may not be clear whether the object is currently engaged or not, or,
if it is engaged, who has engaged it. 

Beware that this can mean, in some circumstances, a secure object might not necessarily be
disengaged immediately after a call to `Disengage`. 



-----------------------------------------------------------------------------------------------
## Endorsement of the Engagement

The call to `Engage` usually needs to be [endorsed](../security/endorse.md#eng). 

.....



-----------------------------------------------------------------------------------------------
## Example

The Ada construct called an Asynchronous Transfer of Control (ATC) makes it possible to impose
a timeout on using a secure object. 





????? But it already has a timeout?





For example, the following code attempts to make use of a secure object named `Thingie`, but
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
      end select;

      Thingie.Disengage;

   exception
      -- handle specific exceptions here, but disengage Thingie if necessary

      when others =>
         Thingie.Disengage; -- disengages it if it is engaged
         raise; -- re-raise the same exception
   end;
```







```ada
   declare
      Thingie: Thingie_Access;
      Previous_Timeout: Duration;

   begin
      Thingie := Thingie_Access (Some_Directory.Find("name_of_thingie"));

      if Thingie = null
      then
         raise Thingie_Not_Found_Error;
      end if;

      begin
         Previous_Timeout := Thingie.Timeout;
         Thingie.Set_Timeout (5.0); -- 5 seconds
         Thingie.Engage;

         -- do things with Thingie

         Thingie.Set_Timeout (Previous_Timeout);
         Thingie.Disengage;

      exception
         -- handle specific exceptions here, but disengage Thingie if necessary

         when Thingie_Not_Found_Error =>
            Log ("Could not find Thingie. ");
            raise; -- re-raise the same exception

         when Timeout_Expired =>
            Thingie.Set_Timeout (Previous_Timeout);
            Thingie.Disengage; -- disengages it if it is engaged
            Log ("Unable to do things with Thingie within 5 seconds. ");
            raise; -- re-raise the same exception

         when others =>
            Thingie.Set_Timeout (Previous_Timeout);
            Thingie.Disengage; -- disengages it if it is engaged
            Log ("Error occurred while trying to do things with Thingie. ")
            raise; -- re-raise the same exception
      end;
   exception
      Log ("Error occurred trying to find Thingie. ")
      raise; -- re-raise the same exception
   end;
```











It is important to always catch any exceptions and disengage any possibly engaged system
objects (and then either remedy or re-raise the exception). 

It is, in most cases, also important to ensure that a secure object cannot be held engaged for
more than a short period of time. 

The above example demonstrates this. `Thingie` is disengaged, if it needs to be, if any
exception occurs or if the timeout is triggered. 





-----------------------------------------------------------------------------------------------
# Transactions

_Transactions_ protect the integrity of the data (and device settings) of a computer systems
from accidental threats. They offer a way to deal with multiple programs accessing and updating
the same data (or devices) at potentially the same time, and a way to deal with unexpected
failures (of hardware or software) disrupting the orderly updating of data (or devices). 

Almost all activity within AdaOS is transactional (uses transactions), and it is strongly
encouraged that software built on the AdaOS platform does the same. 

.....




-----------------------------------------------------------------------------------------------
## Introduction

The term _system state_ can be used to describe the current state of all the things in/of a
computer (system) that can potentially change during the operation of the computer. In theory,
this means: 

 * all the registers and other settings in the CPU (or CPUs); 

 * everything in memory, including caches etc.; 

 * all the registers and settings in peripheral circuits; 

 * usually, some or all of the data in (persistent) storage accessible to the computer; 

 * possibly the state of some devices connected to the computer; 

 * you get the idea. 

It is generally assumed that the system state has some initial value when the computer is
powered up or reset (rebooted) and has completed the [bootstrap process](../pxcr/booting.md).
Then, conceptually, the execution of each machine instruction modifies that state (even though
instructions can be executed in parallel, out of order, speculatively, and so on). 
 
A _transaction_ is a conceptual entity that is associated with a set of changes to the system
state. The idea is that the transaction achieves several useful things: 

 * In effect, all other transactions see the system state as it was before any other
   transaction made any changes to it. 

 * The whole set of changes can be reversed, so that the system state goes back to how it was
   just before the transaction started, notwithstanding any changes that were not affected by
   the changes of the transaction. This reversal is atomic; the changes are all reversed
   together in one go, which cannot be interrupted by the actions of any other transaction.
   This is termed _abandoning_ the transaction. 

 * The changes made by a transaction can all be made permanent together in one go, which cannot
   be interrupted by the actions of any other transaction. This is termed _committing_ the
   transaction. 

There are other properties that a traditional transaction is supposed to have that are not
provided for or enforced by AdaOS. The characteristics of transactions that AdaOS mandates are
enough to readily build transactions which have further desirable properties. 

It is important to understand that transactions exist to achieve certain useful kinds of
behaviour. Whilst in theory they could apply to the entirety of system state, in practice they
will always be imperfect in they way they apply to some parts of system state. This is an
acceptable compromise in order to keep the transaction mechanisms efficient in terms of the
(extra) resources they cause to be used. 

.....

In practice, the system state protected by transactions can be viewed as being the accumulation
of the states of all the [system objects](../objects/objects.md) currently existing in the
system. In fact, one might expand that to apply to all the system objects in a particular
[domain](../security/domains.md), and use the term _domain state_ instead. 

.....





-----------------------------------------------------------------------------------------------
## Transaction Lifetime {#}

An AdaOS transaction has to be created before any changes can be associated with it. Once
created, things that make changes to the system state can be linked to the transaction, and as
such, all changes made by all of those things are linked to the transaction. 

A transaction is aimed at achieving a change to the system state that achieves something
useful, but ensuring that the whole change is either made in its entirety or not made at all. 

When all the changes have been made to transaction that are needed to achieve the transaction's
objective, the changes all need to made permanent, and the transaction can then cease to exist. 

The process of making all of a transaction's changes permanent is called _committing_ the
transaction. 

If something goes wrong, and the changes of a transaction cannot all be done, for any reason,
then all the changes made so far must be undone, and then the transaction can then cease to
exist (a new transaction could be created in order to try again to achieve the failed
transaction's objective). 

The process of undoing (or 'rolling back') all of a transaction's changes is, in AdaOS
terminology, called _abandoning_ the transaction. 

Everything to do with transactions must be arranged so that abandoning a transaction cannot
itself fail, or at least that if it does fail, it fails in ways that cause minimal harm. 

If the process of committing a transaction fails for any reason, the transaction is
automatically abandoned. 

Similarly, if a [transaction controller](#tc) is finalised before .....
, the transaction is
automatically abandoned. 



-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Distributed Transactions {#}

AdaOS transactions are assumed to be, in general, _distributed_, which simply means that
different [executional instances](../adaos/instances.md)---which may be executing on different
computers---can all participate in one transaction, with all the expected properties of the
transaction still being upheld. 

This poses some challenges, .....






 based on 
X/Open (OASIS) Distributed Transaction Standard (DTS), also known as _XA_. 

This would be based on a two-phase commit protocol that can support distributed transactions. 






-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Committal and Abandonment {#c+a}

.....








Although the interface type `Transaction_Controller` does not mandate any particular
implementations of its primitive subprograms, there are certain assumtions that need to 
be 



-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Transaction Controllers {#tc}

In AdaOS, a transaction is represented by an object which is called a _transaction controller_. 

The package `AdaOS.Transactions` contains the visible declaration of the limited interface type
`Transaction_Controller`. Objects of a type derived from `Transaction_Controller` are
transaction controllers. 

The package `AdaOS.Transactions` contains the following declarations: 

```ada
type Transaction_Controller is limited interface;

procedure Commit
   (Controller: not null access Transaction_Controller := Task_Transaction;
    Abandoned:  out Boolean) is abstract;

procedure Commit
   (Controller: not null access Transaction_Controller := Task_Transaction) is abstract;

procedure Abandon
   (Controller: not null access Transaction_Controller := Task_Transaction) is abstract;
```

The overloading of the procedure `Commit` which has the parameter `Abandoned` attempts to
commit the transaction represented by the given transaction controller `Controller`. If this
attempt fails for any reason, the transaction is automatically abandoned, and the `Abandoned`
out-parameter is set to `True`; otherwise, it is set to `False`. The procedure does not return
until the committal or abandonment has fully completed. 

The overloading of the procedure `Commit` which does not have the parameter `Abandoned`
attempts to commit the transaction represented by the given transaction controller
`Controller`. If this attempt fails for any reason, the transaction is automatically abandoned,
and then, once the abandonment has fully completed, the exception `Transaction_Error` is
propagated. 

Both overloadings of `Commit` only return, if the commit succeeded, once the committal has
fully completed. 

The procedure `Abandon` abandons the transaction represented by the given transaction
controller `Controller`. This procedure only returns when the abandonment has fully completed. 

Abandoning a transaction, under any circumstances, should never fail. However, if it does fail,
the exception `Transaction_Error` must be propagated (which should have a useful diagnostic
message).  

The procedure `Restart` restarts the transaction represented by the given transaction
controller `Controller`. This is discussed in [Bookmarks](#bm). 

If any of the `Commit`, `Abandon`, or `Restart` procedures is called on a transaction
controller which is not in the `Pending` [mode](#mode), the procedure propagates the exception
`Mode_Error`. 



-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Transaction Mode {#mode}

Every [transaction controller](#tc) is, at any one point in its lifetime, in one of five modes:

 * improper

 * setting up

 * pending

 * committed

 * abandoned

The package `AdaOS.Transactions` contains the following visible declaractions:

```ada
type Transaction_Mode is (Improper, Setting_Up, Pending, Committed, Abandoned);

function Mode (Controller: Transaction_Controller) return Transaction_Mode is abstract;
```

The following table summarises the possible transitions from one mode to another. 

| From Mode    | To Mode      | How                                        |
| ------------ | ------------ | ------------------------------------------ |
| Improper     | Any          | Unspecified                                |
| Setting Up   | Pending      | `Start_Transaction`                        |
| Pending      | Committed    | Successful `Commit` (either overloading)   |
| Pending      | Abandoned    | `Abandon`                                  |
| Pending      | Abandoned    | Failed `Commit` (either overloading)       |
| Any          | Improper     | Unspecified                                |

.....


### Improper Mode

In theory a transaction controller should newver be in the `Improper` mode, but this mode
exists to indicate that the controller is in some kind of erroneous or failed that cannot be
adequately covered by any of the other modes. 

When a transaction controller is in this mode, it should be assumed that it cannot be fixed,
and the only thing to do is to dispose of it and either try again or give up trying to use it. 

It is permitted for some action to get a transaction controller out of this mode, but such
actions are not mandated for the type `Transaction_Controller`. If some type dervived from it
can offer this kind of funcitonality, it should be well documented. 


### Setting-Up Mode

When a transaction is created, it will normally be in this mode. 

In this mode, actions that set the basic configuration of the controller can be called, 
.....

The procedure `Start`



### Pending Mode

In this mode, the transaction has been started


### Committed Mode




### Abandoned Mode




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## Participants {#part}

A _transaction aware_ object is any object that is able to be a part of a transaction, and to
honour the transaction principles in respect of any changes that occur to its own state as a
result of (proper, normal) interactions with the rest of the system. 

To indicate that an object is transaction aware, it must be derived from the limited interface
type `Transaction_Controller`. 

A _participant_ of a particular transaction is any transaction aware object that has _joined_
that transaction. The transaction is said to have _adopted_ the participant. 

The participant is the _child_ of the transaction which it has joined, and the transaction
which it has joined is the _parent_ of the participant. 

.....

The package `AdaOS.Transactions` contains the following visible declaractions:

```ada

function Can_Adopt 
   (Controller:  not null access Transaction_Controller := Task_Transaction;
    Participant: not null access Transaction_Controller'Class) 
return
   Boolean is abstract;

procedure Adopt
   (Controller:  not null access Transaction_Controller := Task_Transaction;
    Participant: not null access Transaction_Controller'Class) is abstract;

procedure Detach 
   (Controller:  not null access Transaction_Controller := Task_Transaction;
    Participant: not null access Transaction_Controller'Class) is abstract;
```

The function `Can_Adopt` returns `True` if the given transaction controller `Controller` is
able and willing to adopt participant `Participant`. It returns `False` otherwise. Some kinds
of transaction (controller) will not be able to adopt any participants at all. They will always
return `False`. This function will always return `False` if the transaction controller is not
in the `Setting_Up` mode. 

The procedure `Adopt` causes the given transaction controller `Controller` to adopt participant
`Participant`. If that participant is already joined to `Controller`, this procedure does
nothing (there is no error). If `Controller` is unable to adopt the putative participant, then
`Transaction_Error` is propagated. 

The procedure `Detach` causes the given participant `Participant` to no longer be joined to
transaction controller `Controller`. If that participant is not currently joined to
`Controller`, this procedure does nothing (there is no error). 

Any attempt to call the procedures `Adopt` or `Detach` on a transaction controller that is not
in the `Setting_Up` [mode](#mode) propagates the exception `Mode_Error` .....

.....



-----------------------------------------------------------------------------------------------
## Nested Transactions {#}

????? Needs rewording?

A _nested transaction_ is almost the same thing as a [participant](#part). A participant is any
object that is derived from the interface type `Transaction_Controller`. This means
that a transaction controller can also be a participant. A participant can itself have
participants, or, to put it other words, a parent transaction can have child transactions which
can themselves have child transactions, and so on for any number of levels. 

A participant/child that is actually a transaction controller---rather than some other kind of
participant and, in particular, can have and is intended to have participants/children---is
called a _nested transaction_. 

Nested transactions form a [hierarchy](../intro/hierarchy.md), and provide a way for a big
transaction, with a lot of inferior participants, to be structured and more readily managed.
Because the hierarchy allows transactions to be composed (from smaller child transactions),
special code for the sub-transactions can be modularised (isolated from the parts of the big
transaction). 



-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## Starting a Transaction {#start}

.....

The package `AdaOS.Transactions` contains the following visible declaraction:

```ada
procedure Start
   (Controller: not null access Transaction_Controller := Task_Transaction) is abstract;
```

The procedure `Start` causes a transaction controller to transition from `Setting_Up` mode into
`Pending` mode. 

Any attempt to call the procedure `Start` on a transaction controller that is not in the
`Setting_Up` [mode](#mode) propagates the exception `Mode_Error` .....



-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Bookmark {#bm}

If a transaction fails (resulting in it being abandoned), it can retried. Provided the
conditions that caused it to fail are (probably) no longer extant, the retry will (probably)
succeed. A transaction might be retried many times, if necessary. 

However, if a transaction fails at some arbitrary point during its processing, it might
represent a great deal of rework to start the transaction again from scratch. 

Transactions are intended to be as short and quick as practicable. However, sometimes it may be
a better strategy to avoid having to restart the whole transaction if the transaction fails
after having performed a large amount of processing. 

In order to facilitate this, it is possible to ask a [transaction controller](#tc) to create a
token value that is called a _bookmark_. 

A bookmark is represented by the interface type `Transaction_Bookmark`, which is visibly
declared in the package `AdaOS.Transactions`. 

The package also contains the following declarations: 

```ada

function Can_Create_Bookmark
   (Controller: not null access Transaction_Controller := Task_Transaction) 
return 
   Boolean is abstract;

function Create_Bookmark 
   (Controller: not null access Transaction_Controller := Task_Transaction) 
return 
   access Transaction_Bookmark'Class is abstract;
```

The function `Can_Create_Bookmark` returns `True` if the given transaction controller is able
to create a bookmark, at the time when the function is called, or `False` otherwise. Some
transaction controllers are unable to create bookmarks at all. For these, the function will
always return `False`. This fnction will always return `False` if the transaction controller is
not in the `Pending` [mode](#mode). 

The function `Create_Bookmark` creates a new bookmark (system object) and returns (an access
value referencing) that bookmark.  

A bookmark is permanently associated with the transaction controller that created it, which is
called the bookmark's _controller_. 

The package `AdaOS.Transactions` contains the following declarations: 

```ada

function Is_Restart_Viable
   (Controller: not null access Transaction_Controller := Task_Transaction; 
    Bookmark:   access Transaction_Bookmark'Class := null) 
return 
   Boolean is abstract;

procedure Restart
   (Controller: not null access Transaction_Controller := Task_Transaction;
    Abandoned:  out Boolean; 
    Bookmark:   access Transaction_Bookmark'Class := null) is abstract;

procedure Restart
   (Controller: not null access Transaction_Controller := Task_Transaction; 
    Bookmark:   access Transaction_Bookmark'Class := null) is abstract;

procedure Delete_Bookmark
   (Controller: not null access Transaction_Controller := Task_Transaction; 
    Bookmark:   not null access Transaction_Bookmark'Class) is abstract;
```

The function `Is_Restart_Viable` returns `True` only if: 

 * the given bookmark was created by the given transaction controller; and 

 * it is possible for the controller to restart at the specified bookmark, or for the
   controller to fully restart if no bookmark is specified (`Bookmark = null`).

The function returns `False` otherwise. 

......

The overloadings of the procedure `Restart` causes the given transaction controller to restart.

If a bookmark is specified, the transaction controller restarts from the bookmark. The
transaction attempts to roll back (undo) the effects of all the changes (to system state) made
after the bookmark was created. 

If a bookmark is not specified (`Bookmark = null`), the transaction controller fully restarts.
The transaction attempts to roll back (undo) all the effects of all the changes (to system
state) transaction controller has made. 

For the overloading of the procedure `Restart` with an `Abandoned` parameter, if the attempt to
restart fails for any reason, the transaction is automatically abandoned, and the `Abandoned`
out-parameter is set to `True`; otherwise, it is set to `False`. 

For the overloading of the procedure `Restart` without an `Abandoned` parameter, if the attempt
to restart fails for any reason, the transaction is automatically abandoned, and the exception
`Transaction_Error` is propagated. 

......



-----------------------------------------------------------------------------------------------
## Deadlock {#dead}






### Restarting Strategy







-----------------------------------------------------------------------------------------------
## Current Transaction {#curr}

Because the use of transactions is so prevalent throught AdaOS and the software that uses
AdaOS, there are many subprograms (functions and procedures) that take a transaction controller
as a parameter. These are termed _transactional subprograms_. 

In order to reduce the visual clutter associated with explicitly specifying a transaction
controller in every call of these transactional subprograms, every task is assumed to have a
[task attribute](../adaos/taskattr.md#ct) which holds a _current transaction_ for the task. 

Almost all transactional subprograms have a default value of (an access value referencing) the
calling task's current transaction controller for its the transaction controller parameter. 

So a sequence of Ada statements that make multiple calls to transactional subprograms can just
set the task's current transaction controller and then not explicitly specify the transaction
controller parameter in all of those calls. 

.....






In fact, many subprograms can make calls to transactional subprograms rather oblivious to the
transactionality aspects of what they are doing. Whatever the current transaction is when the
subprogram is called will determine the transaction used, and the subprogram's body can focus
more on the essence of what it is doing without the distraction of transactionality. 

When a task creates another task, the values of its task attributes are inherited by the new
task, including the current transaction. 

.....



-----------------------------------------------------------------------------------------------
## {#}





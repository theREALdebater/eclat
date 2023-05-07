-----------------------------------------------------------------------------------------------
# Transactions

The _system state_ is the current state of all the things in/of a computer (system) that can potentially 
change during the operation of the computer. In theory, this means:

 * all the registers and other settings in the CPU (or CPUs); 

 * everything in memory, including caches etc. 

 * all the registers and settings in peripheral circuits; 

 * in some situations, some or all of the data in storage accessible to the computer; 

 * you get the idea. 

It is usually taken that the system state has some initial value when the computer is powered
up or reset (rebooted) and has completed the [bootstrap process](../pxcr/booting.md). Then,
conceptually, the execution of each machine instruction modifies that state. 
 
A _transaction_ is a conceptual entity that is associated with a set of changes to the system
state. The idea is that the transaction achieves several useful things: 

 * The whole set of changes can be reversed, so that the system state goes back to how it was
   just before the transaction started. 

 * All other transactions see the system state before any other transaction made any changes to
   it. 

 * The changes made by a transaction can all be made permanent together in one go, which cannot
   be interrupted by the actions of any other transaction. 

There are other properties that a transaction is supposed to have that are not provided or
enforced by AdaOS. The characteristics of transactions that AdaOS mandates are enough to build
transactions which have further desirable properties. 

It is important to understand that transactions exist to achieve certain useful properties.
Whilst in theory they apply to the entirety of system state, in practice they will always be
imperfect in they way they apply to some parts of system state. This is an acceptable
compromise in order to keep the transaction mechanisms efficient in terms of the (extra)
resources they use. 



In practice, the system state can be viewed as being the accumulation of the states of all the
system objects currently existing in the system. 

.....





-----------------------------------------------------------------------------------------------
## Lifetime {#}

An AdaOS transaction has to be created before any changes can be associated with it. Once
created, things that make changes to the system state can be linked to the transaction, and as
such, all changes made by all of those things are linked to the transaction. 

A transaction is aimed at achieving a change to the system state that achieves something
useful, but ensuring that the whole change is either made in its entirety or not made at all. 

When all the changes have been made to transaction that are needed to achieve the transaction's
objective, the changes all need to made permanent, and the transaction can then cease to exist. 

The process of making all of a transaction's change permanent is called _committing_ the
transaction. 

If something goes wrong, and the changes of a transaction cannot all be done, for any reason,
then all the changes made so far must be undone, and then the transaction can then cease to
exist (a new transaction could be created in order to try again to achieve the failed
transaction's objective). 

The process of undoing (or 'rolling back') all of a transaction's changes is called
_abandoning_ the transaction. 

Everything to do with transactions must be arranged so that abandoning a transaction cannot
itself fail, or at least that if it does fail, it fails in ways that are cause minimal harm to
the operation of the system. 

If the process of committing a transaction fails for any reason, the transaction is
automatically abandoned. 


-----------------------------------------------------------------------------------------------
## Distributed Transactions {#}

AdaOS transactions are assumed to be, in general, _distributed_, which simply means that
different [executional instances](../adaos/instances.md)---which may be executing on different
computers---can all participate in one transaction, with all the expected properties of the
transaction still being upheld. 

This poses some challenges, .....



-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Transaction Controllers {#tc}

In AdaOS, a transaction is represented by a [system object](../objects/objects.md) which is
called a _transaction controller_. 

The package `AdaOS.Transactions` contains the visible declaration of the synchronised interface
type `Transaction_Controller`, which is derived from `System_Object` of the package
`AdaOS.Objects`. 


```ada

procedure Commit_Transaction 
   (Controller: not null access Transaction_Controller := Task_Transaction;
    Abandoned:  out Boolean) is abstract;

procedure Abandon_Transaction 
   (Controller: not null access Transaction_Controller := Task_Transaction) is abstract;
```

The procedure `Commit` attempts to commit a transaction. The transaction is that represented by
the given transaction controller. If this attempt fails for any reason, the transaction is
automatically abandoned, and the `Abandoned` out-parameter is set to `True`; otherwise, it is
set to `False`. 

The procedure `Abandon` abandons a transaction. The transaction is that represented by the
given transaction controller. 

Abandoning a transaction, under any circumstances, should never fail, however, if it does fail,
the exception `Transaction_Error` must be propagated (which should have a useful diagnostic
message).  



-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Participants {#part}

A _transaction aware_ system object is any system object that is able to be a part of a
transaction, and to honour the transaction principles in respect of any changes that occur to
its own state as a result of (proper, normal) interactions with the rest of the system. 

To indicate that a system object is transaction aware, it must be derived from the interface
type `Transaction_Controller`. 

A _participant_ of a particular transaction is any transaction aware system object that has
_joined_ that transaction. Transaction is said to _adopt_ the participant. 

The participant is the _child_ of the transaction which it has joined, and the transaction
which it has joined is the _parent_ of the participant. 

.....

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
return `False`. 

The procedure `Adopt` causes the given transaction controller `Controller` to adopt participant
`Participant`. If that participant is already joined to `Controller`, this procedure does
nothing (there is no error). 

The procedure `Detach` causes the given participant `Participant` to no longer be joined to
transaction controller `Controller`. If that participant is not currently joined to
`Controller`, this procedure does nothing (there is no error). 

.....



-----------------------------------------------------------------------------------------------
## Nested Transactions {#}

A _nested transaction_ is almost the same thing as a [participant](#part). A participant is a
system object that is derived from the interface type `Transaction_Controller`. Which means
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
## {#}





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

The package also contains the following declaration: 

```ada

function Can_Be_Bookmarked 
   (Controller: not null access Transaction_Controller := Task_Transaction) 
return 
   Boolean is abstract;

function Bookmark (Controller: not null access Transaction_Controller := Task_Transaction) 
return 
   access Transaction_Bookmark'Class is abstract;
```

The function `Can_Be_Bookmarked` returns `True` if the given transaction controller is able to
create a bookmark, or `False` otherwise. Some transaction controllers are unable to create
bookmarks at all. For these, the function will always return `False`. 

The function `Bookmark` creates a new bookmark (system object) and returns (an access value
referencing) that bookmark.  

A bookmark is permanently associated with the transaction controller that created it, which is
called the bookmark's _controller_. 

The package `AdaOS.Transactions` contains the following declarations: 

```ada

function Is_Viable (Controller: not null access Transaction_Controller := Task_Transaction; 
                    Bookmark:   not null access Transaction_Bookmark'Class) 
return 
   Boolean is abstract;

procedure Restart (Controller: not null access Transaction_Controller := Task_Transaction; 
                   Bookmark:   not null access Transaction_Bookmark'Class;
                   Abandoned:  out Boolean) is abstract;

procedure Delete (Controller: not null access Transaction_Controller := Task_Transaction; 
                  Bookmark:   not null access Transaction_Bookmark'Class) is abstract;
```

The function `Is_Viable` returns `True` only if: 

 * the given bookmark was created by the given transaction controller; and 

 * it is possible for the controller to restart at the bookmark.

The function returns `False` otherwise. 

......

The procedure `Restart` causes the given transaction controller to restart at the given
bookmark. The transaction attempts to roll back (undo) the effect of all the changes (to system
state) made after the bookmark was created. If this attempt fails for any reason, the
transaction is automatically abandoned, and the `Abandoned` out-parameter is set to `True`;
otherwise, it is set to `False`. 



-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}





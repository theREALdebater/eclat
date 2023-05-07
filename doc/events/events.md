-----------------------------------------------------------------------------------------------
# Events

ECLAT introduces a general _event model_, that provides a way for different pieces of software 
to communicate with one another without having to worry about the details of how to connect up 
and transport the data. 

This model can be---and should be---used as the basis of [messaging](messaging.md) mechanisms. 

.....

..... security .....



-----------------------------------------------------------------------------------------------
## Event Objects                                                                         {#obj}

An _event_ is a plain object (usually of a record type that is not limited or synchronised, not
remote) which contains information pertaining to something happening in a program that other
programs may be (or perhaps will certainly be) interested in. 

Every event type is derived from the abstract limited tagged private type `Event_Object` which
is declared in the package `AdaOS.Events`. 

All event objects must be serialisable .....

.....


### `mkevlib`

The command-line tool [`mkevlib`](../tools/mkevlib.md) can be used to generate an ECLAT support library that contains
declarations of event types, as well as procedures that can be used by ????? to manipulate
event data in various ways. 



-----------------------------------------------------------------------------------------------
## Channels                                                                             {#chan}

An _event channel_ is a [system object](../objects/objects.md) through which [events](#obj)
flow. An event channel system object is of a type derived from the task interface
`Event_Channel`, which is declared in the package `AdaOS.Events` (and which is derived from the
interface `AdaOS.Objects.System_Object`). 

A program can _send_ events into an event channel and _receive_ events out of an event channel,
via [sender and receiver](#send) objects. Event objects are of value (non-limited) types. When
they are sent or received, it is *copies* of the event objects that are transferred, even if
pointers (access values) are used to do it in a more efficient way. 

Event channels form a [hierarchy](../intro/hier.md), totally different to the Ada type
hierarchy or any other class hierarchy. An event channel can have a super event channel, or it
can have no super event channel. If an event channel has no super event channel, it will be
(one of potentially many) at the top of the hierarchy. 

If an event is sent to an event channel with superior event channels, a duplicate of the event
is also (effectively) sent to all the superior event channels. 



-----------------------------------------------------------------------------------------------
## Event Senders and Receivers                                                          {#send}

In order to send events into an event channel, an Ada program must _publish_ to the event
channel, which creates an ephemeral object called an _event sender_. Events can be sent to the
sender, which causes those events to go into the channel. 

Similarly, to receive events from an event channel, an Ada program must _subscribe_ to the
event channel in an event broker, creating an object called an _event receiver_. The program
waits on this object to receive events that have been sent into the channel. 

Any number of tasks can publish and/or subscribe to any event channel; every time an event is
sent into an event channel, all the subscribers to the event channel receive a copy of that
event. 

The event channel hierarchy means that the receiver will also receive messages sent to any
inferior event channel, i.e. any child of the given event channel, and any of a child of a
child, and so on. 

.....

```ada

type Event_Access is access constant Event_Object'Class; 

type Event_Sender is synchronized interface;

procedure Write (Sender: not null access Event_Sender;
                 Event:  in Event_Object'Class) is abstract;

type Event_Receiver is synchronized interface;

procedure Read (Receiver: not null access Event_Receiver;
                Event:    out Event_Access) is abstract;
```

.....




-----------------------------------------------------------------------------------------------
## Brokers                                                                              {#brok}

A _event broker_ is a [service](../services/services.md) that implements event channels. 

An event broker is also an [object container](../objects/containers.md) which can contain: 

 * event channels; 
 
 * links to event channels;
 
 * if it is a [multiplexor](#mux), other event brokers (which are therefore sub-brokers). 

Every event channel is a member of exactly one event broker. 



-----------------------------------------------------------------------------------------------
## Channel Names                                                                         {#nam}

It is not unusual for an event broker to be a [directory](../objects/containers.md#dir). In
this case, some or all of its members will have a name. 

.....

Every [compartment](../adaos/compart.md) has a system event broker, which is a directory, and
itself is a member of the compartment's root directory, with the name `chan`. 

.....



-----------------------------------------------------------------------------------------------
## Multiplexors                                                                          {#mux}

An _event multiplexor_ is a special kind of [event broker](#brok) whose purpose (usually, its
only purpose) is to make all the [event channels](#chan) of two or more other brokers operate
as if they were all in just the one broker (the multiplexor). 

A multiplexor contains (has as its members) links to two or more other brokers, which are
called the _sub-brokers_ of the multiplexor. A multiplexor must be a directory, and all of its
sub-brokers must all be directories. 

The multiplexor implicitly creates _representatives_ of all the channels in all of its
sub-brokers. The set of representatives is the set of the *names* of the channels in all the
sub-brokers. 

If two or more sub-brokers have a channel of the same name, the representative is a new (but
synthetic) channel which: 

 * automatically copies all events coming into a channel of the same name in any of the
   sub-brokers into the same-named channels in all the other sub-brokers and also into the
   representative; 

 * automatically copies all events coming into representative into all the same-named channels
   in all the sub-brokers. 

If only one sub-broker has a channel of a particular name, the representative is simply a link
to that channel. 

Any changes (create, rename, delete) to an event channel in any of the sub-brokers causes the
appropriate change or changes to be made in the multiplexor automatically. 

A multiplexor is also a broker (which is a directory), so it provides all the functionality of
a regular broker. In particular, a multiplexor could be a sub-broker of another multiplexor,
and so on for any number of levels. This is very unlikely to happen in practice, but it is
possible (and supported). 



-----------------------------------------------------------------------------------------------
## Expiry

The interface `Event_Object` defines a primitive operation which is a function named
`Expires_After`, which returns a value of type `Ada.Calendar.Time`. 

Any broker that is about to forward the event (to another broker or to a receiver) should call 
this function first; if the returned time is before the current time (e.g. as returned by the 
`Ada.Calendar.Clock` function), the event should be discarded instead of being forwarded. 

However, the discarding of events based on time should never be assumed by any receiver (or
receiving broker). A receiver must always be prepared to receive an event that has expired;
typically the receiver will immediately discard the event rather then processing it (but this
is not strictly required). 

.....



-----------------------------------------------------------------------------------------------
## Unicast and Multicast

When an [event channel](#chan) is created, it has a property that can be set called its
_picking limit_. This limit is intended to be the maximum number of receivers that can receive
(a copy of) any one event sent to the channel. 

The picking limit of any channel which has a parent is not permitted to be
greater than that of its parent (if its parent has a non-zero picking limit). The picking limit
of a channel cannot be changed. 

When a subscriber to an event channel receives an event from any broker, the event's _picking
count_ is incremented (it starts at zero). If the picking count becomes equal to (or greater
than) the picking limit of the class (unless the picking limit is zero), the event is
immediately deleted by the broker, so preventing any other subscriber receiving the event. 

A channel with a picking limit of 1 is called a _unicast_ channel: only one subscriber to the
channel will receive an event. A channel with a picking limit of greater than 1 is called a
_multicast_ channel. 

A channel with a picking limit set to 0 has no limit at all, and is called a _broadcast_
channel. This is the default. 

.....

Any receiver subscribing to a non-broadcast channel can set its _priority_ at any time. The
priority is of a large, signed integer type, and defaults to 0, which represents a neutral
value. Negative values represent lower priority levels, and positive value higher levels. 

When a broker is choosing which receiver to send an event to next, the receiver with the 
highest priority is chosen. If there are two or more receivers with the same priority, the 
choice is arbitrary (and should be fair, meaning that no one receiver is favoured over time). 

.....

A unicast channel can be used to distribute the work represented by the events being pushed
through it across all the receivers of that channel. Receivers that are heavily loaded with
work can reduce their priority; receivers that have little work to do can increase their
priority. The receivers can cooperate in this manner in an effort to balance the overall
workload amongst themselves. 


### Resending

A typical technique used by unicast event channels is to _resend_ a received event, after
having processed it. 

A broker never picks the .....


### Messaging {#msg}

..... [messaging](messaging.md) .....




### Chaining

A variant of the resending technique is to send a different event, rather than resending the
original event. The different event may contain new information that arises from the processing
of the original event. This technique is called _chaining_. 


.....

### Callbacks

Sometimes it is necessary for a sender of a particular kind of event to have messages sent back
to it from receivers of the event in response to it. 

In this case, the sender can create a new event channel for this purpose, which is called a
_callback_. The sender can include the name of the callback's name in the original event
(message). 

To reply to the original, a receiver can then send one or more messages to the callback event
channel. 

.....



-----------------------------------------------------------------------------------------------
##



-----------------------------------------------------------------------------------------------
## System Broker {#sb}

There is always one broker, the _system broker_, which is created by the 

?????

and is available to all executional instances. 

The system broker is also a [multiplexor](#mux). 

.....

Because the system broker is a multiplexor, other brokers can be registered with it, giving all 
software a single point to subscribe to messages that are intended to broadcast without limited 
scope. 



-----------------------------------------------------------------------------------------------
## Standard Channels {#std}

There are some [event channels](#chan) that AdaOS publishes to the [system broker](#sb), ......

| Name               | Purpose                                                   |
| ------------------ | --------------------------------------------------------- |
| `sys/scry`         | [Object scrying](../objects/scrying.md)                   |
| `sys/scrycache`    | Object scrying[cache](../objects/scrying.md#cache)        |
| `sys/cmpt/C`       | [Signals](../rts/signals.md) and other component events   |
| `` |  |
| `` |  |
| `` |  |
| `` |  |
| `` |  |
| `` |  |

.....





-----------------------------------------------------------------------------------------------
## Uses of Events

The event infrastructure is used by .....




### Messaging

..... [messaging](#msg) .....






### Object Scrying

..... [scrying](../objects/scrying.md) .....


### Signals and System Shutdown

..... [executional instance](../rts/instances.md) ..... [signals](../rts/signals.md) .....




### Logging

The [Logging](logging.md) and [Auditing](auditing.md) infrastructures .....



-----------------------------------------------------------------------------------------------
## Event Batching

.....

For convenience, the `AdaOS.Events` package declares a type named `Event_Batch`, which is
itself derived from `Event_Object` but contains an indefinite vector (a dynamically resizable
array) of event objects. 

The task type `Event_Batcher` is also declared. A task of this type can be fed events, and it 
will automatically send out a batch after either a certain number of events have been 
accumulated or a certain amount of time has passed since the first event was fed in. 

.....



-----------------------------------------------------------------------------------------------
## Example




For example, .....




```ada

procedure AdaOS.Services.Await_Program_Termination
is
   Signals:  access Event_Channel'Class;
   Receiver: access Event_Receiver'Class;
   Signal:   access Signal_Event'Class;

   Shutting_Down: Boolean := False;
   Any_Worker_Active: Boolean;
begin
   Signal_Channel := Task_Instance.Compartment.Program_Signal_Event_Channel;

   select
      Signal_Channel.Engage;
      Receiver := Signal_Channel.Subscribe;
      Signal_Channel.Disengage;
   or
      delay 0.5;
      raise Foobar_Error with "Could not engage Signal_Channel";
   end select;

   loop
      if Shutting_Down
      then
         -- Set Any_Worker_Active to True if any worker task not yet shut down.
         exit when not Any_Worker_Active;
      end if;

      select
         Receiver.Read (Signal);
         if Signal in Fatal_Signal'Class
         then
            -- Log receipt of signal
            if not Shutting_Down
            then
               Shutting_Down := True;
               -- Inform all worker tasks they must shut down.
            end if;
         end if;
         if Signal in ?????
         then
            ?????
         end if;
      or
         delay 0.1; 
         -- Check on the heartbeat of all the other tasks.
      end select;
   end loop;
   -- Receiver object will be finalised here

exception
   when others =>
      if Signal_Channel /= null then Signal_Channel.Disengage; end if;
      raise;

end;
```

.....










-----------------------------------------------------------------------------------------------
##




-----------------------------------------------------------------------------------------------
# Object Scrying

If a piece of software, the _requestor_, wishes to [find](objects.md#find) one or more system
objects which it knows the OIDs of but not their containers, it can use a mechanism called
_object scrying_ to ascertain their containers. 

A system object which is able to tell the requestor the container of a sought object is called a 
_responder_. 

The _scrying event class_, an [event channel](../events/events.md) named `system.scrying` is
published by the [system broker](../events/events.md#sb). 

The requestor sends a _scrying request_, an event object of a type derived from `Scrying_Request`,
declared in the package `AdaOS.Objects.Scrying`, to the scrying event channel. 

Containers capable of being responders subscribe to the [scrying cache event channel](#cache),
and when they receive a scrying request, search among their own members for any objects that
match. 



-----------------------------------------------------------------------------------------------
## Scrying Process

A requestor creates a scrying request, setting it up with:

 * a list of the OIDs of the objects it is seeking; 
 
 * a time limit for the search; 
 
 * a callback event channel name. 

The requestor sends this event to the scrying event channel. 
    
Each potential responder receives the event and, if it chooses to, performs an initial search
of its data for any (or all) of the OIDs stored in the event. If it is or finds a container of
one or more of the OIDs, it returns the list of matching OIDs, together with a reference (a remote access value) indicating the
container, back to the requestor via the callback event class name. 
    
The callback event channel name is used by responders to send lists of matching .....

```ada

type Scrying_Report is new Event_????? with
   record
      Objects:   in Object_Array; ????? indefinite?
      Container: in Container_Access;
   end record;
```
It is declared in the package `AdaOS.Objects.Scrying`. 

Event may be sent to this event class multiple times, once for each container that has at least
one of the OIDs being sought. However, events should not be sent after the time limit (an
absolute time of type `Ada.Calendar.Time`) has expired. 

A responder can be anything that can receive events, but usually it will be containers
themselves that are responders, and they will search their own members for matching OIDs (but
not recursively, since their sub-containers will have received the event anyway). 

It is more than possible that the same container will be reported multiple times; the requestor 
will need to cope with this, for example by ignoring reported containers that have already 
been reported. 

A possible algorithm that requestors could use to estimate a good time limit is to try scrying in 
a loop: at first a very small relative time limit (the absolute time limit minus the current 
time) is chosen, but if that produces no results at all, a longer relative time limit is 
chosen, and looping continues until some (or all) of the OIDs are found, or a maximum relative 
time limit is reached. 

For convenience, this algorithm is implemented by the procedure `Search`, declared in the
package `AdaOS.Objects.Scrying`, .....



-----------------------------------------------------------------------------------------------
## Scrying Cache {#cache}

The [run time system](rts.md) maintains a _scrying cache_, which maps OIDs to OIDs: each
mapping maps the OID of a system object to the OID of its container. 

The scrying cache is a system object. 

The cache subscribes to the scrying event channel `system.scrying`, and for each event received:

 1. The cache attempts to find the sought OIDs it has cached. If an object's OID is found, the
    cache sends report events as many times as necessary to report each container that an OID
    has been found in. 
    
 2. Unless all the sought OIDs were found by the cache, the cache then replaces the scrying
    event's callback (the 'original' callback) with its own callback (the 'cache' callback),
    and then forwards the event to the _scrying cache event class_, which is named
    `system.scrying_cache`. 

 3. The cache callback adds any found OIDs (and their container) to its internal tables, and 
    then sends the same report events to the original callback. 
    
When adding an OID-container pair to its internal tables, the cache will need to 'evict' an 
existing entry if it is already full. Entries are evicted in order of age (oldest first). 

A scrying cache must also listen for the `?????` event, and empty its internal tables when 
received. This event is typically sent .....

..... security .....

..... maintains statistical information which can be used .....




-----------------------------------------------------------------------------------------------
## Security

Any responder subscribing to either the scrying event channel or the scrying cache event channel
must make a note of the requestor's [authority](../security/security.md#auth), which the
requestor must put into the scrying request object.

To put an authority into a scrying request object, the procedure `Set_Authority` must be
called, whose first parameter is the scrying request and second parameter is the authority. The
second parameter defaults to the current authority of the calling task. 

The authority put into a scrying request object is obtained by calling the function
`Authority`, whose single parameter is the scrying request. 




????? Every responder must set its own [current authority](../security/security.md#ca) to the
authority of the corresponding scrying request when calling the callback of that request.
Failure to do so will result in the call (of the callback) propagating the exception
`Security_Violation` (unless the authorities match by good fortune). 




This, in turn, means that the responder must ignore any request whose authority is not in its
[compartment's](../rts/compart.md) [ambit](../security/security.md#amb), since it cannot set
its current authority to an authority outside its compartment's ambit. 



-----------------------------------------------------------------------------------------------
## Example

.........




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## References

[1]: <http://ada-auth.org/standards/12rm/html/RM-E-2-2.html> "Ada Reference Manual, E.2.2 
     Remote Types Library Units" ?????needed?








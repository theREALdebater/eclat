-----------------------------------------------------------------------------------------------
# Remote Communications

AdaOS provides mechanisms for [effective systems](../intro/intro.md#effsys) to communicate with
one another. These mechanisms are collectively termed _remote communications_. 

Remote communications mechanisms can be divided into those used for the AdaOS Native
[platform](../pxcr/targets.md#plat) and hosted platforms. 






-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## {#}

.....




### Mechanisms

.....

 * [System objects](../objects/objects.md) and shared memory

 * Host IPC

 * Transmission Control Protocol (TCP) 

 * TCP over Transport Layer Security (TLS)

 * QUIC

 * SCTP



Facilities for executional instances, often termed 'processes', to communicate with each other
are found in most general-purpose operating systems, and the term __IPC__ (Inter-Process
Communication) is widely recognised to denote such facilities. 

.....



### Annex E

.....

[Distributed Systems Annex](#dsa)

.....


### AdaOS Native

For AdaOS Native, complete Internet Protocol (IP) stacks are provided for both version 4 (IPv4)
and version 6 (IPv6). 

......



### Hosted Platforms


The mechanisms include: 




.....



### Nyota

..... [Nyota](../tools/nyota.md) command-line tool .....




-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## Annex E (Distributed Systems) {#dsa}

The Distributed Systems Annex, or [Annex E][1] of the Ada Reference Manual lays out a way in
which an Ada program (application software) can be divided into multiple separately-executing
_partitions_, and how those partitions can communicate with one another. 

ECLAT provides a mechanism to divide a single Ada program into multiple partitions, each
partition producing a partition, with the purpose of making it possible for different
partitions to execute on different computers (without changing the essential behaviour of the
program). 




Each partition partition is built as 
a separate module, and 

Currently, the modules can be configured to operate as services within a 
single realisation or alternatively as separate realisations .....
What ECLAT does not yet support is running partitions that communicate with one another on 
different computers. This will be supported in the future ..... Syrinx .....


.....












-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 







-----------------------------------------------------------------------------------------------
## Packets {#pack}

The AdaOS Native platform ..... 

The interface type `Communication_Packet` is declared within the package `AdaOS.Communication`.
An object of a type derived from `Communication_Packet` represents a _packet_. Any such type
must be serialisable. 



.....






-----------------------------------------------------------------------------------------------
## Addresses {#addr}

An _address_ is an object of the synchronised interface type `Packet_Address` declared in the 
package `AdaOS.Communication`. 

An address is a value that contains all the information necessary to get a packet to its 
correct (intended) destination. 

The _destination_ of an address may be an [executional instance](instances.md), but it could 
also be any other entity capable of receiving packets. 

An address provides the functionality of sending packets to its destination. 

The type `Packet_Address` has one primitive operation: 

 * the procedure `Send`, which [sends a packet](#send) to the address' destination. 



.....





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Routers {#rout}

A _packet router_ is an object of the synchronised interface type `Packet_Router` declared in
the package `AdaOS.Communication`. 

The type `Packet_Router` has two primitive operations: 

 * the function `Address`, which returns an object in `Packet_Address'Class` given a string 
   value that is the address' name; 

 * the procedure `Await`, which [receives packets](#receiving) that have been sent to the 
   calling instance. 

.....

An [executional instance](instances.md) is represented by the class of types whose root type is
`Executional_Instance`, which is declared in the package `AdaOS.Execution`. This type has a
primitive operation `Router`, a function which returns an access value referencing a packet
router, known as the _instance router_. 

Currently, there is an overloading of the `Address` function which takes an `Instance`
parameter, of type `AdaOS.Execution.Executional_Instance'Class`, and returns an address, the
_instance address_. This address represents the instance as a destination for packets. 

Currently, the overloading of the `Address` function which takes a string `Name` parameter is 
unable to resolve any names. 

The instance router of an executional instance receives packets sent to its instance address. 

.....







-----------------------------------------------------------------------------------------------
## 







-----------------------------------------------------------------------------------------------
## Sending a Packet {#send}

The `Send` procedure, a primitive operation of the type `Packet_Address`, sends a packet to the 
address.

.......



-----------------------------------------------------------------------------------------------
## Receiving a Packet {#rcv}

.......



### Example 1: Footle Bartle

In this example, we imagine that we are processing 'footles' as they are sent to us by another 
instance. 

If we receive a packet that isn't a footle work unit, we just ignore it and wait for the next 
packet.

If 60 seconds pass without us getting a work unit, we exit the loop. 

```ada
   declare
      P: access constant AdaOS.Communications.System_Packet'Class;
   begin
      loop
         select
            Footle_Router.Await (P);
            if P.all in Footle_Bartle.Footle_Work_Unit'Class
            then
               Process_Next_Footle (Footle_Bartle.Footle_Work_Unit (P.all));
            end if;
         or
            delay 60.0; -- one minute
            exit loop;
         end select;
      end loop;
   end;
```





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Peers {#peer}

If two or more computers are able to communicate with each other, and are (to be) considered
part of some network, the computers are called _peers_ within that network, and ....

The [run time system](../pxcr/rts.md) maintains an array representing all the network's peers.

The array is indexed, from 1 upwards. An index value is called a _peer number_.

For each peer, the array maintains a .....






-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Remote Types {#remtyp}

.....

In contrast to an [intrinsic access type](../memory/general.md#deref), 

.....

The [run time system](../pxcr/rts.md) maintains an array representing all the system's remote objects.







This includes all active [system objects](../objects/objects.md) as well as all active
non-system objects and all ephemeral objects. 

For each remote object, the array maintains a memory pointer to the remote object ......

The array is indexed, from 1 upwards. An index value is called an _object number_. 

.....


### Remote Access Types

Each different remote subprogram of a partition is identified by a _remote subprogram 
number_. 

A _remote object_ is an object of a remote type. 

A _remote object number_ is a notional place reserved by 

?????an [executional instance](Instances.md) 

[compartment](compart.md)

for a 
remote object by an instance in which that remote item exists. 

Each remote object number is an integer between 1 and the 
remote object limit for the 

?????instance

compartment. 

```ada
type Remote_Object_Number is new Realizor.Unsigned_Integer;
```






Each 

?????instance 

compartment

keeps a _remote lookup_ table, which maps remote object numbers to the memory address of 
the remote object, and also keeps administrative information for each object. 

.....

```ada
Remote_Lookup: array (Remote_Object_Number range 1 .. Self.Remote_Object_Limit) of System.Address;
```


### ?????

A remote access type contains a different value depending on whether object it references is local or remote. 

If it is local:

 * a memory pointer to the object; 
 
If it is remote: 
 
 * the peer number of the computer the object resides on; 
 
 * the compartment number in that computer in which the object resides; 
 
 * the object number of the object in that compartment. 

.....

as if with the following declaration:

```ada
type <remote_access_to_subprogram_type>
is
   record
      Peer:          Peer_Number;
      Compartment:   Compartment_Number;
      Subprogram:    Remote_Subprogram_Number;
   end record;

type <remote_access_to_object_type>
is
   record
      Peer:          Peer_Number;
      Compartment:   Compartment_Number;
      Object:        Remote_Object_Number;
   end record;
```

......


### Remote Procedure Calls

.....




-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## References

[1]: <https://ada-auth.org/standards/12rm/html/RM-E.html> "Ada Reference Manual, Annex E:
     Distributed Systems"

[2]: <https://datatracker.ietf.org/doc/html/rfc4960> "RFC 4960: Stream Control Transmission
     Protocol"

[3]: <https://datatracker.ietf.org/doc/html/rfc9000> "RFC 9000: QUIC: A UDP-Based Multiplexed
     and Secure Transport"






-----------------------------------------------------------------------------------------------
# Sockets

An AdaOS _socket_ is an abstraction of a two-way communication path between two separate pieces
of executing software. 

In particular, the two separate pieces of executing software may be executing on different
computers. Some technology enables the computers to communicate with each other, but that is
(normally) irrelevant to the sockets and software using them. 

, .....


Communication is entirely in terms of _packets_. A packet is sequence of standard Ada stream
elements (see ARM 13.13.1). Packets can be sent in either direction through the socket. 



The (piece of executing software) two ends of a socket are called 





The sockets of a system (which is generally one computer) are identified by number, an integer 
from 0 upwards; there will be an upper limit, but that is implementation defined. 

A process _listens_ to a specific socket; only one process at a time can be listening to a 
specific socket, so when a process starts listening to a socket, that socket is locked until 
the process stops listening to it. The listening process is called the _service_ process. Any 
number of other processes, called _client_ processes, can send data _packets_ to any socket; 
the listening service process receives the packets and send packets back to the client in 
response. 

An RPC socket is a special kind of socket. The two communicating processes (the service and the 
client) send a particular kind of packets to each other, designed to simulate the interaction 
between a procedure when it is called by another procedure within a conventional program. It is 
as if the service process has a set of _remote procedures_ that the client process calls. 

Accordingly, each packet sent by the client to the service represents a call to one of the 
remote procedures, and is called an _invocation packet_. The service can send a _response 
packet_ back to the client. 

An invocation packet must contain all the data pertaining to the call (invocation) of a remote 
procedure:

 * It must identify which remote procedure is being invoked; generally the service will offer 
   several. 
   
 * It must contain certain flags, of which more later.
 
 * It must contain the values for all of the in-parameters of the remote procedure. 

.....



ECLAT only requires the RTS to provide RPC sockets; not any other kind of sockets. Given that 
these RPC sockets are an abstraction of actual mechanisms and protocols, it is quite possible 
to use them to simulate any other kind of socket. 

From the point of view of executional efficiency, this makes sense; an RPC socket may actually 
by implemented as a traditional procedure call, which will almost certainly be the most 
efficient mechanism available on any hardware. Since ECLAT will (eventually) be aiming at the 
highest possible level of executional optimisation, .....



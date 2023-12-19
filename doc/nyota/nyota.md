-----------------------------------------------------------------------------------------------
# Nyota

__Nyota__ is an Ada source text library and command-line tool supporting distributed computing,
where an overall computing objective is met by multiple pieces of software running on the same
or different computers. 

For example, 

.....





-----------------------------------------------------------------------------------------------
## 


-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Communication Model

The different pieces of software in a distributed computing scenario need to be able to
communicate with other.

A great deal of experience proves that it is very wise to manage this communication, mediating
it by well-defined interfaces. Furthermore, it is wise to take the opportunity to enable the
different pieces of software, as far as is practicable, to be built using different programming
languages and different software preparation technologies (compilers). 

These well-defined interfaces are called _service interfaces_ (often confusingly called
Application Program Interfaces, or APIs), and are based on the _client-server_ concept. 

The client-server model always casts the two pieces of software that are communicating, via a
specific service interface, into two roles: one piece is the _client_, and it usually initiates
a round of communication; the other piece is the 'server'---actually, we will term it the
_service_ hereafter, to try to avoid confusion with the use of 'server' to mean a computer
whose main purpose is running services---and its job is to fulfil the requests of the client. 

Building on this client-server concept, Nyota also adopts the Remote Procedure Call (RPC) style
of communication, and also (on top of that) object-oriented method calls. 





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
## Interface Technologies (Protocols) {#prot}

Nyota is based on [remote procedure call]

supports multiple different interface technologies 

























```xml

<nyota xmlns="">
   <service-interface name="">
      <types>
         ...
      </types>



   </service-interface>
</nyota>
```







-----------------------------------------------------------------------------------------------
## Command-Line Tool

The `nyota` command-line tool generates Ada source text that forms a skeleton for remote
communications presented to the application software layer as 
[system objects](../objects/objects.md). 

The tool reads a set of NyLan source text files, as well 
as configuration information, and generates a corresponding set of Ada source 
text files.

For each service interface, the Ada source text generated comprises:

 * a package specification that can be used by a client program to use the service interface

 * the corresponding package body

 * a package specification that can be used by a server program to handle incoming 
   requests to the service interface

 * a corresponding package body, with `is separate` stubs for every method/procedure 
   of the service interface, and also using (and parametrised by) the configured protocol

 * for each such stub, a skeleton corresponding separate procedure body (if it doesn't 
   already exist), which can be filled in to implement the procedure



to generate the following Ada
source text code: 

 * skeleton for the service itself

 * complete implementation of the service program 

 * complete implementation of the replication controller service and its service program 

 * skeleton for a test harness program 

 * in the future, optionally, a web API (SOAP or REST) wrapper program, which would be either
   an FCGI program or a plug-in for Larunda. 










-----------------------------------------------------------------------------------------------
## 


-----------------------------------------------------------------------------------------------
## 


-----------------------------------------------------------------------------------------------
## 


-----------------------------------------------------------------------------------------------
## 


-----------------------------------------------------------------------------------------------
## Replication {#rep}

.....

Each method (primitive operation) of a service is assigned one of three _replication routing_
settings: 

 * _pan-replicant routing_

 * _single-replicant routing_

 * _custom routing_ 

A method is, by default assigned as pan-replicant if it is not explicitly assigned. 








### Pan Replicant Routing

When a call is made to a method with pan-replicant routing, the same call is routed, by the
replication controller, to the same method of *all* the currently active replicants. 

Thus, the method will be executed in each of the replicants, and will receive the same values
for all its input parameters. 

Each replicant is therefore expected to update its state (files, database, anything that is
persistent) identically, and is expected to return the same values in its output parameters. 

The replication controller gathers the return values of the output parameters from all of the
replicants, and checks that they are all equal. It waits for all of the replicants to finish
execution. If any response is different, or if any (or all) of the method invocations fails (by
propagating an exception), then .....

.....







### Single Replicant Routing

When a call is made to a method with single replicant routing, the call is routed, by the
replication controller, to the same method of any one of the currently active replicants, chosen
at random. 

.....

Single replicant routing is suitable for methods that do not (significantly) change the
service's state. Often a service has many 'query' methods that only return information derived
from the service's state, but which do not modify the state. Since each replicant has the same
state as all the others, it should be sufficient for any one of the replicants to handle this
kind of method. 




### Custom Routing 













-----------------------------------------------------------------------------------------------
## Configuration

Nyota configuration items specify aspects such as:

 * What kind of protocol is to be used, e.g.:

    - Native Ada RPC over IP/UDP (maybe secured by DTLS)

    - REST web API using JSON

    - non-REST web API using SOAP/XML

    - etc.

 * Rate limiting, throttling, maximum request size, etc.

 * Handling of errors, error codes, etc.

 * Use of e.g. OAuth 2.0 for request authentication

 * Mapping of Nyota-defined types, procedures, classes, and methods to the equivalent 
   entities of the target protocol

 * Mapping from Nyota identifiers to those to be used in (non-Ada) programming languages












-----------------------------------------------------------------------------------------------
## Example: Dropbox



-- Example Nyota interface definition language: Dropbox

-- Web API using JSON https://www.dropbox.com/...






```ada






```






```ada
interface Dropbox.OAuth2 is

   -- .../oauth/token

   procedure Get_Token( Code:          in String; 
                        Grant_Type:    in String; 
                        Client_ID:     in Nullable_String;
                        Client_Secret: in Nullable_String;
                        Redirect_URI:  in Nullable_String;
                        Access_Token:  out String;
                        Token_Type:    out String; -- will always be "bearer"
                        Account_ID:    out Nullable_String;
                        Team_ID:       out Nullable_String;
                        UID:           out String ); -- deprecated and should never be used

```









-----------------------------------------------------------------------------------------------
## References





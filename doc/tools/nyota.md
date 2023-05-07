-----------------------------------------------------------------------------------------------
# Nyota

__Nyota__ is an Ada source text library and command-line tool supporting distributed computing,
where an overall computing objective is met by multiple pieces of software running on the same
or different computers. 

For example, 

.....








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
communications presented to the application software layer as [system
objects](../objects/objects.md). 

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




Nyota uses the Ockham Configuration Framework (q.v.) to get configuration data.


-----------------------------------------------------------------------------------------------
## Converters

Nyota is accompanied by command-line tools that can convert from other IDLs into Nyota. Often these 
tools must use heuristics, for example to create Ada-convention identifiers, but the
tools take the drudgery, and some of the risk of error, out of doing such conversions 
purely by hand. 

The results of a conversion will usually need to be inspected and corrected by hand.










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





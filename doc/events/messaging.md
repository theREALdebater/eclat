-----------------------------------------------------------------------------------------------
# Messaging

The [event](events.md) model can be---and should be---used as the basis of _messaging_
mechanisms. 

Messaging is a way for the separately executing parts of an overall software system to 
communicate with one another (in particular between different computers connected in a 
network). This approach to communication can have decisive advantages over Remote Procedure 
Call (RPC) in some scenarios. 

Although an RPC interface between separate parts of a software system is easier to use, since
calling the remote subprograms (procedures and functions) is almost the same as calling
ordinary local subprograms, it can have practical problems precisely because it ignores the
extra complexities of actually transporting the call requests and replies over a network. 

Unlike a local call, a remote call can fail because the transport of its request, or the 
corresponding reply, fails (or times out). Even if the transport does not fail altogether, it 
may be delayed by what is, for software, a very long time (even if this is actually only a few 
seconds), having undesirable effects that would never occur if the call were local. 

Messaging is able to deal more realistically with the exigencies of network communications. 
Since messages are one-way, asynchronous, and queued, the sender is not directly delayed by any 
delay or failure in the transport of the messages. 

Usually, a messaging approach is not so easy to use, because it requires a receiver to handle
incoming messages (for example, acknowledging the processing of a previous outgoing message),
and serial numbers being placed in messages to help identify them, and so on. But the payoff is
that this approach can be more resilient to the problems and disturbances inherent to
distributed computing. 

The ECLAT event model framework is specially designed to fit the requirements of messaging, and 
it should be used to implement a messaging mechanism whenever possible. 

For example, an event broker could use a network transport service to ferry events to brokers 
in other computers in the network. It could implement its own queue to buffer events waiting to 
be sent across the network. 




-----------------------------------------------------------------------------------------------
## What is Messaging? {#}

One way of explaining messaging is to start with an example that uses RPC to communicate
between two tasks that might be running on different computers, and then to show the same
example changed to use messaging. This same example can be used to demonstrate the potential
benefits of messaging, compared to RPC, for certain scenarios. 

The example imagines that a central traffic controller sends instructions to a set of (several
dozen) different traffic lights to change colour. 

Let us assume it is vital that every light, when sent a signal to change its colour, must
return a reply acknowledging that it has received the change of colour and actioned it. 

These declarations help to set the scene:

```ada

type Light_Proxy is limited private;

type Traffic_Light is new Positive range 1 .. 200; -- there are 200 different traffic lights

type Light_Status is (Red, Amber, Green, Amber_And_Red);

function Next is new Cyclic_Succ (Light_Status);

Proxies: array (Traffic_Light) of Light_Proxy;

task Traffic_Controller
is
   entry Start;
   entry Stop;
end;

task body Traffic_Controller
is
   Old_Statuses, New_Statuses: array (Traffic_Light) of Light_Status;
   Is_Changed: array (Traffic_Light) of Boolean;
begin
   accept Start;
   loop
      select
         accept Stop
         do
            exit;
         end;
      or
         delay 0.001; -- one millisecond
         New_Statuses := <calculate new light colours>;
         Is_Changed := <all lights whose old status /= new status>;
         
         for Light in Traffic_Light
         loop
            if Is_Changed (Light)
            then
               Proxies(Light).Change (New_Statuses(Light)); -- RPC
            end if;
         end loop;
      end select;
   end loop;
end;
```


````

A sketch of the RPC solution follows: 

```ada

type Light_Proxy is limited
   record
      ...
   end record;

function Change (Proxy: in out Light_Proxy; Status: in Light_Status)
is
begin
   <make an RPC to change the light's colour, and wait for it to reply>;
end;
```

The inner loop (`for Light in ...`) is significant here. The essential point to note is that,
using this simple RPC approach, we have to wait for each light to reply before a signal to
change the next light is sent. This is very inefficient. Supposing each call to `Change` takes
about 100 milliseconds, then the entire loop could take more than 10 seconds to execute. This
may be simply too slow. 

In order to call `Change` on every changed light simultaneously, we could have 200 separate
tasks all calling at once, but this would entail the overhead of 200 tasks. There is a
potentially better way. 

.....

```ada

type Light_Proxy is limited
   record
      ...
   end record;

function Change (Proxy: in out Light_Proxy; Status: in Light_Status)
is
begin
   ;
end;
```

















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
## Messaging Package {#}

The generic package `AdaOS.Events.Messaging` provides a convenient abstraction of many of the 
details of using events to implement effective messaging functionality. 

.....






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
## {#}










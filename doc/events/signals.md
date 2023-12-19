-----------------------------------------------------------------------------------------------
# Program Signals

A _program signal_ is a kind of communication from a [compartment](../programs/compart.md) to
its [executional instances](../programs/instances.md). 

......




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 

The interface type `Program_Signal`, visibly declared in the package `AdaOS.Programs.Signals`,
represents a program signal .....

A program signal is a kind of [event](events.md). The type `Program_Signal` is derived from the
type `Event_Object` which is declared in the package `AdaOS.Events`. 

......




-----------------------------------------------------------------------------------------------
## 

When a [compartment](../programs/compart.md) is created, a special
[event channel](../events/events.md#chan) is created for the compartment to send events to its
[executional instances](../programs/instances.md). 

The name of the event channel is:

    system.signals.C

where `C` is the name of the compartment.

......




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




?????-----------------------------------------------------------------------------------------------
?????## Signal Queue {#queue}

?????Every [executional instance](../rtc/instances.md) has a _signal queue_, which is initially 
?????empty. 

?????Whenever a signal is received by the instance, the signal is pushed onto its signal queue.

?????.......




-----------------------------------------------------------------------------------------------
## 




?????-----------------------------------------------------------------------------------------------
?????## 

?????The procedure `Await_Signal`, declared in the package `AdaOS.Signals`, can be called to wait 
?????for the next signal to be received. 

?????......




?????The procedure `Fetch_Signal`, also declared in the package `AdaOS.Signals`, can be called to 
'peek' as to whether a signal has been received. 

?????This procedure always returns immediately: 

????? * If a signal has not been received, this procedure returns with its `Signal` out-parameter 
   set to `null`; 
   
????? * if a signal has been received, this procedure returns with its `Signal` out-parameter 
   set to the received signal. 

?????Both these procedures remove the signal from the [signal queue](#queue) unless `null` is 
returned in the `Signal` out-parameter. 



-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## Standard Signals

.....

The following standard signal types are declared in `AdaOS.Signals`:

| Type                   | Description 
| ---------------------- | --------------------------------------------------------------------
| `Program_Abort_Signal` | "abort", abnormal termination 
| `Numeric_Error_Signal` | floating point exception 
| `Attention_Signal`     | "interrupt", interactive attention request sent to the program 
| `Terminate_Signal`     | "terminate", termination request sent to the program 
| `` |  
| `` |  
| `` |  
| `` |  
| `` |  
| `` |  

??????These signals are all empty (they have no components of their own). They will inherit the
components of the type `Program_Signal` that they all derive from, but these components are
private to the Ada implementation. 



Each of these different signal types are declared as types derived from `Program_Signal`, ......



.....







### `Program_Abort_Signal` (Posix `SIGABRT`)

"abort", abnormal termination


### `Numeric_Error_Signal` (Posix `SIGFPE`)

floating point exception



### `Attention_Signal` (Posix `SIGINT`)

"interrupt", interactive attention request sent to the program


### `Terminate_Signal` (Posix `SIGTERM`)

"terminate", termination request sent to the program

..... shut down (stop executing), immediately or very quickly, for ......


### `Continue_Signal` (Posix `SIGCONT`)

.....


### `Fatal_Signal`

.....

`Program_Abort_Signal` or `Numeric_Error_Signal` or `Terminate_Signal`


### ``

.....


### ``

.....


### ``

.....


### ``

.....







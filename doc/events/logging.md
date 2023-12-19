-----------------------------------------------------------------------------------------------
# Logging

Whenever something happens, during the execution of a program, that the developers (or 
testers, evaluators, etc.) might want to know about, it is convenient to be able to write some 
information about it to a conceptual (or actual) store that is generally called a 'program 
execution log', or 'program log', or just _log_. 

AdaOS provides a standard infrastructure for _logging_. This infrastructure is based on the
AdaOS [event](events.md) infrastructure, but with extra functionality that can be used 'out of
the box' by most programs. 

AdaOS also provides an infrastructure for [auditing](auditing.md) based on the logging
infrastructure. 


.....







-----------------------------------------------------------------------------------------------
## .....

The package `AdaOS.Logging` provides convenient logging facilities for Ada programs. 

When this package initialises, it .....

.....



-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
## Logging Levels {#lev}

.....

from highest to lowest:

| Name          | Usage                                                             |
| ------------- | ----------------------------------------------------------------- |
| `Critical`    | The program has failed and cannot continue execution.             |
| `Error`       | Program execution can continue, but likely to be a failure.       |
| `Warning`     | Serious issue, but program can probably continue successfully.    |
| `Caution`     | Might be a serious issue, but program unaffected.                 |
| `Important`   | Something the user needs to know, but has no impact on program.   |
| `Note`        | Things the user might want to know, but could be ignored.         |
| `Debug`       | Information only for the programmer likely to aid debugging.      |
| `Trace`       | Fully detailed information for the programmer.                    |

.....

The package `AdaOS.Logging` contains the following declaration:

```ada
   type Logging_Level
   is
      (Trace, Debug, Note, Important, Caution, Warning, Error, Critical);
      
   All_Levels: constant Logging_Level := Trace; -- at this level, all events will be logged
```

.....


### Logging Level: Critical

This level is for events that are expected to result in a program or software subsystem to 
immediately terminate or to be rendered unable to continue with its main functionality. 

For example, an exception that is caught, but cannot be handled in a way that resolves it, 
would be logged at this level. 


### Logging Level: Error

This level is for events that are associated with something seriously going wrong. Probably 
this kind of event indicates a serious fault in a program or software subsystem. It is likely 
that some significant portion of functionality has failed or is unavailable. 

For example, this level would be used to log an exception that is caught and handled in a way 
that partially resolves it, perhaps by tidily aborting an attempted important operation, and 
cleaning up after it. 


### Logging Level: Warning

This level is for events where something is seriously wrong, but functionality substantially 
remains viable. Something is wrong enough that it needs urgent investigation. 

For example, this level would be used to log an exception that is caught and handled for an
unimportant function, for example a function that attempts to tidy up data for the program. 


### Logging Level: Caution

This level is an event that, whilst not indicating that anything is seriously wrong, the user 
needs to be aware of a potential problem or pitfall. 

For example, this level would be used if a program detected that it is getting close to running
out of a resource that it needs, such as storage space. 


### Logging Level: Important

This is the 'normal' logging level, for events that are significant enough to warrant being 
logged, but do not indicate anything is wrong. 

For example, the normal starting up and the normal shutting down of a program would be logged 
at this level. 


### Logging Level: Note

This level is for an event that, whilst is is logged during the normal operation of the 
program, is only to give a little 'background' information on what is going on. The 
implication is that events at this level can be discarded (cleared, purged) with impunity. 

Typically, a lot of information will be logged at this level. 

For example, parameters of the program's execution, such as pivotal values, modes, and so
forth, from program arguments, environment variables, [configuration](../intro/config.md), or
other sources, may be reported at this level. 

This is the default logging level for all the logging procedures provided by AdaOS. 


### Logging Level: Debug

This level is used for significant events that may provide information to assist testers and
programmers in the testing and debugging of software. 

For example, this level might be used to report that the program is about to enter a loop that
processes many items. The programmer may be interested that execution has reached this point in
the program, perhaps, because there seems to be a program that occurs either before or after
the loop, and the programmer is trying to find out which. 


### Logging Level: Trace

This level is also used, besides the Debug level, for events that provide information to assist testers and programmers
in the testing and debugging of software.

However, the Trace level is used for events that are extremely detailed, rather than the
significant debugging events of the Debug level. 

For example, this level might be used to report the execution of each iteration in a loop that
processes many items. The programmer may, perhaps, have determined that a problem occurs
somewhere within the loop, and is trying to trying to pinpoint it. 

Because the loop, in this example, processes many items, a large amount of logging information
will be produced. So the programmer has it logged a Trace level so that it can be filtered out
or turned off for those runs which do not require it, without losing the essential information
logged at Debug level. 


-----------------------------------------------------------------------------------------------
### Locus {#loc}

An _execution locus_ is an opaque value that identifies the library in which the body of a
subprogram was declared. 

.....

The package `System.Composition` contains the following visible declarations:

```ada
type Execution_Locus is private;

function Current_Locus return Execution_Locus with Inline, .....;

function Locus (E: in Exception_Information) return Execution_Locus;

function Name (Locus: in Execution_Locus) return String;

function Wide_Name (Locus: in Execution_Locus) return Wide_String;

function Wide_Wide_Name (Locus: in Execution_Locus) return Wide_Wide_String;
```

The function `Current_Locus` returns the locus of the innermost enclosing subprogram body of
the declaration or statement in which the function `Locus` was called. 

The function `Name` returns the full name of the library identified by an `Execution_Locus`
value. There are also functions `Wide_Name` and `Wide_Wide_Name`. 


-----------------------------------------------------------------------------------------------
## Logging Events

The private abstract type `Logging_Event`, derived from `Event_Object` (declared in the package
`AdaOS.Events`), declares the function `Level`, which should return the logging level of the
event. By default, it returns `Note`. 

.....


### Level

Generally, concrete logging event types should override this function to return the appropriate
[logging level](#lev) for events of the type. In most cases, this is likely to be a constant
for any one type, but it might be variable sometimes. 


### Locus

The following function is declared in the visible part of the package `AdaOS.Logging`:

```ada
function Locus (Event: in Logging_Event) return Logging_Locus is abstract;
```

This function returns the [locus](#loc) associated with the event. 

This is set to the value of the `Locus` parameter of all the logging procedures provided by
AdaOS. This parameter always has a default which calls the `Current_Locus` function, so, by
default, this will indicate the declaration or statement in which the given logging procedure
was called. 


-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
## Current Logging Broker {#currbrok}

A _current logging broker_ can be configured for a [compartment](../rts/compart.md). 

The function: 

    AdaOS.Execution.Task_Instance.Compartment.Logging_Broker

returns the current logging broker, of type `access AdaOS.Events.Event_Broker'Class`, of the 
calling [executional instance's](../pxcr/instances.md) compartment. 

The procedure:

    AdaOS.Execution.Task_Instance.Compartment.Set_Logging_Broker (B);

.....

The default value is the [system broker](?????). 

?????In most situations, the system broker will be the most appropriate broker to use for logging. 











-----------------------------------------------------------------------------------------------
##


A _current logging level_ can be configured for a [compartment](../rts/compart.md).

The function:

    AdaOS.Execution.Task_Instance.Compartment.Logging_Level

returns the current logging level of the calling [executional instance's](../pxcr/instances.md)
compartment. 

The procedure:

    AdaOS.Execution.Task_Instance.Compartment.Set_Logging_Level (Level);

can be called to change the logging level to `Level`.

The convenient constant `All_Levels` can be used to indicate that everything should be logged.

The default level is `Important`.





-----------------------------------------------------------------------------------------------
##


### 'Log' Procedure

The package `AdaOS.Logging` contains the following declaration:

```ada
procedure Log (Event:  in Logging_Event'Class;
               Ignore: in Boolean := False;
               Locus:  in Execution_Locus := System.Composition.Current_Locus)
with
    Inline;
```

This procedure automatically checks that the `Ignore` parameter is not `True` and that the
`Level` of the given `Event` is higher or equal to the currently configured logging level (got
by calling `Task_Instance.Compartment.Logging_Level`). Otherwise, the call to `Log` does
nothing. 

Because `Log` is declared as inline, this check will efficiently eliminate logging calls that 
are not currently required. 

In particular, make the actual `Event` parameter of this procedure an expression that
constructs the event object. If the event is not to be logged, the expression will not be
evaluated, so the computing resources (processor time, memory, etc.) necessary to construct the
event object will not be used up needlessly. If the expression is too big, pot it into a
function; ensure that the function is marked as `Pure`, so the call to it can be eliminated if
appropriate. 

The `Ignore` parameter is there as a convenient way to control logging based on an expression. .....



-----------------------------------------------------------------------------------------------
## Logging a Text Message

The package `AdaOS.Logging` .....

```ada
type Text_Logging_Event (Level: Logging_Level := Note) is new Logging_Event with private;
```
.....

```ada
function Text (Event: in Text_Logging_Event) return Unbounded_String;

procedure Set_Text (.....);
```





The non-standard extension package `Ada.Text_IO.Logging` contains the following declaration: 

    function Standard_Logging return File_Type;

Each completed line sent to this file causes a new `Text_Logging_Event` value to be
constructed, whose `Text` property is set to the line (without any line termination control
characters) and, if the line is not empty or only contains whitespace, is sent to the event
class returned by a call to the function `Current_Logging`. 

A global property `Current_Logging` is also declared in `Ada.Text_IO.Logging`, which is
initialised to `Standard_Logging` unless logging is [redirected](../rts/compart.md#redir) for
the program instance. 

A global property `Current_Logging_Level` is declared in `Ada.Text_IO.Logging`, which is
initialised to `Note`. This is the [logging level](#lev) used to construct the
`Text_Logging_Event` objects. 



-----------------------------------------------------------------------------------------------
##






-----------------------------------------------------------------------------------------------
##






-----------------------------------------------------------------------------------------------
##






-----------------------------------------------------------------------------------------------
##












-----------------------------------------------------------------------------------------------
## Exception Logging

The package `AdaOS.Logging.Exceptions` defines certain fundamental abstract types for logging
exceptions. 

.....

The type `Exception_Event`, which is derived from `AdaOS.Events.Event_Object`, represents an 
exception occurrence in a form suitable to be logged. 

.....


### .....



A _current exception logging event channel_ can be configured for a 
[compartment](../rts/compart.md).

The function: 

    AdaOS.Execution.Task_Instance.Compartment.Exception_Logging

returns the current exception logging event channel, of type `access 
AdaOS.Events.Event_Channel'Class`. 

This function never returns null. If, when this function is called---or any other subprogram
that uses the the current exception logging event channel---and the current exception logging
event channel is not set, then a new event channel object, with the name `"C.exceptions"`
(where `C` is the name of the compartment) and [current logging broker](#currbrok), is
automatically created. 

The procedure `Set_Exception_Logging` has two overloadings.

Calling: 

    AdaOS.Execution.Self.Compartment.Set_Exception_Logging (C);

with one parameter `C`, of type `access AdaOS.Events.Event_Channel'Class`, sets the current
exception logging event channel to `C`. This procedure can be called with an parameter value of
null, to unset the current exception logging event channel (so that a new one will be
automatically created on demand, as described just above). 

However, calling:

    AdaOS.Execution.Task_Instance.Compartment.Set_Exception_Logging ("foobar");

with a parameter of type `String`, creates a new event channel object using the value of this 
parameter as a name. 

The procedure: 

    AdaOS.Logging.Log (E, L);

has the parameter `E`, of type `Ada.Exceptions.Exception_Information`. 

This procedure creates an `Exception_Event` object from the exception information `E`, and then
sends it to the current exception logging event channel. 

The parameter `L`, of type `Logging_Level`, determines the [logging level](#lev) of the
`Exception_Event` object, and the level at which the exception is logged; it is optional, and
has a default of `Error`. 

The [locus](#loc) of the `Exception_Event` object is derived from the exception information `E`
using the `Locus` function. 



-----------------------------------------------------------------------------------------------
## Unhandled Exceptions

The non-standard extension package `Ada.Exceptions.Logging` contains the declaration of an
access-to-procedure type:

    type Exception_Handler is access procedure (X: in Exception_Occurrence);

and of a global variable of this type:

    Handle_Unhandled_Exception: Exception_Handler;

as well as a function:

    function Default_Unhandled_Exception_Handler return Exception_Handler;

An _exception handler_ is a procedure that takes one in-parameter of type `Exception_Occurrence`. 

.....

If an exception `E` is propagated out of the program's main procedure, then: 

 * If `Handle_Unhandled_Exception` is not null, then a call to `Handle_Unhandled_Exception (E)`
   is automatically made. If an exception is propagated out of this call, the exception is
   ignored. 

 * If `Handle_Unhandled_Exception` is null, exception `E` is ignored.

In both cases, the program is then terminated .....



When the program starts, `Handle_Unhandled_Exception` has the value returned by a call of
`Default_Unhandled_Exception_Handler`, which never returns null, and has the following
behaviour: 

 * .....




-----------------------------------------------------------------------------------------------
### Exception Pin-Tracing

The non-standard extension package `Ada.Exceptions.Logging` 

also provides a mechanism for logging all 
exceptions wherever they occur and regardless of whether they are caught and handled, or how 
they are handled, which is called _exception pin-tracing_. 

.....

Package `Ada.Exceptions.Logging` declares the type: 

The procedure `Set_Exception_Tracer` allows an exception tracer to be set. It takes one
in-parameter, named `X`, of type `Exception_Occurrence`, which is allowed to be null. 

Whenever an exception is raised (regardless of it being handled or not), if an exception 
tracer has been set (and there is no pin, q.v., set for the exception and place):

 1. the exception tracer procedure is called, with its parameter set to the exception; 
    
 2. a binary flag, called a _pin_, is set for the combination of the exception raised and the
    place (scope) where it was raised. 
    
A pin prevents a particular exception being raised in a particular place being traced more 
than once. This may be very important, since a handled exception may end up being raised a 
very large number of times. 

It is not advised that a normal, working program tends to raise an exception a large number of 
times, but the pinning mechanism is there to accommodate programs which do this.

?????All pins which have been set will be reset whenever `Set_Exception_Tracer` is called. 

?????Procedure `Set_Exception_Tracer` can be called with a null parameter in order to stop 
pin-tracing. It could subsequently be called again, with the same or a different exception 
tracer, to start pin-tracing once more. 



Count instead of pins?



    pragma Pin_Trace("foobar");



Default pin tracer? (Debugging)




-----------------------------------------------------------------------------------------------
## Exception Information

The standard package `Ada.Exceptions` declares the function `Exception_Information`, which
returns a `String` value. The ARM recommends that this function returns information that is
useful for debugging, but what this function returns is implementation defined. 

For the ECLAT [Ada Library](../adalib/adalib.md) implementation, this function returns a string
structured as a multi-line plain-text document, and uses an _exception enhancement_
architecture to provide the content of this document, aside from the exception message, which
is automatically added. 






For the ECLAT Ada Library implementation, the function `Exception_Information` is accompanied
by two further, non-standard, declarations that are the same except that the return type is
`Wide_String` or `Wide_Wide_String`. 






### Exception Enhancers

The non-standard package `Ada.Exceptions.Enhancers` contains declarations of the 

```ada
type Exception_Enhancer is tagged with private;

procedure 



procedure Register (Enhancer: access Exception_Enhancer'Class);

procedure Unregister (Enhancer: access Exception_Enhancer'Class);

```





### Predefined Enhancements

There are several _predefined enhancers_ made declared within package `Ada.Exceptions.Enhancers` .....



all declared as `aliased` objects, so they can be passed straight into the procedure `Activate_Enhancer`. 




### Stack Trace

The non-standard package `Ada.Exceptions.Enhancers.Stack_Tracing` contains the declaration of
the type `Stack_Trace_Enhancer` and the object `Stack_Tracer`. 

The object `Stack_Tracer` is an 'aliased' object of the type `Stack_Trace_Enhancer`. This
object adds a stack trace to `Exception_Information`. 

The format in which the stack is added is controlled by the following properties of the type
`Stack_Trace_Enhancer`: 

 * `Stack_Order` is of the enumerated type `Stack_Tracing_Order`, which has the two literals
   `Most_Recent_First` and `Most_Recent_Last`. The default is `Most_Recent_First`. 

 * `Maximum_Stack_Depth` is a `Natural`, and ..... This property has the default value of 20. 

 * `Maximum_Access_Depth` is a `Natural`, and determines how many times the ...... This property has the default value of 5. 



These are properties. Remember that the value of a property `P` is retrieved by a function
named `P` and set by a procedure named `Set_P`. 





```xml
<stack-trace xmlns:st="....." order="most-recent-first">
   <frame>
      <procedure>ACME.Gardening.Tools.Rake.Pull</procedure>
        <parameter>
            <name>Strength</name>
            <mode>in</mode>
            <type>Integer</type>
            <value>10</value>
        </parameter>;
        <parameter>
            <name>Grip</name>
            <mode>in</mode>
            <type>ACME.Gardening.Tools.Rake.Grip_Type</type>
            <default>Hands_Apart</default>
            <value>Hands_Apart</value>
        </parameter>
        <variable>
           .....
        </variable>



   </frame>
   ...
</stack-trace>
```




### Example



```ada
```








-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
## Default Event Classes

.....

To log an event, the event should normally be sent to the event channel `System.Logging`.

Although logging event consumers should always provide a means by which they can be configured
to receive from specific event channels, they should receive from the channel `System.Logging`
by default. 







-----------------------------------------------------------------------------------------------
## Log Capture

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
##





-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
##





-----------------------------------------------------------------------------------------------
##






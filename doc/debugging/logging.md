-----------------------------------------------------------------------------------------------
# Logging and Auditing

We introduce a logging and auditing infrastructure that is used by all the ECLAT and Realizor 
programs, and can be used by any Ada program.

Logging and auditing is based on [events](../events/Events.md). 

.....




-----------------------------------------------------------------------------------------------
## Logging Events (#logevents)

A _logging event_ is something that happens during the execution of a program which might need 
to be recorded for subsequent analysis. 

Typically, logging events are needed to ascertain:

 * areas of inefficiency in the program; or 
 
 * deviations from expected behaviour of the program; or 
 
 * something that went wrong. 

A logging event is represented by an object of a type derived from the interface type, 
`Logging_Event`, which is declared in the package `AdaOS.Logging` and is itself derived from 
the type `Event_Object` (declared in the package `AdaOS.Events`). 

.....







-----------------------------------------------------------------------------------------------
## Auditing Event (#auditevents)

An _auditing event_ is something that happens during the execution of a program which does 
(without any doubt) need to be recorded, and must not be repudiable. 

.....

An auditing event is represented by an object of a type derived from the interface type, 
`Auditing_Event`, which is declared in the package `AdaOS.Auditing` and is itself derived from 
the type `Logging_Event`. 

.....



-----------------------------------------------------------------------------------------------
## 

A logging event is _logged_ simply by being sent to a logging event channel.

.....





-----------------------------------------------------------------------------------------------
## 

An auditing event is *not* normally sent to any event channel. Instead, an auditing event is 
stored by making a call to the `Log` procedure of an _audit recorder_. 

.....

An audit recorder is an entity that stores audit events in a manner that accords with the 
principles of (computerised) auditing:

 * it must not be possible for the storage of an event to fail (it must be possible to retrieve 
   every event that has been stored); 
 
 * it must not be possible for the same event to be accidentally stored twice; 
 
 * as a matter of security, it must not be possible for unauthorised software to interfere with 
   the storage of audit events; 
 
 * .....
 
The principles of event security also apply to audit events; .....

An audit recorder is represented by an object of a type derived from the interface type 
`Audit_Recorder`, which is also declared in the package `AdaOS.Auditing`

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










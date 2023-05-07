-----------------------------------------------------------------------------------------------
# Auditing

When it is necessary to .....

When something happens that it may be necessary to prove happened later on, or that has 
information associated with it that may need to be retrievable later on, information about it 
is written to what is very often called an 'audit log', or just _audit_. 

AdaOS provides a standard infrastructure for [logging](logging.md) and _auditing_. This 
infrastructure is based on the AdaOS [event](events.md) infrastructure, but with extra 
functionality that can be used 'out of the box' by most programs. 




-----------------------------------------------------------------------------------------------
## 

The standard AdaOS event forwarding middleware enables an event channel to marked as _audited_. 

When an event is forwarded to an audited event channel, the event is (serialised and) forwarded 
to an _auditor_, at the same time as being forwarded onward to the normal logging mechanism. 

An auditor is an object that is passed the event and is expected to record (store or save) the 
event in a way that accords with auditing requirements. 


.....

Every (system object that is an) event channel has a primitive operation which is the procedure 
`Audit`:

```ada
procedure Open_Audit (Class:   in out AdaOS.Events.Event_Class; 
                      Auditor: in out AdaOS.Events.Auditing.Auditor'Class);
```

.....

```ada
procedure Close_Audit (Class: in out Event_Class);
```
.....


-----------------------------------------------------------------------------------------------
## Auditors {#auditors}

An _auditor_ is an object that is passed the event and is expected to record (store or save) 
the event in a way that accords with auditing requirements. 


..... [proofs](#proofs) .....


.....

```ada






procedure Close (Auditor: in out Event_Auditor);
```
.....





-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## Auditing Proofs {#proofs}

Most [auditors](#auditors) will also record a _proof_ along with every event they record. In 
order to do so, the auditor will make use of a system object called a _prover_. 

Every prover is associated with a _salt_ value and a _serial_ number. 

A salt value is really just a very big number (integer), and does not change. The serial number 
is a non-negative integer that starts at 0 when the audit file is created, and is incremented 
by 1 after every event is recorded. 

For every event about to be recorded, a _proof_ is computed. This value, which is also a big number just like the salt, is 
computed, using a secure hashing algorithm, from the combination of:

 * the salt, 
 
 * the serial number, and 
 
 * the serialised binary data of the event itself. 
 
The purpose of the proofs is to make the events _non-repudiable_, which means that .....







-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## Default Auditing

To audit an event, the event should normally be sent to the event channel `System.Auditing`.


This event channel 




This event channel should be marked as audited by default. 



By default the auditor is the _system auditor_.


### System Auditor

The system default auditor writes events into an _audit file_.



When an event is forwarded to an audited event channel, the event is (serialised and) written 
into an _audit file_, before being forwarded onward to the normal logging mechanism. 

Writing an event into the audit file is transactional, and is a 
part of the [current transaction](?????). 

Every event is separately written into the audit file, within 
its own separate _auditing transaction_. The sequence of actions is:

 1. The auditing transaction is created; 
 
 2. the audit file is opened, with this auditing transaction; 
 
 3. the auditing [proof](#proofs) is computed and appended to the audit file; 
 
 3. the event is serialised and appended to the audit file; 
 
 4. the audit file is closed; 
 
 5. the auditing transaction is committed. 

If there is a current transaction, the auditing transaction will be a sub-transaction of it; 
otherwise the auditing transaction will be standalone. 

If the auditing transaction is standalone, the calling task will wait until the commit has 
completed, and so the calling task can be assured that the event data has been actually 
(physically) flushed out to the storage medium (e.g. hard disk). If the auditing transaction 
is part of a bigger transaction hierarchy, then this guarantee will apply when the outermost 
transaction has been committed. 

Because all the above actions are carried out for every event sent to an audited class, 
auditing is likely to use up considerably more resources than just logging .....

....


### Creating a File Auditor

AdaOS provides a function to create a new file auditor:

```ada
function New_File_Auditor (Salt:   in     AdaOS.Events.Auditing.Salt;
                           Name:   in     String; 
                           Form:   in     String := "") return File_Auditor'Class;
```






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
## Compound Auditor

A _compound auditor_ is an auditor that simply sends out every incoming event to two or more 
other auditors, and is a way of having a single event channel audited by two or more auditors. 

This is an unusual requirement, but AdaOS provides an implementation of a compound auditor 
for those occasions when it is needed. 

```ada
package AdaOS.Events.Auditing.Compound_Auditing
is
   type Compound_Auditor is new Auditor with private;
   
   procedure Include (Compound: in out Compound_Auditor;
                      Child:    in out Auditor'Class);



```

.....




-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 







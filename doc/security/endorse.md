-----------------------------------------------------------------------------------------------
# Endorsements

A primitive operation of a [secure object type](security.md#secobj) can have its `Endorsed`
aspect set to `True`, in which case the subprogram is said to be _endorsed_, otherwise it is
_non-endorsed_. 

If a subprogram is marked as endorsed, it should make the necessary security checks before
continuing with the actions of the subprogram. For each call of the subprogram, these checks
are called an _endorsement_ of the call. In practice, these calls will always be remote calls. 

In the majority of cases, it is [system objects](../objects/objects.md) that will have endorsed
operations. The subprograms `Engage`, `Is_Engaged`, or `Engagement_Identity`,
`Engagement_Authority`, and `Engagement_Transaction` will need to perform endorsement, and
typically most (or all) other operations of the system object will also perform endorsements. 

An endorsement ensures that the caller has permission to carry out the operation, and also
determines whether auditing is required, as well as possibly other qualifications of performing
the operation. 

The operation is expected to use the cited authority to check that the caller has permission to
perform the operation it has requested, and possibly also to qualify the execution of the
operation (for example, activating auditing). 

The operation might use the cited identity in an [audit log](../events/logging.md#aud) relating
to the call. 

Most (or all) system objects will use a special kind of module, called a
[guardian](guardians.md), to implement the endorsements specially for it. 



-----------------------------------------------------------------------------------------------
## Endorsement Process



.....



At the beginning of its execution, every endorsed subprogram that is using a
[guardian](guardians.md) needs to: 

 * create and initialise the appropriate [endorsement context](#cxt); 
 
 * execute the endorsement (which will be implemented by the guardian), using the context; 
 
 * examine the results in the endorsement context and act accordingly. 

.....




-----------------------------------------------------------------------------------------------
## Endorsement Libraries {#lib}

For most system objects, an [endorsement information document](#eid) should be written .....

An _endorsement library_ is an ECLAT [library](../eclat/libraries.md) that contains the
declarations of types that represent all the [endorsement contexts](#cxt) declared in an
endorsement information document. 

The `eidgen` command-line tool reads an endorsement information document (file) and generates
the Ada source text that can be compiled to create a corresponding endorsement library. 

.....






-----------------------------------------------------------------------------------------------
## Endorsement Contexts {#cxt}

An _endorsement context_ is the set of information associated with an endorsed subprogram (or
possibly two or more endorsed subprograms, in rare cases). The context contains information
primarily related to security, as well as information about the result of an endorsement. 

The abstract private type `Endorsement_Context`, declared in the package `AdaOS.Security`,
represents an endorsement context. 


### Operations

An endorsement context contains a set of _input properties_, that should be set before the
endorsement is performed, and a set of _output properties_ that are set by the endorsement. The
base type `Endorsement_Context` defines three input properties, and three output properties,
but these may be added to by a derived type. 

The following functions are declared in the same package: 

```ada

   function Time_of_Call       (Context: in Endorsement_Context) return Ada.Calendar.Time;
   function Caller_Principal   (Context: in Endorsement_Context) return Security_Identity;
   function Caller_Authority   (Context: in Endorsement_Context) return Security_Authority;
   
   function Is_Permitted       (Context: in Endorsement_Context) return Boolean;
   function Must_Audit         (Context: in Endorsement_Context) return Boolean;
   function Permission_Message (Context: in Endorsement_Context) return Wide_String;
   
   function Construct_Context (Time_of_Call:     in Ada.Calendar.Time;
                               Caller_Principal: in System_Identity;
                               Caller_Authority: in System_Authority)
   return
      Endorsement_Context;

   procedure Endorse (Context: not null access Endorsement_Context);
```


### Input Properties

The function `Time_of_Call` returns the absolute time when the call (that is being endorsed)
was made, as returned by a call to the function `Ada.Calendar.Clock`. 

The functions `Caller_Principal` and `Caller_Authority` returns the
[identity](security.md#ident) and [authority](security.md#auth) respectively of the calling
task. 


### Output Properties

The function `Is_Permitted` returns `True` if the endorsement results in the call being
permitted to go ahead, meaning that the subprogram being endorsed may perform the actions
and/or return the information promised by the associated documentation. If this function
returns `False`, the subprogram must not carry out any such actions or return any such
information. It should instead propagate the exception `Security_Violation`, calling the
function `Permission_Message` to obtain the message for the exception (it might augment this
message). 

The function `Permission_Message` should return a wide string containing a human-readable
explanation of why permission was or was not granted. 

For example, it may mention: 

 * which subprogram was called (or which action was invoked); 

 * the name of the endorsement context type; 

 * the values of all the input properties; 

 * which permissions were granted or denied; 

 * why. 

The function `Must_Audit` returns `True` if the call being endorsed is required to audit the
call. Auditing the call usually means that an [audit event](../events/auditing.md) should be
sent for each of the following: 

 * the beginning of the call itself; 

 * just before every significant action, such as making a change to system state that is
   (relatively) permanent or that has an effect which would not easily be reversed; 

 * just before retrieval of important or (potentially) sensitive information. 

However, auditing requirements need to be carefully considered and documented. 


### Constructing an Endorsement Context

??????The function `Construct_Context` returns an object of the type `Endorsement_Context`
constructed using values for all of its input properties. 


### Executing the Endorsement {#exec}

The procedure `Endorse` endorses the call using the input property values used to construct the
given `Context`. The output properties of `Context` will be set by the procedure. 

This procedure must be expected to be overridden. The default implementation sets the output
properties as follows: 

 * `Is_Permitted` returns `True` if `Caller_Principal` is equal to `Task_Principal`
   and `Caller_Authority` is equal to `Task_Authority`, or `False` otherwise; 

 * `Must_Audit` returns `False`; 

 * `Permission_Message` returns an appropriate message. 



...... should use the value returned by the function `Caller_Authority` to perform checks 
on whether the ......



...... should use the value returned by the function `Caller_Principal` to add 
information to, for example, log messages
to help identify who or what was initiating the .......




-----------------------------------------------------------------------------------------------
## Endorsing an Engagement {#eng}

When a system object is [engaged](../objects/objects.md#eng), by calling the `Engage` procedure
(a primitive operation of the type `System_Object`), the call needs to be endorsed. 







????? Can this all go?

The package `AdaOS.Objects.Engagement` contains declarations aimed at assisting in the implementation of 
a new system object type: 

```ada

   type Engagement_Context is new Endorsement_Context with private;

   function Object     (Context: in Engagement_Context) return Object_Access;
   function Controller (Context: in Engagement_Context) return Transactions.Controller_Access;

   procedure Endorse_Engagement (Object:  in out System_Object'Class;
                                 Context: in out Engagement_Context'Class);
```

An implementation of `Engage` can call `Endorse_Engagement` in order to endorse the call (to
`Engage`). 

The type `Engagement_Context` is implemented (in the package's private part) very
straightforwardly as follows: 

```ada

   type Engagement_Context is new Endorsement_Context
   with
      record
         Object:     Oject_Access;
         Controller: Transactions.Controller_Access;
      end record;
```

The procedure `Endorse_Engagement` is implemented (in the package's body) as follows: 

```ada

   procedure Endorse_Engagement (Object:  in out System_Object'Class;
                                 Context: in out Engagement_Context'Class)
   is
   begin
      Object.Endorse (Context);
   end;
```

This implementation simply invokes the procedure `Endorse` of the type `System_Object`. 

The type `Engagement_Context` is not very likely to (need to) have a new type derived from it.
The most likely reason for doing so would be if further output properties were needed. 

















-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Endorsement Information Documents (#eid)

An _endorsement information document_ (or _EID_) is an XML file, of a specific schema, which
contains some of the information needed by [guardian generators](#ggen). 

An EID contains declarations of one or more [endorsement contexts](#cxt), and, for each
endorsement context, its input and output properties. 

.....



The XML schema of EIDs is .....



An endorsement context is identified by the service and operation it corresponds to.

.....






```xml
<?xml version="1.0"?>
<security 
   xmlns="urn:adaos:ns:?????" 
   xmlns:xsd="?????" 
   xmlns:db="?????" 
   xsd:schema="?????">

   <import name="ada.calendar"/>
   <import name="acme.ssc.weather"/>
   <import name="acme.fgs.weather.control"/>

   <service name="acme.fgs.weather">
      <operation name="make-forecast">
         <endorse>
            <input>
               <property name="core" type="acme.fgs.weather.control.core"/>
               <property name="location" type="acme.ssc.weather.location"/>
               <property name="date" type="ada.calendar.time"/>
            </input>
         </endorse>
      </operation>
   </service>

</security>
```










```xml
<?xml version="1.0"?>
<security xmlns="?????">
   <import name="ada.calendar"/>
   <import name="adaos.security"/>
   <import name="prunae-?????"/>
   <endorsements>
      <service name="">
         <operation name="">
            <endorsement>
               <input>
                  <property name="weight-of-food" type="prunae-?????.kilogramme"/>
                  <property name="time-of-call" type="ada.calendar.time"/>
                  <property name="caller-identity" type="adaos.security.identity"/>
                  <property name="caller-authority" type="adaos.security.authority"/>
               </input>
               <output>
                  <property name="permitted" type="boolean"/>
                  <property name="auditing-required" type="boolean"/>
               </output>
            </endorsement>
         </operation>
      </service>
   </endorsements>
   
</security>
```

?????I think the extra parameters should all be wrapped into core



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






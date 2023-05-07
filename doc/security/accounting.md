-----------------------------------------------------------------------------------------------
# Accounting and Limiting

??????The AdaOS Native platform will have complete support for _accounting_ and _limiting_. 

AdaOS on hosted platforms implements the full set of accounting interfaces, but only provides 
a placebo implementation of them. 

_Accounting_ is based on the principle that a [principal](security.md#princ) should pay---in 
some sense---for the amount of every system resource used up by programs executing on their 
behalf. 

The 'money' which principals use to pay for resources may be some real-life currency, or it may 
be some arbitrary accounting unit. How any such payments are requested or made is not dealt 
with here, neither is the question of exchange between currencies. 

_Limiting_ is the enforcement of an upper limit on the amount of each resource a principal's 
programs are collectively permitted to use up. 

Since existing executional platforms (operating systems) do not have this support, the ECLAT 
hosted [run time systems](RTS.md) provide only a placebo in which ECLAT-compiled programs can 
interact with the RTS as if that support existed. 



-----------------------------------------------------------------------------------------------
## Resources

For the purposes of accounting and limiting, a _resource_ is anything of value that an 
executing program uses up (permanently). 

For example a resource is:

 * the time used of a processor (core); 
 
 * the time used of a unit (e.g. one byte) of main memory; 
 
 * the time used of a unit of a primary storage medium (e.g. a hard disk); 
 
 * one sheet of paper printed on; 
 
 * a unit (e.g. a ml) of ink used up (by printing). 

There will be many more. 

.....






-----------------------------------------------------------------------------------------------
## Ethos

There is a strong principle that a principal should pay exactly for the resources they (i.e. 
their programs) have _received_.

This means that resources used up by the software of other principals in the delivery of those 
resources do not count. 

For example, if user Fred's program uses one byte of storage, but that requires a megabyte of
storage to actually be used up (because of block size, overheads, etc.), Fred should still only 
pay for one byte. 

That's all that Fred requested or got (reliably, officially) delivered. Fred should not be 
charged for a megabyte. Fred only asked for, and received, one byte of storage. It is not 
Fred's fault that supplying that single byte actually costs one megabyte. A more efficient 
implementation might be able to deliver the byte by using up a smaller actual allocation of 
memory. 

Ultimately, the terms of who pays for what need to be transparent, understandable, sensible, 
and agreed in advance by all affected parties. In other words, it should be fair. 


### Unfamiliarity

It is human nature to be wary of an idea that is new and unfamiliar. 

.....


### Purpose

The principles of accounting and limiting are intended to facilitate the sharing of computer 
resources. 

.....



-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 

.....

The [boot image](../pxcr/images.md) has a set of accounts configured. 

Each [principal](security.md#princ) configured for the image has a counter associated with each
account. Each counter is initialised to 0, and is increased by the usage the principal makes of
the resource associated with the account (during the execution of the image). 

A counter is reduced when the principal 'pays' for the associated usage. 

.....


-----------------------------------------------------------------------------------------------
## Payment

.....

The ????? program enables principals to 'pay' for their accounts by generating a _statement_.

.....

A cycle of statement generation is configured with a _universal unit_ and a _unit cost_ for
each account. The unit cost is a factor; multiplying a counter of the account (in its own unit)
by the unit cost gives the equivalent value in the universal unit. 

For each account, the statement shows the usage of the associated resource, in its own unit, as
well as the unit cost for the account and an _extension_. The extension is the usage multiplied
by the unit cost, and is in universal units. 



Naturally, at the end of the statement is a total of all the extensions. 

.....





-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Account Definition

An _account definition file_ is an [XML][1] file, of a specific schema, that contains
definitions of accounts. 






```xml
<?xml version="1.0"?>
<accounts 
      xmlns="urn:adaos:ns:accounts.stup.1.0" 
      xmlns:db=" http://docbook.org/ns/docbook">

   <unit name="sec" ada-id="Second" uuid="">
      <label>second</label>
      <abbrev>s</abbrev>
   </unit>

   <unit name="byte-sec" ada-id="Byte_Second" uuid="">
      <label>byte-second</label>
      <abbrev>B.s</abbrev>
   </unit>

   <principal-account name="" uuid="" ada-id="CPU_Time">
      <label>Central Processor Unit Time</label>
      <abbrev>CPU</abbrev>
      <unit name="sec"/>
      <description>
         <db:para>
Time one central processor unit core (or equivalent) has spent executing machine instructions 
in a subprogram of a task owned by the principal.
         </para>
      </description>
   </principal-account>

   <principal-account ada-id="GPU_Time">
      <label>Graphical Processor Unit Time</label>
      <abbrev>GPU</abbrev>
      <unit name="sec"/>
      <description>
         <db:para>
Time one graphical co-processor CUDA core (or equivalent) has spent executing machine 
instructions in a subprogram of a task owned by the principal.
         </db:para>
      </description>
   </principal-account>

   <principal-account ada-id="RAM_Time">
      <label>Random Access Memory</label>
      <abbrev>RAM</abbrev>
      <unit name="byte-sec"/>
      <description>
         <db:para>
Time one byte of random access memory has been allocated by a task of the owner.
         </db:para>
</description>
   </principal-account>

   <principal-account ada-id="" uuid="">
      <label></label>
      <abbrev></abbrev>
      <unit name=""/>
      <description>
         <db:para>
         </db:para>
      </description>
   </principal-account>

   <principal-account ada-id="" uuid="">
      <label></label>
      <abbrev></abbrev>
      <unit name=""/>
      <description>
         <db:para>
         </db:para>
      </description>
   </principal-account>

   <principal-account ada-id="" uuid="">
      <label></label>
      <abbrev></abbrev>
      <unit name=""/>
      <description>
         <db:para>
         </db:para>
      </description>
   </principal-account>

   <principal-account ada-id="" uuid="">
      <label></label>
      <abbrev></abbrev>
      <unit name=""/>
      <description>
         <db:para>
         </db:para>
      </description>
   </principal-account>

</accounts>
```




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## System Objects



When a [system object](../objects/objects.md) is engaged, 






-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Hosted Placebo

Hosted run time systems 




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

[1]: <https://www.w3.org/XML/> "Extensible Markup Language (XML)"









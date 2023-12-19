-----------------------------------------------------------------------------------------------
# Guardians {#guard}

A _guardian_ is special kind of module that helps endorsed subprograms perform their
[endorsements](endorse.md). 



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
## Example

Let us look into the implementation of a remote procedure `Eat`, which is a primitive operation 
of the type `Plum`. The type `Plum` is derived from a type declared in a remote types package, 
so that its primitive operations, including `Eat`, are remote subprograms. 

We will suppose that only people who have a strong stomach are permitted to eat plums over a 
certain weight. 

```ada
with Measures;

package body Prunae
is
   procedure Eat (Food: in out Plum)
   is
      Endorsement: Prunae_Guardian.Endorsement_Result;
   
   begin
      -- (log request to eat Food)

      Endorsement.Food := Food; -- set input parameters

      Prunae_Guardian.Endorse; -- execute endorsement

      if Endorsement.Must_Audit then
         -- (audit request to eat Food)
      end if;

      if Endorsement.Is_Permitted then

         if Endorsement.Must_Audit then
            -- (audit permission granted to eat Food)
         end if;

         -- (log permission granted to eat Food)

         begin
            -- (eat the Food)
         exception
            when others =>
               if Endorsement.Must_Audit then
                  -- (audit failure to eat Food)
               end if;

               -- (log failure to eat Food)
         end;

      else

         if Endorsement.Must_Audit then
            -- (audit permission denied to eat Food)
         end if;

         -- (log permission denied to eat Food)
         
         raise Security_Violation with Endorsement.Permission_Message;

      end if;
   end Eat;
   
   -- (other contents of the package body)

end Prunae;
```

We pass the plum into the guardian, and leave it to the guardian to work out the weight of the 
plum, and whether the caller has a strong enough stomach to eat the plum. 

It is expected that the date and time of the call is also passed in as a property 
`Time_of_Call`. That would, for example, enable us at a later point to add a rule that plums 
may not be eaten after midnight, or that they may not be eaten in September. Only the 
(specification data governing the) guardian would need to be changed; the Ada source text could 
remain untouched, and nothing would need to be recompiled or rebuilt. Only the executable would 
have to be re-realised, which would (automatically) entail the guardian being regenerated. 

The same library will need to include a declaration of the package `Prunae_Guardian`, with a 
specification such as the following:

```ada
with AdaOS.Security;
with Measures;

package Prunae_Guardian
   with 
      Guardian,
      Guardian_Name => "prunae-guardian",
      Namespace_Full => "urn:datatype:adaos-1:1.0:prunae"
      Namespace_Short => "prunae"
is
   type Prunae_Endorsement_Context
   is
      new AdaOS.Security.Endorsement_Context
   with 
      private;

   procedure Endorse (Food: in Plum) 
      return 
         Endorsement_Result'Class;

private
   type Prunae_Endorsement_Context
   is
      new AdaOS.Security.Endorsement_Context
   with
      Endorsement_Context,
      Endorse_Procedure_Name => "prunae-guardian.endorse"
   with 
      record
         Weight_of_Food: Measures.Kilogrammes
            with 
               Property_Name => "weight-of-food",
               Property_Type => "prunae:kilogramme";
      end record;
end;
```

A possible completion of this package specification is as follows: 

```ada
package body Prunae_Guardian
is
   function Prunae_Context (Food: in Plum) return Prunae_Endorsement_Context
   is
      (Current_Context with Weight_of_Food => Weight (Food));

   procedure Endorse (Food: in Plum) 
      return 
         Endorsement_Result'Class
   is
      (Prunae_Endorsement_Context'Endorse (Prunae_Context (Food)));
end Prunae_Guardian;
```

Normally, packages of this kind would be put into a [stub
library](../eclat/building.md#stublibs). Libraries using it would be configured to include it. 

The implementations of the procedure `Prunae_Endorsement_Context'Endorse` will be imported from 
the generated guardian module. 



-----------------------------------------------------------------------------------------------
## Guardian Generators (#ggen)

Usually, each different service module will have a corresponding guardian module to help it.
The guardian module does not exist prior to the realisation being carried out. Instead, the
Realizor uses a particular kind of [Realizor helper plugin](../pxcr/helpers.md) called a
_guardian generator_ to [generate](../pxcr/modules.md#genmod) the guardian modules for the
realisation; the normal realisation is performed after the guardians have been generated. 

.....


### Null Guardian Helper

Currently, ECLAT is supplied with one guardian generator module, called the __Null Guardian 
Helper__. 

As its name suggests, the guardians generated by this helper perform no actual actions; they 
always permit every operation, and never require auditing. The purpose of this helper is to 
serve as a dummy placebo until the real guardian generators arrive in the future. 

The only file that the Null Guardian Helper needs, in order to .....


### Future Guardian Generators

In future there will be real guardian generators supporting:

 * mandatory security (labels)

 * discretionary security (ACLs)

 * role-based security

 * property-based security

 * possibly other security models

The property-based security guardian generator will use [XACML][1] files to specify the rules 
that it uses to make decisions. 



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 






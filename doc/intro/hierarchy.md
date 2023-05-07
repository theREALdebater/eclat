-----------------------------------------------------------------------------------------------
# Hierarchies

.....


-----------------------------------------------------------------------------------------------
## Rules and Terminology



Let us assume that we are talking about some entity named X, and that this entity forms a 
hierarchy. 

At the top of this hierarchy is one X, the _top X_. 

Every X except the top X has a _superX_, which is 
another X. 

The set of compartments whose supercompartment is C are the 
_subcompartments_ of C. A compartment C is an _inferior_ of another compartment D if C is its 
subcompartment or an inferior of one of its subcompartments. C is a _superior_ of D if D is an 
_inferior_ of C. Thus, the top compartment has no superiors, and all other compartments are 
its inferiors.


### Example

Supposing X is an 'employee', and that the employees (of some organisation, presumably) are 
arranged in a hierarchy. Please note that this example is 100% fictional. People's names and 
roles are chosen for simplicity (not realism). 

| Employee        | Position in the Organisation               |
| --------------- | ------------------------------------------ |
| Gina            | Chief |
| Fred            | Manager |
| Bobby           | Manager |
| Janice          | Team Leader |
| Esme            | Team Leader |
| Paul            | Team Leader |
| Podar           | Worker |
| Kris            | Worker |
| Gyorgi          | Worker |
| Sam             | Worker |
| Alex            | Worker |
| Cath            | Worker |

```
Gina
   ----> Fred
      ----> Janice
         ----> Kris
         ----> Gyorgi
      ----> Paul
         ----> Podar
         ----> Sam
   ----> Bobby
      ----> Esme
         ----> Alex
         ----> Cath
```

Now carefully read and understand the following statements: 

Gina is the top employee. Gina therefore has no superemployee, and no superior emplyees. On 
the other hand, everyone else has exactly one superemployee and at least one superior 
employee. 

Gina's subemployees are Fred and Bobby. Gina's inferior employees are all the other eleven 
employees. 

Fred's superemployee is Gina. Gina is Fred's only superior employee. 

Fred's subemployees are Janice and Paul. 

......













-----------------------------------------------------------------------------------------------
## 


### Compartment Hierarchy

Warning: The next paragraph may cause your brain to melt if you read it too quickly. 

Compartments form a hierarchy. At the top of this hierarchy is one compartment, the _top 
compartment_. Every compartment except the top compartment has a _supercompartment_, which is 
another compartment. The set of compartments whose supercompartment is C are the 
_subcompartments_ of C. A compartment C is an _inferior_ of another compartment D if C is its 
subcompartment or an inferior of one of its subcompartments. C is a _superior_ of D if D is an 
_inferior_ of C. Thus, the top compartment has no superiors, and all other compartments are 
its inferiors.

A compartment is notionally 'inside' its supercompartment. This means that if an executional 
instance is in a compartment `C`, all of the inferior compartments of `C`, and all the 
resources within them, will be accessible (albeit indirectly) to that instance. 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 









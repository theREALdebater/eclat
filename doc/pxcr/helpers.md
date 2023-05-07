-----------------------------------------------------------------------------------------------
# Realizor Helpers

The Realizor can have one or more _Realizor helpers_ added to it, as [plugins](Plugins.md).

Every Realizor helper plugin module must be in the module class `realizor.helper`. 

Each helper plugin module must register its helper (or helpers) with the Realizor in its 
initialisation code. 

A Realizor helper is represented by a type derived from the interface type `Realizor_Helper` 
declared in the pure package `Realizor.Plugins.Helpers`. 

A Realizor helper plugin module must, therefore, declare an aliased object, `H` say, of a type
derived from `Realizor_Helper`, and then register `H` under the name `N` (of the type
`Wide_String`) by calling:

    Realizor.Plugins.Helpers.Registration.Register 
       (Realizor.Plugins.Helpers.Helper_Access (H'Access), N);



-----------------------------------------------------------------------------------------------
## Helper Execution
    
The Realizor calls the `Execute` procedure, a primitive operation of `Realizor_Helper`, 

It passes in a context object that the helper can use to obtain 
some information about the pending realisation, and to do certain things to change its state 
(in particular, regarding its configuration). 

Helpers can generate on-the-fly modules .....

The `Execute` procedure is executed prior to performing the realisation unless it includes the 
dependency `$REALIZE`, in which case it is executed after the realisation has been completed. 

......



-----------------------------------------------------------------------------------------------
## Supplied with ECLAT

......



### Program Helper


Program Helper generates the configuration data for the assemblies and the program.

[Building a Program (Partitions)](../eclat/building.md#progprod)


### Plugin Helper

[Plugin Helper](../pxcr/helpers.md#plugin) uses this array to select plugins to 
add to an image.

[Plugin Products](../eclat/building.md#plugprod)




### Run-Time Support Configuration Helper

The [Run-Time Support Configuration Helper](../pxcr/realizor.md#rtsch) 



### Tethys Configuration Helper

The [Tethys Configuration Helper](../services/tethys.md#helper) is a Realizor helper that 






### Guardian Generators

Realizor helpers for [guardian generators](../security/guardians.md#ggen) are used to add a
layer of security to the use of [system objects](../objects/objects.md). 





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






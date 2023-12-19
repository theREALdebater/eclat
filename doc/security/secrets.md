-----------------------------------------------------------------------------------------------
# Secrets

A small item of data that needs to be protected from being disclosed to an unauthorised party
is called a _secret_. 

It is typical for a password, access key, or cryptographic key to be a secret. 

AdaOS provides a convenient and standardised way to store and retrieve secrets securely. 





-----------------------------------------------------------------------------------------------
## Vault {#vault}

A _vault_ is a store for putting secrets into and fetching them again. 

Within a vault, each secret is uniquely identified by a string that is called a _key_ (or a
_key to a secret_ if a distinction is needed from other kinds of key). 





A vault is a [system object](../objects/objects.md). 

The following is declared in the package `AdaOS.Secrets`: 

```ada
type Secure_Vault is limited interface and Objects.System_Object;
```




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Ersatz Vault {#ersatz}

AdaOS provides a concrete vault type, declared in the package `AdaOS.Secrets.Ersatz`: 

```ada
type Ersatz_Vault is limited new Secure_Vault with private;
```

The type `Ersatz_Vault` represents an _ersatz vault_, which is a vault of secrets, but is in
fact in no way secure, and whose purpose is to facilitate the early stages of the development
and testing of software that uses one or more vaults. 


### Advice

It will be typical for software that uses vaults to make use of the 
[echelon](../adaos/envvars.md#ech) environment variable to decide whether to use a 
development vault or one which is actually more secure. 



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








-----------------------------------------------------------------------------------------------
# Shepherds

A _shepherd_ is a system object and an [object container](../objects/containers.md) whose
primary job is to contain a set of [service programs](servprog.md) and look after running them,
restarting them, retiring them, and so on. 

The type `Service_Program_Shepherd` is declared in the package `AdaOS.Services`: 

```ada
type Service_Program_Shepherd is task interface and Object_Container;
```

.....



Usually, a shepherd is not just a container, but a [directory](../objects/containers.md#dir),
so that its member service programs are all identified by name. 

The function `Service_Programs` of a [compartment](compart.md) returns (an access value
referencing) the compartment's shepherd. It returns `null` if there is no shepherd for the
compartment. 

A shepherd provides the following functionality:

 * As a container, it maintains a set of member service programs. 

 * As a directory (if it is one), it maintains a name to identify each member. 
   
 * It supports one or more member service programs being configured to [start
   immediately](#auto), and keep running until [shutdown](startshut.md) of the (compartment
   that implements the) shepherd. 

 * It supports member service programs being configured to start automatically [on
   demand](#ondem), and being stopped and then terminated after timeout periods. 

 * It provides for member service programs to be configured to be [automatically
   restarted](#restart) if they fail. Various options provide sensible control over this, so
   that, for example, a continuously failing program does not tie up large amounts of system
   resource. 

.....

```ada
function Shepherd (Program: not null access Service_Program)
return
   access Service_Program_Shepherd'Class is abstract;
```

The function `Shepherd` returns the shepherd of a service program. 



-----------------------------------------------------------------------------------------------
## 

A shepherd is represented by the limited interface type `Service_Shepherd`, which
is derived from `Object_Container` and declared in the package `AdaOS.Services.Controlled`. 

The package `AdaOS.Services.Controlled` contains the following visible declarations: 

```ada

type Controlled_Service is limited interface and System_Service;

type Service_Shepherd is limited interface and Objects.System_Object;

function Service (Shepherd: not null access Service_Shepherd;
                  Service:  in Objects.Object_Id)
return
   access Controlled_Service'Class is abstract;
```

The function `Service` returns (an access value referencing) the controlled service identified
by the given [object identifier](../objects/objects.md#oid) in the parameter `Service`. 

The shepherd is responsible for [awakening](../objects/objects.md#state) the service, if it is
dormant (and so it does not already have an implementor). 

Don't forget that, as a system object, the shepherd must be engaged before the `Activate`
function can be called (and *must* be disengaged afterwards). 




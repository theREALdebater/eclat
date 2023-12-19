-----------------------------------------------------------------------------------------------
# Task Attributes

Every [task](../pxcr/tasks.md) has the following attributes: 

| `Task_Instance`       | Executional instance
| `Task_Transaction`    | Current transaction
| `Task_Authority`      | Current authority
| `Task_Identity`       | Current identity

.....

These attributes are all implemented using instantiations of the standard Ada generic package
`Ada.Task_Attributes` (see ARM C.7.2).

.....


-----------------------------------------------------------------------------------------------
## Executional Instance {#inst}

..... [executional instance](instances.md) .....

The package `AdaOS.?????` contains the following declaration: 

```ada
function Task_Instance (T: in Task_Id := Current_Task) 
return
   access Executional_Instance'Class;
```




-----------------------------------------------------------------------------------------------
## Current Transaction {#ct}

..... [current transaction](../intro/trans.md#curr) .....

The package `AdaOS.?????` contains the following declarations: 

```ada
function Task_Transaction (T: in Task_Id := Current_Task)
return
   access Transaction_Controller'Class;

procedure Set_Task_Transaction (Transaction: in ?????_Transaction; 
                                T:           in Task_Id := Current_Task);
```




-----------------------------------------------------------------------------------------------
## Current Authority {#auth}

..... [current authority](../security/security.md#ca) .....

The package `System.Security` contains the following declarations: 

```ada
function Task_Authority (T: Task_Id := Current_Task) return Security_Authority;

procedure Set_Task_Authority (Authority: in Security_Authority; T: in Task_Id := Current_Task);
```

Calling the function `Task_Authority` returns the current authority of the calling task. This
function, can be passed one parameter, of type `Ada.Task_Identification.Task_Id`, to retrieve
the value for any task. 

The procedure `Set_Task_Authority` can be called to set the current authority of the calling
task. Its first parameter is the new value. It can be passed a second parameter to set the
value for any task. 

For this procedure, the calling task must either be the task identified by `T` (the current
task) or the task identified by `T` (as defined in the RM section 9.3) must depend on the
calling task (as defined in the RM section 9.3). In addition, the `Authority` must be a member
of the ambit of the identity of the task identified by `T`. In any other case, the exception
`Ada.Security.Security_Violation` is propagated (and the current authority is not changed). 




-----------------------------------------------------------------------------------------------
## Identity {#ident}

..... [identity](../security/security.md#ca) .....

The package `System.Security` contains the following declarations: 

```ada
function Task_Identity (T: Task_Id := Current_Task) return Security_Identity;

procedure Set_Task_Identity (Identity: in Security_Identity; T: in Task_Id := Current_Task);
```

Calling the function `Task_Identity` returns the identity of the calling task. This function
can be passed one parameter, of type `Ada.Task_Identification.Task_Id`, to retrieve the value
for any task. 

The procedure `Set_Task_Identity` can be called to set the identity of the calling task. Its
first parameter is the new value. It can be passed a second parameter to set the value for any
task.

For this procedure, the calling task must either be the task identified by `T` (the current
task) or the task identified by `T` (as defined in the RM section 9.3) must depend on the
calling task (as defined in the RM section 9.3). In addition, the `Identity` must be equal to
or an inferior identity of the task identified by `T`. In any other case, the exception
`Ada.Security.Security_Violation` is propagated (and the identity is not changed). 

If the identity of the task is actually changed, then if its authority is not in the ambit of
the new identity, its authority is automatically changed to the default authority of the new
identity. 



-----------------------------------------------------------------------------------------------
## xxx

..... [xxx](xxx) .....

```ada
function xxx (T: Task_Id := Current_Task) return access xxx'Class;

procedure Set_xxx (xxx: access xxx'Class; T: Task_Id := Current_Task);
```




-----------------------------------------------------------------------------------------------
## xxx

..... [xxx](xxx) .....

```ada
function xxx (T: Task_Id := Current_Task) return access xxx'Class;

procedure Set_xxx (xxx: access xxx'Class; T: Task_Id := Current_Task);
```




-----------------------------------------------------------------------------------------------
## xxx

..... [xxx](xxx) .....

```ada
function xxx (T: Task_Id := Current_Task) return access xxx'Class;

procedure Set_xxx (xxx: access xxx'Class; T: Task_Id := Current_Task);
```




-----------------------------------------------------------------------------------------------
## xxx

..... [xxx](xxx) .....

```ada
function xxx (T: Task_Id := Current_Task) return access xxx'Class;

procedure Set_xxx (xxx: access xxx'Class; T: Task_Id := Current_Task);
```




-----------------------------------------------------------------------------------------------
## xxx

..... [xxx](xxx) .....

```ada
function xxx (T: Task_Id := Current_Task) return access xxx'Class;

procedure Set_xxx (xxx: access xxx'Class; T: Task_Id := Current_Task);
```




-----------------------------------------------------------------------------------------------
## xxx

..... [xxx](xxx) .....

```ada
function xxx (T: Task_Id := Current_Task) return access xxx'Class;

procedure Set_xxx (xxx: access xxx'Class; T: Task_Id := Current_Task);
```




-----------------------------------------------------------------------------------------------
## xxx

..... [xxx](xxx) .....

```ada
function xxx (T: Task_Id := Current_Task) return access xxx'Class;

procedure Set_xxx (xxx: access xxx'Class; T: Task_Id := Current_Task);
```




-----------------------------------------------------------------------------------------------
## xxx

..... [xxx](xxx) .....

```ada
function xxx (T: Task_Id := Current_Task) return access xxx'Class;

procedure Set_xxx (xxx: access xxx'Class; T: Task_Id := Current_Task);
```




-----------------------------------------------------------------------------------------------
## xxx

..... [xxx](xxx) .....

```ada
function xxx (T: Task_Id := Current_Task) return access xxx'Class;

procedure Set_xxx (xxx: access xxx'Class; T: Task_Id := Current_Task);
```




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 





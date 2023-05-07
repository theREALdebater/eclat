-----------------------------------------------------------------------------------------------
# Object Containers

An _object container_ is a [system object](objects.md) that conceptually contains a set of
other system objects, called its _members_. 

Usually, the members of a container can be added, removed, and renamed dynamically. Each member
is accessed as normal; it being the member of a container doesn't affect that. In fact, almost
all system object are members of a container. 

It is important to understand that a container isn't just some kind of index of references to
its members, but instead it conceptually *contains* its members. To be honest, some or all of
those members could be [links](#links) to objects in other containers; nevertheless the links
themselves are contained by the container. 

More concretely, it is a principle that if an object is a member of one container, it cannot be
a member of any other container at the same time (it can generally be moved to another
container dynamically). If the container is deleted, all its members are automatically deleted
at the same time (if something prevents any one of them being deleted, then the deletion of the
container is also prevented). 

Containers usually play an essential role in the [persistence](#state) of its members. In most 
cases, most or all of the container's members are dormant, and the container holds its members'
saved states (in some storage place).  



.....

The task interface type `Object_Container` is declared in the package `AdaOS.Objects`, and is
derived from `System_Object`. Object containers will be of types derived from
`Object_Container`. 

......



-----------------------------------------------------------------------------------------------
## Placeholders {#ph}

A container may use any mechanism it chooses to implement the functionality required of it. 

However, a technique that many containers will use, in practice, will be to have the ability to
represent any of its members with a _placeholder_. 

A placeholder is unlikely to be a system object (or any kind of remote object), but it will be
stored, in some implementation-dependent way, within the container. 

Whenever a request is made to [find](#find) a member that is being represented by a
placeholder, the container will replace the placeholder with the real system object it
represents. How the container does this, and how long it takes to do it, are dependent on the
implementation of the container. In this way, the real system object is found (not the
placeholder). 

The container may use the same real object next time a request to find it is made, or it might,
at a time of the container's or the object's choosing, replace the real object with its
placeholder once again. Whether this happens or not, and how it happens, should be invisible to
the client (the software finding the member). It should seem to the client that it is simply
finding the system object every time, as if there were no placeholder at all. 



-----------------------------------------------------------------------------------------------
## Operations







```ada

function Contains (Container: in Object_Container;
                   OID:       in Object_Id) return Boolean is abstract;


```


-----------------------------------------------------------------------------------------------
## Metadata {#meta}

The package `AdaOS.Objects` declares the abstract tagged non-limited type `Member_Metadata`.
Values of types derived from `Member_Metadata` contain some information about a system object
that is a member of a container. This information is called the member object's _metadata_. 

The idea is that a metadata object is relatively small, compared to the system object it
describes, and is a value (that can be copied and compared). It always contains the OID of the
system object, and the object's [external tag][1] (a token identifying the object's Ada type). 

```ada

type Member_Metadata is abstract tagged private;

function OID (Metadata: in Member_Metadata) return Object_Id;

function External_Tag (Metadata: in Member_Metadata) return String;

procedure Set_OID (Metadata: in out Member_Metadata; OID: in Object_Id);

procedure Set_External_Tag (Metadata: in out Member_Metadata; Tag: in String);
```




```ada

function Metadata (Container: not null access Object_Container;
                   OID:       in Object_Id) return Member_Metadata'Class is abstract;
```

The overloading of the function `Metadata` with an `OID` parameter returns the metadata held by
the given `Container` on one of its members. The system object is identified by its OID. If the
system object is not a member of the container, or if the container does not hold any metadata
for the object (which is very unusual), the exception `?????` is propagated. 

The metadata object returned by this function is a *copy* of the metadata that the container
holds: modifying the copy will have no effect on the metadata held by the container (although
typically no visible operation is provided to modify a metadata object anyway). 

It is also possible to get the metadata for all of the objects in a container:

```ada

package Metadata_Maps is 
   new Ada.Containers.Indefinite_Hashed_Maps (Object_Id, 
                                              Member_Metadata'Class, 
                                              AdaOS.Objects.Hash);

function Metadata (Container: not null access Object_Container) 
return 
   Metadata_Maps.Map'Class is abstract;
```

The overloading of the function `Metadata` without an `OID` parameter returns the metadata held
by the given `Container` on all of its members. 



-----------------------------------------------------------------------------------------------
## Finding an Object {#find}

In order to start interacting with a system object that is a member of a [container](#cont), 
it is possible to _find_ the object by giving its [object identifier](#oid). 

The object identifier of the object must be supplied to the container the object is a member
of. The container returns an access value referencing the object. 

There are security limitations on finding an object. You cannot find a system object if you do
not have the requisite permissions. 

The function `Find` is a primitive operation of the type `Object_Container`:

```ada

function Find (Container: not null access Object_Container;
               OID:       in Object_Identifier) 
   return 
      Object_Access is abstract;
```

The function `Find` returns (an access value referencing) the system object that has the given
`OID`, if the object is a member of the `Container` and the caller has permission to find the
object. Otherwise, this function returns null. 

Once an object has been found, it can be [engaged](#eng) and used. 

Some kinds of container may have overloadings of the function `Find` that use something else to
uniquely identify the object, such as a [name](#findname) for example. 



-----------------------------------------------------------------------------------------------
## Scrying an Object {#scry}

What if you know the [OID](#oid) of an object, but you don't know which container it is in?

You can use a mechanism called [object scrying](scrying.md) to find the object. 

There are security limitations on the scrying mechanism. You cannot find out an object's
container if you do not have the requisite permissions. 

This mechanism is very powerful and flexible, but it is often a bit heavyweight. For
convenience, a function is declared, in package `AdaOS.Objects`, which will find the container
of an object:

```ada

function Container (OID: in Object_Id) return acess Object_Container'Class;
```

This function may be very inefficient. If it is used repeatedly (e.g. in a loop), it may be
better to use the scrying mechanism at a lower level (directly sending a scrying event, with
its own call-back function). 



-----------------------------------------------------------------------------------------------
## Directories {#dir}

An AdaOS _directory_ is a [container](#cont) that is able to identify, within itself, each of
its members by a unique name, which is a [path string](../intro/strings.md#path). 

This is in addition to the universal identification of system objects by their [object 
identifiers](#oid). 

A directory is represented by the type `Object_Directory`



### AdaOS Native versus Host Directories

On the AdaOS Native platform, all directories are AdaOS directories. 

On hosted [platforms](../pxcr/targets.md#plat), host paths obey the rules of the host 
platform, but all other directories are AdaOS directories. A host directory is not the same 
as an AdaOS directory, and it will have different rules and characteristics. 

The remainder of this section refers solely to AdaOS directories.


### Names and Paths

..... the names of the members of a directory  
are the same as for the node names of a [compartment member path](paths.md#cmptpath), because 
the node names of a compartment member path *are* the names of the members of directories.

..... 


### Finding an Object By Name {#findname}

Directories have the primitive operation `Find`, which finds a member object by name. This
function takes one parameter, a simple name, and returns (an access value referencing) the
member object. 

```ada
function Find (Container: not null access Object_Directory; 
               Name:      in  Path_String) -- simple name
return 
   access System_Object is abstract; 
```

The `Name` passed into this function must be a simple name, not a [path](paths.md). This
effectively means the name must not contain a path separator. 

If there is anything wrong with the name passed in, the exception `AdaOS.Path_Error` is
propagated. 



-----------------------------------------------------------------------------------------------
## Links {#links}

An AdaOS _link_ is a special kind of system object whose only (or primary) purpose is to 
'point' to another system object. 

The system object the link points to is called the _target_ of the link. The link is said to 
_designate_ the target. 
 
The task interface type `Object_Link` .....

...........


### AdaOS versus Host Links

On the AdaOS Native platform, all links are AdaOS links. 

On hosted [platforms](../pxcr/targets.md#plat), links, called _host links_, obey the rules of
the host platform, but all other links are AdaOS Native links, or just _native links_. A host
link is not the same as a native link, and it will have different rules and characteristics. 

The remainder of this section refers solely to native links.


### Kinds of Link

There are three kinds of native link: existential; compositional; reference. 

Both an _existential link_ and a _compositional link_ make the target a conceptual part of some
entity. Often that entity is (represented by) the container of the link. 

An _existential link_ is a link that the target is intended to always have designating it. It
is used when the existence of the target is meaningless without the link designating it.
Deleting the link causes the target to be automatically deleted as well. An object is only
permitted to have at most one existential link designating it. 

......

A _compositional link_ does not tie the target so strongly to the link (or the conceptual
entity the compositional link is making the target a part of). The link can be deleted without
affecting the target. 

The target cannot be deleted while it has an existential or a compositional link designating
it. 

......

A _reference link_ ties the existence of the link to that of the target. If the link is
deleted, the target is unaffected. However, if the target is deleted, any reference links
designating it are also deleted automatically. 

For example, a reference link might be used to create an alias for an object; subsequently
deleting the alias has no effect on the object it designated, but deleting the object the alias
designates causes the alias to also be deleted (since it would have no meaning without the
object it designates existing). 

An object can have any number of compositional or reference links designating it. 

......

The following table summarises the different rules and behaviour of deletion for the different 
kinds of link: 

| Type of Link       | Try to Delete Link             | Try to Delete Target           |
| ------------------ | ------------------------------ | ------------------------------ |
| Existential        | Target and link both deleted   | Not allowed                    |
| Compositional      | Link deleted, target unchanged | Not allowed                    |
| Reference          | Link deleted, target unchanged | Target and link both deleted   |

For the purposes of the above rules and behaviour, attempting to modify a link can be
considered equivalent to attempting to delete it and then (if that succeeds) re-create it. In
other words, if a link cannot be deleted, it cannot be modified. 




...........


### Link Chains

...........


### Backlinks

.........



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Finding an Object by Path {#findpath}

.....

The following function is visibly declared in the package `AdaOS.Objects`:

```ada

function Split_Path (Path: in Path_String) return Path_Vectors.Vector;
```



.....


The basic [`Find` procedure](#find) searches one dictionary for a member with a given (simple)
name.

More often than not, ..........

a system object of a specific sub-class (of `System_Object'Class`) will be
expected. In addition, often the OID of the required service will not be known, but rather the
required service will fulfil a set of conditions.

The following generic function is declared in the package `AdaOS.Compartments`:

```ada
generic
   type Object_Type is new System_Object with private;

   Default_Timeout: in Duration := 15.0;

   Path_Separator:  in Path_String := "/";
   Self_Directory:  in Path_String := ".";
   Super_Directory: in Path_String := "..";
   Home_Directory:  in Path_String := "~"; -- first node only
   Root_Directory:  in Path_String := ""; -- first node only

function Find_Object 
   (Path:        in Path_String;
    Base:        access Object_Directory'Class := null; -- null for CWD
    Compartment: not null access System_Container'Class := Task_Instance.Compartment;
    Trial:       in Boolean := False) return access Object_Type'Class;
```

The function `Find_Object` makes use of the `Find` 







-----------------------------------------------------------------------------------------------
## Searching for an Object {#search}

..... often the OID of the required service will not be known, but rather the
required service will fulfil a set of conditions.

The generic procedure `Search_Container`, declared in the same package, can be instantiated and
called to find a set of system objects of a specific type or sub-class and, optionally, which
fulfil a specific condition. 

The following generic procedure is declared in the package `AdaOS.Objects`:

```ada

generic
   type Object_Type is new System_Object with private;
   type Object_Access is access Object_Type'Class;

procedure Search_Container 
   (Process:     access procedure (Object: in Object_Access; Continue: out Boolean);
    Matches:     access function (Metadata: in Object_Metadata'Class) return Boolean := null;
    Recurse:     in Boolean := False;
    Container:   access Object_Container'Class := null; -- null for CWD
    Compartment: not null access System_Container'Class := Task_Instance.Compartment);
```

A call to an instantiation of `Search_Container` makes a call-back to the procedure parameter
`Process` once for every _matching_ system object. For each such call-back, the parameter
`Object` is set to (an access value referencing) the system object. If the out-parameter
`Continue` is set to `False` upon return, the search ends, and no further calls to `Process`
are made. 

In any case, the matching system objects are those which are members of the given `Container`
and are in `Object_Type'Class`. 

There are security limitations on searching. A system object never matches if the task calling
the instantiation of `Search_Container` does not have any kind of permission to access the
system object. For every matching object (that is passed as parameter `Object` of procedure
parameter `Process`), the permissions the calling task has over that object are enforced as
normal. 

The function parameter `Matches` can be omitted (i.e. null). If `Matches` is not null, it is
called in order to determine if a system object (which matches according to the other rules)
matches. If the function returns `True`, the object matches (notwithstanding the other rules
that may prevent an object matching), otherwise the object does not match. If `Matches` is
null, every system object matches (notwithstanding the other rules that may prevent an object
matching). 

If the parameter `Recurse` is `True`, then not only is the given `Container` searched, but also
any of its inferior containers as well. 











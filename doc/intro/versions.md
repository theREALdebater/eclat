-----------------------------------------------------------------------------------------------
# Versions

......






-----------------------------------------------------------------------------------------------
## SemVer

...... should be chosen and changed according to the principles of [semantic versioning][1], 
or _SemVer_, ........

We specify four numbers in a version number, rather than SemVer's three. 

?????We are stricter than SemVer, in that we do not allow any 'pre-build' or other kinds of 
extensions to the version number (it comprises four numbers and nothing else).

Build details are indicated elsewhere, not in the version number. 

We introduce the notion of a compatibility version number, the compatibility level of a 
version number, and the compatibility of versions. 

However, the basic principles of SemVer still apply very strongly. 



-----------------------------------------------------------------------------------------------
## Structure

A _version number_ comprises four parts:

 * _major number_ (`A`)
 
 * _minor number_ (`B`)
 
 * _patch number_ (`C`)
 
 * _iteration number_ (`D`)
 
Each of these parts must be an integer between 0 and 4,294,967,295 inclusive. 
 
When written in text, the four parts are written in the above order, in decimal with no 
punctuation, spaces, or grouping characters, but with a `.` period (dot, full stop) character 
in between each part `A.B.C.D` thus. 

For example:

    1.2.3.4
    
expresses a version number with major number 1, minor number 2, patch number 3, and iteration 
number 4. 

.....

The following package is declared: 

```ada
package AdaOS.Versions
is
   type Part is (Major, Minor, Patch, Iteration);

   type Number is mod 2**32;

   type Compatibility_Version is array (Part range Major .. Minor) of Number;

   type Full_Version is array (Part) of Number;

   function Compat (Version: in Full_Version) return Compatibility_Version;
end;
```




-----------------------------------------------------------------------------------------------
## Versioned Packages

A _versioned package_ .....

.....

A versioned package always has a visible parameterless function which returns the type
`Full_Version`, declared as follows: 

```ada
function Version return Full_Version with Inline;
```

It is usually declared 'inline', and simply returns a static value. 

This function returns the version of the package 





.....



-----------------------------------------------------------------------------------------------
## Versioned Entities

ECLAT entities that are _versioned_ are:

 * [libraries](../eclat/libraries.md)

 * [modules](../pxcr/modules.md)
 
 * [executable images](../pxcr/realizor.md#images)

 * [service interfaces](../services/services.md#ver) and service implementations
 
 * [Kantan packages](../kantan/kantan.md)
 
A versioned entity has a different version number associated with it whenever it is changed, 
unless the entity is renamed (because it was changed so much). 

By default, when a versioned entity is created, its version number is 0.0.0.0. 

The version number of an entity identifies its place in the sequence of changes made to it. 

.......



-----------------------------------------------------------------------------------------------
## Entity Surface and Semantics

The _surface_ of a versioned entity is all of the characteristics necessary for usage of the 
entity that are seen by those external entities which use it. In SemVer this is what is meant 
by the expression 'public API'. 

The surface of a library comprises all the non-private declarations in its units, .....

The surface of a module comprises all the exports of the module; for each export, its 
contribution to the surface comprises the name of the export together with the contribution 
the corresponding declaration makes to the surface of its library unit. 

The surface of an executable image never changes (it is the very fact that the image can be 
executed). 





The surface of a service ..... ?????interface or implementation (or both)?






The _semantics_ of a versioned entity is the expected behaviour and characteristics of (every 
part of) the surface of the entity. 

The semantics of a library comprises the collective expected behaviour and characteristics of 
all the parts of the library's surface. 

The semantics of a module comprises the collective expected behaviour and characteristics of 
all the module's exports. 

The semantics of an executable image is the expected behaviour and characteristics of the image 
when it is executed. 

The semantics of a package is the expected behaviour and characteristics of the package when it
is installed. 





The semantics of a service ..... ?????interface or implementation (or both)?



-----------------------------------------------------------------------------------------------
## Version Incrementation

How an entity's version is changed depends on the significance of the change made to it. 

If a versioned entity is changed: 

 * only by recompiling it (library), rebuilding it (module), re-realising it (executable), or 
   regenerating it (package) its iteration number is incremented (by one) and the other three 
   parts of its version number remain the same; 

 * in such a way as not to affect its surface in any way, its patch number is incremented (by 
   one), its iteration number is set to 0, and the other two parts of its version number remain 
   the same; 

 * affecting its surface or semantics that will (probably) impact a relatively small number of 
   the entities that use it, its minor number is incremented (by one), its patch and iteration 
   numbers are set to 0, and the major number remains the same; 

 * affecting its surface or semantics that will (probably) impact all or a relatively large 
   number of the entities that use it, its major number is incremented (by one), and all its 
   other parts are set to 0. 



-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Compatibility {#comp}

The major number and minor number together are a _compatibility version number_, and extracting 
the major and minor numbers (only) of a version number forms the _compatibility level_ of the 
version number. 

Two versions of a particular entity are _compatible_ if they have the same compatibility 
version number. 

When written in text, the two parts are written in the above order, `A.B` thus. 

For example:

    1.2
    
expresses a compatibility version number with major number 1 and minor number 2. (The whole 
version number may have been 1.2.3.4.) 

Two different versions of a versioned entity are _compatible_ if and only if the compatibility 
levels of their version numbers are equal. 

Equality of version numbers and compatibility version numbers is in terms of equality of their 
corresponding parts. 

For example, the following pair of version numbers are compatible (or rather, the entities 
that have these version numbers would be compatible): `1.2.3.4`; `1.2.7.8`. 

The following pair are not compatible: `1.2.3.4`; `1.3.3.4`. 

The idea is that two versions of an entity are considered compatible if their surface and 
semantics are (materially) the same. 



-----------------------------------------------------------------------------------------------
## Comparison

Version numbers are _fully ordered_. This means that for any two different version numbers, 
`X` and `Y` let's say, either `X < Y` or `Y < X` (but never both). 

In the following, we use the notation `X#A` to mean the major version number of version number
`X`, etc. 

The comparison `X < Y` is true if and only if:

 * `X#A < Y#A`, or 
 
 * `X#A = Y#A and X#B < Y#B`, or 

 * `X#A = Y#A and X#B = Y#B and X#C < Y#C`, or 

 * `X#A = Y#A and X#B = Y#B and X#C = Y#C and X#D < Y#D`. 
 
Similarly, the comparison between two different compatibility version numbers `X` and `Y` is 
true if and only if: 

 * `X#A < Y#A`, or 
 
 * `X#A = Y#A and X#B < Y#B`. 

In the above, `A` is the major number, `B` is the minor number, `C` is the patch number, and 
`D` is the iteration number. 

For example: 

 * `1.2.3.4` comes before `2.3.4.5` 

 * `3.4.5.6` comes before `3.4.5.7` 



-----------------------------------------------------------------------------------------------
## Shared Interfaces

All interfaces that are shared need to be versioned. Usually an interface is shared if it is
declared in a separate library. 

.....

The recommended way to version an interface is to append the compatibility version to the name
of the library package in which it is declared. For this purpose, the compatibility version
takes the form `_A_B` where `A` is the major version number, and `B` is the minor version
number. 

For example: 

```ada

package Weather_Prediction_1_0
is
   type Prediction_Service is task interface and AdaOS.Services.System_Service;

   .....
end;
```

The interface type itself has no version numbering, only the package.

A new patch or iteration of an interface `Weather_Prediction_1_0` just replaces
`Weather_Prediction_1_0`, and does not require it to be renamed. 

On the other hand, for a new minor or major version of `Weather_Prediction_1_0`, a new,
separate library would be created, with a package named either `Weather_Prediction_1_1` (for a
new minor version) or `Weather_Prediction_2_0` (for a new major version). 

It is recommended the library itself is named .....




It is important that the package is differentiated (in its name) between different versions,
rather than just the library, because there are occasions when a library `L` needs to depend on
two or more such libraries, typically because `L` must implement conversions between objects
whose types are different versions of the same interface. 




Implementations of the service are also versioned. Each different version of the interface
should have its own separate set of (versions of) the implementation. Each set of
implementation versions (those for any one particular version of the interface) starts at
version `0.0.0.0` and works upward as normal. 

To specify the version of *any* implementation of a particular interface, the compatibility
version of the interface should specified followed by a `/` solidus (forward slash character)
and then the (full) version of the implementation.

For example: `1.2/3.4.5.6`

This specifies implementation version `3.4.5.6` of interface version `1.2`. 

.....







-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## References

[1]: <https://semver.org/> "Semantic Versioning 2.0.0"







-----------------------------------------------------------------------------------------------
# Time

.....



-----------------------------------------------------------------------------------------------
## Representation in AdaOS

In AdaOS, time is internally represented as a fixed-point type which is in the unit of seconds
and epoch of [Modified Julian Date][1] or _MJD_. 

MJD is a well-defined system of representing absolute time, largely used by the scientific
community. Its unit is the SI (Système International d'unités) second, and its _epoch_ (point
in absolute time when it has value 0) is midnight at the beginning of 17 November 1858. 

The advantage of this representation is that most kinds of temporal arithmetic are very
straightforward. The disadvantage is that converting MJD into and from a human-intelligible
form, such as [CE](#ce) requires a relatively significant amount of computation. 

So, for example, 12:08 UTC on Sunday 9th May 2022 is represented by the MJD 59708.50556. 





-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 



The Ada package `AdaOS.Time` .....





The range and delta (accuracy) of the type `General_Time` declared in the package `Ada.Calendar` is
configurable, by means of configuring a module that exports the following three constants: 

| Export                         | Ada                            |
| ------------------------------ | ------------------------------ |
| `adaos.time.general.delta`     | `AdaOS.General_Time'Delta`     |
| `adaos.time.general.minimum`   | `AdaOS.General_Time'First`     |
| `adaos.time.general.maximum`   | `AdaOS.General_Time'Last`      |

The name of the module to be used can be configured as follows:

```xml
<eclat ...>
   <image>
      <time-module name="adaos.time"</time-module>
      ...
   </image>
</eclat>
```

If not explicitly configured, the name `adaos.time` is used.

The type `Time` declared in the standard Ada package `Ada.Calendar` is actually declared thus:

```ada

subtype Time is AdaOS.Time.General_Time;
```


-----------------------------------------------------------------------------------------------
## Common Era

[Common Era][2], or CE

, the familiar year/month/day/hour/minute/second format)

[ISO-8601][3]






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## System Clock

It is assumed that the computer has a clock that can be read to obtain the real current
absolute date and time (sometimes termed 'wall time', as it is the time that would be shown by
a wall clock). 

On a [hosted platform](../pxcr/targets.md#plat), the clock is read by obtaining a value from
whatever the host clock is. 

On the AdaOS Native platform, the clock is periodically read from the computer clock peripheral
(circuitry or device). The clock tick interrupt is other wise used to update the clock value.
This value is updated whenever the clock peripheral is read. 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## Clock Offset

Every [compartment](compart.md) maintains a _clock offset_.

This is a 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## Network Time Synchronisation

.....



### Network Time Protocol

..... [Network Time Protocol][4] ......




..... [Network Time Security](https://datatracker.ietf.org/doc/html/rfc8915) ......




### Precision Time Protocol

..... [Precision Time Protocol][5] version 2.1 (IEEE 1588-2019) .....






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## 






-----------------------------------------------------------------------------------------------
## References

[1]: <https://en.wikipedia.org/wiki/Julian_day> "Wikipedia: Julian day"

[2]: <https://en.wikipedia.org/wiki/Common_Era> "Wikipedia: Common Era"

[3]: <https://en.wikipedia.org/wiki/ISO_8601> "Wikipedia: ISO 8601"

[4]: <https://en.wikipedia.org/wiki/Network_Time_Protocol> "Wikipedia: Network Time Protocol"

[5]: <https://en.wikipedia.org/wiki/Precision_Time_Protocol> "Wikipedia: Precision Time Protocol"


-----------------------------------------------------------------------------------------------
# ECLAT Implementation of the C Standard Library 

.....




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## File I/O

.....

..... [](../objects/files.md) .....

In all of the Ada standard library I/O packages, there are procedures named `Create` and `Open` 
that have a `Name` parameter and a `Form` parameter.


In all of the C standard library I/O packages, there are functions named `fopen` and `?????` 
that have a `filename` parameter and a `mode` parameter.


### The `filename` Parameter

.....


### The `mode` Parameter

In all the `fopen` and `?????` procedures, the `mode` parameter, of type `const char *`, can be used 
to qualify the required access to the file, 







but the recognised values of this parameter, and 
their meaning, is implementation defined.




The ECLAT CSL interprets the `mode` parameter in a unified manner. 

The value of the `mode` parameter is the name of a [SONG](#song). The SONG is a conceptual 
entity that provides all the information needed to customise access to a file. 

.....


The C standard mandates the following values for the `mode` parameter:

| Mode   | Meaning                                                               |
| ------ | --------------------------------------------------------------------- |
| `r`    | read a text file                                                      |
| `w`    | write a text file (create if does not already exist)                  |
| `a`    | append to a text file (create if does not already exist)              |
| `r+`   | read or write a text file (start at beginning of file)                |
| `w+`   | read or write a text file (empty file if it already exists)           |
| `a+`   | read or write a text file (start one past end if file already exists) |

The C standard also mandates equivalents of the above for binary files. They are the same but
with a `b` in them. Unfortunately, the `b` can precede or follow the `+` (if there is one),
with the same meaning regardless. 

Thus the standard allows the following modes:

    r w a r+ w+ a+ rb wb ab rb+ wb+ ab+ r+b w+b a+b 

These standard modes can be supported by configuring SONGs with the same names (`r`, `w`,
etc.). 








-----------------------------------------------------------------------------------------------
## Standard Streams

.....




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 










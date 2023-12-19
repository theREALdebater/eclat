-----------------------------------------------------------------------------------------------
# Makefiles





-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}

A _target_ is a name for a thing that may need to be done, or a file or set of files that need
to be created or updated. 

.....



-----------------------------------------------------------------------------------------------
## {#}

A _prerequisite_ is a name for a file that may have been created or updated. 

.....



-----------------------------------------------------------------------------------------------
## {#}

A _command_ is an [AdaShell](../adashell/adashell.md) command.

AdaShell is designed to be embeddable in other programs, and indeed the [ECLAT command-line
tool](../tools/eclat.md), named `eclat` (or `eclat.exe`), has AdaShell embedded in it, meaning
that it can directly execute AdaShell commands. 

.....




Command lines can have one or more of the following three prefixes:

 * a `-` hyphen-minus, specifying that errors are ignored

 * an `@` at sign, specifying that the command is not printed to standard output before it is
   executed

 * a `+` plus sign, the command is executed even if `eclat` is invoked in "do not execute" mode

Ignoring errors and silencing echo can alternatively be obtained via the special targets
`.IGNORE` and `.SILENT`. 






-----------------------------------------------------------------------------------------------
## Makefiles {#file}

A _makefile_ is a plain text file that contains  .....




A makefile usually has a name like this: 

    makefile
    Makefile
    makefile.txt
    Makefile.txt
    C,makefile.txt

where `C` is the name of a configuration .....







-----------------------------------------------------------------------------------------------
## {#}

A [makefile](#file) contains one or more _rules_. 

Each rule takes the following general form: 

```
target1 target2 target3: prerequisite1 prerequisite2
   command1
   command2
   command3
   command4
```

A declaration can be abbreviated by putting a `;` semicolon and then one or more commands after
the dependencies: 

```
target: prerequisite; command
```

.....








-----------------------------------------------------------------------------------------------
## {#}




https://en.wikipedia.org/wiki/Make_(software)

https://nullprogram.com/blog/2017/08/20/








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




-----------------------------------------------------------------------------------------------
## {#}









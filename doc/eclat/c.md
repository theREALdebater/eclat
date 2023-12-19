-----------------------------------------------------------------------------------------------
# C

The __ECLAT-C__ compiler supports compilation of C source text. 

It expects syntax based on the C17 standard (ISO/IEC 9899:2017) version of the C language, and
is accompanied by a standard library that is based on C17 but with many additions aimed at
supporting a broad range of typical C software. 

.....








ECLAT supports incremental compilation of C source text files. 

.....

If a 






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
## Support Libraries {#supplib}

.....

The entire standard library is supported. The header files are written in the C language, and
all have the `.h` file type extension. 

However, all the functions declared in these header files are implemented in the Ada
programming language, with the appropriate linkage. 

There are two ECLAT libraries: 

 * the _elementary C support library_ , containing the implementations of all the functions
   declared in the standard header files `<float.h>`, `<iso646.h>`, `<limits.h>`,
   `<stdalign.h>`, `<stdarg.h>`, `<stdbool.h>`, `<stddef.h>`, `<stdint.h>`, and
   `<stdnoreturn.h>` (not all of these header files necessarily have any functions declared in
   them); 

 * the _extended C support library_, containing the implementations of all the functions
   declared in all the other standard header files. 

The elementary C support library corresponds to the 'freestanding execution environment'
defined in the C standard. The elementary and extended libraries together correspond to the
`hosted execution environment`. 




The extended C support library has a dependency on the ??????
libraries .....

The elementary C support library does not depend on any other library, and is therefore suitable for .....

.....












-----------------------------------------------------------------------------------------------
## Program Startup {#main}

The _main_ function is named `__main` (the word `main` with two `_` underscores before it),
which must return nothing and have no arguments, as if declared thus: 

    void __main();

This function must *not* actually be declared anywhere.

The _conventional main_ function is named `main` and must have a return type of `int` and
either no parameters or two or three parameters as if declared thus:

    int main(void);

or:

    int main(int argc, char *argv[]);

or:

    int main(int argc, char *argv[], char *envp[]);

.....

In the header file `stdlib.h` there is a prototype of `main` and a definition of `__main`
which calls `main` function: 

```c
#include <unistd.h>

#pragma default_return(main, int, 0);
int main(int argc, char *argv[], char *envp[]);

void __main()
{
   char *envp[] = environ;
   char *argv[] = __adaos_argv;
   int argc = 0; while (argv[argc] != (void*)NULL) argc++;
   exit( main(argc, argv, envp) );
}
```




.......




-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## {#}






-----------------------------------------------------------------------------------------------
## Include File Locations {#loc}






L/gcc/T/V/include
/usr/local/include
L/gcc/T/V/include-fixed
L/T/include
/usr/include/T
/usr/include

`L` is the library directory
`T` is the name of the [target](../pxcr/realizor.md#targ)
`V` is the version of the ECLAT-C compiler



The standard header `stdlib.h` is implicitly included at the beginning of every file that
ECLAT-C compiles. 


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
## References

[1]: <https://www.gnu.org/software/c-intro-and-ref/manual/html_node/index.html> "GNU C Language Manual"










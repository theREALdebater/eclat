-----------------------------------------------------------------------------------------------
# C

ECLAT supports the C17 standard (ISO/IEC 9899:2017) version of the C language. 

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




The extended C support library has a dependency on the [run time system](../rts/rts.md)
libraries .....

The elementary C support library does not depend on any other library, and is therefore suitable for .....

.....












-----------------------------------------------------------------------------------------------
## Program Startup {#main}

The _main_ function is named `main` and must have a return type of `int` and either no
parameters or two parameters as if declared thus:

    int main(void);

or:

    int main(int argc, char *argv[]);




.......




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






-----------------------------------------------------------------------------------------------
## {#}










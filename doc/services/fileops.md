-----------------------------------------------------------------------------------------------
# File Operations Services

A _file operations service_ is a service that performs operations on [files](../adaos/files.md)
as an important part of its functionality. 

The following synchronised interface type is declared in the package `AdaOS.Services`: 

```ada

type File_Operations_Service is task interface and System_Object;
```

Any service that is a file operations service derives from `File_Operations_Service`. 

Each file operations service will have its own _collection_ of files. How the files are stored,
whether they have names (and, if so, what the names are and how they ae managed), will be an
internal detail of the service, never to be exposed externally (to the users of the service).
Usually, each different service's collection will be independent of other collections. 

Similarly, the type (in an informal sense) of the files, exactly what they contain, their
relationships with each other, the rules governing the files, and so forth are all under the
control of the service. 

Typical file operations are:

 * Fetching (downloading) one or more files; 

 * Creating files and writing data into them; 

 * Deleting files. 

In addition, there will generally be other operations specific to the type of service. 





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






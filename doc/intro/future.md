-----------------------------------------------------------------------------------------------
# Future

Hopefully, at some point, I’ll be able to implement .....



-----------------------------------------------------------------------------------------------
## Regex

The package `Regex` will allow a regular expression ('regex') to be used to search a string, an
iterable of strings, or a text file, supporting wide characters and wide wide characters. 

A regular expression can be compiled into a binary form which can be used more efficiently for 
multiple searches. 

Options such as case-insensitive are supported. 

Search-and-replace is supported, in a string (returning a result object or a new string with 
replacements made), an unbounded string (making the replacements in the object directly), 

?????an iterable of strings (), 

?????and a text file. 



-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Ithaca

__Ithaca__ is an object database system for Ada.

Ithaca is based on the concept of the 'persistent object', which is an Ada object that is 
referenced by an object of a custom access type (named `Persistent_Access`). 

Persistent objects must be of a type that can be serialised (using the attributes `Output` and 
`Input`), and this is how the objects are written into the database and read out of it. 

Caching of objects in memory, and flushing of new and dirty objects back into the database, is 
handled automatically, but flushing can be triggered by calling a procedure (named `Save`). 

There is full transaction support, ......





-----------------------------------------------------------------------------------------------
## Daedalus

__Daedalus__ will be a full implementation of the ISO standard SQL relational database language 
for ECLAT and the Realizor, with some extensions and a few omissions. 

Daedalus is based on the concept of static structure and code, so aspects of the SQL language 
that relate to dynamic changes to structure and code are omitted (and irrelevant). 

Daedalus comprises:

 * an ECLAT plugin; 
 
 * a Realizor plugin. 
 
Tables, views, procedures, functions, and other SQL entities are declared in source text files
and compiled into an ECLAT library (similarly to Ada library units). SQL entities can be 
exported from SQL and imported into Ada; Ada entities can be exported from Ada and imported 
into SQL. In this way, SQL procedures can call Ada subprograms and Ada subprograms can call SQL 
procedures. 

An Ada package (`Daedalus`) enables a Daedalus database (a set of control files and 
data files) to be opened and closed. Calls into SQL procedures enables the Ada program to 
interact with the database. 

Usually a service is programmed in Ada and has an associated database. Outside software 
accesses the database exclusively through calls into the service. 

Tables can be stored in multiple actual tables (called 'assemblies'). A variety of formats of 
assembly can be chosen. Indexes are implemented as (extra) assemblies. 

There will be functionality to easily archive (move) or back up (copy) data in a Daedalus 
database, and of course to read it back in again. 

A special feature will be that statistics files can be generated and used by the Realizor to 
re-realise Daedalus modules optimised for actual usage patterns. 

There will be a facility to support ODBC (stored procedures only), as well as the facility to 
replace accessing a Daedalus database with accessing an ODBC database instead (without having 
to change the Daedalus code). 

There will be a tool to extract the structure and stored procedures (and functions) of a 
Microsoft SQL Server or an Oracle database, and convert it into the equivalent Daedalus code, 
as well as to transfer the data. 





-----------------------------------------------------------------------------------------------
## Midori

__Midori__ will be a new language for ECLAT that provides for industrial-strength declarative 
programming. 

Midori's basic syntax is very similar to that of Ada, but instead of declaring procedures and 
functions, one declares 'predicates' that express a relationship between a set of input data
and corresponding output data. 

Midori uses the predicates to transform an input data sequence into an output sequence (or 
merely to test whether the output sequence is empty or not). 

Midori promises to make many complicated software problems easier to maintain. It also lends 
itself to the inherent parellisation of programs, to take advantage of (massively) multi-
processor computers. 

Midori will have special syntax to easily read a SQL table (or view) as if it were a predicate 
generating multiple data instances (one per row of the table). It will also have full 
facilities for interfacing with Ada code. 




-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Titian

__Titian__ will be an Ada library that provides support for Graphical User Interfaces (GUIs) 
and graphical printing and font and image manipulation. 

Functionality provided includes:

 * lots of stock GUI components; 

 * colours and palettes;
 
 * keyboard and mouse events; 
 
 * clipboard; 
 
 * fonts and text formatting; 

 * discrete graphics (pixel/raster-based); 
 
 * scaled graphics (scalable/vector); 
 
 * output to a screen, printer, in-memory image format, or file; 
 
 * loading of icons and files into various in-memory image formats. 
 
.....



-----------------------------------------------------------------------------------------------
## Allegra

__Allegra__ will be a scripting language based on Ada syntax.

.....





-----------------------------------------------------------------------------------------------
## AdaShell

__AdaShell__ will be a shell scripting interpreter, whose syntax is based in the Unix Bourne 
shell and its derivatives. 

A Simple Ada Shell project already exists on [GitHub][1]. 

......





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Stedi

__Stedi__ will be a source text editor which is extensible enough to become a full Integrated 
Development Environment (IDE). 

Stedi will have plugins for:

 * browsing the file system, manipulating files, and selecting files to edit; 
 
 * performing colorisation and beautification for specific programming languages; 
 
 * integration with ASIS data to provide enhanced Ada programming support; 
 
 * text file comparison, capable of being programming language-aware; 
 
 * comprehensive Git client functionality; 
 
 * .....





-----------------------------------------------------------------------------------------------
## EIDOS

__EIDOS__ will be a GUI debugger, which can operate remotely over a network. 

I intend to make this debugger a lot more sophisticated than typical current offerings. 

It'll be both a plugin for Stedi and a standalone GUI application. 







-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## ALDUS

__ALDUS__ (Advanced Library Distribution and Upload System) will be a library manager to make 
it easy to create and manage a _sanctuary_ of wrapped libraries. It will also make it easy to 
browse for and use libraries in sanctuaries, as well as updating to newer versions, etc. 

There'll be a GUI and a command-line tool. 

ALDUS will manage dependencies between libraries .....

ALDUS will also allow a library to be downloaded as source code, so it can be modified locally 
(and then perhaps uploaded again as a new version). ALDUS will make it easy to wrap a library 
that has been locally developed and upload it into a sanctuary. 

ALDUS will allow pre-built modules to be downloaded. Each module comes with its own [stub 
library](../eclat/building.md#stublibs), giving easy access for Ada programs to all the 
module's exports. This would have the advantage of speeding up compilation and building of 
libraries based on the module, and it would be a suitable form for proprietary software to be 
made available for use by other software. 

ALDUS will deal with licensing .....



-----------------------------------------------------------------------------------------------
## Kantan

Kantan will be a package manager system. A package is an archive file (in the ZIP format, 
probably) that contains all the files needed to introduce a specific piece of functionality to 
a computer in a controlled manner. Associated with every package is meta-data (probably in the 
form of an XML file), that gives details of the package (its name, version, description, 
licence, etc). 

A plugin for ECLAT will extend the build process to wrap up the output from a build into a 
Kantan package and generate the meta-data (file). 

A Choreographer (q.v.) plugin will enable a package, after it has passed testing, to be added 
to a catalog of packages. 

A GUI will allow the user to: 

 * browse a catalog and select packages to be installed; 
 
 * browse a list of already installed packages, view available updates and upgrades for each, 
   select packages to be uninstalled, updated, or upgraded; carry out the selecte operations. 

A command-line tool will provide the same functionality. 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Syrinx: Distributed Computing

The service __Syrinx__ ..... 

?????

 will act as a gateway to a network, allowing services running on other 
computers to be transparently accessed as if they were normal (local) services. 





-----------------------------------------------------------------------------------------------
## Nyota

__Nyota__ will be an ECLAT plugin that adds various bits of functionality:

 * Easy XML, JSON, and YAML serialisation of Ada types just by adding an aspect. 
 
 * All the infrastructure needed to support remote services (and remote assemblies). 
 
 * Easy exposure of a service as a SOAP-based or RESTful web API, again by adding a few 
   aspects and a bit of configuration.
   
 * Support for turning a remote service (or web API) into a replicant service, which can be run 
   on multiple computers simultaneously, to provide redundancy and load balancing, with support 
   for database synchronisation and so forth. 

.......

Nyota will _wrap_ every remote procedure call .....

 * .....

 * the wrapper will then call an endorsement procedure .....

 * .....

.....


-----------------------------------------------------------------------------------------------
##






-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Choreographer

__Choreographer__ will be a build and deployment management system ......

There will be plugins that provide a variety of extra functionality:

 * building using a specific toolset (e.g. ECLAT); 

 * provisioning of virtual servers on demand; 
 
 * running a set of automated integration (or sanity) tests after deployment; 
 
 * ensuring that the necessary compartment exists on target computers, and 
   automatically fetching and installing necessary components etc.; 
   
 * .....




There will be a GUI for managing deployments and a command-line tool that provides the same 
functionality. 

The will be a service, the Choreographer Agent, which is installed on every computer that is to 
have software deployed onto it, .....








-----------------------------------------------------------------------------------------------
## 

..... OAuth2 server ......



-----------------------------------------------------------------------------------------------
## 

..... e-mail server ......



-----------------------------------------------------------------------------------------------
## Puca

__Puca__ is an XSLT (XML Style Language Transformation) library and command-line tool. 





-----------------------------------------------------------------------------------------------
## Athena

__Athena__ is an XQuery library and ECLAT plugin ......





-----------------------------------------------------------------------------------------------
## ??????


XML-FO or HTML to PDF or printer






-----------------------------------------------------------------------------------------------
## Nibelheim

__Nibelheim__ is an XML technologies GUI workbench ......




-----------------------------------------------------------------------------------------------
## Larunda

__Larunda__ will be a replicant service which implements an HTTP server.

......





-----------------------------------------------------------------------------------------------
## VIPA

__Versatile Interactive Page Architecture__, or __VIPA__ will be an industrial-strength web 
site CMS (content management system).

VIPA comprises:

 * a service which delivers web pages; 
 
 * a GUI web site management application; 
 
 * a GUI web page authoring (and testing) application. 

The VIPA service will have two variants supporting operation either as a Larunda plugin or an 
an FCGI application. Both variants will support session identity (using cookies) and user 
identity (via users signing into a session or OAuth2). 

The web page authoring is very like a word processor. Dynamic elements can be inserted and 
configured. 

VIPA will have 'core' functionality supported by a variety of dynamic elements. For example, 
user sign-in and registration. 

VIPA plugins will add a variety of further dynamic elements, 

......





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## NAILS

__NAILS__ (Notification, Advisory, Information, and Liaison System) is a customer issue 
tracking system.

NAILS is intended to enable a software maintainer to present a professional-standard issue 
tracking system to a customer. 

NAILS ......

.........





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## Hearthstone

__Hearthstone__ will be a collaborative documentation system, but with a lot of further 
functionality added via plugins. 

Hearthstone will comprise:

 * a document database and attendant service, which is a replicant service; 
 
 * a GUI desktop application; 
 
 * a VIPA plugin, providing a web interface. 
 
The central concept is of a 'doclet', which is like a word-processing or rich-text document, 
but is intended to be small (a paragraph or two). Doclets are linked together, using a form of 
hyperlink, and categorised for easy searching. 

A doclet is locked whilst a user is working on it, to prevent the lost update problem. The idea 
is that having an overall (large) document made up of many doclets linked together menas that 
many different people can be working on the overall document in a controlled way. 

Users can create doclet templates, with multiple fields and actions. The fields and actions 
(buttons) can be arranged according to some simple layout options. 

A field has a 'type'; its values are of this type. One type is 'rich text', so a simple doclet 
with just some text in it can have just one rich text field. There are field types such as 
date, time, and numeric as well. 

Actions can have pre-defined logic or can be custom-defined using Allegra scripts. 

Plugins can add further types and pre-defined actions, as well as adding Allegra objects, and 
this opens the way for significant extra functionality to be added.

Major planned plugins will support:

 * NAILS (issue tracking) integration and back-end management; 
 
 * SDLC (software development lifecycle); 
 
 * release information, integrating with Choreographer; 
 
 * project planning and budgeting.

SDLC:

 * software specification and maintenance documentation; 
 
 * Agile processes (Scrum, Kanban); 
 
 * software testing management; 
 
 * .....
 
Integrations with:

 * [Confluence](); 
 
 * [Trello](); 
 
 * [Bitbucket](); 
 
 * [Fisheye](); 

 * [Jira](https://www.atlassian.com/software/jira); 

 * [Favro](https://www.favro.com); 

 * [Microsoft DevOps](?????); 

 * [Zoho Sprints](?????); 

 * [Asana](?????); 

 * [monday.com](?????); 

 * [Wrike](?????); 

 * [ProjectManager](?????); 

 * [Assembla](?????); 

 * [Clarizen Go](?????); 

 * [Axosoft](?????); 

 * [](?????); 

 * [](?????); 

 
.....





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## 





-----------------------------------------------------------------------------------------------
## References

[1]: <https://github.com/jschaf/Simple-Ada-Shell> "GitHub: Simple Ada Shell"






-----------------------------------------------------------------------------------------------
# Allegra





-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Commands {#cmd}

A _command_ is either a _procedure_ or a _function_. 

When a function is executed (and does not propagate an exception), it _returns_ a value, of any
[type](#typ). 

When a procedure is executed, it may do things (and update things in the executional
environment), but it does not return a value. 











-----------------------------------------------------------------------------------------------
## Command Parameters {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Basic Syntax {#syn}

.....

The character encoding of the file is .....

An Allegra script is made up of a sequence of lines of text. Typically, these will be in a
plain text file, with a sequence of characters, called a _line break_, to indicate the end of a
line (and the beginning of the next line). 

Each different platform (operating system) has its own conventional sequence of characters
forming a line break, usually control characters. 

On Unix-derived platforms, including modern macOS, the line break is usually the Line Feed (LF)
character, whose code point is 10. On CP/M, MS-DOS, and Windows heritage platforms, the line
break is usually Carriage Return (CR, code point 13) followed by LF; sometimes this sequence is
abbreviated CRLF. On old Macintosh platforms line break was CR. 

Apart from the line break, no control characters are perm,itted in an Allegra script. This
includes the Horizontal Tabulation (HT, code point 9) character that is often used in
programming language text files. You will need to replace these with spaces, if you
accidentally put them into an Allegra script. 

Each line is parsed into _words_. A word isn't quite what you might intuitively think. It is
one of the following: 

 * an [escaped string](#escstr)

 * a [literal string](#strlit)

 * a [quoted string](#quostr)

 * a [literal numeric](#numlit)

 * a [function call](#fcall)

 * a [literal list](#listlit)

 * a [command break_](#cmdbrk)

 * a [procedure list](#proclist)


### Escaped String {#escstr}

An _escaped string_ is a sequence of characters which begins and ends with a `'` single-quote character

The following rule applies: 

 * It cannot contain a `'` single-quote character that is not one of a pair of adjacent
   single-quote characters. 

The type of an escaped string is [string](#str), and its value is the sequence of characters
that make up the literal string, except that a pair of adjacent `'` single-quote characters are
interpreted as one single-quote character in the value. 

For example, .....






### Literal String {#strlit}

A _literal string_ is a sequence of characters which may have one or more [function
calls](#fcall) in it. 

The following rules apply, other than within any function calls: 

 * It must not contain any whitespace characters;

 * It must not begin with a digit, open brace, double quote, or single quote
   character; 

 * It cannot not contain a parenthesis character anywhere, as they would be interpreted as the
   beginning or end of a function call. 

Each function call is executed, and it is replaced by its result in the string's value. If the
result `V` is not of type [string](#str), then the function call `(V) as string` is executed
to convert it into a string. 

The type of a literal string is a string, and its value is the sequence of characters that make
up the literal string, except that a function call is replaced by its value, as just described. 

For example, .....



If parenthesis characters are required, the function call `(lpar)` can be used instead of an
`(` left parenthesis character, and `rpar` can be used instead of a `)` right
parenthesis character. 


### Quoted String {#quostr}

.....

The following rules apply other than within any function calls: 

A quoted string begins and ends with a `"` double-quote character. It may contain one or more
[function calls](#fcall). 

Each function call is executed, and the result is inserted into the string in its place. If the
result `V` is not of type [string](#str), then the function call `(V) as string` is executed
to convert it into a string. 

Otherwise, all the 

........




### Literal Numeric {#numlit}

.....


### Function Call {#fcall}

A _function call_ ............







### Literal List {#listlit}

......





### Procedure List {#proclist}

A _procedure list_ is a list of procedure calls. 

This syntactic element is unusual, in that it relies on what comes immediately after the
procedure list in the corresponding [profile](#prof). 

Whatever does come immedaitely after the procedure list must begin with a string literal





### Command Break {#cmdbrk}

A _command break_ is the `;` semi-colon character. It's purpose is to separate two or more
commands that are all on one line. 

.....


-----------------------------------------------------------------------------------------------
## Abbreviations {#}

.....


### ?????

is actually 



### ?????





### ?????





### ?????






### Commas in Function Calls

A function call with a comma in it, for example:

    (foo x, bar y z)

is exactly equivalent to two function calls, one after the other, with a space inbetween:

    (foo x) (bar y z)

This even works in a [literal string](#strlit), since the extra space inbetween is considered a
part of the literal string's value, rather than separating the literal string into two, as it
would normally. 

Multiple commas work in the same way, being equivalent to a sequence of three or more function
calls, all with a space in between. For example:

    (foo x, bar y z, hum (dingle doo))

is exactly equivalent to:

    (foo x) (bar y z) (hum (dingle doo))

and so on.



-----------------------------------------------------------------------------------------------
## Command Instances {#inst}

An Allegra script is interpreted as a sequence of _command instances_. Each command instance
should fit a pattern that is set by a [command profile](#prof). 


.....






-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Command Profiles {#prof}

Every [command](#cmd) is associated with one or more _command profiles_. 

A command profile defines whether a [command instance](#inst) matches the command, and which
parts of the command instance match the parameters of the command. 










-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Command Definitions {#dfn}

There are two kinds of [command](#cmd) that are _command definitions_: 

 * a _procedure definition_, which creates a new procedure; 
 
 * a _function definition_, which creates a new function. 

A command definition creates a new [command](#cmd), whose profile can be recognised within the
immediately surrounding [scope](#scope) of the command definition. 








-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Types {#typ}

.....

 * [null](#null)

 * [Boolean](#bool)

 * [numeric](#num)

 * [string](#str)

 * [list](#list)

 * [command instance](#cmdtyp)

 * 


### Null {#null}

.....


### Boolean {#bool}

.....


### Numeric {#num}

.....


### String {#str}

An Allegra _string_ is an Ada unbounded wide string .....

that corresponds to the Ada type `Ada.Strings.Wide_Unbounded.Wide_Unbounded_String` 

......





### List {#list}






### Command Instance {#cmdtyp}








For example, if a command instance: 

    123.456 fmt "$00,000.00"

is assigned to a variable named `f`, like this: 

    f := (cmd '123.456 fmt "$00,000.00"')

then the command instance could be executed by writing: 

    ((f))

The inner pair of parentheses causes `f` to be executed, resulting in the command instance, and
then the outer pair of parentheses causes the command instance itself to be executed, resulting
in the string: 

    $00,123.46

.....



-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Profiles {#prof}

A _profile_ is a string whose value describes the recognisable syntax of a specific command. 

Although all commands must obey a basic syntax fixed by Allegra, each different command has a
syntax of its own. 

.....



A profile must comprise a sequence of _profile elements_. 

Each profile element must be one of: 

 * a _constant_; 

 * a _variable_; 

 * an _optional element_; 

 * a _repetition element_. 

A _sub-profile_ is the same as a profile, except that it occurs inside an element of another
profile or sub-profile. 

......

Certain additional rules apply to a profile: 

 * 







### Example Profile

The 'if' command has the following profile: 

    if (condition) then <true commands> 
    {elsif (other condition) then <other commands>} 
    [else <false commands>]
    end if

This profile is unusually complex, so it has been presented here on several lines, but in fact
it would all just be one long line. 




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




-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## {#}












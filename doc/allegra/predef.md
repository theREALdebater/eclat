-----------------------------------------------------------------------------------------------
# Predefined Commands



-----------------------------------------------------------------------------------------------
## Essentials

.....


### Null

The function:

    null

returns the null value, the one and only value of the [null type](allegra.md#null).


### False and True

The function:

    false

returns the false value of the [Boolean type](allegra.md#bool). 

The function:

    true

returns the true value of the [Boolean type](allegra.md#bool). 


### 



### 



### 



### 



### 



-----------------------------------------------------------------------------------------------
## Control Structures {#}

.....


### If {#}

The procedure:

    if (condition) then <true commands> 
    {elsif (other condition) then <other commands>} 
    [else <false commands>]
    end if

executes one of the sequences of commands (`true commands`, any one of multiple `other
commands`, or `false commands`). 

If the `condition` is true, then `true commands` is executed (and no other commands). 

If the `condition` is false and there is at least one `elsif` clause, then the `elsif` clauses
are iterated through in the order they are specified. For the first `other condition` which is
true, the corresponding `other commands` are executed (and no other commands). 

If the `condition` is false and there are no `elsif` clauses or if the `other condition` of all
of the `elsif` clauses is false, then this procedure executes the `false commands` (and no
other commands), or if there is no `else` clause, this procedure does nothing. 

.....


### While Loop {#}

The procedure:

    while (condition) loop <commands> end loop

executes the `commands` repeatedly for zero or more iterations. 

For each iteration, the `condition` is evaluated. If it is false, the procedure is completed
(there is no error). Otherwise, the `commands` are exectued. Then the next iteration begins. 


### For Loop {#}

The procedure:

    for (variable) in (range) loop <commands> end loop

executes the `commands` repeatedly for zero or more iterations. 

Before the first iteration, the `range` is evaluated and its value is kept. 
If the range is empty (its first value is greater then its last value), the procedure is completed
(there is no error). 

Otherwise, the variable is set to the first value of the range, and the first iteration begins.

.....


### {#}




### {#}







-----------------------------------------------------------------------------------------------
## Comparisons and Logical Functions {#}



### Equality {#equal}

The function:

    (left) = (right)

returns the [Boolean](allegra.md#bool) true value if `left` is equal to `right`.

Equality is determined by a set of rules: 

 * Two values are not equal if they are of different types. 

 * Two values of the [null](allegra.md#null) type are equal.

 * Two values of the [Boolean](allegra.md#bool) type are equal if they are either both false or
   both true. For readability, it is recommended that the [whenever](#whenever) function is
   used instead. 

 * Two values of the [numeric](allegra.md#num) type are equal if their numeric values are
   exactly equal, and they are not equal otherwise. 

 * Two values of the [string](allegra.md#string) type are equal if they both have exactly the
   same sequence of characters in them. They are equal if they are both empty (and not equal
   otherwise). 

 * Two values of the 

 * Two values of the 

 * Two values of the 


### Inequality {#ineq}

The function:

    (left) /= (right)

returns the [Boolean](allegra.md#bool) true value if `left` is not equal to `right`. 

```allegra

function '(left) /= (right)' is
   return (not ((left) = (right)))
end function
```


### Less Than {#lessthan}

The function:

    (left) < (right)

returns the [Boolean](allegra.md#bool) true value if `left` is less than `right`.

The _ordering_ of two values ......


### Less than or Equal {#}




### Greater Than {#}




### Greater Than or Equal {#}




### Effective String Equality {#effeq}

The function:

    (left) ~= (right)

returns the [Boolean](allegra.md#bool) true value if the string value `left` is _effectively_
equal to the string value `right`. 

The concept of two strings being effectively equal is if, in general usage, a human would say
they were the same. For example, usually `façade` would be considered the same as `Facade`. 

Two strings are effectively equal if their normalised sequences of characters are effectively
equal. 

A string is _normalised_ when its character sequence is normalised according to Normalized Form
C of the [Unicode standard](?????). 

Two characters are effectively equal if any of: 

 * they both have the same code point value; or 
 
 * they are both the same base letter, 

 * 

.....

Two letters are the same base letter if: 

 * ignoring their case (lower or upper); or 

 * 

.....

If either `left` or `right` is not a [string](allegra.md#string) value (or they both aren't),
this function raises the exception: 

    Expected a string value


### {#}




### Logical Negation {#}

The function:

    not (condition)

returns the [Boolean](allegra.md#bool) true value if the `condition` is the Boolean 
false value, and it returns false if `condition` is true. 

If the `condition` is not a Boolean value, this function raises the exception: 

    Expected a Boolean value


### Logical Conjunction {#}




### Logical Disjunction {#}




### Whenever {#whenever}

The function:

    (left) whenever (right)

returns the [Boolean](allegra.md#bool) true value if `left` and `right` are either both Boolean
true or both Boolean false, and returns false otherwise.

If either `left` or `right` is not a Boolean value (or they both aren't), this function raises
the exception: 

    Expected a Boolean value


### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}






-----------------------------------------------------------------------------------------------
## Arithmetic and Numeric Functions {#}



### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




-----------------------------------------------------------------------------------------------
## Range Operations [#range]



### Construct a Range {#mkrng}

The function:

    (first) .. (last)

returns a range, .....





### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}



-----------------------------------------------------------------------------------------------
## Date and Time {#time}

.....


### Time Now {#}

.....

    clock

This function returns the number of seconds, accurate to at least the nearest millisecond,
that, at the time the function is called, have elapsed since the MJD epoch (midnight at the
beginning of 17th November 1858). 





### {#}

    year (year) month (month)

Returns a [year-month](#ym) value. 

The `year` is a year in the proleptic Common Era (CE) calendar, with the year preceding 1 CE
being 0 CE, and the year before that being -1, and so on. 

The `month` is either an integer in the ragne 1 to 12 or it is the name of a month, either
abbreviated to three letters or in full. 

Month names should be .....

......


### {#}

    year (year) month (month) day (day)

This function returns the number of days that, at the date represented by the given parameter
values, have elapsed since the MJD epochal day (17th November 1858). 

BEWARE! This function returns the number of days (an integer), not the number of seconds.

.....

An alternative profile exists for this function: 

    ymd (year, month, day)



### {#}

    year (year) month (month) day (day) hours (hours) minutes (minutes) seconds (seconds)

This function returns the number of seconds, accurate to the millisecond, that, at the date and
time represented by the given parameter values, have elapsed since the MJD epoch (midnight at
the beginning of 17th November 1858). 




An alternative profile exists for this function: 

    ymdhms (year, month, day hours, minutes, seconds)



### {#}

    (date) as date [format (format)]


### {#}

    (time) as time [format (format)]

.....

    


### {#}




### {#}




### Time Constants {#}



    seconds-per-day
    seconds-per-hour
    seconds-per-minute




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}






-----------------------------------------------------------------------------------------------
## String and List Interrogation and Manipulation {#}

There are several commands which are applicable to both strings and lists.

.....



### Length of a List or String {#len}

The function:

    (value) length

returns the number of elements in a list `value` or the number of characters in the string
`value`. 

The length of an empty list or string is 0. 

If the `value` is not a string or list value, this function raises the exception: 

    Expected a string or list value


### Element of a List or Character of a String {#elem}

The function:

    (value) # (index)

returns one element of a list or one character of a string. 

The first element of a list has _index_ 1. The second element has index 2, and so on.

Similarly, the first character of a string has index 1, the second has index 2, and so on. 

.....

If the `value` is not a string or list value, this function raises the exception: 

    Expected a string or list value

If the `index` is not a numeric value, or is not an integer, this function raises the exception: 

    Expected an integer numeric index

If the value of the `index` is is less than 1, this function raises the exception: 

    Expected the index to be positive

If the value of the `index` is is greater than the length of `value`, this function raises the
exception: 

    Index beyond length of value


### Slice of a List or String {#slice}

The function:

    (value) ## (range)

returns all or part of a list or string. 









### {#}




### {#}




### {#}



### {#}




### {#}




### {#}




### {#}




-----------------------------------------------------------------------------------------------
## String Interrogation and Manipulation {#}



### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}




### {#}







-----------------------------------------------------------------------------------------------
## List Interrogation and Manipulation {#}



### {#}




### {#}




### {#}




### {#}




### {#}




### {#}






-----------------------------------------------------------------------------------------------
## File or Member Operations {#file}

.....


### Get Current Directory {#pwd}

The function:

    current directory

returns the full path of the current directory. 

.....

```allegra

procedure pwd is
   write (current directory)
end procedure
```





### Change Directory {#}

The procedure:

    set current directory to (path)

changes the path of the current directory. 

If `path` is relative, then the current directory's new path will be the `path` relative to the
current value of the current directory. 

If `path` is absolute, that will be the new path of the current directory. 

.....


```allegra

procedure 'cd (path)' is
   set current directory to (path)
end procedure

procedure 'pushd (path)' is
   append (current directory) to "directory stack"
   set current directory to (path)
end procedure

procedure popd is
   path := ("directory stack" # ((directory stack) length))
   "directory stack" := ("directory stack" # (1 .. ((directory stack) length)))
   set current directory to (path)
end procedure
```






### {#}




### List Files or Members {#list}

The function:

    list

returns a list of the full path of each of the files or members of the current directory. 

The function:

    list (pattern)

returns a list of the full path of each of the files or members whose full path matches the
`pattern`. 

If the pattern is absolute (it begins with a `/`) .....









### Delete a File or Member {#del}

The procedure:

    delete (path)

deletes the file or member identified by `path`, if it exists. 

If no file or member can be identified by the `path`, this command does nothing (and there is
normally no error). 

Deletion of a file or member can have consequences, depending on the platform, filesystem, or
the type of file or member. For example, deleting a link might cause the file or member it
targets to be deleted also. 

.....

Note that this command, as with all Allegra file operations except `list`, can only be passed
one path, and only acts on one file or member. There is no pattern matching (or 'globbing').
This command can be combined with the `list` command to act on multiple files or members. 

For example, to delete all the files in the current directory whose name ends with `.txt`, the
following could be used: 

    for f in (list *.txt) loop; delete (f); end loop

For convenience, a procedure such as `rm` could be defined:

```allegra

procedure 'rm (pattern)' is
   for f in (list (pattern)) loop
      delete (f)
   end loop
end procedure
```

Now the command:

    rm *.txt

will delete all the files in the current directory whose name ends with `.txt`. 

A recursive version of this procedure might be defined, but beware of the potential destructive
power of such a command. It might be prudent to get user confirmation in
[interactive](#interactive) mode if there are a lot of items to delete. 


### Moving or Renaming a File or Member {#}




### Copying a a File or Member {#}




### {#}




### {#}





### {#}




### {#}





### {#}




### {#}





### {#}




### {#}





### {#}




### {#}





### Last-Modified Time {#lastmod}

The function:

    (file) last-modified

Returns the date and time (seconds elapsed since the MJD epoch) that the `file` was most
recently modified. 

.....


### {#}





### {#}




### {#}





### {#}




### {#}





### {#}




### {#}






-----------------------------------------------------------------------------------------------
## {#}



### {#}




### {#}




### {#}




### {#}




### {#}




### {#}






-----------------------------------------------------------------------------------------------
## Character Replacements {#}

.....

| Character  | Command  | Description                           |
| ---------- | -------- | ------------------------------------- |
| `(`   | `(lpar)`      | left parenthesis                      |
| `)`   | `(rpar)`      | right parenthesis                     |
|   9   | `(ht)`        | horizontal tabulation                 |
|  10   | `(lf)`        | line feed                             |
|  13   | `(cr)`        | carriage return                       |
|  12   | `(ff)`        | form feed                             |
| ???   | `(vt)`        | vertical tabulation                   |
| 13,10 | `(crlf)`      | CR followed by LF                     |
|       | `(nl)`        | new line of platform
| `` | `()` |  |
| `` | `()` |  |
| `` | `()` |  |
| `` | `()` |  |
| `` | `()` |  |
| `` | `()` |  |
| `` | `()` |  |








-----------------------------------------------------------------------------------------------
## Miscellaneous {#misc}

.....


### Are We in Interactive Mode? {#interactive}

The function:

    is interactive

returns the [Boolean](allegra.md#bool) value true if the stdin and stdout are both connected to
the console of a human being. It returns false otherwise. 

.....


### {#}




### {#}




### {#}




### {#}




### {#}






-----------------------------------------------------------------------------------------------
## Environment Variables {#envvar}



### Get Value of an Environment Variable {#env}

The function:

    env (name)

returns the value of the environment variable whose name is `name`. 

The value returned by this function will always be a string. 

.....




### Set Value of an Environment Variable {#env}

The procedure:

    setenv (name) := (value)

sets the value to `value` of the environment variable whose name is `name`. 

If the given `value` is not a string, then the function `(value) as string` is automatically
called to convert it into a string. 

.....


### {#}




### {#}




### {#}




### {#}




### {#}






-----------------------------------------------------------------------------------------------
## {#}



### {#}




### {#}




### {#}




### {#}




### {#}




### {#}






-----------------------------------------------------------------------------------------------
## {#}



### {#}




### {#}




### {#}




### {#}




### {#}




### {#}















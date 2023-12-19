-----------------------------------------------------------------------------------------------
# Locales


.....


..... the _locale system_ .....


..... `Locale_Database`, declared in the package `Ada.Locales.Localization` .....


..... `Single_Locale` .....



???? also C




..... [Unicode Locale Data Markup Language][1], or __LDML__ .....






-----------------------------------------------------------------------------------------------
## 

Section A.19 of the [Ada Reference Manual][1] defines the package `Ada.Locales`. 

All of the Ada language facilities added by AdaOS are child packages of `Ada.Locales`. 

The package specification is defined as follows: 

```ada

package Ada.Locales
with
   Preelaborate,
   Remote_Types
is
   type Language_Code is new array String (1 .. 3)
      with Dynamic_Predicate =>
         (for all E of Language_Code => E in Character range 'a' .. 'z');

   type Country_Code is new array String (1 .. 2)
      with Dynamic_Predicate =>
         (for all E of Country_Code  => E in Character range 'A' .. 'Z');

   Language_Unknown : constant Language_Code := "und";
   Country_Unknown : constant Country_Code := "ZZ";

   function Language return Language_Code;
   function Country return Country_Code;

end Ada.Locales;
```

For the types `Language_Code` and `Country_Code` the [Annotated Ada Reference Manual][2] makes
the following comment: 

 * Discussion: {AI12-0037-1} These types are derived from type `String` so that they can easily
   be converted to or from type `String`. That's important if one of these values needs to be
   input or displayed (via `Text_IO`, perhaps). We use the predicate to ensure that only possible
   component values are used. Ada does not allow converting between unrelated types with
   components that don't statically match, so we cannot declare new types with constrained
   components if we want conversions to or from type String. 

The AARM says the following about this package: 

The _active locale_ is the locale associated with the partition of the current task. 

Implementation Note: {AI05-0233-1} Some environments define both a system locale and the locale
of the current user. For such environments, the active locale is that of current user if any;
otherwise (as in a partition running on a server without a user), the system locale should be
used. 

Function `Language` returns the code of the language associated with the active locale. If the
`Language_Code` associated with the active locale cannot be determined from the environment, then
`Language` returns `Language_Unknown`. Otherwise, the result is a lower-case string representation
of an ISO 639-3:2007 alpha-3 code that identifies a language. 

Discussion: Some common language codes are: "eng" – English; "fra" – French; "deu" – German;
"zho" – Chinese. These are the same codes as used by POSIX systems. We considered including
constants for the most common languages, but that was rejected as the likely source of
continual arguments about the constant names and which languages are important enough to
include.

Function `Country` returns the code of the country associated with the active locale. If the
`Country_Code` associated with the active locale cannot be determined from the environment,
then `Country` returns `Country_Unknown`. Otherwise, the result is an upper-case string
representation of an ISO 3166-1:2020 alpha-2 code that identifies a country.

Discussion: Some common country codes are: "CA" – Canada; "FR" – France; "DE" – Germany; "IT" –
Italy; "ES" – Spain; "GB" – United Kingdom; "US" – United States. These are the same codes as
used by POSIX systems. We didn't include any country constants for the same reasons that we
didn't include any language constants. 

The [Rationale][3] says:

When writing portable software it is often necessary to know the locality in which the software
is to be run. Two key items are the country and the language (human language that is, not
programming language).

Knowledge of the locale is important for writing programs where the convention for certain
information varies. Thus in giving a date we might want to add the name of the day of the week
and clearly in order to do this we need to know what language to use. An earlier (really
grotesque) attempt at providing this information introduced a host of packages addressing many
issues. However, it was decided that for simplicity and indeed reliability all that is really
needed is to know the language to use and the country.

Canada is interesting in that it has just one country code ("CA") but two language codes ("eng"
and "fra"). In Quebec, a decimal value for a million dollars and one cent is written as
$1.000.000,01 whereas in English language parts it is written as $1,000,000.01 with the comma
and stop interchanged.

Sometimes, several locales might be available on a target. Some environments define a system
locale and a locale for the current user. In the case of an Ada program the active locale is
the one associated with the partition of the current task.

Finally, note that subsequent to ISO standardization, some serious difficulty was found in the
practical use of the types `Language_Code` and `Country_Code`. Accordingly, they have been
changed as described in Section 9.5 of the Epilogue.


-----------------------------------------------------------------------------------------------
## Locale Identifiers

A _locale identifier_ uniquely identifies a locale or a set of locales. 

```ada

type Locale_Identifier
is
    record
        Language: Language_Code;
        Country:  Country_Code;
    end record;
```









?????

The locale system uses [BCP 47, RFC-5646][2] language tags, but trivially modified so that they 
can be expressed as legal Ada identifiers. 

The complete set of locale identifiers is declared in the enumeration type `Locale_Identifier`, 
which is declared in the package `Ada.Locales.Global`. This set is, of course, not complete, 
and it should be assumed that it will be added to over time. 

Note that the locale system uses these language tags to identify locales, not just languages. 
This is extending the meaning of the BCP 47 tags somewhat, but it works in practice. 







......


### Language Tags as Ada and C Identifiers





?????

In order to enable a language tag as defined in RFC-5646 to be used as a legal Ada or C 
identifier, all `-` hyphen characters are changed to be an `_` underscore character. 

For example:

    en-GB
    
is changed into:

    en_GB
    
Note that the case (upper or lower) of the letters is not changed. In the Ada language, 
equality of these identifiers will be case-insensitive. However, in the C language, they will 
be case sensitive. 

......



?????

The functions `Image` (Ada) and `locale_id_to_string` (C) return a string that represents the
preferred form (as regards upper or lower case letters) of the identifier in string form, with
the `_` underscores changed back to `-` hyphens. 

The functions `Value` (Ada) and `locale_string_to_id` (C) do the reverse: they attempt to
recognise a string as a specific locale identifier. Its recognition of letters is
case-insensitive. 



????? We might want functions that convert between the combination of ISO 639-3:2007 alpha-3
      code (language) and ISO 3166-1:2020 alpha-2 code (country), and [BCP 47, RFC-5646][2].





### Locale Hierarchy

Locale identifiers actually form a hierarchy; some locale identifiers are 
_covered_ by others. If a locale identifier `I1` is covered by another locale identifier `I2`, 
then in any situation where one may use `I2` to identify a locale, `I1` could be used to get the same effect. ??????

Therefore, the functions `Includes` (Ada) and `locale_includes` (C) are also declared, in the
package `Ada.Locales.Global` (Ada) and the header file `locale.h` (C). 

In Ada, if a locale identifier `I1` is covered by another locale identifier `I2`, then: 

    Includes (I2, I1)
    
will return `True`; it will return `False` otherwise. 

In C, if a locale identifier `I1` is covered by another locale identifier `I2`, then: 

    locale_includes(I2, I1)
    
will return `true`; it will return `false` otherwise. 


### Canonical Locales

The function `Canonical` takes a locale identifier as its only parameter and returns a locale 
identifier. If the given ID is superseded by another ID, that other ID is returned, otherwise 
the given ID is returned. 

We have (arbitrarily) defined `en_UK` to be the canonical alternative of `en_GB`, although the 
standards define them as synonyms without one taking any precedence over the other. 

.....


### ?????

The Ada functions `Includes` and `Canonical` are declared-pure, so they .....

......





-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Text Localisation

.....




### Text Character Classification

.....




### Text Collation

.....




### Language Translation

.....

ECLAT is supplied with a tool named [Phrasier](#phrasier) to assist with string translation in
Ada and C programs. 

.....




-----------------------------------------------------------------------------------------------
## Numerics

.....


```ada
package.Locales.Numerics
is


   function Number_Fill (Locale: in Locale_Identifier) return Character;
   function Number_Fill (Locale: in Locale_Identifier) return Wide_Character;
   function Number_Fill (Locale: in Locale_Identifier) return Wide_Wide_Character;

--| The function `Number_Fill` returns the .....
--| For example, for the locale ("eng", "US"), the number fill character is '*'. 

   function Number_Separator (Locale: in Locale_Identifier) return Character;
   function Number_Separator (Locale: in Locale_Identifier) return Wide_Character;
   function Number_Separator (Locale: in Locale_Identifier) return Wide_Wide_Character;

--| The function `Number_Separator` returns the .....
--| For example, for the locale ("eng", "US"), the number separator character is ','. 

   function Number_Radix_Mark (Locale: in Locale_Identifier) return Character;
   function Number_Radix_Mark (Locale: in Locale_Identifier) return Wide_Character;
   function Number_Radix_Mark (Locale: in Locale_Identifier) return Wide_Wide_Character;

--| The function `Number_Radix_Mark` returns the .....
--| For example, for the locale ("eng", "US"), the number radix mark is '.'. 



end;
```






### Units of Measure

.....



### Monetary Amounts

.....

```ada
package Ada.Locales.Money
is
   function Currency_Name (Locale: in Locale_Identifier) return String;
   function Currency_Name (Locale: in Locale_Identifier) return Wide_String;
   function Currency_Name (Locale: in Locale_Identifier) return Wide_Wide_String;

--| The function `Currency_Name` returns the .....
--| For example, for the locale ("eng", "US"), the currency name is "United States Dollar". 

   function Currency_Unit (Locale: in Locale_Identifier) return String;
   function Currency_Unit (Locale: in Locale_Identifier) return Wide_String;
   function Currency_Unit (Locale: in Locale_Identifier) return Wide_Wide_String;

--| The function `Currency_Unit` returns the .....
--| For example, for the locale ("eng", "US"), the currency unit is "USD". 

   function Currency (Locale: in Locale_Identifier) return String;
   function Currency (Locale: in Locale_Identifier) return Wide_String;
   function Currency (Locale: in Locale_Identifier) return Wide_Wide_String;

--| The function `Currency` returns the .....
--| For example, for the locale ("eng", "US"), the currency is "$". 

   function Currency_Subdenomination (Locale: in Locale_Identifier) return String;
   function Currency_Subdenomination (Locale: in Locale_Identifier) return Wide_String;
   function Currency_Subdenomination (Locale: in Locale_Identifier) return Wide_Wide_String;

--| The function `Currency_Subdenomination` returns the name of the subdenomination of the 
--| currency. 
--| 
--| For example, for the locale ("eng", "US"), the subdenomination is "Cent". 




   function Currency_Subdenomination_Digits (Locale: in Locale_Identifier) return Natural;

--| The function `Currency_Subdenomination_Digits` returns the .....
--| 
--| 
--| For example, for the locale ("eng", "US"), the number of subdenomination digits is 2. 


   function Currency_Picture (Locale: in Locale_Identifier; 
                              Fore:   in Natural; 
                              Aft:    in Natural) return Ada.Text_IO.Editing.Picture;

   function Currency_Picture (Locale: in Locale_Identifier; 
                              Fore:   in Natural) return Ada.Text_IO.Editing.Picture;

   function Currency_Picture (Locale: in Locale_Identifier) return Ada.Text_IO.Editing.Picture;

--| The function `Currency_Picture` returns the .....
--| 
--| For example, for the locale ("eng", "US"), the picture's string, with default `Fore` of 
--| 10 and `Aft` of 2, is "$$,$$$,$$$,$9.99". 
--| 
--| There are three overloadings. The one with a `Fore` parameter but no `Aft` assumes the 
--| value of calling `Currency_Subdenomination_Digits(Locale)` for `Aft`. The one with no 
--| arguments also assumes an implementation-defined value for `Fore` between 8 and 15. 

   function Currency_Fill (Locale: in Locale_Identifier) return Character;
   function Currency_Fill (Locale: in Locale_Identifier) return Wide_Character;
   function Currency_Fill (Locale: in Locale_Identifier) return Wide_Wide_Character;

--| The function `Currency_Fill` returns the .....
--| For example, for the locale ("eng", "US"), the currency fill character is '*'. 

   function Currency_Separator (Locale: in Locale_Identifier) return Character;
   function Currency_Separator (Locale: in Locale_Identifier) return Wide_Character;
   function Currency_Separator (Locale: in Locale_Identifier) return Wide_Wide_Character;

--| The function `Currency_Separator` returns the .....
--| For example, for the locale ("eng", "US"), the currency separator character is ','. 

   function Currency_Radix_Mark (Locale: in Locale_Identifier) return Character;
   function Currency_Radix_Mark (Locale: in Locale_Identifier) return Wide_Character;
   function Currency_Radix_Mark (Locale: in Locale_Identifier) return Wide_Wide_Character;

--| The function `Currency_Radix_Mark` returns the .....
--| For example, for the locale ("eng", "US"), the currency radix mark is '.'. 

   



end;
```



-----------------------------------------------------------------------------------------------
## Date and Time

.....

```ada
package Ada.Text_IO.Time_IO
is
   





end;
```



-----------------------------------------------------------------------------------------------
## Addresses

.....




-----------------------------------------------------------------------------------------------
## Telephone Numbers

.....




-----------------------------------------------------------------------------------------------
## 

????? Paper




-----------------------------------------------------------------------------------------------
## 

????? Identification




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
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Environment Variables and Locale Categories

There is a set of environment variables that control the ............

..............


These names are defined in the locale.h header file. 

Each category corresponds to a set of databases that contain the relevant information for each
defined locale. The location of the system's database is given by the following path: 

    /usr/lib/locale/L/C
    
where `L` and `C` are the names of locale and category, respectively. For example, the database
for the `LC_CTYPE` category for the `german` locale would be found in: 

    /usr/lib/locale/german/LC_CTYPE

A value of `C` for locale specifies the default environment. 

A value of `""` for locale specifies that the locale should be taken from environment
variables. For each of the above categories (not `LC_ALL`), the first of three environment
variables with a nonempty value is used: `LC_ALL`, the category's name (e.g., `LC_CTYPE`), and
`LANG`.

At program start-up, the equivalent of: 

    setlocale(LC_ALL, "C")

is executed. This has the effect of initializing each category to the locale described by the
locale `C`.

If a pointer to a string is given for locale, `setlocale` attempts to set the locale for the
given category to locale. If `setlocale` succeeds, the description of the new locale is
returned. If `setlocale` fails, a null pointer is returned and the program's locale is not
changed.

A null pointer for locale causes `setlocale` to return the current locale associated with the
category. The program's locale is not changed.


### Security considerations

?????For processes that have gained privilege, locale data files without general read permission
will not be accepted by the localization code. This prevents malicious users from gaining
access to protected system files through internationalized system routines. Files


### 

The variable `` .....

LC_COLLATE
Defines character-collation or string-collation information.

LC_COLLATE
Specifies the locale to use for LC_COLLATE category information. The LC_COLLATE category determines character-collation or string-collation rules governing the behavior of ranges, equivalence classes, and multicharacter collating elements.

LC_COLLATE affects the behavior of functions such as strcoll and strxfrm. LC_MONETARY affects the monetary formatted information returned by localeconv. 


### 

The variable `` .....

LC_CTYPE
Defines character classification, case conversion, and other character properties.

LC_CTYPE
Specifies the locale to use for LC_CTYPE category information. The LC_CTYPE category determines character handling rules governing the interpretation of sequences of bytes of text data characters (that is, single-byte versus multibyte characters), the classification of characters (for example, alpha, digit, and so on), and the behavior of character classes.

LC_CTYPE affects the behavior of the character handling functions (isalpha, tolower, and so on) and the multibyte character functions (such as mbtowc and wctomb). 





### 

?????relevant at all?

The variable `` .....

LC__FASTMSG
Specifies that default messages are used for the C and POSIX locales and that NLSPATH are ignored when LC__FASTMSG is set to true. Otherwise, POSIX compliant message handling will be performed. The default value will be LC__FASTMSG=true in the /etc/environment file.




### 

The variable `` .....

LC_MESSAGES
Defines the format for affirmative and negative responses.

LC_MESSAGES affects the behavior of functions such as gettxt, catopen, catclose, and catgets [see catopen(3C) and catgets(3C)]. LC_ALL names the program's entire locale.

LC_MESSAGES
Specifies the locale to use for LC_MESSAGES category information. The LC_MESSAGES category determines rules governing affirmative and negative responses and the locale (language) for messages and menus.
Application developers who write applications that do not display multibyte characters on a terminal should make sure the LC_MESSAGES value is not set to C@lft. If necessary, disable the setting with the putenv("LC_MESSAGES=") subroutine. The result is output that uses translated message catalogs. C@lft is disabled by login sessions that can display multibyte characters. Processes launched using cron or inittab retain the C@lft LC_MESSAGES value and use the setlocale() subroutine to establish the language environment for default messages.



### 

The variable `` .....

LC_MONETARY
Defines rules and symbols for formatting monetary numeric information.

LC_MONETARY
Specifies the locale to use for LC_MONETARY category information. The LC_MONETARY category determines the rules governing monetary-related formatting.



### 

The variable `` .....

LC_NUMERIC
Defines rules and symbols for formatting nonmonetary numeric information.

LC_NUMERIC
Specifies the locale to use for LC_NUMERIC category information. The LC_NUMERIC category determines the rules governing nonmonetary numeric formatting.

LC_NUMERIC affects the decimal-point and thousands separator characters for the formatted input/output functions and the string conversion functions as well as the non-monetary formatting information returned by localeconv [see localeconv(3C)]. 



### 

The variable `` .....

LC_TIME
Defines a list of rules and symbols for formatting time and date information.

LC_TIME
Specifies the locale to use for LC_TIME category information. The LC_TIME category determines the rules governing date and time formatting.

LC_TIME affects the behavior of functions such as ascftime, cftime, and strftime. 

$TZ
Refers to time zone. It can be in several formats, either specifying the time zone itself or referencing a file (in /usr/share/zoneinfo).


### 

The variable `` .....

$LANG is used to set to the default locale. For example, if the locale values are pt_BR, then the language is set to (Brazilian) Portuguese and Brazilian practice is used where relevant. 


### 

The variable `LC_ALL` .....

Different aspects of localization are controlled by individual $LC_-variables ($LC_CTYPE, $LC_COLLATE, $LC_DATE etc.). $LC_ALL can be used to force the same locale for all aspects.

For category LC_ALL, the behavior is slightly different. If a pointer to a string is given for locale and LC_ALL is given for category, setlocale attempts to set the locale for all the categories to locale. The locale may be a simple locale, consisting of a single locale, or a composite locale. A composite locale is a string returned by a previous call to setlocale with LC_ALL for which the locale did not consist of identical category values. If setlocale fails to set the locale for any category, a null pointer is returned and the program's locale for all categories is not changed. Otherwise, a description of the new locale is returned.




### 

The variable `` .....

LANGSPEC


### 

The variable `` .....

LC_PAPER=ro_RO.UTF-8


### 

The variable `` .....

LC_NAME=ro_RO.UTF-8


### 

The variable `` .....

LC_ADDRESS=ro_RO.UTF-8


### 

The variable `` .....

LC_TELEPHONE=ro_RO.UTF-8


### 

The variable `` .....

LC_MEASUREMENT=ro_RO.UTF-8


### 

The variable `` .....

LC_IDENTIFICATION=ro_RO.UTF-8





### Priorities

Environment variables in the high priority class are:
LC_ALL
LC_COLLATE
LC_CTYPE

Environment variables in the medium priority class are:
LC_MESSAGES
LC_MONETARY
LC_NUMERIC
LC_TIME

The environment variable in the low priority class is:
LANG

LANGSPEC ?????
























-----------------------------------------------------------------------------------------------
## Localisation Databases

????? Relevant?

/usr/lib/locale/C/LC_CTYPE
LC_CTYPE database for the C locale

/usr/lib/locale/C/LC_NUMERIC
LC_NUMERIC database for the C locale

/usr/lib/locale/C/LC_TIME
LC_TIME database for the C locale

/usr/lib/locale/C/LC_COLLATE
LC_COLLATE database for the C locale

/usr/lib/locale/C/LC_MESSAGES
LC_MESSAGES database for the C locale

/usr/lib/locale/C/LC_MONETARY
LC_MONETARY database for the C locale

/usr/lib/locale/locale/category
files containing the locale-specific information for each locale and category






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
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## String Translation {#phrasier}

A simple command-line tool named __Phrasier__ is supplied with ECLAT, and can be used to 
facilitate multi-lingual translation of phrases in Ada or C programs.

The tool works by reading one or more files, called _Phrasier files_, containing phrase 
translation information, and generating a set of Ada source text files which can be compiled 
into a library to enable Ada programs to perform the translations. 

.....


### Phrasier Files

A Phrasier file is a plain text file, encoded in UTF-8, with a very simple syntax. 

The file may start with (but does not require) a Byte Order Mark. 

The file comprises a sequence of _clauses_. Each clause comprises:

 1. a _keyword_;
 
 2. zero or more whitespace characters, which are ignored;
 
 3. a _delimiter_;
 
 4. a _value_, which is any sequence of zero or more characters; 
 
 5. the repetition of the same delimiter as in 3. 

Zero or more whitespace characters may precede or follow a clause, and are ignored. 

A keyword is one of the following words:

 * `phrasier`

 * `prototype`

 * `type`

 * `language`

 * `country`

 * `phrase`

 * `is`

Upper and lower case letters are not distinguished. Using any other keyword is an error. 

A delimiter is a sequence of between one and five consecutive repetitions of a single 
character, which may be any glyph character which is not a whitespace character. 

A _prototype stanza_ is a `prototype` clause followed by a `type` clause. 

A _phrase stanza_ is a `phrase` clause followed by an `is` clause.

A _language section_ is the sequence of a `language` clause followed by zero or more phrase 
stanzas.


### Phrasier Clause

A Phrasier file must start with a `phrasier` clause, whose value is the Phrasier version 
number. Currently, this must be: 

    1.0

Then there is a sequence of zero or more prototype stanzas.

Then there is a sequence of zero or more language sections. 

.....


### Prototype Stanza

.....


### Language Section

.....




### Phrase Stanza

.....


### Recommendations

.....





### Processing

.....



Phrasier outputs a 







### Example

.....

```
phrasier "1.0"

prototype "Amount" 
    Ada type "Foo.Bar.Money" 
    C type "money_t"

prototype "Deadline" 
    Ada type "Ada.Calendar.Date" 
    C type "date_t"

grouping "Invoicing" of "Finance"
language "eng"
country "GB"

phrase "You Must Pay" 
    is "You must pay ${Amount} by ${Deadline}"
```




......



```ada
package Ada.Locales.Phrases
is
   package Finance
   is
      package Invoicing
      is
      
         function You_Must_Pay (Amount: in Money; Deadline: in Date) return Wide_String;
         
      end Invoicing;
   end Finance;
end AdaOS.Localization.Phrases;
```

......


```c
#include <?????>

function wchar* PHRASE_you_must_pay(money_t amount, date_t deadline);
```







-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## References

[1]: <http://www.unicode.org/reports/tr35/> "Unicode Locale Data Markup Language (LDML)"

[2]: <https://tools.ietf.org/html/rfc5646> "Tags for Identifying Languages"







===============================================================================================

setlocale -- modify and query a program's locale
Synopsis
   #include <locale.h>
   

   char *setlocale(int category, const char *locale);




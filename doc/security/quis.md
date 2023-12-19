-----------------------------------------------------------------------------------------------
# Quis 

__Quis__ is a [stock service](../services/services.md#stock) that enables users to authenticate
themselves, and manages _user sessions_ for authenticated users. 

The process of a user authenticating themselves (proving their identity) to AdaOS is termed
_logging in_. In fact, if multiple password entry if used, the same process also (to some
extent) proves the authenticity of the computer to the user. 

The converse operation of dissociating a logged-in user with the computer is termed _logging
out_. 

Note that the terms 'log on', 'sign in', and 'sign on' are widely used more-or-less
interchangeably with 'log in'. Similarly, the terms 'log off', 'sign out', and 'sign off' are
used instead of 'log out'. Sometimes a distinction between these terms is asserted, but there
is no general agreement on any such differences. 

AdaOS terminology is to use 'log in' and 'log out', but no distinction is made with the other
terms. 

The Quis service is only relevant to the AdaOS Native [platform](../pxcr/realizor.md#targ),
since it is assumed that host platforms will perform their own authentication and user session
management. 



-----------------------------------------------------------------------------------------------
## Introduction

This subsection will attempt to explain the need for user authentication and user sessions, but
cannot be a substitute for a full treatise on the subject. 

For a system that will have more than one user, .....

Otherwise, a way must be provided to ensure---to some degree of security enforcement that has
to be defined by the owner of the computer system or systems involved---that one user cannot
interfere with another user, except in ways that are legitimate. 

.....

When an AdaOS Native computer system [boots](../pxcr/booting.md), the 
[top compartment](../adaos/compart.md#top) is created in order to 
[run](../adaos/programs.md#run) the boot image's [main program](?????). 

The top compartment's [primary ambit](security.md#amb) is the [top
authority](security.md#auth). Since the top authority is at the apex of the authority
[hierarchy](../intro/hierarchy.md), this means that the ambit of the top compartment includes
all authorities of the entire [effective system](../intro/intro.md#effys). 

.....

..... [user console](../native/consoles.md) ..... 

..... Quis constrols a _user session_, but the console controls a _console session_ .....

.....


-----------------------------------------------------------------------------------------------
## Quis Configuration {#conf}



The Quis [saved state](../config/config.md) is kept in a file named `quis.dat` in the directory `/quis`
and .....



The Quis state update file is used 






It will be very unusal, but there may sometimes be situations where the system has multiple
consoles and it is required for different Quis _login configurations_ to apply to different
groups of them. 

Multiple login configurations can also be used to quickly change the way login works on all or
a group of consoles. 

There is always a _default login configuration_, named `default`. Other login configurations
must have a different name. 

.....



The files for each different Quis login configuration's should be located within its 
_login configuration directory_. 

The path for a login configuration named `C` is `/quis/login/C`. 

.....




The Quis _login script_ for a login configuration .....

The script must be located in the login configuration directory and be named
`C,quis-config.allegra` for a configuration named `C`. 

.....



-----------------------------------------------------------------------------------------------
## Sessions and Logging In and Out {#sess}

A _session_ is a period of time (which is supposed to be continuous) of a specific 
authenticated user (a real human being) using a specific [console](#con). 

To enable a [console](../native/consoles.md) to have users logging in and out, it must be
configured to be controlled by a Quis [service configuration](#conf). As such, the service
associated with the configuration must have [permission](/security.md#perm) to access the
console, and---crucially---all the inferior users of the console's owner (some or all of whom
will be logging into the console) must *not* have any permissions to access the console. 

When a user successfully authenticates on a particular console, Quis creates a new system
object that represents the corresponding user session and
[console session](../native/consoles.md#sess). The user's programs use that console session (as
if they were interacting with a normal console session). 

The user is given permission, by Quis, to use the console session, and that is how the
authenticated user interacts with the computer. 



?????

The user first _logs in_ (to authenticate), on a particular console, then a session begins for
the user to interact with the computer on that console (only), and then the user _logs out_,
whereupon the session ends. 

.....


A user is not allowed to interact on a console unless they have a [session](#sess) on that
console, other than to _log in_ (authenticate). 

When, and only when, a user has authenticated, Quis starts (creates) a new session, enabling
the user to interact with the computer. 




When the user logs out, Quis ends (deletes) the session, at which point a user must log in
again to start a new session. 

The actual console interaction (for logging in or out, or any other kind of interaction) with
the user is not done by Quis. Instead, it is expected that some other program, for example
[MVTC](?????), will do this. 




Whilst the user is 








-----------------------------------------------------------------------------------------------
## User Profiles and Names {#prof}

Each [Quis service configuration](#conf) is configured with a set of _login profiles_. 

Each login profile contains: 

* a _login name_; 

* a link to a user. 



.....







-----------------------------------------------------------------------------------------------
## Passwords {#pw}

Each user of the system is associated with a set of _passwords_. 

If the user has no passwords set, the user is _disabled_, meaning that they cannot log in. 

One of the user's passwords must be designated the user's _primary password_. Any other
passwords are the _secondary passwords_ of the user. 

.....






### Advice

Regarding computer passwords, there are a number of conventions that have become prevalent
recently that are, upon careful analysis, detrimental to the cause of good security. 

We have therefore undone many of these conventions. When passwords are chosen according to our
guidance, this will have the effect of decreasing the likelihood of a breach of security at an
organisation. 

Please always adhere strictly to the following advice. It all comes from decades of front-line
experience in the matter. Some is echoed by the [NCSC][1]. 

 1. Do not enforce regular password changes! Passwords really only need to be changed when you
    suspect a compromise of the login credentials. If a user must regularly change a password,
    they are much more likely to write it down or choose weak replacement passwords. 

 2. Do not prevent users from choosing a replacement password that matches a previously chosen
    password. It is better for a user to choose a password they can remember, even if they have
    used it before, than to be enticed into choosing weak passwords because they cannot re-use
    previous passwords. Give guidance to the user warning them that it is better not to re-use
    passwords. 

 3. Do not disallow any character (other than control characters and the code points FFFE and
    FFFF, which Quis does not allow anyway). Do not require (a minimum number of) any class of
    character, and do not impose any limitations on what the user can enter. Such restrictions
    tend to entice the user into choosing weak passwords, or writing them down. 

The overarching principle is to help the user to choose memorable and strong (unguessable)
passwords, but not to restrict the passwords the user can choose. 


### Quis Password Policies

Quis adheres to the following principles regarding passwords. 

When entering a password: 

 * Leading and trailing spaces and leading and trailing non-letter characters are removed.

 * Control characters and the code points FFFE and FFFF (hexadecimal) are not allowed.

 * The password is normalised according to Unicode normalisation algorithm C before being
   stored. 

 * The password is concealed (e.g. by showing asterisks instead of the actual characters typed)
   by default, but the user is able to turn the concealment off (and on again). A message warns
   the user not to allow their passwords to be seen by anyone. 

When comparing any two passwords for equality:

 * Upper case and lower case letters are not distinguished. Additionally, certain other
   characters are not distinguished (see table below). 

 * Any sequence of space and non-letter characters are treated as if they were one 'separator'
   character. This notional separator character is considered different to all letter
   characters, but otherwise any one of these separator characters is considered equal to any
   other. 

In each row of the table below, the characters listed are all considered to be equal to one
another. 

| `0` `O` `o            | Letter 'o' and numeral zero                 |
| `1` `I` `i` `L` `l`   | Letter 'i', letter 'L', and numeral one     |
| `$` `5` `S` `s`       | Letter 's', numeral five, and dollar sign   |

These are termed 'homographs', because they look the same (or similar), .....

When a user is entering a new (or replacement) password, Quis:

 * ensures that the user can readily read the guidelines for inventing a password; 

 * allows pasting of text from the clipboard into the password, as plain text, but with any
   control characters or FFFE or FFFF removed; 

 * allows the user to copy all or part of the password, as plain text, but shows a warning
   message to the user that they must be careful not to compromise a password. 


### Guidelines

.....

 1. Think of something memorable that only you could possibly know about. It might be an
    incident in your past, or an item that is meaningful to you, for example. 

 2. Think of three (or more) normal words that relate to something only you could possibly know
    about. 

 3. They should be words that you will be able to remember, but no-one else in the world should
    be able to guess all of them together. 

 4. Try to put the words in a sequence that you will remember.

 5. The objective is to avoid the necessity for you to ever write the password down. Try to
    ensure that you never allow anyone to discover your password. 

 6. Do not re-use a password that you suspect has become compromised. 

 7. Do not ever divulge a password to anybody, under any circumstances, unless you are sure
    that you have a legal obligation to do so. We are working to convince governments to make
    it a criminal offence under absolutely all circumstances for any person (or legal entity)
    to try to make someone divulge a password. 

 8. If you know or strongly suspect that your password has become known to someone, change it
    (or have it changed) as soon as possible. 


### Example Password

An example of the process of choosing a password might be as follows. 

When I was a child, at the Boys Brigade, I once fell through the corrugated plastic roof of an
outhouse and ripped my trousers; I was on my own, and I never told anyone about this incident,
not even family. 

So, my password will be: 

    roof trousers ripped
    
When entering this password, I could type any of:

    Roof Trousers Ripped
    (roof-trousers-ripped)
    $$$ ROOF $$$ TROUSERS $$$ RIPPED $$$

These would all be considered the same password by Quis. 

I don't need to write this password down, because I only need to think of this incident, and
the password will come to mind. 

Any one of the words could be quickly guessed, using a computer to try all the words in a
standard English dictionary. But all three together would take a very long time to guess
without the aid of a large-memory quantum computer (which don't exist yet, as of 2023). 



-----------------------------------------------------------------------------------------------
## Basic Login Procedure

.....


### Page 1 (Login Name)

The first page that is displayed is for the user to enter their login name. 

This page has three main sections: 

 * At the top, an information section; 

 * In the middle, the entry of the login name; 

 * At the bottom, a welcome message. 

The sections, and the lines within them, are formatted according to the size of the text and
the screen. Spacing is added if it can be, otherwise, wording is reduced to enable it to fit on
the screen. 

.....



### Page 2 (Primary Password)




### Pages 3 onwards

..... [challenge response](#cr)



### Login Name

.....





### Information Section

The information section .....

:

 * The name of the computer system; 

 * The name of the Quis configuration (`Default` for the default configuration); 

 * The name of the console;

 * 

 * The current date and time, accurate to the minute, which is dynamically updated. 

.....






### Welcome Message

The welcome message is a simple piece of plain text that is dynamically modifiable by the owner
of the Quis service, and is a way for the owner to introduce the computer system to people who
intend to log into the computer. 

In particular, the message is likely to be a good way to prove, to some extent, that the
computer system they are about to log into is not a fake or spoof of the real thing. 

It is also often a good way to inform users in advance of anything that may affect their
decision to log in. For example, it can be used to announce that the system has a partial
fault, or is operating in a degraded mode for some reason. 

The message can also contain a brief statement of the terms and conditions that apply to users
who log in.

The message text is generated dynamically by the execution of an
[Allegra](../allegra/allegra.md) function named `console login message`. This function should
be declared in the login script for the [login configuration](#conf). 

This function should return a list of strings. The first string is the most important part of
the message. The second string (if any) is the next most important, and so on. If there is not
enough space on the console's screen to display all of the parts, then Quis will omit parts,
starting from the least important, until the remaining message fits. 

The function will be called once every minute. Use the `nl` function as a line break, or just
use a multi-line string literal, but note that Quis will automatically word-wrap lines in a
part, according to the width of the screen. 

Do not have any spaces at the beginning or end of any line. Lines shorter than the width of the
screen are automatically centred by Quis. Do not have any tabs (HT characters) or any control
characters at all. Do not have multiple adjacent spaces in any line. 

The message should not have any blank lines. If there is enough space on the screen, Quis will
automatically put in blank lines between the parts of the message. 

For example:

```allegra

function 'system health status' is
   use 'system monitor'
   if (system has fault) then
      return "experiencing a problem, which is being worked on urgently"
   else
      return "fully operational"
   end if
end function

function 'console login message' is

   return {
"
Welcome to the ACME computer system.
" "
This computer system is currently (system health status).
" "
The full terms and conditions for the use of this computer system are shown on the notice board in every computer room.
"
   }
end function
```

The message here has three parts. A function such as `system health status` can be used to
generate dynamic content in the message. 

Note that the long line at the end of this message will be word-wrapped if necessary, so it is
not necessary to put any explicit line breaks into it. 

You will notice that there are many other functions and procedures defined in the script, which
contain the logic for carrying out most of the other log-in functionality. These could be
changed or augmented, in order to customise the operation of Quis, but this shoiuld only be
done with great care, since a mistake could easily introduce security vulnerabilities.



-----------------------------------------------------------------------------------------------
## Challenge-Response {#cr}

In addition to the primary password, the Quis plugin named Quis Challenge-Response provides the
option of a _challenge-response_ (or _CR_) system, to strengthen the protection provided by one
password alone. 

The essence of CR is that, after having provided an acceptable password, the user completes the
authentication by answering one or more questions posed by the computer. 

.....










-----------------------------------------------------------------------------------------------
## Multi-Factor Authentication {#mfa}

In addition to a password, the Quis plugin named Quis Multi-Factor Authentication provides the
option of using _multi-factor authentication_ (or _MFA_), to strengthen the protection provided
by just passwords. 

.....


 * Yubikey (FIDO universal 2nd factor authenticators)

 * Smartcards

 * Bespoke devices

 * ?











-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Emergency Log-In {#emer}

Quis can be configured to allow a user to have an _emergency login code_, which they can use as
an alternative to entering their password and performing any other authentication actions (such
as [CR](#cr) or [MFA](#mfa) for example). 

.....

The code can only be used once. When a user uses their emergency login code to log in, it is deleted, 
and a code renewal process is initiated. 





Since an emergency login code can only be issued and managed by a user's superuser, the top
user cannot have an emergency login code. However, the computer can have a [secret back
door](#backdoor) which serves a similar purpose. 




### Code Renewal

.....



 1. A new emergency login code is automatically generated randomly. A secure random number
    generator is used for this. 

 2. The user's superuser is automatically sent a message, advising them that the user has used
    their emergency login code, that a new code has been generated, and what that new code is. 

 3. The superuser must then inform the user (as soon as possible) of their new emergency login
    code. This should be done a secure means, which cannot be intercepted (e.g. seen or
    overheard by any other person). 






-----------------------------------------------------------------------------------------------
## Secret Back Door {#backdoor}

It may seem surprising, but it is advisable to create a _secret back door_ for every computer
under your control (for which you have normal login credentials for the top user). 

Quis provides a secret back door mechanism which allows someone to log in as the top user of a
computer with only a special emergency code. 

.....







-----------------------------------------------------------------------------------------------
## {#}




-----------------------------------------------------------------------------------------------
## Guest User {#guest}

Normally, a _guest user_ is created, which allows a user to log in without providing any password. 




The typical permissions given to the guest user are: 

 * 






It is very likely to be necessary that a computer has a guest account:

 * If a legitimate user becomes unable to log in, the guest account should provide the means
   for the user to communicate with a person who can resolve the situation (for example by
   resetting the user's login password). 

 * Someone who is not a registered user of the computer may nevertheless need to be able to
   instigate a (software) shutdown of the computer. 









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

[1]: <https://www.ncsc.gov.uk> "NCSC (National Cyber Security Centre), UK"




-----------------------------------------------------------------------------------------------
# Consoles

In AdaOS terminology, a _user console_ is a set of devices that together enable one user (a
real human being) to interact with the computer. 

Typically, a console will comprise the following sub-devices: 

 * a keyboard

 * a display screen

 * optionally, mouse or or other pointing device

 * optionally audio input and output (e.g. speakers, headphones, a microphone, a headset with
   microphone, etc.) 

 * optionally, a camera or image capturing device

 * possibly a console-specific printer or hardcopy device

However, it's impossible to sensibly pin down what constitutes a console. Speakers might be
shared between consoles, for example, as might a printer. 


-----------------------------------------------------------------------------------------------
## 

It typical, nowadays, for a computer to have only one console. In a way a 'personal computer'
*is* a console, as well as a computer. In the past it was more typical for one computer to have
multiple consoles, in some cases hundreds or even thousands of consoles. 






-----------------------------------------------------------------------------------------------
## 

AdaOS assumes it is possible for a computer to have multiple consoles. It can be assumed that
each console will have a different user (if it has any user at all) using it, and, at least
sometimes, multiple users will be using the computer simultaneously. 

AdaOS Native is designed to accommodate this situation. If it so happens that a computer has
only one console, and so can only be used by one user at a time, this is still supported by
AdaOS; it is merely a special case of having multiple consoles and multiple simultaneous users. 




-----------------------------------------------------------------------------------------------
## Console Objects {#obj}

On the AdaOS Native platform, a user console is represented by a single
[system object](../objects/objects.md) of a type derived from the limited interface type
`User_Console` (itself derived from `System_Object`). 




-----------------------------------------------------------------------------------------------
## Console Sessions {#sess}


.....


A console object is an object factory, and can create a _console session_ object, a system
object of a type derived from the limited interface type `AdaOS.Consoles.Console_Session`
(itself derived from `System_Object`). 

When created in this way, the console session object is already
[engaged](../security/engaging.md), and remains engaged until it is no longer needed.
Disengaging it will cause the object to be deleted. 

An object that is a console session is also an
[object directory](../objects/containers.md#dir). Each part of the console is a member of the
console session. 


-----------------------------------------------------------------------------------------------
## 

.....

| Name      | Sub-device                                          |
| --------- | --------------------------------------------------- |
| `kbd`     | keyboard                                            |
| `screen`  | display screen                                      |
| `mouse`   | mouse or or other pointing device                   |
| `spkrs`   | speakers                                            |
| `mic`     | microphone                                          |
| `headph`  | headphones                                          |
| `camera`  | camera or image capturing device                    |
| `printer` | console-specific printer or hardcopy device         |






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
## Text Consoles {#text}

A _text console_ is a user console whose `screen` is a rectangular
[text display](https://en.wikipedia.org/wiki/Text_display) of some kind. 

.....








-----------------------------------------------------------------------------------------------
## MVTC {#}

The __Multiple Virtual Text Console__ program, or _MVTC_, is a
[service](../services/services.md) that implements a set of _virtual text consoles_ on one
actual [text console](#text). 

These virtual text consoles are implemented in such as way as to only have one virtual console
at a time operational---displaying text and receiving keyboard events---on the real console. 

MVTC adds its own virtual console, the _management page_, which enables the user to manage the
other virtual consoles. 

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
## {#}





-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## {#}





-----------------------------------------------------------------------------------------------
## {#}







-----------------------------------------------------------------------------------------------
# Text Services







-----------------------------------------------------------------------------------------------
## Text Serialisers {#ser}

When reading a text [file](../rts/files.md), there is a need to deserialise the binary data
stream (in Ada, of type `Root_Stream_Type'Class`) into a sequence of characters (in Ada, each
of type `Wide_Wide_Character`). 

Similarly, when writing into a text file, there is a need to serialise the sequence of
characters into a binary data stream. 

A _text serialisation service_ is a service which provides text serialisation and
deserialisation. 

A text serialiser, when active, is an [object factory](../objects/objects.md#fact) whose
product objects are _text serialisers_. 

A text serialiser pertains to a specific kind of character encoding, and is able to serialise
and deserialise according to that encoding. 

.....




-----------------------------------------------------------------------------------------------
## Textifiers

[Events](../events/events.md), including [logging](../events/logging.md) and
[auditing](../events/auditing.md) events, are sent and stored as binary data. 

When originating in an Ada program, they are serialised using the default Ada object
serialisation, which simply uses whatever the in-memory representation of the object is, by
default. This can be overridden (and will need to be if the object contains something that
cannot be meaningfully serialised in this way, such as access values for example), but the
result is still basically binary, and therefore opaque to any software that doesn't know the
actual type of the object. 

Services that are collectively known as _event textifiers_ provide the means for software to
get these binary objects converted into meaningful (human-readable) text. 

The [Event Utilities](?????) command-line tool provided with ECLAT uses event textualisers in
order to print events on the console for users to examine. 





-----------------------------------------------------------------------------------------------
## 




-----------------------------------------------------------------------------------------------
## Search

A program that wishes to use textifiers has the difficulty of not (generally) knowing which
textifiers are available or relevant. 




By sending (ironically) a _textifier search_ event to the event channel: 

    adaos.textifier.search





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
## 




-----------------------------------------------------------------------------------------------
## 











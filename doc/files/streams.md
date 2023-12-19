-----------------------------------------------------------------------------------------------
# Text Streams

.....







-----------------------------------------------------------------------------------------------
## Serialisers

Each text stream is a binary stream, of type `Ada.Streams.Root_Stream'Class`, and will require
a [text serialiser](../services/text.md#ser) to convert them into (in the case of reading) or
from (in the case of writing) a sequence of `Character`, `Wide_Character`, or
`Wide_Wide_Character`. 

For each predefined text stream, there is an accompanying serialiser, which will be appropriate
to the encoding of the stream, .....

The package `AdaOS.Text` includes the following visible declarations: 

```ada
type Text_Serializer_Access is access all Text_Serializer'Class;

type Text_Stream_Information is
   record
      Stream:     Ada.Streams.Stream_IO.Stream_Access
      Serializer: Text_Serializer_Access;
   end record;
```









-----------------------------------------------------------------------------------------------
## Predefined Text Streams {#predef}

..... _predefined text streams_ .....
, as
follows:

 * output
 * input
 * error
 * printer
 * auxiliary output
 * auxiliary input
 * log message output
 * automation output
 * debugging output
 * system log message output

.....


The package `AdaOS.Text` includes the following visible declarations: 

```ada
type Predefined_Text_Stream is (Text_Output, 
                                Text_Input,
                                Text_Error,
                                Text_Printer,
                                Text_Output_Aux,
                                Text_Input_Aux,
                                Text_Logging,
                                Text_Automation,
                                Text_Debugging,
                                Text_Logging_Sys);

type Predefined_Text_Stream_Array is 
   array (Predefined_Text_Stream) of Text_Stream_Information;
```

.....


-----------------------------------------------------------------------------------------------
## Predefined Output, Input, and Error

The _standard output_ stream is for a program to write its normal text output. 

Similarly, the _standard input_ stream is for a program to read its normal text input. 

The _standard error_ stream is for a program to write text output that reports errors or any
information of abnormalities. 

This does not include debugging, tracing, and diagnostic information which should be written
into the standard debugging output stream (q.v.). 

If the main (or only) device or facility for user interaction is a text console---in 
particular if the 

?????system's RTS is a platform's console 
[variant](../pxcr/targets.md#variant)

---then the standard output, input, and error of the [top 
compartment](../rts/compart.md#top) will be that text console. 

The output and error, when sending characters to a text console, will cause those characters to 
be printed or displayed on the console. Typically some control characters will be interpreted. 
For example, the CR (carriage return) will cause subsequent printing or display to be at the 
beginning of the current line, and the LF (line feed) character will cause subsequent printing 
or display to continue on the next line (thus the combination of CR and LF will start 
printing or display at the beginning of the next line). 

The input, when getting characters from a text console, will receive characters that have been 
typed on the console's keyboard. Typically the characters are also _echoed_ (printed or 
displayed) on the console as they are typed. Typically, special keys (e.g. arrow/cursor keys, 
function keys) will produce small sequences of characters whose values are peculiar to the 
make and model of the console. In some cases, the typed characters are not received until the 
Return/Enter key is pressed (at which point all outstanding characters are received suddenly). 
If this is the case, there may be special functionality available, such as editing the line 
before its characters are received. 


### Predefined Printer

The _standard printer_ stream is for a program to write text output that is to be printed on a 
hard copy device, such as a printer that writes the text onto paper. 

It is possible that this stream may be used to write text into a file that is intended to be
used in a way similar to hardcopy (paper), such as a page description file, an e-publishing
format file. 

.....


### Predefined Auxiliary Output and Input

The _standard auxiliary output_ and _standard auxiliary input_ streams are typically for 
writing text to, and getting text from, a secondary text console. 

.....

These streams will be null (not pointing to any object) if there is no secondary console. If 
there is a secondary console that has no input capability, then the standard auxiliary input 
will be null. 


### Predefined Log Output

The _standard log output_ stream is for a program to write text messages to be logged. 

......


### Predefined Automation

The _standard automation output_ stream is for [automation](automation.md) commands
corresponding to the user interactions with the program to be written out. 

......




### Predefined Debugging Output

The _standard debugging output_ stream is for a program to write debugging, tracing, and
diagnostic information, unless this can be considered to be the normal output of the program
(e.g. because the program's main purpose is to output such information). 

If the standard debugging output stream is null, then the same information should be omitted
(not written anywhere). 

......




### Predefined System Log Output

The _standard system log output_ stream is for a program to write text messages to be logged. 

However, unlike messages sent to the standard log output, messages should be written into the
standard system log output stream if they pertain primarily to the execution of the system
outside of any programs. This generally means messages that are to do with [system start-up and shutdown](startshut.md). 



......










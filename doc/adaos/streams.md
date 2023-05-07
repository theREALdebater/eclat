-----------------------------------------------------------------------------------------------
# Text Streams

.....

The package `AdaOS.Text` includes the following visible declarations:

```ada
type Text_Stream is abstract tagged limited private
with
   Preelaborable_Initialization;

type Stream_Mode is (In_File, Out_File);

function Mode (Stream: in Text_Stream) return Stream_Mode is abstract;


procedure Read (Stream: in out Text_Stream;
                Item:   out    Wide_Wide_String;
                Last:   out    Natural) is abstract;

procedure Write (Stream: in out Text_Stream;
                 Item:   in     Wide_Wide_String) is abstract;






type Text_Stream_Access is access all Text_Stream'Class;

```








-----------------------------------------------------------------------------------------------
## Predefined Text Streams

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
   array (Predefined_Text_Stream) of Text_Stream_Access; 
```

.....


### Predefined Output, Input, and Error

The _standard output_ stream is for a program to write its normal text output. 

Similarly, the _standard input_ stream is for a program to read its normal text input. 

The _standard error_ stream is for a program to write text output that reports errors or 
any information of abnormalities. 

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


### Predefined Automation Output

......




### Predefined Debugging Output

......




### Predefined System Log Output

......










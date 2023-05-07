package AdaOS.Text
with
   Remote_Types
is
   type Character_Reader is abstract limited private;
   
   type Reader_Access is access all Character_Reader'Class;
   
--| The type `Character_Reader` represents a source of characters that can be read one at a 
--| time. There may or may not be an end to the sequence of characters. 

   function End_Of_File (Reader: in Character_Reader) return Boolean is abstract;

--| The function `End_Of_File` returns `True` if there are no more characters to be read from 
--| the given `Reader`. 

   function Get (Reader: in Character_Reader) return Character;
   function Get (Reader: in Character_Reader) return Wide_Character;
   function Get (Reader: in Character_Reader) return Wide_Wide_Character;

---| 

   type Character_Writer is abstract limited private;
   
   type Writer_Access is access all Character_Writer'Class;
   
--| The type `Character_Writer` represents a sink of characters that can be written one at a 
--| time. It does not provide a meachanism to indicate the end of the sequence of characters. 

   procedure Put (Writer: in out Character_Writer; Value: in Character);
   procedure Put (Writer: in out Character_Writer; Value: in Wide_Character);
   procedure Put (Writer: in out Character_Writer; Value: in Wide_Wide_Character);

---| 



   type Text_File is limited new System_Object with private;
   
   type Text_Access is access all Text_File'Class;
   
--| The type `Text_File` represents a store of a sequence of characters, of any length, as a 
--| system object. 

   function Reader (Text: not null access Text_File) return Reader_Access;

--| The function `Reader` returns (an access value designating a) character reader. The 
--| characters of the text file can be read, starting at the first character in the file. When 
--| the last character has been read, the function `End_Of_File` will return `True`. The 
--| reader can only be used for as long as the text file remains engaged. As soon the the file 
--| is diengaged, attempting to call any operation of the reader propagates the exception `?????`

   function Writer ..... ;

--| The function `` .....

   function To_String (Text: in Text_Object) return String;
   function To_Wide_String (Text: in Text_Object) return Wide_String;
   function To_Wide_Wide_String (Text: in Text_Object) return Wide_Wide_String;

--| The function `` .....





   function To_File (Value: in String) return Text_Access;
   function To_File (Value: in Wide_String) return Text_Access;
   function To_File (Value: in Wide_Wide_String) return Text_Access;
   
--| The function `` .....

private
   type Text_File is abstract limited new System_Object with null record;

end;


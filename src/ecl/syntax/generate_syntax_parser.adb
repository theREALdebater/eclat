with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Hashed_Maps;

use Ada.Strings;
use Ada.Strings.Unbounded;
use Ada.Text_IO.Unbounded_IO;
use Ada.Containers;




procedure Generate_Syntax_Parser 


is
   
   package Word_Sets is 
      new Indefinite_Hashed_Sets( String, Ada.Strings.Hash, "=", "=" );
   
   package Word_Maps is 
      new Indefinite_Hashed_Maps( String, String, "<", "=" );
   
   Reserved_Words: Word_Sets.Set;
   Delimiters: Word_Maps.Map; -- e.g. "&" -> "'&'" and "<>" -> "box"

   procedure Load_Reserved_Words
   is
      F: File_Type;
      W: Unbounded_String;
   begin
      Open( F, "ada2012-rw.txt", In_File );
      while not End_Of_File(F) loop
         Get_Line( F, W );
         Trim( W, Side => Both );
         Reserved_Words.Include( To_String(W) );
      end loop;
      Close( F );
   end;
      
   procedure Load_Delimiters
   is
      F: File_Type;
      Text: Unbounded_String;
      Raw: Unbounded_String;
      Name: Unbounded_String;
   begin
      Open( F, "ada2012-delim.txt", In_File );
      while not End_Of_File(F) loop
         Get_Line( F, Text );
         Trim( Text, Side => Both );
         P := Index( Text, " " );
         if P = 0 then
            Raw := Text;
            Name := "'" & Text & "'";
         else
            Raw := Unbounded_Slice( Text, 1, P-1 );
            Name := Unbounded_Slice( Text, P+1, Length(Text) );
         end if;
         Trim( Raw, Side => Both );
         Trim( Name, Side => Both );
         Delimiters.Insert( To_String(Raw), To_String(Name) );
      end loop;
      Close( F );
   end;
   
   procedure Recognize_Reserved_Word (Word: in Unbounded_String;
                                      Recognized: out Boolean;
                                      Name: out Unbounded_String)
   is
      N: Natural := Word.Length;
   begin
      if N > 2 and then Element( Word, 1 ) = '"' and Element( Word, N ) = '"' then
        Name := Unbounded_Slice( Word, 2, N-1 );
        Recognized := Reserved_Words.Contains( To_String(Name) );
      else
        Recognized := False;
      end if;
      if not Recognized then
         Name := Word;
      end if;
   end Recognize_Reserved_Word;
   
   procedure Recognize_Delimiter (Word: in Unbounded_String;
                                  Recognized: out Boolean;
                                  Text: out Unbounded_String;
                                  Name: out Unbounded_String)
   is
      N: Natural := Word.Length;
      C: Word_Maps.Cursor;
   begin
      if N > 2 and then Element( Word, 1 ) = '"' and Element( Word, N ) = '"' then
         Text := Unbounded_Slice( Word, 2, N-1 );
         C := Delimiters.Find(Text);
         if C = Word_Maps.No_Element then
            Recognized := False;
         else
            Name := Element( C );
            Recognized := True;
         end if;
      else
         Recognized := False;
      end if;
      if not Recognized then
         Text := Word;
         Name := Word;
      end if;
   end Recognize_Reserved_Word;
      
   procedure Recognize_Optional (Word: in Unbounded_String;
                                 Recognized: out Boolean;
                                 Inner: out Unbounded_String)
   is
      N: Natural := Word.Length;
   begin
      Inner := Word;
      if N > 2 and then Element( Word, 1 ) = '[' and Element( Word, N ) = ']' then
        Inner := Trim( Unbounded_Slice( Word, 2, N-1 ), Side => Both );
        Recognized := True;
      else
        Recognized := False;
      end if;
      if not Recognized then
         Inner := Word;
      end if;
   end Recognize_Reserved_Word;
   
   type Element_Kind is (Syntactic_Element, Reserved_Word, Delimiter, Identifier);
      
   type Sequence_Element is
      record
         Kind: Element_Kind;
         Name: Unbounded_String;
         Is_Optional: Boolean;
      end record;         

   package Element_Lists is 
      new Ada.Containers.Doubly_Linked_Lists( Sequence_Element, "=" );
   
   type Syntactic_Production is abstract tagged with
      record
         Name: Unbounded_String;
      end;
   
   type Production_With_Alternatives is new Syntactic_Production with
      record
         Names: Word_Sets.Set;
      end record;
   
   type Production_With_Sequence is new Syntactic_Production with
      record
         Elements: Element_Lists.List;
      end record;
   
   Identifier_Production: constant Syntactic_Production := Production_With_Sequence;
   
   procedure Initialize_Identifier_Production
   is
   begin
      Identifier_Production.Elements.Append( 
         Syntactic_Element'(Identifier, To_Unbounded_String( "identifier" ), False) 
      );
      Identifier_Production.Name := To_Unbounded_String( "identifier" );
   end;

   type Program_Mode is (Awaiting_Command, 
                         Awaiting_Define_Name, 
                         Awaiting_Define_Kind,
                         Awaiting_One_Of_Element, 
                         Awaiting_Sequence_Element,
                         Awaiting_Equivalent);

   Mode: Program_Mode := Awaiting_Command;
   
   F: File_Type;
   Text: Unbounded_String;
   Defining: Unbounded_String;
   One_Of: Word_Sets.Set;
   Elements: Element_Lists.List;
   Is_Optional, Is_Delimiter, Is_RW: Boolean;
   Inner_1, Delim_Text, Delim_Name: Unbounded_String;
   Kind: Element_Kind;
   
   procedure Load_Productions (Has_Error: out Boolean; 
                               Error_Line: out Natural;
                               Error_Message: out Unbounded_String);
   is
   begin
      Has_Error := False;
      Load_Reserved_Words;
      Load_Delimiters;
      Initialize_Identifier_Production;
      Productions.Insert( "identifier", Identifier_Production );
      Open( F, "ada2012-syntax.txt", In_File );
      while not End_Of_File(F) loop
         Get_Line( F, Text );
         Text.Trim( Side => Both );
         if Text.Length > 0 and then Element( Text, 1 ) /= ';' then
            case Mode is
               when Awaiting_Command =>
                  if Text = "define" then
                     Mode := Awaiting_Define_Name;
                  else
                     Error_Message := To_Unbounded_String( "Expecting comment or 'define'" );
                     Error_Line := Line;
                     Has_Error := True;
                     exit;
                  end if;
               when Awaiting_Define_Name =>
                  Defining := Text;
               when Awaiting_Define_Kind =>
                  if Text = "oneof:" then
                     Mode := Awaiting_One_Of_Element;
                  elsif Text = "seq:" then
                     Mode := Awaiting_Sequence_Element;
                  elsif Text = "as" then
                     Mode := Awaiting_Equivalent;
                  else
                     Error_Message := To_Unbounded_String( "Expecting 'oneof:' or 'seq:' or 'as'" );
                     Error_Line := Line;
                     Has_Error := True;
                     exit;
                  end if;
               when Awaiting_One_Of_Element =>
                  if Text = "end" then
                     Mode := Awaiting_Command;
                  else
                     One_Of.Include( Text );
                  end if;
               when Awaiting_Sequence_Element =>
                  if Text = "end" then
                     Mode := Awaiting_Command;
                  else
                     Recognize_Optional( Text, Is_Optional, Inner_1 );
                     Recognize_Delimiter( Inner_1, Is_Delimiter, Delim_Text, Name );
                     if Is_Delimiter then
                        Kind := Delimiter;
                     else
                        Recognize_Reserved_Word( Inner_1, Is_RW, Name );
                        if Is_RW then 
                           Kind := Reserved_Word; 
                        else
                           Kind := Syntactic_Element;
                           Name := Inner_1;
                        end if;
                     end if;
                     Elements.Append( Sequence_Element'(Kind, Name, Is_Optional) );
                  end if;
               when Awaiting_Equivalent =>
                  Elements.Append( Sequence_Element'(Syntactic_Element, Text, Is_Optional => False) );
                  Mode := Awaiting_Command;
            end case;
         end loop;
         Close( F );
      end Load_Productions;


                  
   Has_Error: Boolean; 
   Error_Line: Natural;
   Error_Message: Unbounded_String;
                  
begin
   Load_Productions( Has_Error, Error_Line, Error_Message );
   if Has_Error then
      Put_Line( Error_Message );
      Put( "Line: " );
      Put( Error_Line );
      New_Line;
   else
      Put_Line( "Loaded okay");
      Check_Non_Terminals( Has_Error, Error_Line, Error_Message );
      if Has_Error then
         




end Generate_Syntax_Parser;


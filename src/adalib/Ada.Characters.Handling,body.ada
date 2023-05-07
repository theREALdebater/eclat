--------------------
--#Module:  Ada.Characters.Handling [package body]
--#Product: TW-ADA-WIN32-00
--#Host:    TW-ADA-WIN32-00
--#Target:  WIN32
--#Created: 1998-03-13
--#Update:  0 [initial version]
--#Author:  NJR
--#Status:  2 [ready]

-- Copyright 1998 of the entire contents of this file is owned by:
-- ThoughtWing Software, 3 Brambledown Road, South Croydon, United Kingdom.
-- All rights reserved.

-- See RM A.3.2.



--------------------
with 

package Ada.Characters.Handling body is

   subtype Lower_Control is
                     Character range Character'Val(0) .. Character'Val(31);

   subtype Lower_Graphic is
                   Character range Character'Val(32) .. Character'Val(126);

   subtype Upper_Control is
                  Character range Character'Val(127) .. Character'Val(159);

   subtype Upper_Graphic is
                  Character range Character'Val(160) .. Character'Val(255);

   function Is_Control (Item: in Character) return Boolean is
   begin
      return Item in Lower_Control or Item in Upper_Control;
   end;

   function Is_Graphic (Item: in Character) return Boolean is
   begin
      return Item in Lower_Graphic or Item in Upper_Graphic;
   end;
   
   function Is_Letter            (Item: in Character) return Boolean;
   function Is_Lower             (Item: in Character) return Boolean;
   function Is_Upper             (Item: in Character) return Boolean;
   function Is_Basic             (Item: in Character) return Boolean;
   function Is_Digit             (Item: in Character) return Boolean;
   function Is_Decimal_Digit     (Item: in Character) return Boolean
                                                          renames Is_Digit;
   function Is_Hexadecimal_Digit (Item: in Character) return Boolean;
   function Is_Alphanumeric      (Item: in Character) return Boolean;
   function Is_Special           (Item: in Character) return Boolean;

   -- Conversion functions for Character and String:

   function To_Lower (Item: in Character) return Character;
   function To_Upper (Item: in Character) return Character;
   function To_Basic (Item: in Character) return Character;

   function To_Lower (Item: in String) return String;
   function To_Upper (Item: in String) return String;
   function To_Basic (Item: in String) return String;

   -- Classifications of and conversions between Character and ISO 646:

   subtype ISO_646 is
                    Character range Character'Val(0) .. Character'Val(127);

   function Is_ISO_646 (Item: in Character) return Boolean;
   function Is_ISO_646 (Item: in String)    return Boolean;

   function To_ISO_646 (Item:       in Character;
                        Substitute: in ISO_646:= ' ') return ISO_646;

   function To_ISO_646 (Item:       in String;
                        Substitute: in ISO_646:= ' ') return String;

   -- Classifications of and conversions between Wide_Character and
   -- Character:

   function Is_Character (Item: in Wide_Character) return Boolean;
   function Is_String    (Item: in Wide_String)    return Boolean;

   function To_Character (Item:       in Wide_Character;
                          Substitute: in Character:= ' ') return Character;

   function To_String    (Item:       in Wide_String;
                          Substitute: in Character:= ' ') return String;

   function To_Wide_Character (Item: in Character) return Wide_Character;
   function To_Wide_String    (Item: in String)    return Wide_String;

end Ada.Characters.Handling;



--------------------
-- End of File.


--------------------
--#Module:  Ada.Strings.Maps [package specification]
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

-- See RM A.4.2.



--------------------
package Ada.Strings.Maps is

   pragma Restricted_Hierarchy(,Standard);

   pragma Preelaborate(Maps);

   ----------
   -- Representation for a set of character values:
   type Character_Set is private;
   
   Null_Set: constant Character_Set;
   
   type Character_Range is
      record
         Low:  Character;
         High: Character;
      end record;
      -- Represents Character range Low..High
      
   type Character_Ranges is array (Positive range <>) of Character_Range;

   function To_Set    (Ranges: in Character_Ranges) return Character_Set;
   function To_Set    (Span:   in Character_Range)  return Character_Set;
   function To_Ranges (Set:    in Character_Set)    return Character_Ranges;

   function "=" (Left, Right: in Character_Set) return Boolean;
   
   function "not" (Right: in Character_Set) return Character_Set;
   
   function "and" (Left, Right: in Character_Set) return Character_Set;
   function "or"  (Left, Right: in Character_Set) return Character_Set;
   function "xor" (Left, Right: in Character_Set) return Character_Set;
   function "–"   (Left, Right: in Character_Set) return Character_Set;

   function Is_In (Element: in Character;
                   Set:     in Character_Set) return Boolean;

   function Is_Subset (Elements: in Character_Set;
                       Set:      in Character_Set) return Boolean;

   function "<=" (Left:  in Character_Set;
                  Right: in Character_Set) return Boolean
                                                         renames Is_Subset;

   ----------
   -- Alternative representation for a set of character values:
   subtype Character_Sequence is String;

   function To_Set (Sequence:  in Character_Sequence) return Character_Set;
   function To_Set (Singleton: in Character)          return Character_Set;

   function To_Sequence (Set: in Character_Set) return Character_Sequence;

   ----------
   -- Representation for a character to character mapping:
   type Character_Mapping is private;

   function Value (Map:     in Character_Mapping;
                   Element: in Character) return Character;

   Identity: constant Character_Mapping;

   function To_Mapping (From, To: in Character_Sequence)
                                                  return Character_Mapping;

  function To_Domain (Map: in Character_Mapping) return Character_Sequence;
  function To_Range  (Map: in Character_Mapping) return Character_Sequence;

   type Character_Mapping_Function is
                    access function (From: in Character) return Character;


private

   type Character_Set is array (Character) of Boolean;
   pragma Pack(Character_Set);

   Null_Set: constant Character_Set := (others => False);

   type Character_Mapping is array (Character) of Character;
   pragma Pack(Character_Mapping);

   Identity: constant Character_Mapping :=
                                (Character'Val(000) => Character'Val(000),
                                 Character'Val(001) => Character'Val(001),
                                 ...
                                 Character'Val(254) => Character'Val(254),
                                 Character'Val(255) => Character'Val(255));

end Ada.Strings.Maps;



--------------------
-- End of File.


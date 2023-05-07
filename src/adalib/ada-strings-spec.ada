--------------------
--#Module:  Ada.Strings [package specification]
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

-- See RM A.4.1. This package specification has no body.



--------------------
package Ada.Strings;

   pragma Restricted_Hierarchy(Strings,Standard);

   pragma Pure(Strings);
   
   Space:      constant Character      := ' ';
	Wide_Space: constant Wide_Character := ' ';
	
   Length_Error, Pattern_Error, Index_Error, Translation_Error: exception;

   type Alignment  is (Left, Right, Center);

	type Truncation is (Left, Right, Error);
	type Membership is (Inside, Outside);
	type Direction  is (Forward, Backward);
	type Trim_End   is (Left, Right, Both);

end Ada.Strings;



--------------------
-- End of File.


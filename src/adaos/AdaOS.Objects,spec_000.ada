-----------------------------------------------------------------------------------------------
--/ AdaOS.Objects package specification:

package AdaOS.Objects is

   pragma Remote_Types;

   type System_Object is synchronized interface;

   type System_Object_Access is access all System_Object;

   --| A system object is ...

   function Object_Description (Object: in System_Object) return Wide_String is abstract;
   function Object_Status      (Object: in System_Object) return Wide_String is abstract;
   function Object_Type        (Object: in System_Object) return Wide_String is abstract;

   --| The Object_Description function returns a textual description of a system object. The
   --| result should assume no other knowledge of the object, and should identify the object as
   --| much as reasonably possible. The result of this function will not normally change over
   --| time.

   --| The Object_Status function returns a textual description of a system object's current
   --| state. The result should be broad and general, and not be too detailed or verbose. The
   --| result of this function will change as the object's state changes.

   --| Both these functions should return text that is suitable for a programmer to use for
   --| debugging and system diagnostic purposes (not for end users or normal system uses).

   --| The Object_Type function returns an identifier that uniquely identifies the purpose and
   --| behaviour of the object. This function is intended to be used by programs as a test
   --| prior to narrowing the object's type (enabling the program to make use of the object).

end;

--\


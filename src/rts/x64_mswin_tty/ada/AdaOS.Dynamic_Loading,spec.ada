with Ada.Calendar;

package AdaOS.Dynamic_Loading is

   type Object_Library is limited private; -- any collection of dynamically loadable objects
   
   function Default_Library return access Object_Library;
   
   type Loadable_Object is limited private; -- a dynamically loadable object
   
   function Name    (Object: in Loadable_Object) return String;
   function Form    (Object: in Loadable_Object) return String;
   function Version (Object: in Loadable_Object) return String;

   function Last_Updated (Object: in Loadable_Object) return Ada.Calendar.Time;
   
   function Is_Loaded (Object: in Loadable_Object) return Boolean;
   
   procedure Load (Object: in out Loadable_Object);
   
   --| The procedure Load loads a dynamically loadable object and initialises it. Does nothing 
   --| if the object
   --| is already loaded (there is no error or exception). Propagates exception 
   --| Loading_Error if object cannot be loaded.
   
   type Object_Set(<>) is private;
   
   function Iterate (Objects: in out Object_Set;
                     Process: not null access 
                        procedure (Object: not null access Loadable_Object));

   -- plus all the usual set operations          
   
   Default_Name_Pattern:    constant String := "*"; -- implementation-specific              
   Default_Version_Pattern: constant String := "*"; -- implementation-specific              
                        
   function Search (Name_Pattern:    in     String         := Default_Name_Pattern;
                    Version_Pattern: in     String         := Default_Version_Pattern;
                    Library:         in out Object_Library := Default_Library)
                                                                          return Object_Set;
                                                                          
   function Search (Objects:         in out Object_Set;
                    Name_Pattern:    in     String := Default_Name_Pattern;
                    Version_Pattern: in     String := Default_Version_Pattern) return Object_Set;
                                                                          
   procedure Load_All (Name_Pattern:    in     String         := Default_Name_Pattern;
                       Version_Pattern: in     String         := Default_Version_Pattern;
                       Library:         in out Object_Library := Default_Library);
   
   procedure Load_All (Objects:         in out Object_Set;
                       Name_Pattern:    in     String := Default_Name_Pattern;
                       Version_Pattern: in     String := Default_Version_Pattern);
   
                                                                          
                                                                          
                                                                          

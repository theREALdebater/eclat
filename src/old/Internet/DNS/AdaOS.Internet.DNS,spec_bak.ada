with Ada.Finalization, Tenet; -- for private part only
with Ada.Strings.Bounded_Strings; use Ada.Strings.Unbounded_Strings;
with AdaOS.Internet.IPv4, AdaOS.Internet.IPv6;


package AdaOS.Internet.DNS is


   ------------------------------------------------------------
   -- Domain names:

   package Name_Strings is
      new Ada.Strings.Bounded_Strings.Generic_Bounded_Strings(63);

   subtype Name_Element is Name_Strings.Bounded_String;

   type Domain_Name is private; -- actually controlled

   type Path_Array is array (Positive range <>) of Name_Element;

   Invalid_Name: exception;

   function To_Domain_Name (Source: in Path_Array) return Domain_Name;

   function To_Array (Source: in Domain_Name) return Path_Array;

   function Is_Null (Source: in Domain_Name) return Boolean;

   procedure Clear (Source: in out Domain_Name);


   ------------------------------------------------------------
   -- Resource Records:

   type Resource_Type is (
      A,     -- a host address
      CNAME, -- identifies the canonical name of an alias
      HINFO, -- identifies the CPU and OS used by a host
      MX,    -- identifies a mail exchange for the domain (RFC-974)
      NS,    -- the authoritative name server for the domain
      PTR,   -- a pointer to another part of the domain name space
      SOA ); -- identifies the start of a zone of authority

   for Resource_Type use (
      A     => ,
      CNAME => ,
      HINFO => ,
      MX    => ,
      NS    => ,
      PTR   => ,
      SOA   => );

   type Resource_Class is range 0 .. 65535;

   for Resource_Class'Size use 16;

   Internet_Class: constant Resource_Class := 0; -- 'IN'
   -- CH (Chaos) not specifically supported

   type Time_To_Live is delta 1.0 range 0.0 .. 4_294_967_295.0; -- seconds

   type Preference_Rating is range 0 .. 65535; -- lower = better

   for Preference_Rating'Size use 16;

   subtype Host_Name is Name_String;

   type Basic_Resource_Record is tagged with
      record
         Owner: Pathname;
         Code:  Resource_Type;
         Class: Resource_Class;
         Life:  Time_to_Live;
      end record;













   type Host_Address_IN_RR is new Resource_Record with
      record
         Host_Address: AdaOS.Internet.IPv4.Node_Address;
      end record;




   type Canonical_Name_IN_RR is new Resource_Record with
      record
         Canonical_Name: Host_Name;
      end record;




   type Mail_Exchange_IN_RR is new Resource_Record with
      record
         Preference:  Preference_Rating;
         Mail_Server: Host_Name;
      end record;





   type Name_Server_Host_IN_RR is new Resource_Record with
      record
         Name_Server: Host_Name;
      end record;





   type Redirection_IN_RR is new Resource_Record with
      record
         New_Server: Domain_Name;
      end record;







   type Zone_of_Authority_IN_RR is new Resource_Record with
      record
         ???;
      end record;

   




private

   type String_Ref is access String;

   package String_Lists is new Tenet.Lists.Unbounded(String_Ref);

   type Domain_Name is new Ada.Finalization.Controlled with
      record
         Path: aliased String_Lists.List;
      end record;

   procedure Adjust   (Object: in out Domain_Name);
   procedure Finalize (Object: in out Domain_Name);

   procedure Write_Domain_Name (
      Stream: access Ada.Streams.Root_Stream_Type;
      Item:   in     Domain_Name );

   procedure Read_Domain_Name (
      Stream: access Ada.Streams.Root_Stream_Type;
      Item:   out    Domain_Name );

   for Domain_Name'Write use Write_Domain_Name;
   for Domain_Name'Read  use Read_Domain_Name;

end AdaOS.Internet.DNS;


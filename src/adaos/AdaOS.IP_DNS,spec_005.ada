with AdaOS.IPv4, AdaOS.IPv6;

package AdaOS.IP_DNS is

   pragma Remote_Types;


   --/ ...

   type Time_To_Live is new Interfaces.Signed_32 range 0..2**32-1;

   --| .......

   --\




   --/ Resource records:

   type Resource_Record_Class is (RRC_IN,  -- Internet
                                  RRC_CH,  -- Chaos
                                  RRC_HS); -- Hesiod

   type Resource_Record_Type is (Zero_Marker,
                                 RRT_A,            -- a host address                              [RFC 1035]
                                 RRT_NS,           -- an authoritative name server                [RFC 1035]
                                 RRT_MD,           -- a mail destination (Obsolete - use MX)      [RFC 1035]
                                 RRT_MF,           -- a mail forwarder (Obsolete - use MX)        [RFC 1035]
                                 RRT_CNAME,        -- the canonical name for an alias             [RFC 1035]
                                 RRT_SOA,          -- marks the start of a zone of authority      [RFC 1035]
                                 RRT_MB,           -- a mailbox domain name (EXPERIMENTAL)        [RFC 1035]
                                 RRT_MG,           -- a mail group member (EXPERIMENTAL)          [RFC 1035]
                                 RRT_MR,           -- a mail rename domain name (EXPERIMENTAL)    [RFC 1035]
                                 RRT_NULL,         -- a null RR (EXPERIMENTAL)                    [RFC 1035]
                                 RRT_WKS,          -- a well known service description            [RFC 1035]
                                 RRT_PTR,          -- a domain name pointer                       [RFC 1035]
                                 RRT_HINFO,        -- host information                            [RFC 1035]
                                 RRT_MINFO,        -- mailbox or mail list information            [RFC 1035]
                                 RRT_MX,           -- mail exchange                               [RFC 1035]
                                 RRT_TXT,          -- text strings                                [RFC 1035]
                                 RRT_RP,           -- for Responsible Person                      [RFC 1183]
                                 RRT_AFSDB,        -- for AFS Data Base location                  [RFC 1183]
                                 RRT_X25,          -- for X.25 PSDN address                       [RFC 1183]
                                 RRT_ISDN,         -- for ISDN address                            [RFC 1183]
                                 RRT_RT,           -- for Route Through                           [RFC 1183]
                                 RRT_NSAP,         -- for NSAP address, NSAP style A record       [RFC 1706]
                                 RRT_NSAP_PTR,     -- for domain name pointer, NSAP style         [RFC 1348]
                                 RRT_SIG,          -- for security signature                      [RFC 4034][RFC 3755][RFC 2535]
                                 RRT_KEY,          -- for security key                            [RFC 4034][RFC 3755][RFC 2535]
                                 RRT_PX,           -- X.400 mail mapping information              [RFC 2163]
                                 RRT_GPOS,         -- Geographical Position                       [RFC 1712]
                                 RRT_AAAA,         -- IP6 Address                                 [RFC 3596]
                                 RRT_LOC,          -- Location Information                        [RFC 1876]
                                 RRT_NXT,          -- Next Domain - OBSOLETE                      [RFC 3755][RFC 2535]
                                 RRT_EID,          -- Endpoint Identifier                         [Patton]
                                 RRT_NIMLOC,       -- Nimrod Locator                              [Patton]
                                 RRT_SRV,          -- Server Selection                            [RFC 2782]
                                 RRT_ATMA,         -- ATM Address                                 [ATMDOC]
                                 RRT_NAPTR,        -- Naming Authority Pointer                    [RFC 2915][RFC 2168]
                                 RRT_KX,           -- Key Exchanger                               [RFC 2230]
                                 RRT_CERT,         -- CERT                                        [RFC 4398]
                                 RRT_A6,           -- A6 (Experimental)                           [RFC 3226][RFC 2874]
                                 RRT_DNAME,        -- DNAME                                       [RFC 2672]
                                 RRT_SINK,         -- SINK                                        [Eastlake]
                                 RRT_OPT,          -- OPT                                         [RFC 2671]
                                 RRT_APL,          -- APL                                         [RFC 3123]
                                 RRT_DS,           -- Delegation Signer                           [RFC 4034][RFC 3658]
                                 RRT_SSHFP,        -- SSH Key Fingerprint                         [RFC 4255]
                                 RRT_IPSECKEY,     -- IPSECKEY                                    [RFC 4025]
                                 RRT_RRSIG,        -- RRSIG                                       [RFC 4034][RFC 3755]
                                 RRT_NSEC,         -- NSEC                                        [RFC 4034][RFC 3755]
                                 RRT_DNSKEY,       -- DNSKEY                                      [RFC 4034][RFC 3755]
                                 RRT_DHCID,        -- DHCID                                       [RFC 4701]
                                 RRT_NSEC3,        -- NSEC3                                       [RFC 5155]
                                 RRT_NSEC3PARAM,   -- NSEC3PARAM                                  [RFC 5155]
                                 RRT_HIP,          -- Host Identity Protocol                      [RFC 5205]
                                 RRT_NINFO,        -- NINFO                                       [Reid]
                                 RRT_RKEY,         -- RKEY                                        [Reid]
                                 RRT_SPF,          --                                             [RFC 4408]
                                 RRT_UINFO,        --                                             [IANA-Reserved]
                                 RRT_UID,          --                                             [IANA-Reserved]
                                 RRT_GID,          --                                             [IANA-Reserved]
                                 RRT_UNSPEC,       --                                             [IANA-Reserved]
                                 RRT_TKEY,         -- Transaction Key                             [RFC 2930]
                                 RRT_TSIG,         -- Transaction Signature                       [RFC 2845]
                                 RRT_IXFR,         -- incremental transfer                        [RFC 1995]
                                 RRT_AXFR,         -- transfer of an entire zone                  [RFC 1035]
                                 RRT_MAILB,        -- mailbox-related RRs (MB, MG or MR)          [RFC 1035]
                                 RRT_MAILA,        -- mail agent RRs (Obsolete - see MX)          [RFC 1035]
                                 All_Records,      -- A request for all records                   [RFC 1035]
                                 RRT_TA,           -- DNSSEC Trust Authorities                    [Weiler]           2005-12-13
                                 RRT_DLV);         -- DNSSEC Lookaside Validation                 [RFC 4431]

   type Resource_Record (???Name_Length: Natural;
                         RRT:         Resource_Record_Type) is
      record
         ???Name:  Wide_String(1..Name_Length); -- fully qualified domain name
         Class: Resource_Record_Class; -- usually IN (Internet)
         TTL:   Time_To_Live;
         case RRT is
            when Zero_Marker | RRT_NULL | ..... =>
               null record;
            when RRT_A =>
               Address_IPv4: IPv4.Node_Address;
            when RRT_AAAA =>
               Address_IPv6: IPv6.Node_Address;
            when RRT_NS | RRT_CNAME | RRT_PTR | RRT_TXT | ...... =>
               Response_Text: Wide_String;
            when
            ..........
         end case;
      end record;



   --\
   -----
   --/ Name server:

   type Name_Server is interface and Objects.System_Object;

   type Name_Server_Access is access Name_Server;

   function Name (Resolver: in Name_Server) return Wide_String is abstract;



   function Resolve (Resolver: in Name_Server_Access;
                     Name:     in Wide_String) return IPv4.Node_Address is abstract;

   function Resolve (Resolver: in Name_Server_Access;
                     Name:     in Wide_String) return IPv6.Node_Address is abstract;

   function Resolve (Resolver: in Name_Server_Access;
                     Name:     in Wide_String) return Wide_String is abstract;

   --| All overloadings of the Resolve function perform a recursive query for the
   given domain name.

   --| The first expects to find an A record (potentially following
   a chain of CNAME records first), returning an IPv4 node address.

   --| The second expects to find a AAAA record (.....)
   returning an IPv6 node address.

   --| The third
   expects to find a PTR or TXT record, returning its payload (as a string).

   --| In all cases,
   if the expectation is not met, or if the given domain name finds no record
   (or if it finds only records that have been repudiated), then Resolution_Error
   is propagated.

   function Lookup (Resolver: in Name_Server_Access;
                    Name:     in Wide_String) return Resource_Record is abstract;

   --| The Lookup function performs a non-recursive query for the given domain name,
   returning whichever resource record the given resolver has for the domain name.
   This function
   propagates Resolution_Error if there is no record (that has not been repudiated).



   --\





   --\




   --/ Exceptions:

   Resolution_Error: exception;



   --\

private

   for Resource_Record_Class use (RRC_IN => 1,
                                  RRC_CH => 3,
                                  RRC_HS => 4);

   for Resource_Record_Class'Size use 16;

   for Resource_Record_Type use (Zero_Marker      => 0,
                                 RRT_A            => 1,
                                 RRT_NS           => 2,
                                 RRT_MD           => 3,
                                 RRT_MF           => 4,
                                 RRT_CNAME        => 5,
                                 RRT_SOA          => 6,
                                 RRT_MB           => 7,
                                 RRT_MG           => 8,
                                 RRT_MR           => 9,
                                 RRT_NULL         => 10,
                                 RRT_WKS          => 11,
                                 RRT_PTR          => 12,
                                 RRT_HINFO        => 13,
                                 RRT_MINFO        => 14,
                                 RRT_MX           => 15,
                                 RRT_TXT          => 16,
                                 RRT_RP           => 17,
                                 RRT_AFSDB        => 18,
                                 RRT_X25          => 19,
                                 RRT_ISDN         => 20,
                                 RRT_RT           => 21,
                                 RRT_NSAP         => 22,
                                 RRT_NSAP_PTR     => 23,
                                 RRT_SIG          => 24,
                                 RRT_KEY          => 25,
                                 RRT_PX           => 26,
                                 RRT_GPOS         => 27,
                                 RRT_AAAA         => 28,
                                 RRT_LOC          => 29,
                                 RRT_NXT          => 30,
                                 RRT_EID          => 31,
                                 RRT_NIMLOC       => 32,
                                 RRT_SRV          => 33,
                                 RRT_ATMA         => 34,
                                 RRT_NAPTR        => 35,
                                 RRT_KX           => 36,
                                 RRT_CERT         => 37,
                                 RRT_A6           => 38,
                                 RRT_DNAME        => 39,
                                 RRT_SINK         => 40,
                                 RRT_OPT          => 41,
                                 RRT_APL          => 42,
                                 RRT_DS           => 43,
                                 RRT_SSHFP        => 44,
                                 RRT_IPSECKEY     => 45,
                                 RRT_RRSIG        => 46,
                                 RRT_NSEC         => 47,
                                 RRT_DNSKEY       => 48,
                                 RRT_DHCID        => 49,
                                 RRT_NSEC3        => 50,
                                 RRT_NSEC3PARAM   => 51,
                                 RRT_HIP          => 55,
                                 RRT_NINFO        => 56,
                                 RRT_RKEY         => 57,
                                 RRT_SPF          => 99,
                                 RRT_UINFO        => 10,
                                 RRT_UID          => 101,
                                 RRT_GID          => 102,
                                 RRT_UNSPEC       => 103,
                                 RRT_TKEY         => 249,
                                 RRT_TSIG         => 250,
                                 RRT_IXFR         => 251,
                                 RRT_AXFR         => 252,
                                 RRT_MAILB        => 253,
                                 RRT_MAILA        => 254,
                                 All_Records      => 255,
                                 RRT_TA           => 32768,
                                 RRT_DLV          => 32769);

   for Resource_Record_Type'Size use 16;












procedure AdaOS.Programs.Hermes is










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














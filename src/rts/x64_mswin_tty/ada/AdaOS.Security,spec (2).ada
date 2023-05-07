---------------------------------------------------------------------------
-- Module:  AdaOS.Security (Ada Library Package)
-- Library: AdaOS Security

-- Copyright (C) 2001 The AdaOS Project
-- For license details, see the end (bottom) of this file.


---------------------------------------------------------------------------
-- AdaOS.Security

--


---------------------------------------------------------------------------
with Tenet.Sets.Unbounded;
with AdaOS.Kernel, AdaOS.Accounting;

use AdaOS.Kernel, AdaOS.Accounting;


package AdaOS.Security is


???   pragma Remote_Call_Interface;


   ------------------------------------------------------------
   -- Authority Type:


   type Authority_ID is private;

   -- An 'authority' represents a set of permissions (a permit to access a specific set of entities). An authority
   -- identifier is a token (32-bit) value which designates an authority.


   Max_Authority_Name_Length: constant := 32;

   function Name (Authority: in Authority_ID) return Wide_String;

   -- Each authority has a (text) name, which this function returns. There are no syntactical restrictions on these
   -- names, but a name must have a length no greater than Max_Authority_Name_Length.


   ------------------------------------------------------------
   -- Current authority:

   -- Each Ada task has one 'current authority' as a part of its state, which can change from time to time. The
   -- current authority of a task is implicitly cited with every remote call that it makes (which is routed through
   -- the Partition Communication Subsystem). When a remote call invokes a subprogram (whose body is in a remote
   -- call interface package, the current authority of the task which executes the subprogram is set to the cited
   -- authority, just before the subprogram is called (and the prior current authority is restored just after it
   -- completes).


   function Current_Authority return Authority_ID;

   -- The Current_Authority function returns the current authority of calling task.


   procedure Set_Current_Authority (New_Authority: in Authority_ID);

   procedure Set_Current_Authority (New_Authority: in  Authority_ID;
                                    Old_Authority: out Authority_ID);

   -- The Set_Current_Authority procedure sets the current authority of the calling task. If an Old_Authority
   -- parameter is supplied, it is set to the authority identifier of the current authority just before it is
   -- changed to New_Authority.


   ------------------------------------------------------------
   -- Authority sets:


   type Authority_Set is private;


   Null_Set : constant Authority_Set;

   function "="   (Left, Right : in Authority_Set) return Boolean;
   function "not" (Right : in Authority_Set)       return Authority_Set;
	function "and" (Left, Right : in Authority_Set) return Authority_Set;
	function "or"  (Left, Right : in Authority_Set) return Authority_Set;
	function "xor" (Left, Right : in Authority_Set) return Authority_Set;
	function "-"   (Left, Right : in Authority_Set) return Authority_Set;

   function is_in (Element : in Authority_ID;
	                Set     : in Authority_Set) return Boolean;

   function is_Subset (Elements : in Authority_Set;
	                    Set      : in Authority_Set) return Boolean;

   function "<=" (Left  : in Authority_Set;
	               Right : in Authority_Set) return Boolean renames Is_Subset;

   -- Alternative representation for a set of Authority_ID values:

	type Authority_Sequence is array (Positive range <>) of Authority_ID;

   function To_Set (Sequence  : in Authority_Sequence) return Authority_Set;
   function To_Set (Singleton : in Authority_ID)       return Authority_Set;

   function To_Sequence (Set: in Authority_Set) return Authority_Sequence;


   ------------------------------------------------------------
   -- Available authorities:

   -- There is a set of authorities which are deemed 'available' to a process. This set may change from time to
   -- time. A process is not permitted to cite an authority which is not available to it. When a remote call is
   -- made by a process, the call is checked (by a secure mechanism) to ensure that the cited authority is 
   -- available to the process; if it is not, the call fails (without invoking any remote call in the server
   -- process), and Authority_Error is raised.


   function Available_Authorities return Authority_Set;

   -- The Available_Authorities function returns the set of authorities available to the calling process.


   function is_Available_Authority (Authority: in Authority_ID) return Boolean;

   -- The is_Available_Authority function returns True if the given Authority identifier is of an authority which
   -- is available to the calling process.


   ------------------------------------------------------------
   -- Authority management:


   -- A process can 'allocate' a new authority. A new authority is created, and the process becomes the 'owner' of
   -- the authority. The authorities a process owns are always available to it. When an authority is allocated, it
   -- is initially available only to its owner.

   -- A process, called the 'grantor', can 'grant' an authority available to it to another process, called the
   -- 'beneficiary'. An entity called a 'franchise' is created, which is a triple-relationship between the grantor,
   -- the authority, and the beneficiary. An authority is available to a process if there is at least one franchise
   -- of the authority of which it is the beneficiary.

???   -- A process, called the 'donor', can 'donate' an authority it owns to another process, called the 'recipient'.
???   -- The recipient becomes the owner of the authority, and the donor ceases to be its owner. A franchise is
???   -- created for the authority whose grantor is the recipient and whose beneficiary is the donor (this ensures
???   -- that the authority remains available to the donor).

   -- A process can 'revoke' a franchise of which it is the grantor. The franchise is deleted (it ceases to exist).

   -- A process can 'deallocate' an authority it owns. The authority is deleted (it ceases to exist), as are any
   -- of its franchises.


   type Franchise_ID is private; -- token value which designates a franchise


   procedure Create_Authority (Name: in Wide_String; Authority: out Authority_ID);

   -- The Create_Authority procedure allocates a new authority, and sets the Authority parameter to its
   -- identifier. The initial name of the authority is the given Name.


   procedure Rename_Authority (Authority: in Authority_ID; New_Name: in Wide_String);

   -- The Rename_Authority procedure sets the name of the specified Authority to the given New_Name.


???   procedure Donate_Authority (Authority: in  Authority_ID;
???                               Recipient: in  Process_ID;
???                               Franchise: out Franchise_ID);

???   -- The Donate_Authority procedure donates the specified Authority to the specified Recipient, and sets the
???   -- Franchise parameter to the identifier of the created franchise.


   procedure Grant_Franchise (Authority:   in  Authority_ID;
                              Beneficiary: in  Process_ID;
                              Franchise:   out Franchise_ID);

   -- The Grant_Franchise procedure grants the specified Authority to the specified Beneficiary, and sets the
   -- Franchise parameter to the identifier of the created franchise.


   procedure Revoke_Franchise (Franchise: in Franchise_ID);

   -- The Revoke_Franchise procedure revokes the specified Franchise.


   procedure Delete_Authority (Authority: in Authority_ID);

   -- The Delete_Authority procedure deallocates the specified Authority.


   function Am_Owner (Authority: in Authority_ID) return Boolean;

   -- The Am_Owner function returns True if the calling process is the owner of the specified Authority, and False
   -- otherwise.


   ------------------------------------------------------------
   -- Accounting:

   -- Authorities and franchises are accountable entities.

   function Authority_Account return Resource_Account_ID;
   function Franchise_Account return Resource_Account_ID;

   -- The Authority_Account and Franchise_Account functions return the resource account identifiers for the
   -- authorities owned by or franchises created by the calling process.


   ------------------------------------------------------------
   -- Exception:

   Authority_Error: exception; -- permission not granted


   ------------------------------------------------------------
   -- Mechanism:

   -- The checking of ...

   Allocation, deallocation, granting, and revocation of authorities are performed by a protocol that involves
   -- requests being made by processes of their super-processes, and vice versa. For this reason, these operations
   -- will generally be (relatively) expensive of processor time.

   -- The (internal) super-process of each process maintains a set of authorities which are deemed 'available' to
   -- that process. The process is not permitted to cite an authority which is not available to it. When a remote
   -- call is made by a process, the call is routed through its super-process, which checks that the cited
   -- authority is available; if it is not, the call is rejected (and Security_Error is raised).




end;


---------------------------------------------------------------------------
-- Copyright (C) 2001 The AdaOS Project

-- This library unit ("AdaOS.Security") forms one module of the
-- library entitled "AdaOS Security".

-- This library unit is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public License, as
-- extended by the paragraph immediately below, published by the Free
-- Software Foundation; either version 2.1 of the License, or (at your
-- option) any later version.

-- The terms of this license should be interpreted with the meaning of
-- "uses material from a header file that is part of the Library", in
-- addition to the meanings implied by the licence, to include meaning
-- "instantiates a generic unit which is either a module of the Library or
-- is declared within such a module". This extension of meaning also
-- applies to all works based on the Library.

-- This library unit is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
-- General Public License for more details.

-- You should have received a copy of the GNU Lesser General Public License
-- along with this library; if not, please contact the AdaOS Project, or
-- write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
-- Boston, MA  02111-1307  USA.


---------------------------------------------------------------------------
-- The AdaOS Project

-- The AdaOS Project aims to create a fundamentally new operating system,
-- which combines the features of reliability and good software engineering
-- with those of easy installation, configuration, maintenance, and use not
-- typically found in current offerings. Concomitant with these objectives,
-- we intend to write most of AdaOS in the Ada 95 programming language.

-- We hope that AdaOS will eventually become a fully-fledged general-
-- purpose operating system, providing a highly secure distributed
-- transaction-based promiscuous application execution environment.

-- http://www.AdaOS.org
-- enquiries@AdaOS.org


---------------------------------------------------------------------------
-- (End of File)


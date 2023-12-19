


The following configuration Ada source text would be compiled as part of the variant of the 
library which is to form the unit testing program: 

```ada
package ACME.Authentication renames ACME.Authentication_Live with Mocking;
   -- All visible procedures with only in-parameters do nothing unless substituted
   -- All other visible subprograms raise System.Mocking.Failure unless substituted
   -- TODO

for ACME.Authentication.Reset'Tracing use True;
   -- can then interrogate ACME.Authentication.Reset'Trace for record of calls of Reset
   -- it's an array of ACME.Authentication.Reset'Trace_Record
   
for ACME.Authentication.Users'Substituted use True;
for ACME.Authentication.Search_By_Id'Substituted use True;
   -- must be substituted in a scope when called, directly or indirectly, from within the scope
```



Then a unit test such as the following would use the configured mock package: 

```ada
with SATIS;
with ACME.Authentication;

procedure Test_Authentication_Page
is
   Specific_User: User_Record := new User_Record (...);
   
   function Mock_Users return User_Lists.List'Class
   with
      Substitutes => ACME.Authentication.Users
   is
      Result: new User_Lists.List;
   begin
      Result.Append (...);
      ...;
      return Result;
   end;
   
   function Mock_Search_By_Id (Id: in User_Id) return User_Record
   with
      Substitutes => ACME.Authentication.Search_By_Id
   is
   begin
      if Id /= 1 then raise System.Mocking.Failure with "Expected Id = 1"; end if;
      return Specific_User;
   end;



begin


   -- Check that the Reset procedure has been called exactly once:
   SATIS.Ensure (ACME.Authentication.Reset'Trace'Length = 1);
end;
```



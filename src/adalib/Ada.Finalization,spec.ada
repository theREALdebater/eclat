with AdaOS.Instances;

use AdaOS.Instances;

package Ada.Finalization
with
    Pure, Nonblocking => False 
is 
    type Controlled is abstract tagged private;
       with Preelaborable_Initialization;
    
    procedure Initialize (Object: in out Controlled) is null;
    procedure Adjust     (Object: in out Controlled) is null;
    procedure Finalize   (Object: in out Controlled) is null;
    
    type Limited_Controlled is abstract tagged limited private
       with Preelaborable_Initialization;
    
    procedure Initialize (Object: in out Limited_Controlled) is null;
    procedure Finalize   (Object: in out Limited_Controlled) is null;

private
    type Controlled is abstract new Controlled_Object with null record;

    type Limited_Controlled is abstract limited new Controlled_Object with null record;

end Ada.Finalization;


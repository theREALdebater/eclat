


package Interfaces.COM is


   type Interface_ID is new ???.UUID;

   
   
   
   type Object_Instance is limited private;

   type Instance_Pointer is access all Object_Instance
      with Convention => CPP;
   
   

   type IUnknown is interface
      with Convention => CPP,
           COM_IID    => 16#00000000_0000_0000_C000_000000000046#; -- IID_IUnknown
      
   function AddRef  (Object: in out IUnknown) return unsigned_long; -- 32-bit unsigned integer
   function Release (Object: in out IUnknown) return unsigned_long; -- 32-bit unsigned integer
   
   function QueryInterface (Object:  in out IUnknown;
                            IID:     access Interface_ID;
                            Pointer: out    Instance_Pointer) return HRESULT???
   

















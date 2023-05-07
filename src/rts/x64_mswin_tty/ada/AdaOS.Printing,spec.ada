

package AdaOS.Printing is

   type Printout_Job_Specification (Pathname_Length:    Natural range 0..1023;
                                    Description_Length: Natural range 0..1023) is
      record
         Pathname:      Wide_String(1..Pathname_Length);
         Description:   Wide_String(1..Description_Length);
         Formatter:     





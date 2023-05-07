#!adashell
# autoload.sh for the eclat tool
# version 0.0.0.1
# October 2021

# with Allegra, ECLAT; TODO
# use Allegra, ECLAT; TODO

function help
{
  topic=$1
  topic=${topic|main-help}
  dir=$(getcfg help-dir)
  path=$dir/$topic.txt
  if -t $path # can be read as a text file
  then
    less $path
  else
    echo "No help text available at '" $path "'"
  fi
}

function lib
{
  libraries := ECLAT.Libraries;
   if Libraries'Length = 0
   then
      Put_Line ("No libraries");
   else
      for L in Libraries
      loop
         Set_Col (1);
         Put (L.Name);
         Set_Col (15);
         Put (L.);
         Set_Col ();
         Put (L.);
         Set_Col ();
         Put (L.);
         Set_Col ();
         Put (L.);
         Set_Col ();
         Put (L.);
         New_Line;
      end loop;
   end if;
}

procedure Compile_From_Scratch
   with Command => "compile-from-scratch"
is
begin
   ECLAT.Reset; -- deletes library unit <-> source text file index
   ECLAT.Compile; -- so now recompiles all source text files
end;







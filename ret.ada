with Ada.Text_IO;
with Ada.Strings;
with Graph;

procedure ReadInput is
   use Ada.Text_IO;

   Input_Line : String := "";
   End_Of_Input : Boolean := False;

begin

   while not End_Of_Input loop
      -- Read a line of input
      curr_line = Get_Line(Input_Line);

      Ada.Text_IO.Put_Line(curr_line);
      
      end if;
   end loop;
end ReadInput;
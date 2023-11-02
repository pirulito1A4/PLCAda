with Ada.Text_IO;
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
      Get_Line(Input_Line);

      -- Check if the input line indicates the end
      if Input_Line = "exit" then
         End_Of_Input := True;
      else
         -- Process the input line
         Put_Line("You entered: " & Input_Line);
      end if;
   end loop;
end ReadInput;

procedure Communication_Towers is
   use Ada.Text_IO;
   use Ada.Strings;
   package String_Vectors is new Ada.Containers.Vectors (String);
   use String_Vectors;

   -- Define the tower graph
   package Tower_Graph is new Graph (String, String_Vectors.Vector);

   function Parse_Tower_Name (Line : String) return String is
   begin
      -- Extract the tower name from the input line
      return Trim (Element (Element (Line, 1, ":="), 1, "."), Both);
   end Parse_Tower_Name;

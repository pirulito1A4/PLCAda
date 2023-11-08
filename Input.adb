with Ada.Text_IO;         use Ada.Text_IO;

procedure Tower is
   line: String(1 .. 80);
   len: Natural;
begin
   --  Put a String
   loop
       Get_Line(line, len);
       if len > 0 then
          Put_Line(line(1 .. len));
       end if;
       if len = 0 then
           exit;
       end if;
   end loop;
   
   Put_Line("The end");
   
end Tower;

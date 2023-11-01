with Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

package Graph is
   generic
      type Node_Type is private;
      type Edge_Type is private;
   package Node is new Ada.Containers.Doubly_Linked_Lists (Node_Type);
   package Edge is new Ada.Containers.Doubly_Linked_Lists (Edge_Type);

   type Graph_Type is private;

   procedure Add_Node(Graph : in out Graph_Type; New_Node : in Node_Type);
   procedure Add_Edge(Graph : in out Graph_Type; Source, Target : in Node_Type);
   procedure Remove_Edge(Graph : in out Graph_Type; Source, Target : in Node_Type);
   function Has_Path(Graph : in Graph_Type; Source, Target : in Node_Type) return Boolean;
   function Is_Empty(Graph : in Graph_Type) return Boolean;

private
   type Adjacency_List is new Node.Cursor;
   type Adjacency_Lists is new Node.List with private;
   type Graph_Type is record
      Nodes : Adjacency_Lists;
   end record;

   procedure Initialize(Graph : out Graph_Type);
   procedure Clear(Graph : in out Graph_Type);
end Graph;

package body Graph is
   -- Implementation details

   procedure Initialize(Graph : out Graph_Type) is
   begin
      Node.Init (Graph.Nodes);
   end Initialize;

   procedure Clear(Graph : in out Graph_Type) is
   begin
      Node.Clear (Graph.Nodes);
   end Clear;

   procedure Add_Node(Graph : in out Graph_Type; New_Node : in Node_Type) is
   begin
      Node.Append (Graph.Nodes, New_Node);
   end Add_Node;

   procedure Add_Edge(Graph : in out Graph_Type; Source, Target : in Node_Type) is
   begin
      -- Add an edge from the source node to the target node
      declare
         Source_Node : Adjacency_List := Node.Locate (Graph.Nodes, Source);
      begin
         if Node.Has_Element (Source_Node) then
            Edge.Append (Node.Element (Source_Node), Target);
         else
            Node.Append (Source_Node, Target);
         end if;
      end;
   end Add_Edge;

   procedure Remove_Edge(Graph : in out Graph_Type; Source, Target : in Node_Type) is
   begin
      -- Remove the edge from the source node to the target node
      declare
         Source_Node : Adjacency_List := Node.Locate (Graph.Nodes, Source);
      begin
         if Node.Has_Element (Source_Node) then
            Edge.Delete (Node.Element (Source_Node), Target);
         end if;
      end;
   end Remove_Edge;

   function Has_Path(Graph : in Graph_Type; Source, Target : in Node_Type) return Boolean is
      Source_Node : Adjacency_List;
   begin
      Source_Node := Node.Locate (Graph.Nodes, Source);
      return Node.Has_Element (Source_Node) and then Edge.Has_Element (Node.Element (Source_Node), Target);
   end Has_Path;

   function Is_Empty(Graph : in Graph_Type) return Boolean is
   begin
      return Node.Is_Empty (Graph.Nodes);
   end Is_Empty;
end Graph;

with Ada.Text_IO;
with Ada.Strings;
with Graph;

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

   procedure Process_Input is
      Line : String := Get_Line;
   begin
      Tower_Graph.Clear;
      while not End_Of_File loop
         if Line (Line'Last) = '?' then
            -- Query line, check if a path exists
            declare
               Query : String := Trim (Line (1 .. Line'Last - 1), Both);
               Tokens : String_Vectors.Vector;
               Query_Source, Query_Target : String;
            begin
               Create (Tokens, Count => 2);
               Split (Query, ",", Tokens);
               Query_Source := Element (Tokens, 1);
               Query_Target := Element (Tokens, 2);
               if Tower_Graph.Has_Path (Query_Source, Query_Target) then
                  Put_Line ("+ " & Query_Source & " => " & Query_Target);
               else
                  Put_Line ("- " & Query_Source & " => " & Query_Target);
               end if;
            end;
         else
            -- Link or unlink towers
            declare
               Tower_A, Tower_B : String;
            begin
               Tower_A := Parse_Tower_Name (Line);
               Line := Get_Line;
               Tower_B := Parse_Tower_Name (Line);
               if Line (Line'Last) = '#' then
                  -- Remove the link
                  Tower_Graph.Remove_Edge (Tower_A, Tower_B);
               else
                  -- Add the link
                  Tower_Graph.Add_Edge (Tower_A, Tower_B);
               end if;
            end;
         end if;
         -- Read the next line
         Line := Get_Line;
      end loop;
   end Process_Input;

begin
   Process_Input;
end Communication_Towers;

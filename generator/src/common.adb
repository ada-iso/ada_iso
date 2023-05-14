with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Wide_Wide_Text_IO;      use Ada.Wide_Wide_Text_IO;
with Ada.Strings.Wide_Wide_Fixed;use Ada.Strings.Wide_Wide_Fixed;
package body Common is

   function Sanatize (S : Unbounded_Wide_Wide_String) return Unbounded_Wide_Wide_String
   is
      Check  : constant Wide_Wide_String := To_Wide_Wide_String(S);
      Result : Unbounded_Wide_Wide_String := Null_Unbounded_Wide_Wide_String;
   begin
      for Ch of Check loop
         if Ch = '"' then
            Append (Result, """""");
         else
            Append (Result, Ch);
         end if;
      end loop;
      return Result;
   end Sanatize;

   function Get_Next_Field (S : Wide_Wide_String; Next : out Natural)
      return Unbounded_Wide_Wide_String
   is
      Has_Comma : constant Boolean := S (S'First) = '"';
      --  Set it to the start, or start+1 if it has a quote.
      First : constant Natural := (if S'First = S'Last then raise Invalid_File
                              else (if Has_Comma then
                                          S'First + 1 else
                                          S'First));
      --  Set it to the next comma, or the quote if it has a quote.
      Next_Sep : constant Natural := Index (S (First .. S'Last),
                                          (if Has_Comma then
                                                """" else
                                                ","));
      Sep_Found : constant Boolean := Next_Sep > 0;
      --  Actual end of string we're returning.
      Last : constant Natural := (if Sep_Found then
                                    Next_Sep - 1 --  Right before , or "
                              else
                                    S'Last);
   begin

      --  Next is set to the comma/quote +1 (or end)
      Next := (if Sep_Found then
                  (if Has_Comma then -- we need to look for ", or " as eol
                  (if Next_Sep /= S'Last and then
                        (Next_Sep + 2) > S'Last
                  then
                        S'Last --  " is eol
                  else --  Next_Sep check
                        Next_Sep + 2) --  It's probably ",
                  else --  Has_Comma
                  Next_Sep + 1) --  It has no comma
               else --  Sep_Found
                  S'Last); --  No seperator found
      --  Return the result
      return To_Unbounded_Wide_Wide_String (S (First .. Last));
   end Get_Next_Field;

   function Get_Node_String (N : Node) return Unbounded_Wide_Wide_String
   is
      Result : Unbounded_Wide_Wide_String := Null_Unbounded_Wide_Wide_String;
      List   : constant Node_List := Child_Nodes (N);
   begin
      for I in 0 .. Length (List) - 1 loop
         declare
            Current_Node : constant Node := Item (List, I);
         begin
            Append (Result, TWS (Node_Value (Current_Node)));
         end;
      end loop;
      return Result;
   end Get_Node_String;
   function Get_Node_String (N : Node) return Unbounded_String is
   begin
      return To_Unbounded_String (TS (Get_Node_String (N)));
   end Get_Node_String;
end Common;
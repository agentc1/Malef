with Malef;
with Malef.Events;
with Malef.Platform.Events;
with Malef.Platform.Terminal;
with Malef.Platform.Terminal.Input;
with Malef.Platform.Terminal.Output;
with Malef.Window;

procedure Mouse_Demo is
   use Malef;
   use Malef.Events;
   use Malef.Platform.Events;

   procedure Handle (E : Event_Type) is
      use type Mouse_Wheel;
   begin
      if E.Name = Mouse_Event then
         declare
            B  : constant Mouse_Button := E.Button;
            A  : constant Mouse_Action := E.Action;
            Wh : constant Mouse_Wheel  := E.Wheel;
            R  : constant Row_Type     := E.Position.Row;
            C  : constant Col_Type     := E.Position.Col;
            Text : Wide_Wide_String (1 .. 64);
            Len  : Natural := 0;
         begin
            --  Build a simple status line describing the event.
            Text := (others => ' ');
            Len  := 0;

            declare
               procedure Append (S : Wide_Wide_String) is
               begin
                  for Ch of S loop
                     exit when Len = Text'Last;
                     Len := Len + 1;
                     Text (Len) := Ch;
                  end loop;
               end Append;
            begin
               Append ("Button=");
               case B is
                  when Left_Button  => Append ("Left ");
                  when Right_Button => Append ("Right ");
                  when Wheel        => Append ("Wheel ");
               end case;

               Append ("Action=");
               case A is
                  when Move  => Append ("Move ");
                  when Click => Append ("Click ");
                  when Wheel => Append ("Wheel ");
               end case;

               if A = Wheel then
                  Append ("Dir=");
                  if Wh = Up then
                     Append ("Up ");
                  elsif Wh = Down then
                     Append ("Down ");
                  else
                     Append ("Stay ");
                  end if;
               end if;

               Append ("Pos=(");
               Append (Row_Type'Image (R));
               Append (",");
               Append (Col_Type'Image (C));
               Append (")");
            end;

            if Len = 0 then
               return;
            end if;

            --  Draw the status line at the bottom row.
            declare
               Rows : Positive_Row_Count := 0;
               Cols : Positive_Col_Count := 0;
            begin
               Malef.Platform.Terminal.Input.Get_Dimensions (Rows, Cols);
               Malef.Platform.Terminal.Output.Begin_Frame;
               Malef.Platform.Terminal.Output.Put
                 (Position   => (Row => Rows, Col => 1),
                  Item       => Text (1 .. Len),
                  Background => "#000000",
                  Foreground => "#FFFFFF",
                  Style      => Malef.No_Style);
               Malef.Platform.Terminal.Output.End_Frame;
               Malef.Platform.Terminal.Output.Flush;
            end;
         end;
      end if;
   end Handle;

begin
   --  Register a simple handler that reacts to Mouse_Event.
   Malef.Platform.Events.Register (Handle'Access);

   --  Keep the application alive to receive events.
   loop
      delay 1.0;
   end loop;
end Mouse_Demo;


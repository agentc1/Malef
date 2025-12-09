-------------------------------------------------------------------------------
--                                                                           --
--    M A L E F - P L A T F O R M - T E R M I N A L - O U T P U T . A D B    --
--                                                                           --
--                                 M A L E F                                 --
--                                  A N S I                                  --
--                                                                           --
--                              A D A   B O D Y                              --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2021-2024 José Antonio Verde Jiménez  All Rights Reserved  --
-------------------------------------------------------------------------------
-- This file is part of Malef.                                               --
--                                                                           --
-- This program is free software:  you  can redistribute it and/or modify it --
-- under  the terms  of the  GNU  General License  as published by the  Free --
-- Software  Foundation,  either  version 3  of  the  License,  or  (at your --
-- opinion) any later version.                                               --
--                                                                           --
-- This  program  is distributed  in the  hope that  it will be  useful, but --
-- WITHOUT   ANY   WARRANTY;   without   even  the   implied   warranty   of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General --
-- Public License for more details.                                          --
--                                                                           --
-- You should have received  a copy of the  GNU General Public License along --
-- with this program. If not, see <https://www.gnu.org/licenses/>.           --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Environment_Variables;
with Malef.Platform.Generic_Buffer;
with Malef.Platform.Images;
with Malef.Platform.Terminal.Input;

package body Malef.Platform.Terminal.Output is

   -->> State <<--

   Current_Cursor        : Cursor_Type;
   Current_Style         : Style_Type;
   Current_Is_Indexed    : Boolean;
   Current_Background    : RGBA_Type;
   Current_Foreground    : RGBA_Type;
   Current_Background_Id : Palette_Index;
   Current_Foreground_Id : Palette_Index;
   Opened_Frames         : Natural;

   Logical_Cursor       : Cursor_Type := (Row => 1, Col => 1);
   Logical_Cursor_Used  : Boolean := False;

   --  Backing buffer scaffolding (to be completed in follow-up steps).
   --  For now we just define the cell and buffer types so that higher-level
   --  code can start reasoning about an in-memory screen model before we
   --  actually diff it in End_Frame.

   type Cell is record
      Value      : Glyph;
      Background : RGBA_Type;
      Foreground : RGBA_Type;
      Style      : Style_Type;
   end record;

   type Screen_Buffer is array (Row_Type range <>, Col_Type range <>) of Cell;

   type Screen_Buffer_Access is access Screen_Buffer;

   Screen_Current : Screen_Buffer_Access := null;
   Screen_Next    : Screen_Buffer_Access := null;
   Screen_Height  : Positive_Row_Count := 0;
   Screen_Width   : Positive_Col_Count := 0;

   Use_Screen_Diff : constant Boolean := True;

   Debug_Diff : constant Boolean :=
     Ada.Environment_Variables.Exists ("MALEF_DIFF_DEBUG");

   Diff_Log            : Ada.Text_IO.File_Type;
   Diff_Log_Initialized : Boolean := False;

   package Buffer is
      new Platform.Generic_Buffer (
      Capacity => 1024,
      Stream   => Ada.Text_IO.Text_Streams.Stream (
                     Ada.Text_IO.Standard_Output));

   -->> Backing buffer helpers <<--

   procedure Ensure_Screen is
      Rows : Positive_Row_Count;
      Cols : Positive_Col_Count;
   begin
      Malef.Platform.Terminal.Input.Get_Dimensions (Rows, Cols);

      if Rows = 0 or else Cols = 0 then
         Screen_Current := null;
         Screen_Next    := null;
         Screen_Height  := 0;
         Screen_Width   := 0;
         return;
      end if;

      if Screen_Current = null
        or else Screen_Height /= Rows
        or else Screen_Width /= Cols
      then
         declare
            New_Current : constant Screen_Buffer_Access :=
              new Screen_Buffer (1 .. Rows, 1 .. Cols);
            New_Next    : constant Screen_Buffer_Access :=
              new Screen_Buffer (1 .. Rows, 1 .. Cols);
         begin
            Screen_Current := New_Current;
            Screen_Next    := New_Next;
            Screen_Height  := Rows;
            Screen_Width   := Cols;

            for Row in 1 .. Screen_Height loop
               for Col in 1 .. Screen_Width loop
                  Screen_Current (Row, Col) :=
                    (Value      => ' ',
                     Background => (others => 0),
                     Foreground => (others => 0),
                     Style      => [others => False]);
                  Screen_Next (Row, Col) := Screen_Current (Row, Col);
               end loop;
            end loop;
         end;
      end if;
   end Ensure_Screen;

   procedure Log_Diff (Changed, Total : Natural) is
   begin
      if not Debug_Diff then
         return;
      end if;

      if not Diff_Log_Initialized then
         begin
            Ada.Text_IO.Open
              (File => Diff_Log,
               Mode => Ada.Text_IO.Append_File,
               Name => "malef_diff.log");
         exception
            when others =>
               Ada.Text_IO.Create
                 (File => Diff_Log,
                  Mode => Ada.Text_IO.Out_File,
                  Name => "malef_diff.log");
         end;
         Diff_Log_Initialized := True;
      end if;

      Ada.Text_IO.Put_Line
        (Diff_Log,
         "frame changed="
         & Natural'Image (Changed)
         & " total="
         & Natural'Image (Total));
   end Log_Diff;

   procedure Log_Last_Row
     (Next   : Screen_Buffer;
      Height : Positive_Row_Count;
      Width  : Positive_Col_Count) is
   begin
      if not Debug_Diff then
         return;
      end if;

      if Height = 0 or else Width = 0 then
         return;
      end if;

      if not Diff_Log_Initialized then
         begin
            Ada.Text_IO.Open
              (File => Diff_Log,
               Mode => Ada.Text_IO.Append_File,
               Name => "malef_diff.log");
         exception
            when others =>
               Ada.Text_IO.Create
                 (File => Diff_Log,
                  Mode => Ada.Text_IO.Out_File,
                  Name => "malef_diff.log");
         end;
         Diff_Log_Initialized := True;
      end if;

      declare
         use type Col_Type;
         Last_Row : constant Row_Type := Row_Type (Height);
         Line     : String (1 .. Integer (Width));
      begin
         for Col in 1 .. Width loop
            declare
               Cell_Glyph : constant Glyph :=
                 Next (Last_Row, Col_Type (Col)).Value;
               Code  : constant Integer :=
                 Wide_Wide_Character'Pos (Cell_Glyph);
            begin
               if Code >= Character'Pos (Character'First)
                 and then Code <= Character'Pos (Character'Last)
               then
                  Line (Integer (Col)) := Character'Val (Code);
               else
                  Line (Integer (Col)) := '?';
               end if;
            end;
         end loop;

         Ada.Text_IO.Put_Line
           (Diff_Log,
            "last-row=" & Line);
      end;
   end Log_Last_Row;

   -->> Formatting <<--

   procedure Escape is
   begin
      Buffer.Put (ASCII.ESC & '[');
   end Escape;

   procedure Move_To (
      Row : in Row_Type;
      Col : in Col_Type) is
   begin
      if (Row, Col) /= Current_Cursor then
         Current_Cursor := ((Row, Col));
         Escape;           Buffer.Put (Images.Image (Row));
         Buffer.Put (';'); Buffer.Put (Images.Image (Col)); Buffer.Put ('H');
      end if;
   end Move_To;

   function Emit (
      Item : in Style_Type)
      return Boolean
   is
      Images : constant array (Style_Name) of Character := "123456789";
      Any    : Boolean := False;
   begin
      Current_Style := Item;
      Escape;
      for I in Item'Range when Item (I) loop
         if Any then
            Buffer.Put (';');
         end if;
         Buffer.Put (Images (I));
         Any := True;
      end loop;
      return Any;
   end Emit;

   procedure Emit (
      Background : in Palette_Index;
      Foreground : in Palette_Index;
      Semicolon  : in Boolean)
   is
      Bgs : constant array (Palette_Index) of String (1 .. 2) :=
         ["40", "41", "42", "43", "44", "44", "46", "47",
          "00", "01", "02", "03", "04", "04", "06", "07"];
      Fgs : constant array (Palette_Index) of String (1 .. 2) :=
         ["30", "31", "32", "33", "34", "34", "36", "37",
          "90", "91", "92", "93", "94", "94", "96", "97"];
   begin
      Current_Is_Indexed := True;
      Current_Background_Id := Background;
      Current_Foreground_Id := Foreground;
      if Semicolon then
         Buffer.Put (';');
      end if;
      if Background >= 8 then
         Buffer.Put ('1');
      end if;
      Buffer.Put (Bgs (Background)); Buffer.Put (';');
      Buffer.Put (Fgs (Foreground)); Buffer.Put ('m');
   end Emit;

   function To_Alpha (Item, Alpha : in Component_Type)
      return Component_Type is (Component_Type (
      Integer (Item) * Integer (Alpha) / Integer (Component_Type'Last)));

   procedure Emit (
      Background : in RGBA_Type;
      Foreground : in RGBA_Type;
      Semicolon  : in Boolean) is
   begin
      Current_Is_Indexed := False;
      Current_Background := Background;
      Current_Foreground := Foreground;
      if Background (Alpha) /= 0 then
         if Semicolon then
            Buffer.Put (";48;2");
         else
            Buffer.Put ("48;2");
         end if;
         for Item in Red .. Blue loop
            Buffer.Put (";");
            Buffer.Put (Images.Image (To_Alpha (Background (Item),
                                                Background (Alpha))));
         end loop;
      end if;
      if Foreground (Alpha) /= 0 then
         if Foreground (Alpha) /= 0 or else Semicolon then
            Buffer.Put (";38;2");
         else
            Buffer.Put ("38;2");
         end if;
         for Item in Red .. Blue loop
            Buffer.Put (';');
            Buffer.Put (Images.Image (To_Alpha (Foreground (Item),
                                                Foreground (Alpha))));
         end loop;
      end if;
      Buffer.Put ('m');
   end Emit;

   procedure Clear is
   begin
      Buffer.Put (ASCII.ESC & "[0m");
   end Clear;

   procedure Format (
      Background : in Palette_Index;
      Foreground : in Palette_Index;
      Style      : in Style_Type) is
   begin
      if not Current_Is_Indexed
         or else Background /= Current_Background_Id
         or else Foreground /= Current_Foreground_Id
         or else Style /= Current_Style
      then
         Clear;
      end if;
      Emit (Background, Foreground, Emit (Style));
   end Format;

   procedure Format (
      Background : in RGBA_Type;
      Foreground : in RGBA_Type;
      Style      : in Style_Type) is
   begin
      if Current_Is_Indexed
         or else Background /= Current_Background
         or else Foreground /= Current_Foreground
         or else Style /= Current_Style
      then
         Clear;
      end if;
      Emit (Background, Foreground, Emit (Style));
   end Format;

   --<<---------------->>--
   -->> Implementation <<--
   --<<---------------->>--

   procedure Initialize is
   begin
      -- TODO: Call termios
      Buffer.Put (ASCII.ESC & "[?1049h");
      Format (7, 0, [others => False]);
      Opened_Frames := 0;
      -- Keep the hardware cursor visible; Ace/TUI will position it explicitly.
      -- Enable extended mouse reporting (button + motion, SGR coordinates)
      Buffer.Put (ASCII.ESC & "[?1002h");
      Buffer.Put (ASCII.ESC & "[?1006h");
      Ensure_Screen;
      Flush;
   end Initialize;

   procedure Finalize is
   begin
      -- TODO: Call termios
      -- Disable mouse reporting and restore screen/cursor
      Buffer.Put (ASCII.ESC & "[?1006l");
      Buffer.Put (ASCII.ESC & "[?1002l");
      Buffer.Put (ASCII.ESC & "[?25h"   -- Make cursor visible
                & ASCII.ESC & "[?1049l");
      Flush;
   end Finalize;

   procedure Begin_Frame is
   begin
      Opened_Frames := Opened_Frames + 1;
      if Opened_Frames = 1 then
         if Use_Screen_Diff then
            Ensure_Screen;
         end if;
         Buffer.Put (ASCII.ESC & "[?2026h");
      end if;
   end Begin_Frame;

   procedure End_Frame is
   begin
      if Opened_Frames /= 0 then
         Opened_Frames := Opened_Frames - 1;
         if Opened_Frames = 0 then
            if Use_Screen_Diff
              and then Screen_Current /= null
              and then Screen_Next /= null
            then
               declare
                  Current : Screen_Buffer renames Screen_Current.all;
                  Next    : Screen_Buffer renames Screen_Next.all;
                  Changed : Natural := 0;
                  Total   : Natural := 0;
               begin
                  for Row in Current'Range (1) loop
                     if Row = Screen_Height then
                        --  Special-case the bottom row (status line /
                        --  command line): redraw it unconditionally as a
                        --  full-width band so no stale glyphs survive
                        --  incremental diffs or terminal quirks on the
                        --  last row.
                        declare
                           First_Col  : constant Col_Type := 1;
                           First_Cell : constant Cell :=
                             Next (Row, First_Col);
                        begin
                           Move_To (Row, Col_Type (First_Col));
                           Format
                             (First_Cell.Background,
                              First_Cell.Foreground,
                              First_Cell.Style);

                           for Col in 1 .. Screen_Width loop
                              Total := Total + 1;
                              Buffer.Wide_Wide_Put
                                (Next (Row, Col_Type (Col)).Value);
                              Current (Row, Col_Type (Col)) :=
                                Next (Row, Col_Type (Col));
                              Changed := Changed + 1;
                           end loop;
                        end;
                     else
                        for Col in Current'Range (2) loop
                           Total := Total + 1;
                           if Current (Row, Col) /= Next (Row, Col) then
                              declare
                                 Cell_Value : constant Cell :=
                                   Next (Row, Col);
                              begin
                                 Move_To (Row, Col);
                                 Format
                                   (Cell_Value.Background,
                                    Cell_Value.Foreground,
                                    Cell_Value.Style);
                                 Buffer.Wide_Wide_Put (Cell_Value.Value);
                                 Current (Row, Col) := Cell_Value;
                                 Changed := Changed + 1;
                              end;
                           end if;
                        end loop;
                     end if;
                  end loop;

                  Log_Diff (Changed, Total);
                  Log_Last_Row (Next, Screen_Height, Screen_Width);
               end;
            end if;

            if Logical_Cursor_Used then
               Move_To (Logical_Cursor.Row, Logical_Cursor.Col);
               Logical_Cursor_Used := False;
            end if;

            Buffer.Put (ASCII.ESC & "[?2026l");
            Flush;
         end if;
      end if;
   end End_Frame;

   procedure Set_Cursor (
      Position : in Cursor_Type) is
   begin
      if Use_Screen_Diff and then Opened_Frames > 0 then
         Logical_Cursor := Position;
         Logical_Cursor_Used := True;
      else
         Move_To (Position.Row, Position.Col);
      end if;
   end Set_Cursor;

   function Width (
      Item : in Glyph)
      return Col_Type is (
      (case Item is
         when Dbl => 2,
         when Bck => 0,
         when others => 1));

   function Width (
      Item : in Glyph_String)
      return Col_Type is (
      [for Char of Item => Width (Char)]'Reduce ("+", 0));

   procedure Put (
      Position   : in Cursor_Type;
      Item       : in Glyph_String;
      Background : in RGBA_Type;
      Foreground : in RGBA_Type;
      Style      : in Style_Type) is
   begin
      if not (Use_Screen_Diff
              and then Opened_Frames > 0
              and then Screen_Next /= null)
      then
         Move_To (Position.Row, Position.Col);
         Format (Background, Foreground, Style);
         Buffer.Wide_Wide_Put (Item);
         Current_Cursor.Col := @ + Width (Item);
      end if;

      if Screen_Next /= null then
         declare
            Row : constant Row_Type := Position.Row;
            Col : Col_Type := Position.Col;
         begin
            if Row >= 1
              and then Row <= Screen_Height
            then
               for J in Item'Range loop
                  if Col >= 1 and then Col <= Screen_Width then
                     Screen_Next (Row, Col) :=
                       (Value      => Item (J),
                        Background => Background,
                        Foreground => Foreground,
                        Style      => Style);
                  end if;
                  Col := Col + 1;
               end loop;
            end if;
         end;
      end if;
   end Put;

   procedure Put_Indexed (
      Position   : in Cursor_Type;
      Item       : in Glyph_String;
      Background : in Palette_Index;
      Foreground : in Palette_Index;
      Style      : in Style_Type) is
   begin
      if not (Use_Screen_Diff
              and then Opened_Frames > 0
              and then Screen_Next /= null)
      then
         Move_To (Position.Row, Position.Col);
         Format (Background, Foreground, Style);
         Buffer.Wide_Wide_Put (Item);
         Current_Cursor.Col := @ + Width (Item);
      end if;

      if Screen_Next /= null then
         declare
            Row : constant Row_Type := Position.Row;
            Col : Col_Type := Position.Col;
         begin
            if Row >= 1
              and then Row <= Screen_Height
            then
               for J in Item'Range loop
                  if Col >= 1 and then Col <= Screen_Width then
                     Screen_Next (Row, Col) :=
                       (Value      => Item (J),
                        Background => Current_Background,
                        Foreground => Current_Foreground,
                        Style      => Style);
                  end if;
                  Col := Col + 1;
               end loop;
            end if;
         end;
      end if;
   end Put_Indexed;

   procedure Put (
      Position   : in Cursor_Type;
      Item       : in Glyph;
      Background : in RGBA_Type;
      Foreground : in RGBA_Type;
      Style      : in Style_Type) is
   begin
      if not (Use_Screen_Diff
              and then Opened_Frames > 0
              and then Screen_Next /= null)
      then
         Move_To (Position.Row, Position.Col);
         Format (Background, Foreground, Style);
         Buffer.Wide_Wide_Put (Item);
         -- TODO: Use the real character size
         Current_Cursor.Col := @ + Width (Item);
      end if;

      if Screen_Next /= null then
         declare
            Row : constant Row_Type := Position.Row;
            Col : constant Col_Type := Position.Col;
         begin
            if Row >= 1
              and then Row <= Screen_Height
              and then Col >= 1
              and then Col <= Screen_Width
            then
               Screen_Next (Row, Col) :=
                 (Value      => Item,
                  Background => Background,
                  Foreground => Foreground,
                  Style      => Style);
            end if;
         end;
      end if;
   end Put;

   procedure Put_Indexed (
      Position   : in Cursor_Type;
      Item       : in Glyph;
      Background : in Palette_Index;
      Foreground : in Palette_Index;
      Style      : in Style_Type) is
   begin
      if not (Use_Screen_Diff
              and then Opened_Frames > 0
              and then Screen_Next /= null)
      then
         Move_To (Position.Row, Position.Col);
         Format (Background, Foreground, Style);
         Buffer.Wide_Wide_Put (Item);
         Current_Cursor.Col := @ + Width (Item);
      end if;

      if Screen_Next /= null then
         declare
            Row : constant Row_Type := Position.Row;
            Col : constant Col_Type := Position.Col;
         begin
            if Row >= 1
              and then Row <= Screen_Height
              and then Col >= 1
              and then Col <= Screen_Width
            then
               Screen_Next (Row, Col) :=
                 (Value      => Item,
                  Background => Current_Background,
                  Foreground => Current_Foreground,
                  Style      => Style);
            end if;
         end;
      end if;
   end Put_Indexed;

   procedure Flush is
   begin
      Buffer.Flush;
   end Flush;

   procedure Set_Title (
      Item : in Wide_Wide_String) is
   begin
      Buffer.Put (ASCII.ESC & "]0;");
      Buffer.Wide_Wide_Put (Item);
      Buffer.Put (ASCII.BEL);
   end Set_Title;

end Malef.Platform.Terminal.Output;

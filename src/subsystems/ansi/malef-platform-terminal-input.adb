-------------------------------------------------------------------------------
--                                                                           --
--     M A L E F - P L A T F O R M - T E R M I N A L - I N P U T . A D B     --
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

with Ada.Unchecked_Deallocation;
with Interfaces;
with Interfaces.C;
with Malef.Platform.Events;
with Malef.Events;
with Malef.Platform.Generic_Get_Immediate;

package body Malef.Platform.Terminal.Input is

   -- We cannot read from standard input from the main task, since we don't
   -- want the developer to worry about creating a new task. Instead we will
   -- have several tasks that will be running concurrently to get the input.
   --
   -- Note: Initially I wanted to use the Ada.Wide_Wide_Text_IO.Get_Immediate
   --       function. However, there is a bug in the implementation. Usually
   --       when you use the function is called with parameters:
   --
   --          procedure Get_Immediate (
   --             Item      : out Wide_Wide_Character;
   --             Available : out Boolean)
   --
   --       If there is not input available, the `Available' parameter would
   --       be set to False and continue without blocking. However, this does
   --       not happen. In fact if you read the implementation in file
   --       `a-ztextio.adb' you will see the following comment:
   --
   --          «Shouldn't we use getc_immediate_nowait here, like Text_IO???»
   --
   --       The same happens in the `Ada.Wide_Text_IO' package... Also when
   --       using `Ada.Text_IO' if I compile with `-gnatW8' it doesn't read
   --       the UTF-8 characters individually, it tries to convert them to
   --       `Character'. So if I write the japanese A (あ), it doesn't fit on
   --       a `Character' and raises an error. So that's out of the table.
   --
   --       I tried implementing my own function using the C function:
   --       `getc_immediate_nowait'. However there are two problems:
   --
   --       First of all, when you call the function it then sets up the
   --       terminal to avoid echoing (every single time). So when you do a
   --       `delay', the character will echo to the screen. Because you haven't
   --       called the Get_Immediate function yet. That means, you can't use
   --       the `Get_Immediate (Item, Available)' function.
   --
   --       Then, the worst of all was the task termination... I lost a whole
   --       weekend because of that. Basically if you run a different task with
   --       the `Get_Immediate' function such as:
   --
   --          task Input;
   --
   --          task body Input is
   --             Char : Wide_Wide_Character;
   --          begin
   --             loop
   --                Get_Immediate (Char);
   --                Queue.Enqueue (Event_Holders.To_Holder (
   --                   Events.Event_Type'(
   --                      Name => Keyboard_Event,
   --                      Key  => Key_Type (Char)));
   --             end loop;
   --          end Input;
   --
   --       When all the tasks terminate, this one won't terminate. If you try
   --       to abort it, it won't be terminated. Why? Because in the C
   --       implementatino of the `getc_immediate_common' function there is
   --       an infinite loop like:
   --
   --          while (!good) {
   --             nread = read(fd, &c, 1);
   --             if (nread > 0) {
   --                *ch = c;
   --                good = true;
   --             } else {
   --                good = false;
   --             }
   --          }
   --
   --       When you call abort, the `read' function terminates. However, you
   --       haven't read anything son either 0 or -1 is returned. Then `good'
   --       keeps being False and the loop is executed again. Then it gets
   --       blocked on the `read' function. This happens when `nread = -1'.
   --       The C function never finishes, and the task won't get to the
   --       rendez-vous point.

   task type Input_Task is
   end Input_Task;
   -- When the Keyboard is started it will start listening to standard input
   -- for key presses. No other function should use the Get_Immediate
   -- function as it will interfere with the Keyboard.

   type Input_Task_Access is access Input_Task;

   procedure Free is
      new Ada.Unchecked_Deallocation (
      Object => Input_Task,
      Name   => Input_Task_Access);

   Input : Input_Task_Access := null;

   procedure Initialize is
      procedure Driver with
         Import        => True,
         Convention    => C,
         External_Name => "__malef__platform__terminal__input___c_prepare";
   begin
      Driver;
      Input := new Input_Task;
   end Initialize;

   procedure Finalize is
      procedure Driver with
         Import        => True,
         Convention    => C,
         External_Name => "__malef__platform__terminal__input___c_restore";
   begin
      abort Input.all;
      Free (Input);
      Driver;
   end Finalize;

   procedure Get_Byte_Immediate (
      Item : out Interfaces.Unsigned_8) is
      separate;

   procedure Get_Immediate is
      new Malef.Platform.Generic_Get_Immediate (
      Get_Byte_Immediate => Get_Byte_Immediate);

   task body Input_Task is
      use Malef.Events;
      use Malef.Platform.Events;

      Key : Key_Type;

      type Escape_State is
        (Idle,
         Saw_Escape,
         Saw_Escape_Bracket,
         Saw_Escape_Mouse);

      State       : Escape_State := Idle;
      Param_1     : Natural := 0;
      Param_2     : Natural := 0;
      Param_3     : Natural := 0;
      Param_Index : Natural := 0;

      procedure Reset_Escape is
      begin
         State       := Idle;
         Param_1     := 0;
         Param_2     := 0;
         Param_3     := 0;
         Param_Index := 0;
      end Reset_Escape;
   begin
      loop
         Catch_End_Error : declare
         begin
            Get_Immediate (Wide_Wide_Character (Key));

            declare
               Raw_Key : constant Key_Type := Key;
            begin
               --  SGR mouse tracking based on xterm sequences:
               --    ESC [ < Cb ; Cx ; Cy M/m
               case State is
                  when Idle =>
                     if Raw_Key = Key_Type'Val (27) then
                        State       := Saw_Escape;
                        Param_1     := 0;
                        Param_2     := 0;
                        Param_3     := 0;
                        Param_Index := 0;
                     end if;

                  when Saw_Escape =>
                     declare
                        WWC : constant Wide_Wide_Character :=
                          Wide_Wide_Character (Raw_Key);
                     begin
                        if WWC = '[' then
                           State := Saw_Escape_Bracket;
                        else
                           Reset_Escape;
                        end if;
                     end;

                  when Saw_Escape_Bracket =>
                     declare
                        WWC  : constant Wide_Wide_Character :=
                          Wide_Wide_Character (Raw_Key);
                        Code : constant Integer :=
                          Wide_Wide_Character'Pos (WWC);
                        Digit : Integer;
                     begin
                        if WWC = '<' then
                           State       := Saw_Escape_Mouse;
                           Param_1     := 0;
                           Param_2     := 0;
                           Param_3     := 0;
                           Param_Index := 0;
                        elsif Code >= Character'Pos ('0')
                          and then Code <= Character'Pos ('9')
                        then
                           Digit := Code - Character'Pos ('0');

                           if Param_Index = 0 then
                              Param_Index := 1;
                              Param_1     := Natural (Digit);
                           elsif Param_Index = 1 then
                              Param_1 :=
                                Param_1 * 10 + Natural (Digit);
                           elsif Param_Index = 2 then
                              Param_2 :=
                                Param_2 * 10 + Natural (Digit);
                           else
                              Param_3 :=
                                Param_3 * 10 + Natural (Digit);
                           end if;
                        elsif Code = Character'Pos (';') then
                           if Param_Index = 0 then
                              Param_Index := 1;
                           end if;
                           if Param_Index = 1 then
                              Param_Index := 2;
                           else
                              Param_Index := 3;
                           end if;
                        else
                           Reset_Escape;
                        end if;
                     end;

                  when Saw_Escape_Mouse =>
                     declare
                        WWC  : constant Wide_Wide_Character :=
                          Wide_Wide_Character (Raw_Key);
                        Code : constant Integer :=
                          Wide_Wide_Character'Pos (WWC);
                        Digit : Integer;
                     begin
                        if Code >= Character'Pos ('0')
                          and then Code <= Character'Pos ('9')
                        then
                           Digit := Code - Character'Pos ('0');

                           if Param_Index = 0 then
                              Param_Index := 1;
                              Param_1     := Natural (Digit);
                           elsif Param_Index = 1 then
                              Param_1 :=
                                Param_1 * 10 + Natural (Digit);
                           elsif Param_Index = 2 then
                              Param_2 :=
                                Param_2 * 10 + Natural (Digit);
                           else
                              Param_3 :=
                                Param_3 * 10 + Natural (Digit);
                           end if;
                        elsif WWC = ';' then
                           if Param_Index = 0 then
                              Param_Index := 1;
                           elsif Param_Index = 1 then
                              Param_Index := 2;
                           else
                              Param_Index := 3;
                           end if;
                        elsif WWC = 'M' or else WWC = 'm' then
                           if Param_Index >= 3 then
                              declare
                                 B_Code  : constant Natural := Param_1;
                                 X_Pos   : constant Natural := Param_2;
                                 Y_Pos   : constant Natural := Param_3;
                                 Btn_Int : constant Integer :=
                                   Integer (B_Code);
                                 Is_Wheel  : constant Boolean :=
                                   (Btn_Int / 64) mod 2 /= 0;
                                 Is_Motion : constant Boolean :=
                                   (Btn_Int / 32) mod 2 /= 0;
                                 Base      : constant Integer :=
                                   Btn_Int mod 4;
                                 Button     : Mouse_Button := Left_Button;
                                 Action     : Mouse_Action := Click;
                                 Wheel_Dir  : Mouse_Wheel := Stay;
                                 Row       : Row_Type := 1;
                                 Col       : Col_Type := 1;
                              begin
                                 if Y_Pos > 0 then
                                    Row := Row_Type (Integer (Y_Pos));
                                 end if;
                                 if X_Pos > 0 then
                                    Col := Col_Type (Integer (X_Pos));
                                 end if;

                                 if Is_Wheel then
                                    Button := Wheel;
                                    Action := Wheel;
                                    if Base = 0 then
                                       Wheel_Dir := Up;
                                    else
                                       Wheel_Dir := Down;
                                    end if;
                                 elsif Is_Motion then
                                    Action := Move;
                                    case Base is
                                       when 0 =>
                                          Button := Left_Button;
                                       when others =>
                                          Button := Right_Button;
                                    end case;
                                 else
                                    Action := Click;
                                    case Base is
                                       when 0 =>
                                          Button := Left_Button;
                                       when others =>
                                          Button := Right_Button;
                                    end case;
                                 end if;

                                 Queue.Enqueue
                                   (+Event_Type'
                                      (Name     => Mouse_Event,
                                       Time     => From_Start,
                                       Button   => Button,
                                       Action   => Action,
                                       Wheel    => Wheel_Dir,
                                       Position => (Row => Row,
                                                    Col => Col)));
                              end;
                           end if;

                           Reset_Escape;
                        else
                           Reset_Escape;
                        end if;
                     end;
               end case;
            end;

            --  Preserve existing keyboard semantics for consumers such as Ace.
            if Key = Key_Type'Val (0) then
               Key := Key_Unknown;
            elsif Key = Key_Type'Val (27) then
               --  Start of an escape sequence.
               Key := Key_Unknown;
            end if;

            Queue.Enqueue
              (+Event_Type'
                 (Name => Keyboard_Event,
                  Time => From_Start,
                  Key  => Key));
         exception
            when End_Error =>
               Queue.Enqueue
                 (+Event_Type'(Time => From_Start,
                               Name => Input_Closed));
         end Catch_End_Error;
      end loop;
   end Input_Task;

   -->> Get Dimensions <<--

   type winsize is
      record
         ws_row    : Interfaces.C.unsigned_short;
         ws_col    : Interfaces.C.unsigned_short;
         ws_xpixel : Interfaces.C.unsigned_short;
         ws_ypixel : Interfaces.C.unsigned_short;
      end record with
      Convention => C;
   -- This struct is from <sys/ioctl.h> (<bits/ioctl-types.h>)

   function ioctl (
      fd      : in     Interfaces.C.int;
      request : in     Interfaces.C.unsigned_long;
      result  :    out winsize)
      return Interfaces.C.int with
      Import        => True,
      Convention    => C,
      External_Name => "ioctl";

   TIOCGWINSZ : constant Interfaces.C.unsigned_long := 16#5413#;

   procedure Get_Dimensions (
      Rows : out Positive_Row_Count;
      Cols : out Positive_Col_Count)
   is
      ws   : winsize;
      temp : Interfaces.C.int;
   begin
      temp := ioctl (1, TIOCGWINSZ, ws);
      Rows := Row_Type (ws.ws_row);
      Cols := Col_Type (ws.ws_col);
   end Get_Dimensions;

end Malef.Platform.Terminal.Input;

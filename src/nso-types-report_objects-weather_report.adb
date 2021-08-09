With
NSO.Helpers,
Ada.Strings.Fixed,
Ada.Calendar.Formatting,
Ada.Characters.Latin_1,
Ada.Tags,

Gnoga.Types.Colors,
Gnoga.Gui.View,

Ada.Text_IO
;

WITH
Ada.Tags,
Ada.Text_IO;

with
Gnoga.Gui.Base;

Package Body NSO.Types.Report_Objects.Weather_Report is
    Use NSO.Helpers;

    DEBUGGING   : Constant Boolean := False;

    Package Naming is new Name_Binder("Weather", Weather_Report);
    Function Get_Name(Self: Weather_Report) return String
      renames Naming.Get_Name;
    Report_Name : String renames Naming.Report_Name;

    ---------------------------
    -- Characters & Strings  --
    ---------------------------
    Package Latin_1 renames Ada.Characters.Latin_1;

    Left_Bracket   : Character renames Latin_1.Left_Square_Bracket;   -- [
    Right_Bracket  : Character renames Latin_1.Right_Square_Bracket;  -- ]
    Bracket_Divide : Constant String := Right_Bracket & Left_Bracket; -- ][

    New_Line       : Constant String := (Latin_1.CR, Latin_1.LF);


    ---------------------------
    --  DISPLAY REPORT DATA  --
    ---------------------------

    Function Report_Header return String is
        Date_Time      : String renames HTML_Tag("th", "Time / Date");
        Wind_Speed     : String renames HTML_Tag("th", "Wind Speed");
        Temperature    : String renames HTML_Tag("th", "Temperature");
        Seeing         : String renames HTML_Tag("th", "Seeing");
        Conditions     : String renames HTML_Tag("th", "Conditions");
        Wind_Direction : String renames HTML_Tag("th", "Wind Direction");
    Begin
        Return New_Line & HTML_Tag("thead", HTML_Tag("tr",
           Date_Time & Wind_Speed & Wind_Direction &
             Temperature & Seeing & Conditions)
          ) & New_Line;
    End Report_Header;


    Function Report( Cursor : Report_Map.Cursor ) return String is
        Use Report_Map, Ada.Calendar, Ada.Calendar.Formatting;

    Begin
        if not Has_Element(Cursor) then
            Return "";
        else
            Declare
                Function "+"(Input : Sky) return string is
                  ( Sky'Image(Input) );
                Function "+"(Input : Direction) return string is
                  ( Direction'Image(Input) );
                Date           : Time renames Key(Cursor);
                Data           : Report_Data renames Element(Cursor);
                Date_Time      : String renames HTML_Tag("th", Image(Date));
                Wind_Speed     : String renames HTML_Tag("td",+Data.Wind_Speed);
                Temperature    : String renames HTML_Tag("td",+Data.Temperature);
                Seeing         : String renames HTML_Tag("td",+Data.Seeing);
                Conditions     : String renames HTML_Tag("td",+Data.Conditions);
                Wind_Direction : String renames HTML_Tag("td",+Data.Wind_Direction);
            Begin
                return New_Line & HTML_Tag("tr",
                     Date_Time & Wind_Speed & Wind_Direction &
                     Temperature & Seeing & Conditions
                  ) & New_Line;
            End;
        End if;
    End Report;

    Function Report( Object : Report_Map.Map ) return String is
        Use NSO.Helpers;
        Function Report_Data( Cursor : Report_Map.Cursor:= Object.First ) return String is
            Next     : Report_Map.Cursor renames Report_Map.Next(Cursor);
            Has_Next : Boolean renames Report_Map.Has_Element( Next );
        Begin
            Return  Report(Cursor) &
            (if not Has_Next then "" else Report_Data( Next ));
        End Report_Data;

        Caption : Constant String := HTML_Tag("Caption", Report_Name&" Report");
    Begin
        return HTML_Tag("Table", Caption & Report_Header &
             HTML_Tag("tbody", Report_Data) );
    End Report;

    --------------------------
    --  CREATE REPORT DATA  --
    --------------------------

    Function Report(Data : in NSO.JSON.Instance'Class) return Report_Map.Map is
        Use NSO.Types, NSO.JSON, Ada.Strings.Fixed;
        Package Fixed renames Ada.Strings.Fixed;

        Function Filter_Object is new Object_Filter( Report_Name );
        Function Filter Return JSON.Instance'Class is ( Filter_Object(Data) );

        -- The result to return.
        Working : Report_Map.Map;
        Current : Report_Map.Cursor;

        Procedure Process_Records(Name : String; Value : Instance'Class);
        Function  Process_Parameters is new Process_Date_Time(
           Result        => Working,
           Current       => Current,
           Element       => Report_Data,
           Date_Time_Map => Report_Map,
           Default       => Report_Data'(others => <>),
           Report_Name   => Report_Name,
           On_Object     => Process_Records
          );

        Procedure Process_Records(Name : String; Value : Instance'Class) is
            Data  : Report_Data renames Working( Report_Map.Key(Current) );
            Procedure Process_Value(Value  : String) is
                Function "-"(Object: String) return Sky is
                  (Sky'Value(Object));
                Function "-"(Object: String) return Direction is
                  (Direction'Value(Object));
            Begin
                declare
                    Field_Item : Field renames "+"(Name);
                    Field_Pos  : Constant Natural := Field'Pos(Field_Item);
                begin
                    Case Field_Item is
                        When Wind_Speed     => Data.Wind_Speed     := -Value;
                        When Temperature    => Data.Temperature    := -Value;
                        When Seeing         => Data.Seeing         := -Value;
                        When Conditions     => Data.Conditions     := -Value;
                        When Wind_Direction => Data.Wind_Direction := -Value;
                    End Case;
                    Data.Initalized(Field_Pos):= True;
                end;
            End Process_Value;

            Procedure Do_Process is new Apply(On_String  => Process_Value);
        Begin
            Do_Process( Value );
        End Process_Records;

    Begin
        Return Result : Report_Map.Map := Process_Parameters(Data) do
            -- Ensure all fields are initalized.
            for Cursor in reverse Result.Iterate loop
                declare
                    K : Ada.Calendar.Time renames Report_Map.Key(Cursor);
                    E : Report_Data renames Report_Map.Element( Cursor );
                    I : Bit_Vector renames E.Initalized;
                begin
                    if not (for all X in I'Range => I(X)) then
                        Result.Delete( K );
                    end if;
                end;
            end loop;
        End return;
    End Report;



    procedure Add_Self(
       Self    : in     Weather_Report;
       Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class
      ) is
        Use Gnoga.Gui.Element.Common, Gnoga.Gui.Element.Form;
        Package Form_Entry is new Generic_Form_Entry(
           Report_Type => Weather_Report,
           Report      => Self'Access,
           Form        => Form,
           Label       => Self.Date.Value & ' ' & Self.Time.Value
          );

        Package Items is new Form_Entry.Components(Text_Type, Text_Access, 5);

        Wind_Speed     : Text_Type renames Items.Tuple(1).All;
        Wind_Dir       : Text_Type renames Items.Tuple(2).All;
        Temperature    : Text_Type renames Items.Tuple(3).All;
        Seeing         : Text_Type renames Items.Tuple(4).All;
        Conditions     : Text_Type renames Items.Tuple(5).All;

        Function Name is new Form_Entry.Name(
             Indices =>
                 Form_Entry.Index(Self.Date.Value) &
                 Form_Entry.Index(Self.Time.Value)
            );

    Begin
        Wind_Speed.Create (Form, Value => Self.Wind_Speed.Value,
                                 Name  => Name("Wind_Speed"));
        Temperature.Create(Form, Value => Self.Temperature.Value,
                                 Name  => Name("Temperature"));
        Seeing.Create     (Form, Value => Self.Seeing.Value,
                                 Name  => Name("Seeing"));
        Conditions.Create (Form, Value => Self.Conditions.Value,
                                 Name  => Name("Conditions"));
        Wind_Dir.Create   (Form, Value => Self.Wind_Direction.value,
                                 Name  => Name("Wind_Direction"));

        Items.Set_Attributes;
        Items.Place_Items;
    End Add_Self;


    Procedure Weather_Div(Object : in out Weather_Report;
                          Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class ) is
        Use Gnoga.Gui.Element.Form;
        Date           : Date_Type      renames Object.Date;
        Time           : Time_Type      renames Object.Time;
        Wind_Speed     : Number_Type    renames Object.Wind_Speed;
        Temperature    : Number_Type    renames Object.Temperature;
        Seeing         : Number_Type    renames Object.Seeing;
        Conditions     : Selection_Type renames Object.Conditions;
        Wind_Direction : Selection_Type renames Object.Wind_Direction;

        Labels : Array(1..7) of Gnoga.Gui.Element.Form.Label_Type;

        Procedure Add_Directions is new Add_Discrete( NSO.Types.Direction );
        Procedure Add_Conditions is new Add_Discrete( NSO.Types.Sky       );
        Procedure Add_Label(
           Label : in out Gnoga.Gui.Element.Form.Label_Type'Class;
           Form  : in out Gnoga.Gui.Element.Form.Form_Type'Class;
           Item  : in out Gnoga.Gui.Element.Element_Type'Class;
           Text  : String
          ) is
        Begin
            Label.Create(Form, Item, Text, Auto_Place => False);
            Item.Place_Inside_Bottom_Of(Label);
        End Add_Label;

        Use Ada.Calendar.Formatting;
        Function Time_String return String is
            Use Ada.Strings.Fixed, Ada.Strings;
            Time_Image : String renames Image( Ada.Calendar.Clock );
            Space      : Natural renames Index(Time_Image, " ", Time_Image'First);
            Colon      : Natural renames Index(Time_Image, ":", Time_Image'Last, Going => Backward);
        Begin
            Return Result : Constant String :=
              Time_Image( Positive'Succ(Space)..Positive'Pred(Colon) );
        End;

        Function Date_String return String is
            Use Ada.Strings.Fixed, Ada.Strings;
            Time_Image : String renames Image( Ada.Calendar.Clock );
            Space      : Natural renames Index(Time_Image, " ", Time_Image'First);
        Begin
            Return Result : Constant String :=
              Time_Image( Time_Image'First..Positive'Pred(Space) );
        End;
    Begin
        if DEBUGGING then
            Object.Background_Color( Gnoga.Types.Colors.Yellow );
        end if;

        -----------------------
        -- CREATE COMPONENTS --
        -----------------------

        Date.Create(Form => Form, ID => "Weather.Date", Value => Date_String);
        Time.Create(Form => Form, ID => "Weather.Time", Value => Time_String);
        Conditions.Create(  Form, ID => "Weather.Condition" );
        Wind_Direction.Create (Form, ID => "Weather.Wind_Direction");
        Wind_Speed.Create(  Form, ID => "Weather.Wind_Speed", Value => "0");
        Wind_Speed.Maximum( 100 ); Wind_Speed.Minimum( 0 ); Wind_Speed.Step( 1 );
        Temperature.Create( Form, ID => "Weather.Temperature", Value => "70");
        Temperature.Maximum(120); Temperature.Minimum(-40); Temperature.Step(1);
        Seeing.Create( Form, ID => "Weather.Seeing", Value => "5");
        Seeing.Maximum(8); Seeing.Minimum(1); Seeing.Step(1);

        -----------------
        -- ADD OPTIONS --
        -----------------
        Add_Directions(Form, Wind_Direction);
        Add_Conditions(Form, Conditions);


        --------------------
        -- CREATE LABELS  --
        --------------------
        Add_Label( Labels(1), Form, Date,		"Date:");
        Add_Label( Labels(2), Form, Time,		"Time:");
        Add_Label( Labels(3), Form, Conditions,		"Conditions:");
        Add_Label( Labels(4), Form, Wind_Direction,	"Wind Direction:");
        Add_Label( Labels(5), Form, Wind_Speed,		"Wind Speed:");
        Add_Label( Labels(6), Form, Temperature,	"Temperature:");
        Add_Label( Labels(7), Form, Seeing,		"Seeing:");

        -------------------
        -- PLACE OBJECTS --
        -------------------
        Labels(1).Place_Inside_Top_Of( Object );
        For X in Positive'Succ(Labels'First)..Labels'Last loop
            Labels(X).Place_After( Labels( Positive'Pred(X) ) );
        end loop;


        Object.Place_Inside_Bottom_Of( Form );
    End Weather_Div;



    procedure Make --(Report  : in out Weather_Report;
                     -- Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
                     -- Content : in     String := "";
                     -- ID      : in     String := "" ) is
        is new Generic_Create(
           UI_Report_Div => Weather_Report,
           Populate_Div  => Weather_Div,
           Name          => Report_Name
          );

    procedure Create (Report  : in out Weather_Report;
                      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
                      Content : in     String := "";
                      ID      : in     String := ""
                     ) renames Make;

End NSO.Types.Report_Objects.Weather_Report;

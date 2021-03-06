With
Ada.Tags,
Ada.Exceptions.Traceback,

Ada.Strings.Fixed,
Ada.Calendar.Formatting,
NSO.Helpers,
------------
INI.Read_INI,
INI.Section_Vector,
INI.Section_to_Vector,
INI.Section_to_Map,
--INI.Parameters,
-------------------------
Ada.Text_IO,
Gnoga.Server.Connection,
Gnoga.Types.Colors,
Gnoga.Gui.View;

with Gnoga.Gui.Element, Ada.Characters.Latin_1;

Package Body NSO.Types.Report_Objects.Observation_Report is

    DEBUGGING   : Constant Boolean := False;

    Package Naming is new Name_Binder("Observation", Observation_Report);
    Function Get_Name(Self: Observation_Report) return String
      renames Naming.Get_Name;
    Report_Name : String renames Naming.Report_Name;

--    Report_Name : Constant String  := "Observation";

    procedure Add_Self(
       Self    : in     Observation_Report;
       Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class
      ) is
        Use Gnoga.Gui.Element.Common, Gnoga.Gui.Element.Form;

        Package Form_Entry is new Generic_Form_Entry(
           Report_Type => Observation_Report,
           Report      => Self'Access,
           Form        => Form,
           Label       => Self.Date.Value & ' ' & Self.Time.Value
          );

        Function Name is new Form_Entry.Name(
             Indices =>
                 Form_Entry.Index(Self.Date.Value) &
                 Form_Entry.Index(Self.Time.Value)
            );

        Instrument_INI  : INI.Instance renames INI.Read_INI("Insturments.ini");
        Instrument_List : String_Vector.Vector renames
          INI.Section_Vector(Instrument_INI);
--          INI.Section_to_Vector(INI.Parameters.Parameters, "Instrument");

        Package Items is new Form_Entry.Components(
           Selection_Type, Selection_Access, Positive(Instrument_List.Length)
          );

        Function "="(Left, Right : String) return Boolean
          renames Ada.Strings.Equal_Case_Insensitive;
    Begin
        CREATE_INSTRUMENTS:
        For X in Items.Tuple'Range loop
            FORM_SUBMITION:
            Declare
                Device     : String renames Instrument_List(X);
                Instrument : Selection_Type;
                Submition  : Selection_Type renames Items.Tuple(X).all;
                ---- JQUERY WORKAROUND, NO LONGER NEEDED; KEPT FOR REFERENCE.
                --Function Element_Count return Natural with Inline is
                --    Function Direct_Option_Count return Natural is
                --        use Gnoga.Server.Connection;
                --         --JQuery count options: "$('#example option').length"
                --        Result : String renames Execute_Script(
                --           ID     => Self.Connection_ID,
                --           Script => "#"& Device &" option').length"
                --          );
                --    Begin
                --        Return Integer'Value( Result );
                --    End Direct_Option_Count;
                --Begin
                --    Return Instrument.Length;
                --exception
                --    when CONSTRAINT_ERROR =>
                --        return Direct_Option_Count;
                --End Element_Count;
            Begin
                -- Assiciate 'Instrument' with the proper Instrument from the INI file.
                Instrument.Attach_Using_Parent(ID => Device, Parent => Self);

                -- Create the form-element that will be submitted.
                Submition.Create(
                   Form            => Form,
                   Multiple_Select => True,
                   Name            => Name(Device)
                  );

                -- Populate the form-element.
                SUBMITION_POPULATION:
                For Item in 1..Instrument.Length loop
                    Declare
                        Option : Option_Type;
                        Value  : String renames Instrument.Value(Index => Item);
                        Text   : String renames Instrument.Text (Index => Item);
                    Begin
                        if Instrument.Selected(Item) then
                            Option.Dynamic;
                            Option.Create(
                               Form     => Form, Selection => Submition,
                               Value    => Text,
                               Text     => Text
                              );
                            Option.Selected;
                        End if;
                    End;
                End Loop SUBMITION_POPULATION;
            End FORM_SUBMITION;
        End Loop CREATE_INSTRUMENTS;


        Items.Set_Attributes;
        Items.Place_Items;
    End Add_Self;


    Procedure Observation_Div(Object : in out Observation_Report;
                          Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class ) is
        Use Gnoga.Gui.Element.Form, NSO.Helpers;
        Date           : Date_Type renames Object.Date;
        Time           : Time_Type renames Object.Time;

        Instrument_INI  : INI.Instance renames INI.Read_INI("Insturments.ini");
        Instrument_List : String_Vector.Vector renames
          INI.Section_Vector(Instrument_INI);
--          INI.Section_to_Vector(INI.Parameters.Parameters, "Instrument");

        Labels : Array(1..2) of Gnoga.Gui.Element.Form.Label_Type;

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
            Time_Image : String  renames Image( Ada.Calendar.Clock );
            Space      : Natural renames Index(Time_Image, " ", Time_Image'First);
            Colon      : Natural renames Index(Time_Image, ":", Time_Image'Last, Going => Backward);
        Begin
            Return Result : Constant String :=
              Time_Image( Positive'Succ(Space)..Positive'Pred(Colon) );
        End;

        Function Date_String return String is
            Use Ada.Strings.Fixed, Ada.Strings;
            Time_Image : String  renames Image( Ada.Calendar.Clock );
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

        Date.Create(Form => Form, ID => Report_Name &".Date", Value => Date_String);
        Time.Create(Form => Form, ID => Report_Name &".Time", Value => Time_String);

        -----------------
        -- ADD OPTIONS --
        -----------------

--      Ada.Text_IO.Put_Line("-------------EXPANDING-SECTIONS----------------");
        Add_Section_Groups(Form, Parent => Object, Fields => Instrument_List,
                           File         => Instrument_INI );--INI.Parameters.Parameters);

        --------------------
        -- CREATE LABELS  --
        --------------------
        Add_Label( Labels(1), Form, Date,		"Date:");
        Add_Label( Labels(2), Form, Time,		"Time:");

        -------------------
        -- PLACE OBJECTS --
        -------------------

        Labels(1).Place_Inside_Top_Of( Object );
        For X in Positive'Succ(Labels'First)..Labels'Last loop
            Labels(X).Place_After( Labels( Positive'Pred(X) ) );
        end loop;


        Object.Place_Inside_Bottom_Of( Form );
    End Observation_Div;

    procedure Make is new Generic_Create(
       Name          =>  Report_Name,
       UI_Report_Div =>  Observation_Report,
       Populate_Div  =>  Observation_Div
      );



    procedure Create (Report  : in out Observation_Report;
                      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
                      Content : in     String := "";
                      ID      : in     String := "" )
       renames Make;

--------------------------------------------------------------------------------

    Package Latin_1 renames Ada.Characters.Latin_1;
    New_Line       : Constant String := (Latin_1.CR, Latin_1.LF);



    Function Report(Data : in NSO.JSON.Instance'Class) return Report_Map.Map is
        Use NSO.Types, NSO.JSON, Ada.Strings.Fixed;
        Package Fixed renames Ada.Strings.Fixed;
        Package IMP renames Instrument_Mode_Pkg;

        -- The result to return.
        Working : Report_Map.Map;
        Current : Report_Map.Cursor;

        Procedure Process_Instrument(Instrument : String; Value : Instance'Class) is
            Map : IMP.Map renames Working( Report_Map.Key(Current) );

            Procedure Process_Channel(Channel : String) is
                Set : String_Set.Set renames Map(Instrument);
            Begin
                Set.Include( Channel );
            End Process_Channel;
            Procedure Process_Channel(Index : Natural; Value : in Instance'Class ) is
            Begin
                if Value in String_Object then
                    Process_Channel( Value.To_String );
                end if;
            End Process_Channel;
            Procedure Do_Process is new Apply(
               On_String  => Process_Channel,
               On_Array   => Process_Channel
              );
        Begin
            if not Map.Contains(Instrument) then
                Map.Include(Instrument, String_Set.Empty_Set);
            end if;
            Do_Process( Value );
        End Process_Instrument;

        Function Process_Parameters is new Process_Date_Time(
           Result        => Working,
           Current       => Current,
           Element       => Instrument_Mode_Pkg.Map,
           Date_Time_Map => Report_Map,
           Default       => Instrument_Mode_Pkg.Empty_Map,
           Report_Name   => Report_Name,
           On_Object     => Process_Instrument
          );

    Begin
        Return Result : Report_Map.Map := Process_Parameters(Data) do
            if DEBUGGING then
                Ada.Text_IO.Put_Line( "-------- START DEBUG --------"  );
                for X of Result loop
                    for Y in X.iterate loop
                        Ada.Text_IO.Put_Line( Instrument_Mode_Pkg.key(Y) );
                        for Z of Instrument_Mode_Pkg.Element(Y) loop
                            Ada.Text_IO.Put( "    " & Z & ", " );
                        end loop;
                        Ada.Text_IO.New_Line;
                    end loop;
                end loop;
                Ada.Text_IO.Put_Line( "-------- STOP DEBUG --------"  );
            end if;
        End return;
    End Report;



    -- Generate a single observation-event. (TBODY tag + contents.)
    Function Report( Cursor : Report_Map.Cursor ) return String is
        Use Report_Map, Ada.Calendar, Ada.Calendar.Formatting;-- , NSO.Helpers;

        Row_White  : Boolean:= True;
        Table_Head : Constant String:= "th";
        Table_Data : Constant String:= "td";
        Table_Row  : Constant String:= "tr";
        Header_Row : Boolean:= False;

        Function Row_Color return String with Inline is
        Begin
            Return Result : String := "background-color: " &
            (if Row_White then "Beige" else "#AFCECF") & ';' do
                Row_White:= Not Row_White;
            end return;
        End Row_Color;

        -- Renames for helper-functions.
        Function Trim( Input : String ) return String
          renames NSO.Helpers.Trim;
        Function HTML_Tag(Name, Text, Attribute, Value : String) return String
          renames NSO.Helpers.HTML_Tag;

        -- Ugly "patched" HTML_Tag that applies color-stylings to table-rows,
        -- oherwise it passes on the parameters to the "real" HTML_Tag function.
        Function HTML_Tag(Name, Text : String) return String is
          (if Name = Table_Row
           then HTML_Tag(Name, Text, "Style", Row_Color)
           else NSO.Helpers.HTML_Tag(Name, Text)
          ) with Inline;

        -- Because of how the ROWSPAN attribute alters the general structure of
        -- the tags within a table, the TR tag contents are less uniform than
        -- would otherwise be. (eg a TR can contain 1, 2, or 3 TD tags.)
        Function Row_Start return String;
        Function Row_Stop  return String;

        -- Helper-functions for the output of Row_Start & Row_Stop.
        Function Make_Tag( Input : String ) return String is
          ('<' & Input & " style="""&Row_Color&""">") with Inline;
        Function Stop_Tag( Input : String ) return String is
          ( "</" & Input & '>' ) with Inline;


        -- Get the number of rows the data occupies.
        Function Rows( Input : String_Set.Set ) return Natural with Inline;
        Function Rows( Input : Report_Data    ) return Natural with Inline;


        Function Report( Cursor : Instrument_Mode_Pkg.Cursor ) return String is
            Function Report( Cursor : String_Set.Cursor ) return String is
              (if not String_Set.Has_Element(Cursor) then ""
               else HTML_Tag(Table_Data, String_Set.Element(Cursor) )
              );

            -- Collects all the remaining items of the string-set into a string.
            -- NOTE, this should be used on the SECOND item.
            Function Collect( Cursor : String_Set.Cursor ) return String is
              (if not String_Set.Has_Element(Cursor) then ""
               else HTML_Tag(Table_Row, Report(Cursor)) & New_Line &
                       Collect(String_Set.Next(Cursor))
              );

            Use Instrument_Mode_Pkg;
        Begin
            if not Has_Element(Cursor) then
                Return "";
            else
                Declare
                    Set   : String_Set.Set renames Element(Cursor);
                    Name  : String         renames Key(Cursor);
                    Size  : Constant Natural := Natural(Set.Length);
                    First : Constant String_Set.Cursor:= Set.First;
                Begin
                    Return HTML_Tag(
                       Name      => Table_Data,
                       Attribute => "rowspan",
                       Value     => Trim(Natural'Image(Size)),
                       Text      => Name
                      ) & Report(First) & Row_Stop
                      & Collect(String_Set.Next(First));
                End;
            end if;
        End Report;

        -- Collects all the remaining items of the Instrument-map into a string.
        -- NOTE, this should be used on the SECOND item.
        Function Collect( Cursor : Instrument_Mode_Pkg.Cursor ) return String is
          (if not Instrument_Mode_Pkg.Has_Element(Cursor) then ""
           else (Row_Start & Report(Cursor)) &
                   Collect(Instrument_Mode_Pkg.Next(Cursor)) & New_Line
          );


        Function Rows( Input : String_Set.Set ) return Natural is
          ( Natural(Input.Length) );

        Function Rows( Input : Report_Data ) return Natural is
        Begin
            Return Result : Natural := 0 do
                For Instrument of Input loop
                    Result:= Result + Rows( Instrument );
                End loop;
            End return;
        End Rows;


        Function Row_Start return String is
        Begin
            Return Result : String:=
              (if not Header_Row then Make_Tag(Table_Row) else "") do
                Header_Row:= True;
            End return;
        End Row_Start;

        Function Row_Stop  return String is
        Begin
            Return Result : String:=
              (if Header_Row then Stop_Tag(Table_Row) else "") do
                Header_Row:= False;
            End return;
        End Row_Stop;


    Begin
        if not Has_Element(Cursor) then
            Return "";
        else
            Declare
                Date       : Time renames Key(Cursor);
                Instrument : Report_Data renames Element(Cursor);
                Count      : Constant Natural := Rows(Instrument);
                First      : Constant Instrument_Mode_Pkg.Cursor:= Instrument.First;
                Date_Time  : Constant String:= --
                  "<th rowspan=""" & Trim( Natural'Image(Count) ) &
                  """ scope=""RowGroup"">" & Image(Date) & "</th>";
            Begin
                return New_Line & HTML_Tag("TBody",
                      Row_Start
                         & Date_Time
                         & Report(First)
                         & Collect( Instrument_Mode_Pkg.Next(First) )
                  ) & New_Line;
            End;
        End if;
    End Report;

    Function Report( Object : Report_Map.Map ) return String is
        Use NSO.Helpers;

        Function Report_Header return String is
            Function Header(Column : String) return String is
              (HTML_Tag(
                  Name      => "th",
                  Text      =>  Column,
                  Attribute => "scope",
                  Value     => "col"
                 )
              ) with Inline;

            use NSO.Helpers;
            Date_Time      : String renames Header("Time / Date");
            INSTRUMENTS    : String renames Header("INSTRUMENTS");
            Modes          : String renames Header("Channels");
        Begin
            Return New_Line & HTML_Tag(
               "thead", HTML_Tag("tr",
               Date_Time & INSTRUMENTS & Modes)
              ) & New_Line;
        End Report_Header;


        Function Report_Data( Cursor : Report_Map.Cursor:= Object.First ) return String is
            Next     : Report_Map.Cursor renames Report_Map.Next(Cursor);
            Has_Next : Boolean renames Report_Map.Has_Element( Next );
        Begin
            Return  Report(Cursor) &
            (if not Has_Next then "" else Report_Data( Next ));
        End Report_Data;

        Caption : Constant String := HTML_Tag("Caption", Report_Name&" Report");
    Begin
        return HTML_Tag(
           Attribute => "Width",
           Value     => "100%",
           Name      => "Table",
           Text      => Caption & Report_Header & Report_Data
          );
    End Report;

End NSO.Types.Report_Objects.Observation_Report;

With
Ada.Tags,
Ada.Strings.Fixed,
Ada.Calendar.Formatting,
INI.Parameters,
INI.Section_to_Vector,
NSO.Types,
NSO.Helpers,
Gnoga.Types.Colors,
Gnoga.Gui.Element.Common,
Gnoga.Gui.Element.Table;

WITH
Ada.Text_IO;
with System.RPC;
with Gnoga.Gui.Element.Form;
Package Body Report_Form is
    use NSO.Types, NSO.Helpers, INI.Parameters;

    DEBUGGING : Constant Boolean := False;

    -- For when multiple reports will be used.
    Type Drop_Action is (Weather, Observation);

    -- Handles the Start_Drag event.
    Procedure Start_Drag (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
    Begin
        if DEBUGGING then
            Ada.Text_IO.Put_Line( "----START_DRAG----" );
        end if;
    End Start_Drag;

    -- Handler; meant for dragging.
    -- Currently unused.
    procedure Do_It (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
    Begin
        Null; -- Ada.Text_IO.Put_Line( "----DRAGGING----" );
    End Do_It;

    -- Handles the Drop event.
    procedure Do_Drop(
       Object    : in out Gnoga.Gui.Base.Base_Type'Class;
       X, Y      : in     Integer;
       Drag_Text : in     String
      ) is
        Tag    : String renames Ada.Tags.Expanded_Name( Object'Tag );
    Begin
        if DEBUGGING then
            Ada.Text_IO.Put_Line( "DRAG-TEXT: " & Drag_Text );
        end if;

        HANDLE_ACTION:
        Declare

            Action : Drop_Action renames Drop_Action'Value( Drag_Text );
        Begin
            if DEBUGGING then
                Ada.Text_IO.Put_Line( "----DROPPED "& Tag &"!!----" );
            end if;

            case Action is
            when Weather =>
                Declare
                    Use Gnoga.Gui.Element.Form, Gnoga.Gui.View;
                    Form : Form_Type'Class renames Form_Type'Class(Object);
                    View : My_Widget_Type'Class renames My_Widget_Type'Class(Form.Parent.all);
                Begin
                    View.Weather_Div.Add_To_Form(Form);
                End;
            when Observation =>
                Declare
                    Use Gnoga.Gui.Element.Form, Gnoga.Gui.View;
                    Form : Form_Type'Class renames Form_Type'Class(Object);
                    View : My_Widget_Type'Class renames My_Widget_Type'Class(Form.Parent.all);
                Begin
                    View.Observation_Div.Add_To_Form(Form);
                End;
            end case;
        End HANDLE_ACTION;

    exception
        when Constraint_Error => -- Undefined action / not in enumeration.
            if DEBUGGING then
                Ada.Text_IO.Put_Line( "Invalid action:'"&Drag_Text&"'." );
            end if;
    End Do_Drop;



    Procedure Weather_Div(Object : in out Gnoga.Gui.Element.Common.DIV_Type;
                          View   : in out My_Widget_Type ) is

        Date                            : Gnoga.Gui.Element.Form.Date_Type;
        Time                            : Gnoga.Gui.Element.Form.Time_Type;
        Wind_Speed, Temperature, Seeing : Gnoga.Gui.Element.Form.Number_Type;
        Conditions, Wind_Direction      : Gnoga.Gui.Element.Form.Selection_Type;

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
        Object.Create(View.Widget_Form, ID => "Weather");
        Object.Draggable( True );
        Object.On_Drag_Handler( Handler => Do_It'Access );
        Object.On_Drag_Start_Handler( Drag_Text => Drop_Action'Image(Weather), Handler => Start_Drag'Access );
        Object.Cursor( "grab" );
        Object.Minimum_Width (Unit => "ex", Value => 40);
        Object.Minimum_Height(Unit => "ex", Value => 06);
        Object.Style(Name  => "display", Value => "INLINE-BLOCK");

        -----------------------
        -- CREATE COMPONENTS --
        -----------------------

        Date.Create(Form => View.Widget_Form, ID => "Weather.Date", Value => Date_String);
        Time.Create(Form => View.Widget_Form, ID => "Weather.Time", Value => Time_String);
        Conditions.Create(  View.Widget_Form, ID => "Weather.Condition" );
        Wind_Direction.Create (View.Widget_Form, ID => "Weather.Wind_Direction");
        Wind_Speed.Create(  View.Widget_Form, ID => "Weather.Wind_Speed", Value => "0");
        Wind_Speed.Maximum( 100 ); Wind_Speed.Minimum( 0 ); Wind_Speed.Step( 1 );
        Temperature.Create( View.Widget_Form, ID => "Weather.Temperature", Value => "70");
        Temperature.Maximum(120); Temperature.Minimum(-40); Temperature.Step(1);
        Seeing.Create( View.Widget_Form, ID => "Weather.Seeing", Value => "5");
        Seeing.Maximum(8); Seeing.Minimum(1); Seeing.Step(1);

        -----------------
        -- ADD OPTIONS --
        -----------------
        Add_Directions(View.Widget_Form, Wind_Direction);
        Add_Conditions(View.Widget_Form, Conditions);


        -------------------
        -- PLACE OBJECTS --
        -------------------
--          Date.Place_Inside_Top_Of( Object );
--          Time.Place_After(Date);
--          Conditions.Place_After(Time);
--          Wind_Direction.Place_After(Conditions);
--          Wind_Speed.Place_After(Wind_Direction);
--          Temperature.Place_After(Wind_Speed);
--          Seeing.Place_After(Temperature);


        --------------------
        -- CREATE LABELS  --
        --------------------
        Add_Label( Labels(1), View.Widget_Form, Date,		"Date:");
        Add_Label( Labels(2), View.Widget_Form, Time,		"Time:");
        Add_Label( Labels(3), View.Widget_Form, Conditions,	"Conditions:");
        Add_Label( Labels(4), View.Widget_Form, Wind_Direction,	"Wind Direction:");
        Add_Label( Labels(5), View.Widget_Form, Wind_Speed,	"Wind Speed:");
        Add_Label( Labels(6), View.Widget_Form, Temperature,	"Temperature:");
        Add_Label( Labels(7), View.Widget_Form, Seeing,		"Seeing:");

        Labels(1).Place_Inside_Top_Of( Object );
        For X in Positive'Succ(Labels'First)..Labels'Last loop
            Labels(X).Place_After( Labels( Positive'Pred(X) ) );
        end loop;


        Object.Place_Inside_Bottom_Of( View.Widget_Form );
    End Weather_Div;

   --  Used to create our custom view
   overriding
   procedure Create (View    : in out My_Widget_Type;
                     Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
                     ID      : in     String  := "")
   is
        Multipart : Constant Boolean := True;
        PlainText : Constant Boolean := False;

        Function Encoding return Gnoga.Gui.Element.Form.Encoding_Type is
          (if    MultiPart then Gnoga.Gui.Element.Form.Multi_Part
           elsif PlainText then Gnoga.Gui.Element.Form.Text
           else                 Gnoga.Gui.Element.Form.URL_Encode
          ) with Inline, Pure_Function;


        Function Drag_Here_SVG return String is
            SVG_Head  : Constant String :=
              "<svg xmlns=""http://www.w3.org/2000/svg"" " &
                 "version=""1.1"" height=""3.5ex"" width=""20em"">";
            Text_Head : Constant String :=
              "<text x=""50%"" y=""50%"" fill=""dimgray"" font-size=""20"" " &
                 "dominant-baseline=""middle"" text-anchor=""middle"">";
            Text_Tail : Constant String := "</text>";
            SVG_Tail  : Constant String := "</svg>";

            Function Text( Item : String ) return String is
              (Text_Head & Item & Text_Tail) with Inline;

        Begin
            Return SVG_Head & Text("Drag Reports Here") & SVG_Tail;
        End Drag_Here_SVG;

      use Gnoga.Gui.Element.Table;
      Layout_Table : constant Table_Access := new Table_Type;
    begin

      Gnoga.Gui.View.View_Type (View).Create (Parent, ID);

        View.Widget_Form.Create (View);
        View.Widget_Form.On_Drop_Handler(Handler => Do_Drop'Access);
        View.Widget_Form.Background_Color(if DEBUGGING then "RED" else "lightgray");
        View.Widget_Form.Background_Image(
           "data:image/svg+xml;utf8, " & Drag_Here_SVG
          );
View.Widget_Form.Method  (Value => Gnoga.Gui.Element.Form.Post);
View.Widget_Form.Encoding(Value => Encoding);
        -- data:image/svg+xml;utf8, <svg xmlns='http://www.w3.org/2000/svg'><text x='50%' y='50%' fill='red'>TEST!!</text></svg>
                            --Drag_Here_SVG);


      Layout_Table.Dynamic;
      --  Marking an element Dynamic before creation tells Gnoga to garbage
      --  collect, i.e. deallocate the element when it is parent is finalized.
      Layout_Table.Create (View);
      Layout_Table.Border("medium", Gnoga.Gui.Element.Double, Gnoga.Types.Colors.Dark_Red);

      declare
            Type Observation_Conditions is (Excellent, Good, Fair, Poor);
            Procedure Add_Selections is new Add_Discrete(Observation_Conditions);
         row  : constant Table_Row_Access := new Table_Row_Type;
         col1 : constant Table_Column_Access := new Table_Column_Type;
         col2 : constant Table_Column_Access := new Table_Column_Type;
            Operators : NSO.Types.String_Vector.Vector :=
              INI.Section_to_Vector(Parameters,"Observers");

            Type Categories is (fe_Name, fe_Observer, fe_Item);
            Labels : Array(Categories) of Gnoga.Gui.Element.Form.Label_Type;
            Division_X : Gnoga.Gui.Element.Common.DIV_Type renames
              Gnoga.Gui.Element.Common.DIV_Type(View.Weather_Div);
        begin
         row.Dynamic;
         col1.Dynamic;
         col2.Dynamic;

         row.Create (Layout_Table.all);
            col1.Create (row.all); -- was "NAME"
         col2.Create (row.all);
--           View.Name_Input.Create (Form  => View.Widget_Form,
--                                   Size  => 40,
--                                   Name  => "Name");
--           --  The Name of the element is its variable name when submitted.
--
--           View.Name_Input.Required;
--           --  By marking Name_Input required, if the submit button is pushed
--           --  it will not allow submission and notify user unless element
--           --  if filled out.
--
--           View.Name_Input.Place_Holder ("(Only letters and spaces permitted)");
--           View.Name_Input.Pattern ("[a-zA-Z\ ]+");
--           --  Allow only a-z, A-Z and space characters
--
--           View.Name_Input.Place_Inside_Top_Of (col2.all);
--           --  Since forms are auto placed inside the Form, we need to move the
--           --  element where we would like it to display.
--
--              Labels(fe_Name).Create(View.Widget_Form, View.Name_Input, "Name:", False);
--              Labels(fe_Name).Place_Inside_Top_Of( col1.all );

            ---------------------------------
--              View.Stat_Input.Create(
--  --               ID              =>,
--                 Form            => View.Widget_Form,
--                 Multiple_Select => False,
--                 Visible_Lines   => 1,
--                 Name            => "Conditions"
--                );
--              Add_Selections( View.Widget_Form, View.Stat_Input );


            View.Oper_Input.Create(
--               ID              =>,
               Form            => View.Widget_Form,
               Multiple_Select => False,
               Visible_Lines   => 1,
               Name            => "Observer"
              );

            if DEBUGGING then
                Operators.Append("---DEBUG---");
            end if;

            Add_Vector( View.Widget_Form, View.Oper_Input,
                        Operators);
            Labels(fe_Observer).Create(View.Widget_Form, View.Oper_Input, "Observer:", True);

            view.Weather_Div.Create( view );

            view.Observation_Div.Create( View );
            --Division_X.Create(Parent, ID => "Weather");
            --Weather_Div( Division_X, View );
        end;

      declare
         row  : constant Table_Row_Access := new Table_Row_Type;
         col1 : constant Table_Column_Access := new Table_Column_Type;
         col2 : constant Table_Column_Access := new Table_Column_Type;
      begin
         row.Dynamic;
         col1.Dynamic;
         col2.Dynamic;

         row.Create (Layout_Table.all);
         row.Style ("vertical-align", "top");
         col1.Create (row.all, "Message");
         col2.Create (row.all);
         View.Message.Create (Form    => View.Widget_Form,
                              Columns => (if DEBUGGING then 20 else 120),
                              Rows    => (if DEBUGGING then 10 else  20),
                              Name    => "Message");
            View.Message.Style("background-size","100% 1.2em");
            View.Message.Style("background-image",
                               "linear-gradient(90deg, transparent 79px, #abced4 79px, #abced4 81px, transparent 81px),"&
                               "linear-gradient(#eee .1em, transparent .1em)");
            View.Message.Style("Wrap", "hard"); --Word_Wrap(True);

            -- NOTE:	This limit comes from Gnoga.Server.Connection. Within
            -- 		Gnoga_HTTP_Content type, the Text field is defined as:
            --		Aliased Strings_Edit.Streams.String_Stream (500);
            View.Message.Attribute("maxlength", "4000" );
         View.Message.Place_Inside_Top_Of (col2.all);
      end;

      View.My_Submit.Create (Form => View.Widget_Form, Value => "Submit");
      View.My_Submit.Place_After (Layout_Table.all);
    end Create;

End Report_Form;

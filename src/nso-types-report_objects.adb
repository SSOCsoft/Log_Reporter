With
NSO.Helpers,
Ada.Strings.Fixed,
Ada.Calendar.Formatting,
Ada.Tags,

Gnoga.Types.Colors,
Gnoga.Gui.View,

Ada.Text_IO,

Report_Form
;

WITH
Ada.Tags,
Ada.Text_IO;

with
Gnoga.Gui.Base;
with Gnoga.Gui.Element.Form;

Package Body NSO.Types.Report_Objects is
    Use NSO.Helpers;

    DEBUGGING : Constant Boolean := False;

    Package Body Generic_Form_Entry is

        Package Body Components is
            Procedure Place_Items is
            Begin
                -- In order to have a uniform and orderly placement, we place
                -- the first item of the Tuple, then place all remaining items
                -- 'after' it, finally placing the trash-icon.
                Tuple(Tuple'First).Place_Inside_Bottom_Of( Form_Entry );
                For Index in Positive'Succ(Tuple'First)..Tuple'Last Loop
                    Tuple(Index).Place_After( Tuple(Positive'Pred(Index)).All );
                End loop;
                Trash.Place_After( Tuple(Tuple'Last).All );
            End Place_Items;

            Procedure Set_Attributes is
            Begin
                -- Set the attributes of the Tuple's items:
                --  * Transparent background,
                --  * Read-only,
                --  * uniform width.
                For Item of Tuple loop
                    Gnoga.Gui.Element.Form.Read_Only(
                       Value   => True,
                       Element => Gnoga.Gui.Element.Form.Form_Element_Type(Item.All)
                      );
                    Item.Minimum_Width(Unit => "em", Value => 11);
                    Item.All.Background_Color(Value => "transparent");
                End loop;
            End Set_Attributes;


        -- And here we ensure we set all the tuple's items to Dynamic.
        Begin
            For Item of Tuple loop
                Item.Dynamic;
            End loop;
        End Components;

        Procedure Finalize (Object : in out Instance) is
        Begin
            -- ADD TO FORM --
            Form_Entry.Place_Inside_Bottom_Of( Form );
        End Finalize;


        Function Index( Input : String ) return String is
          ( Left_Bracket & Input & Right_Bracket );



        Function Name( Input : String ) return String is
          (Report.Get_Name & Indices & Index(Input));

    -- Initalization: We ensure the DIV is marked dynamic, as well as Trash,
    -- then we create the record's label and the Trash-icon, finally we set
    -- all the attributes for the DIV as well as the trash-icon.
    Begin
        -- SET DYNAMIC --
        Form_Entry.Dynamic;
        Trash.Dynamic;

        -- CREATE FORM INPUTS --
        Form_Entry.Create(
           Parent  =>  Form,
           Content => (if Label'Length not in Positive then "" else
                       "<span><b>Report for: </b>"& Label &"</span>")
          );

        Trash.Create(Form_Entry, URL_Source       => "/img/Trashcan-2.png",
                                 Alternative_Text => "Trash");
        -- SET ATTRIBUTES --
        Form_Entry.Style("background-image", "linear-gradient(to right, lightgray, dimgray)");
        Form_Entry.Border_Radius( "1ex" );
        Form_Entry.Border(Width => "thin", Color => Gnoga.Types.Colors.Dark_Grey);

        Trash.Attribute(Name => "Align",   Value => "Top");
        Trash.Attribute(Name => "OnClick", Value => "return this.parentNode.remove();");
        Trash.Style("max-width",  "3ex");
        Trash.Style("max-height", "3ex");
    End Generic_Form_Entry;



    -- Dispatcher.
    Procedure Add_to_Form(
       Self    : in     Abstract_Report'Class;
       Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class
      ) is
    Begin
        Self.Add_Self( Form );
    End Add_to_Form;





    Procedure Start_Drag (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
    Begin
        if DEBUGGING then
            Ada.Text_IO.Put_Line( "----START_DRAG----" );
        end if;
    End Start_Drag;

    procedure Do_It (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
    Begin
        Null; -- Ada.Text_IO.Put_Line( "----DRAGGING----" );
    End Do_It;

--      procedure Do_Drop(
--         --Object    : in out Gnoga.Gui.Base.Base_Type'Class;
--                Object    : in out Weather_Report'Class;
--         X, Y      : in     Integer;
--         Drag_Text : in     String
--        ) is
--          --Tag    : String renames Ada.Tags.Expanded_Name( Object'Tag );
--      Begin
--          if DEBUGGING then
--              Ada.Text_IO.Put( "----DO_DROP:" );
--              --Ada.Text_IO.Put( Tag );
--              Ada.Text_IO.Put( "----" );
--              ada.Text_IO.New_Line;
--          end if;
--      End Do_Drop;




--      procedure Create (Report  : in out Weather_Report;
--                        Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
--                        Content : in     String := "";
--                        ID      : in     String := "" ) is
--          Block : Gnoga.Gui.Element.Common.DIV_Type;
--          Function Get_View( Object : Gnoga.Gui.Base.Pointer_To_Base_Class;
--                             Hops   : Natural := 200
--                            ) return Gnoga.Gui.View.View_Access is
--              use Gnoga.Gui.View;
--          Begin
--              Return (if Object.all in View_Type'Class then View_Access(Object)
--                        else Get_View(Object.Parent, Natural'Pred(Hops)));
--          End Get_View;
--
--      Begin
--          Gnoga.Gui.Element.Common.DIV_Type(Report).Create(Parent, ID);
--          if DEBUGGING then
--              Ada.Text_IO.Put_Line("*****CREATING REPORT DIVISION*****");
--          end if;
--
--          Declare
--              Temp : Gnoga.Gui.Element.Form.Form_Type;
--          Begin
--              Temp.Create(Parent);
--              Weather_Div( Report, Temp );
--
--
--              Block.Create(Report, "<b><tt>Weather Report</tt></b>");
--              Block.Style( "font-variant", "small-caps" );
--              Block.Style( "padding-left", "1ex" );
--              Block.Style( "margin-bottom", "0.7ex" );
--              Block.Background_Color("lightgray");
--              Block.Border(Width => "thin", Color => Gnoga.Types.Colors.Dark_Grey);
--              Block.Border_Radius("1ex");
--              Block.Place_Inside_Top_Of( Report );
--
--              Temp.Place_Inside_Bottom_Of( Get_View(Report.Parent).all );
--          End;
--
--      End Create;


    Procedure Generic_Create(Report  : in out UI_Report_Div;
                             Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
                             Content : in     String := "";
                             ID      : in     String := "" ) is
        Block : Gnoga.Gui.Element.Common.DIV_Type;
        Function Get_View( Object : Gnoga.Gui.Base.Pointer_To_Base_Class;
                           Hops   : Natural := 200
                          ) return Gnoga.Gui.View.View_Access is
            use Gnoga.Gui.View;
        Begin
            Return (if Object.all in View_Type'Class then View_Access(Object)
                    else Get_View(Object.Parent, Natural'Pred(Hops)));
        End Get_View;

    Begin
        -- This up-casts the report to a DIV and calls its Create function.
        Gnoga.Gui.Element.Common.DIV_Type(Report).Create(Parent, ID);
        if DEBUGGING then
            Ada.Text_IO.Put_Line("*****CREATING "&Name&"-REPORT DIVISION*****");
        end if;

        Declare
            Temp : Gnoga.Gui.Element.Form.Form_Type;
        Begin
            Temp.Create(Parent);
            Populate_Div( Report, Temp );

            -- Set common DIV styling.
            -- (Note: Was at the start of the *_DIV procedures, for weather & observation.)
            Report.Draggable( True );
            Report.On_Drag_Start_Handler(
               Drag_Text => Report.Get_Name,
               Handler   => Report.Drag_Start
              );
            Report.Cursor( "grab" );
            Report.Minimum_Width (Unit => "ex", Value => 40);
            Report.Minimum_Height(Unit => "ex", Value => 06);
            Report.Style(Name  => "display", Value => "INLINE-BLOCK");


            Block.Create(Report, "<b><tt>"& Name &" Report</tt></b>");
            Block.Style( "font-variant", "small-caps" );
            Block.Style( "padding-left", "1ex" );
            Block.Style( "margin-bottom", "0.7ex" );
            Block.Background_Color("lightgray");
            Block.Border(Width => "thin", Color => Gnoga.Types.Colors.Dark_Grey);
            Block.Border_Radius("1ex");
            Block.Place_Inside_Top_Of( Report );

            Temp.Place_Inside_Bottom_Of( Get_View(Report.Parent).all );
        End;
    End Generic_Create;


    Function Object_Filter(Input : JSON.Instance'Class) return JSON.Instance'Class is
        Use NSO.JSON;
    Begin
        if Input in Object_Object then
            Declare
                Object  : Object_Object  renames Object_Object(Input);
                Element : Instance'Class renames Object.Value( Name );
            Begin
                return Element;
            End;
        else
            raise Program_Error with Ada.Tags.Expanded_Name(Input'Tag) &
              " is not supported, parameter must be a JSON Object type.";
        end if;
    Exception
        when Constraint_Error => -- Raised when the report does not exist.
            Return Make_Object;
            --JSON_Class_Input( NSO.Types."+"("{}") );
    End Object_Filter;


    Function Process_Date_Time(Data : JSON.Instance'Class) return Date_Time_Map.Map is
        Use JSON;
        --Result : Date_Time_Map.Map  := Date_Time_Map.Empty_Map;
        Function Filter is new Object_Filter( Report_Name );

        procedure Process_Date(Date : String; Value : Instance'Class) is
            Procedure Process_Time(Time : String; Value : Instance'Class) is
                Time_String : Constant String := Date & ' ' & Time & ":00";
                Date_Time   : Constant Ada.Calendar.Time :=
                  Ada.Calendar.Formatting.Value(Time_String);

                Procedure Do_Process is new Apply(
                   On_Null    => On_Null,
                   On_Boolean => On_Boolean,
                   On_String  => On_String,
                   On_Number  => On_Number,
                   On_Array   => On_Array,
                   On_Object  => On_Object
                  );

            Begin
                if not Result.Contains( Date_Time ) then
                    Result.Include(Date_Time, Default);
                end if;
                Current := Result.Find(Date_Time);
                Do_Process( Value );
            End Process_Time;
            Procedure Do_Process is new Apply(On_Object => Process_Time);
        Begin
            Do_Process( Value );
        End Process_Date;

        Procedure Do_Process is new Apply(On_Object => Process_Date);
    Begin
        Result := Date_Time_Map.Empty_Map;
        Current:= Date_Time_Map.No_Element;
        Do_Process( Object => (if Report_Name'length not in Positive then Data
                               else Filter(Data) ));
        Return Result;
    End Process_Date_Time;

    Package Body Date_Time_Indexed_Report is
        Use Ada.Calendar.Formatting;

        Function Time_String(Time : Ada.Calendar.Time:= Ada.Calendar.Clock) return String is
            Use Ada.Strings.Fixed, Ada.Strings;
            Time_Image : String  renames Image( Time );
            Space      : Natural renames Index(Time_Image, " ", Time_Image'First);
            Colon      : Natural renames Index(Time_Image, ":", Time_Image'Last, Going => Backward);
        Begin
            Return Result : Constant String :=
              Time_Image( Positive'Succ(Space)..Positive'Pred(Colon) );
        End Time_String;

        Function Date_String(Time : Ada.Calendar.Time:= Ada.Calendar.Clock) return String is
            Use Ada.Strings.Fixed, Ada.Strings;
            Time_Image : String  renames Image( Time );
            Space      : Natural renames Index(Time_Image, " ", Time_Image'First);
        Begin
            Return Result : Constant String :=
              Time_Image( Time_Image'First..Positive'Pred(Space) );
        End Date_String;



        Procedure Date_Time_Div(
           Object : in out Date_Time_Report'Class;
           Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class
          ) is
        Begin

            null;
        End Date_Time_Div;

    End Date_Time_Indexed_Report;



    Function Debug_Action_Event return Gnoga.Gui.Base.Action_Event is
      ( Start_Drag'Access );

End NSO.Types.Report_Objects;

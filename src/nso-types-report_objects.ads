With
NSO.JSON,
Ada.Finalization,
Ada.Characters.Latin_1,
Ada.Containers.Ordered_Maps,
Ada.Calendar,
Gnoga.Gui.Base,
Gnoga.Gui.Element.Form,
Gnoga.Gui.Element.Common;

Limited with
Report_Form;

Package NSO.Types.Report_Objects is

    Function Debug_Action_Event return Gnoga.Gui.Base.Action_Event;


    -- The base report-type for NSO reports.
    Type Abstract_Report is abstract new Gnoga.Gui.Element.Common.DIV_Type
      with record
        Drag_Start,
        Drag      : Gnoga.Gui.Base.Action_Event := Debug_Action_Event;
      end record;

    -- We *could* use the drag and drag-start callbacks as class-wide functions,
    -- but this would mean we would lose the ability to have custom callbacks,
    -- as might be used for debugging, and instead treating everything uniformly.
    --      Function Get_Drag_Start_Handler(Input : Abstract_Report'Class) return Action_Event;
    --      Function Get_Drag_Handler      (Input : Abstract_Report'Class) return Action_Event;

    -- Class-wide; calls Add_Self, which populates the form-elements to be sent.
    procedure Add_to_Form(
       Self    : in     Abstract_Report'Class;
       Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class
      );

    -- Adds the data for the report to the form for submission.
    procedure Add_Self(
       Self    : in     Abstract_Report;
       Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class
      ) is abstract;


    Function Get_Name(
       Self    : in     Abstract_Report
      ) return String is abstract with Inline;


    -- Generic_Create adds a draggable DIV to the Parent, complete with a
    -- standard styling; Populate_DiV is the method which contains the
    -- report-speciffic construction.
    Generic
        Type UI_Report_Div(<>) is new Abstract_Report with private;
        with Procedure Populate_Div(
           Object : in out UI_Report_Div;
           Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class
          );
        Name : String;
    Procedure Generic_Create(Report  : in out UI_Report_Div;
                             Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
                             Content : in     String := "";
                             ID      : in     String := ""
                            );


    -- Given an Input, presumed to be a JSON Object-type, Object_Filter will
    -- return the result analogous to Input[Name]; raises PROGRAM_ERROR when the
    -- Input parameter is not an instance of JSON.Object_Object.
    Generic
        Name : String;
    Function Object_Filter(Input : JSON.Instance'Class)
      return JSON.Instance'Class;


    Generic
        Type Element(<>) is private;
        with Package Date_Time_Map is new Ada.Containers.Indefinite_Ordered_Maps(
           Key_Type     => Ada.Calendar.Time,
           Element_Type => Element,
           others       => <>
          );
        Default     : in     Element;
        Current     : in out Date_Time_Map.Cursor;
        Result      : in out Date_Time_Map.Map;
        Report_Name : in     String := "";
        with Procedure On_Null                                                       is null;
        with Procedure On_Boolean(                  Value : in      Boolean        ) is null;
        with Procedure On_String (                  Value : in      String         ) is null;
        with Procedure On_Number (                  Value : in JSON.Number         ) is null;
        with procedure On_Array  ( Index : Natural; Value : in JSON.Instance'Class ) is null;
        with procedure On_Object ( Key   : String;  Value : in JSON.Instance'Class ) is null;
    Function Process_Date_Time(Data : JSON.Instance'Class) return Date_Time_Map.Map;


    Package Date_Time_Indexed_Report is

        -----------------------------
        --  TIME/DATE SUBPROGRAMS  --
        -----------------------------

        Function Time_String(Time : Ada.Calendar.Time:= Ada.Calendar.Clock) return String;
        Function Date_String(Time : Ada.Calendar.Time:= Ada.Calendar.Clock) return String;

        Type Date_Time_Report is abstract new Abstract_Report with record
            Date  : Gnoga.Gui.Element.Form.Date_Type;
            Time  : Gnoga.Gui.Element.Form.Time_Type;
        end record;

        Procedure Date_Time_Div(
           Object : in out Date_Time_Report'Class;
           Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class
          );


    End Date_Time_Indexed_Report;


Private
    -- The Abstract_Form_Entry type is the DIV that is added to the HTML-Form to
    -- be submitted to the system; as such, it is tied to the Report-type that
    -- generates the data to place in the form for submission.
    Type Abstract_Form_Entry(For_Report : not null access constant Abstract_Report'Class)
    is abstract new Gnoga.Gui.Element.Common.DIV_Type
    with null record;

    -- Generic_Form_Entry creates a non-abstract instance of Abstract_Form_Entry
    -- from the Report_type/Report, it also has an Index function for wrapping
    -- a name in brackets, and a Name function for aiding in uniform naming,
    -- lastly
    Generic
        Type Report_Type is new Abstract_Report with private;
        Report : in     access constant Report_Type;
        Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
        Label  : in     String;
    Package Generic_Form_Entry is
        Use Gnoga.Gui.Element.Common;

        Type Instance is new Abstract_Form_Entry(For_Report => Report)
          with null record;

        -- Finalize will add the instance to the Form.
        Overriding
        Procedure Finalize (Object : in out Instance);

--          -- IA is the access-type of the singlton element Form_Entry.
--          Type IA is Access all Instance;

        -- Returns '[' & Input & ']'.
        -- Intended for use working with form-parameters.
        Function Index( Input : String ) return String with Inline;

        -- This function returns the concatenation of:
        --	GET_NAME, Indicies, and Index(Input).
        -- Example given Indices of "[Date][Time]", an Input of "Item" and a
        -- Report.Get_Name of "Steve", the String returned is:
        -- "Steve[Date][Time][Item]".
        Generic
            Indices : String;
        Function Name( Input : String ) return String with Inline;

        -- This object *IS* the DIV which holds the form-elements to submit.
        Form_Entry : Instance;
        -- Every entry should have a "trash icon" to allow deletion of the record.
        Trash      : Constant IMG_Access := new IMG_Type;

        -- Components is a generic for facilitating the creation of a set of
        -- HTML-Form elements which hold the datum of the record for submitting.
        Generic
            Type T is new Gnoga.Gui.Element.Form.Form_Element_Type with private;
              --Gnoga.Gui.Element.Element_Type with private;
            Type A is Access All T;
            Length : Positive;
        Package Components is
            Tuple : Constant Array(1..Length) of A:= (Others => new T);

            Procedure Place_Items;
            Procedure Set_Attributes;
        End Components;



    Private

        Package Latin_1 renames Ada.Characters.Latin_1;

        Left_Bracket   : Character renames Latin_1.Left_Square_Bracket;   -- [
        Right_Bracket  : Character renames Latin_1.Right_Square_Bracket;  -- ]
        Bracket_Divide : Constant String := Right_Bracket & Left_Bracket; -- ][
    End Generic_Form_Entry;


    -- This Name-Binder ensures that the report's string-value of the name is
    -- uniform across the report-package; this is useful for ensuring that the
    -- value is consistent across related-but-disconnected items like the
    -- form submission parameters.
    Generic
        Name : String;
        Type Report_Type(<>) is new Abstract_Report with private;
    Package Name_Binder is
        Report_Name : Constant String := Name;

        Function Get_Name(Self: Report_Type) return String is
          (Report_Name) with Inline;
    End Name_Binder;



End NSO.Types.Report_Objects;

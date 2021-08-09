With
NSO.JSON,
Ada.Containers.Ordered_Maps,
Ada.Calendar,
Gnoga.Gui.Base,
Gnoga.Gui.Element.Form,
Gnoga.Gui.Element.Common;

Limited with
Report_Form;

Package NSO.Types.Report_Objects.Observation_Report with Elaborate_Body is

    Package Instrument_Mode_Pkg is new Ada.Containers.Indefinite_Ordered_Maps(
       "<"          => Ada.Strings.Less_Case_Insensitive,
       "="          => String_Set."=",
       Key_Type     => String,
       Element_Type => String_Set.Set
      );

    -------------------
    --  REPORT DATA  --
    -------------------

    Subtype Report_Data is Instrument_Mode_Pkg.Map;
--         record
--          Frequency : String_Vector.Vector:= String_Vector.Empty_Vector;
--      End record;


    ------------------
    --  REPORT MAP  --
    ------------------

    Package Report_Map is new Ada.Containers.Indefinite_Ordered_Maps(
       "="          => Instrument_Mode_Pkg."=",
       "<"          => Ada.Calendar."<",
       Key_Type     => Ada.Calendar.Time,
       Element_Type => Report_Data
      );


    ----------------------------------------
    --  OBSERVATION REPORT GNOGA ELEMENT  --
    ----------------------------------------

    Type Observation_Report is new Abstract_Report with record
      --Gnoga.Gui.Element.Common.DIV_Type with
        Date                            : Gnoga.Gui.Element.Form.Date_Type;
        Time                            : Gnoga.Gui.Element.Form.Time_Type;
       end record;

   overriding
   procedure Create  (Report  : in out Observation_Report;
                      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
                      Content : in     String  := "";
                      ID      : in     String  := "" );

    overriding
    procedure Add_Self(
       Self    : in     Observation_Report;
       Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class
      );

    Function Report( Data   : JSON.Instance'Class ) return Report_Map.Map;
    Function Report( Object : Report_Map.Map      ) return String;
Private
    Overriding Function Get_Name( Self : Observation_Report ) return String;
End NSO.Types.Report_Objects.Observation_Report;

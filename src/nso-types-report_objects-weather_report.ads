With
NSO.JSON,
Ada.Containers.Ordered_Maps,
Ada.Containers.Indefinite_Ordered_Maps,
Ada.Calendar,
Gnoga.Gui.Base,
Gnoga.Gui.Element.Form,
Gnoga.Gui.Element.Common;

Limited with
Report_Form;

Package NSO.Types.Report_Objects.Weather_Report with Elaborate_Body is

    -----------------
    --  CONSTANTS  --
    -----------------

    Min_See  : Constant :=   1;
    Max_See  : Constant :=   8;
    Min_Wind : Constant :=   0;
    Max_Wind : Constant :=  80;
    Min_Temp : Constant := -40;
    Max_Temp : Constant := 120;


    ----------------------
    --  INTERNAL TYPES  --
    ----------------------

    Type Field is (Wind_Speed, Temperature, Seeing, Conditions, Wind_Direction);
    Function "+"(Item : Field) return String;
    Function "+"(Item : String) return Field;

    Type Wind_Range is range Min_Wind..Max_Wind;
    Function "-"( Input : String ) Return Wind_Range;
    Function "+"( Input : Wind_Range ) Return String;
    Type Temp_Range is range Min_Temp..Max_Temp;
    Function "-"( Input : String ) Return Temp_Range;
    Function "+"( Input : Temp_Range ) Return String;
    Type See_Range  is range Min_See..Max_See;
    Function "-"( Input : String ) Return See_Range;
    Function "+"( Input : See_Range ) Return String;


    -------------------
    --  REPORT DATA  --
    -------------------

    Type Report_Data is record
        Wind_Speed     : Wind_Range:= Max_Wind;
        Temperature    : Temp_Range:= Max_Temp;
        Seeing         : See_Range := Max_See;
        Conditions     : Sky       := Sky'Last;
        Wind_Direction : Direction := Direction'Last;
        Initalized     : Bit_Vector(Natural'Pred(1)..Natural'Pred(5)):=
          (Others => False);
    End record;

    ------------------
    --  REPORT MAP  --
    ------------------

    Package Report_Map is new Ada.Containers.Indefinite_Ordered_Maps(
       --"="          => ,
       "<"          => Ada.Calendar."<",
       Key_Type     => Ada.Calendar.Time,
       Element_Type => Report_Data
      );


    ------------------------------------
    --  WEATHER REPORT GNOGA ELEMENT  --
    ------------------------------------

    Type Weather_Report is new Abstract_Report with record
      --Gnoga.Gui.Element.Common.DIV_Type with
--         record
        Date                            : Gnoga.Gui.Element.Form.Date_Type;
        Time                            : Gnoga.Gui.Element.Form.Time_Type;
        Wind_Speed, Temperature, Seeing : Gnoga.Gui.Element.Form.Number_Type;
        Conditions, Wind_Direction      : Gnoga.Gui.Element.Form.Selection_Type;
       end record;

   overriding
   procedure Create  (Report  : in out Weather_Report;
                      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
                      Content : in     String  := "";
                      ID      : in     String  := "" );

    overriding
    procedure Add_Self(
       Self    : in     Weather_Report;
       Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class
      );

--      procedure Add_To_Form(
--         Self    : in     Weather_Report'Class;
--         Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class
--        );

    Function Report( Object : in Weather_Report ) return String;
    Function Report( Object : in Weather_Report ) return Reports;
    Function Report( Data   : in NSO.JSON.Instance'Class ) return Report_Map.Map;
    --Function Report( Cursor : Report_Map.Cursor ) return String;
    Function Report( Object : Report_Map.Map ) return String;
Private
    Overriding Function Get_Name( Self : Weather_Report ) return String;


    Function Report( Object : in Weather_Report ) return Reports is
       (NSO.Types.Weather);
    Function Report( Object : in Weather_Report ) return String is
       ( Reports'Image( Report(Object) ) );

    Function "+"(Item : Field) return String renames Field'Image;
    Function "+"(Item : String) return Field renames Field'Value;

    Function "-"(Input : String) Return Wind_Range is (Wind_Range'Value(Input));
    Function "-"(Input : String) Return Temp_Range is (Temp_Range'Value(Input));
    Function "-"(Input : String) Return See_Range  is (See_Range'Value(Input));

    Function "+"( Input : Wind_Range ) Return String is (Wind_Range'Image(Input));
    Function "+"( Input : Temp_Range ) Return String is (Temp_Range'image(Input));
    Function "+"( Input : See_Range  ) Return String is (See_Range'Image(Input));
End NSO.Types.Report_Objects.Weather_Report;

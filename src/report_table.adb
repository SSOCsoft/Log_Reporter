with
NSO.JSON.Gnoga_Object,
NSO.JSON.Parameters_to_JSON,
NSO.Types.Report_Objects.Observation_Report,
NSO.Types.Report_Objects.Weather_Report,

Ada.Strings.Unbounded,

Gnoga.Types,
Gnoga.Gui.View,
Gnoga.Gui.Window,
Gnoga.Gui.Element.Table;

Function Report_Table
  (
   Parent      : in out Gnoga.Gui.View.View_Type;
   Main_Window : in     Gnoga.Gui.Window.Window_Type'Class;
   Parameters  : in out Gnoga.Types.Data_Map_Type
  )
  return Gnoga.Gui.Element.Table.Table_Access is

    Function Reformat(Input : String) return String is
        Use Ada.Strings.Unbounded;
        Function "*"(Left: Natural; Right : String) return String is
          (case Left is
               when 0 => "",
               when 1 => Right,
               when 2 => Right & Right,
               when others => ((Left rem 2) * Right) &
                 (2 * ((Left/2)*Right))
          );
        HTML_TAB  : Constant String:= 8 * "&nbsp;";
        HTML_CRLF : Constant String:= "<br />";
        Working   : Unbounded_String;
    Begin
        For Index in Input'Range loop
            Append(New_Item =>
                 (case Input(Index) is
                      when ASCII.HT => HTML_TAB,
                      when ASCII.CR => HTML_CRLF,
                      when others   => (1 => Input(Index))
                 ),
               Source => Working
              );
        End loop;

        return Result : Constant String:= To_String(Working);
    End Reformat;


    Procedure Create_Row(
       Left, Right : in     String;
       Table       : in out Gnoga.Gui.Element.Table.Table_Type'Class ) is
        use Gnoga.Gui.Element.Table;
        row  : constant Table_Row_Access    := new Table_Row_Type;
        col1 : constant Table_Column_Access := new Table_Column_Type;
        col2 : constant Table_Column_Access := new Table_Column_Type;
    Begin
        row.Dynamic;
        col1.Dynamic;
        col2.Dynamic;

        row.Create (Table);
        col1.Create (row.all, Left );
        col2.Create (row.all, Right);
    End Create_Row;

    procedure Create_Title(
       Title  : in     String;
       Table  : in out Gnoga.Gui.Element.Table.Table_Type'Class ) is
        use Gnoga.Gui.Element.Table;
        Head : constant Table_Header_Access := new Table_Header_Type;
        Row  : constant Table_Row_Access    := new Table_Row_Type;
        Col  : constant Table_Heading_Access:= new Table_Heading_Type;
    begin
        head.Dynamic;
        row.Dynamic;
        col.Dynamic;

        head.Create(Table);
        row.Create (Head.all);
        col.Create (row.all, Title, Column_Span => 2 );
    end Create_Title;

    ---------------------------------------------
    -- GET PARAMETER-DATA AND GENERATE REPORTS --
    ---------------------------------------------

    Package GO renames NSO.JSON.Gnoga_Object;
    Package RO renames NSO.Types.Report_Objects;

    Generic
        Parameter_Object : NSO.JSON.Instance'Class;
    Function Bind_Parameters(O : GO.Form_Object) return NSO.JSON.Instance'Class;
    Function Bind_Parameters(O : GO.Form_Object) return NSO.JSON.Instance'Class is
        ( Parameter_Object );

    Function Bound_Parameters is new Bind_Parameters(
       NSO.JSON.Parameters_to_JSON(Parameters)
      );

    Package Reports is new GO.Generic_Reporter(
       Form       => NSO.JSON.Gnoga_Object.Form_Object,
       Params     => NSO.JSON.Gnoga_Object.JSON_Class,
       Parameters => Bound_Parameters, --NSO.JSON.Gnoga_Object.Parameters,
       This       => NSO.JSON.Gnoga_Object.As_Form(Main_Window)
      );

    Function Observation_Table is new Reports.Generate_Report(
       Report     => RO.Observation_Report.Report_Map.Map,
       View       => String,
       Get_Report => RO.Observation_Report.Report,
       Processing => RO.Observation_Report.Report
      );

    Function Weather_Table is new Reports.Generate_Report(
       Report     => RO.Weather_Report.Report_Map.Map,
       View       => String,
       Get_Report => RO.Weather_Report.Report,
       Processing => RO.Weather_Report.Report
      );


    -- Here we are renaming the parameters recieved from the form as an instance
    -- of the JSON class, under the name "Parameter" so that we might select a
    -- parameter via the "**" operator.
    Form_Parameter: NSO.JSON.Instance'Class renames Reports.Original_Parameters;

    -- Here we import the "**" operator, the right-hand side is a string and it
    -- acts as a safe-indexing returning the empty string when the left-hand is
    -- either not an Object or else it does not contain the given string.
    Use all type NSO.JSON.Instance'Class;
        Message    : Constant String:= Form_Parameter ** "Message";
        Observer   : Constant String:= Form_Parameter ** "Observer";
        Name       : Constant String:= Form_Parameter ** "Name";
        Conditions : Constant String:= Form_Parameter ** "Conditions";

    Report_Title   : Constant String:= "Daily Log Report";

    -- Create the result, and a renaming for the dereference thereof.
    Result : Constant Gnoga.Gui.Element.Table.Table_Access:=
                  new Gnoga.Gui.Element.Table.Table_Type;
    Table  : Gnoga.Gui.Element.Table.Table_Type renames Result.All;
Begin
        Table.Dynamic;
        Table.Create( Parent );
--Ada.Text_IO.Put_Line( Form_Parameter.To_String );
        ---------------------
        -- STYLE THE TABLE --
        ---------------------

        Table.Style(Name => "width",        Value => "50%");
        Table.Style(Name => "margin",       Value => "auto");
        Table.Style(Name => "clear",        Value => "both");
        Table.Style(Name => "position",     Value => "absolute");
        Table.Style(Name => "top",          Value => "53%");
        Table.Style(Name => "left",         Value => "50%");
        Table.Style(Name => "margin-right", Value => "-50%");
        Table.Style(Name => "transform",    Value => "translate(-50%, -50%)");
        Table.Border(Width => "thick");

        -----------------
        --  ADD CELLS  --
        -----------------

        Create_Title(		Report_Title,				Table );
--        Create_Row( "Name",			Name,			Table );
        Create_Row( "Observer",			Observer,		Table );
        Create_Row( "Message",			Reformat(Message),	Table );
--        Create_Row( "Conditions",		Conditions,		Table );
        Create_Row( "Weather Report",		Weather_Table,		Table );
        Create_Row( "Observation Report",	Observation_Table,	Table );

    Return Result;
End Report_Table;

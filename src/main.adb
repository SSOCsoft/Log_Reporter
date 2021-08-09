with
Ada.Exceptions.Traceback, System.Address_Image,
Gnoga.Application.Multi_Connect,
Gnoga.Gui.Base,
Gnoga.Gui.Window,
Gnoga.Gui.View,
Gnoga.Gui.Element.Common,
Gnoga.Gui.Element.Form,

Gnoga.Types,


Gnoga.Gui.Document,
Gnoga.Gui.Element.Table,

NSO.JSON.Gnoga_Object,
NSO.Types,
INI.Parameters,
--  INI.Print, INI.Section_to_Map,
Config,
Report_Form,
Report_Table,
Send_Report,
Ada.Containers.Indefinite_Ordered_Maps,
Ada.Text_IO.Text_Streams,
Ada.Strings.Fixed
;

WITH Ada.Containers.Indefinite_Multiway_Trees, Ada.Strings.Equal_Case_Insensitive;
--WITH NSO.JSON.Parameters_to_JSON;
WITH Gnoga.Server.Connection;
with NSO.Types.Report_Objects;
with NSO.Types.Report_Objects.Observation_Report, NSO.Types.Report_Objects.Weather_Report;
with Ada.Strings.Unbounded;
with Ada.Strings;
procedure Main is
    use Report_Form;

    DEBUGGING  : Constant Boolean := False;

    ID_For_Email : Positive:= Positive'First;
   --  Since this application will be used by multiple connections, we need to
   --  track and access that connection's specific data. To do this we create
   --  a derivative of Gnoga.Types.Connection_Data_Type that will be accessible
   --  to any object on a connection.



   type App_Data is new Gnoga.Types.Connection_Data_Type with
      record
         My_View   : Gnoga.Gui.View.View_Type;
         My_Widget : My_Widget_Type;
--           My_Done,
         My_Exit   : Gnoga.Gui.Element.Common.Button_Type;
      end record;
   type App_Access is access all App_Data;

   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Application event handlers

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type);
   --  Setup GUI for each connection.

   procedure On_Result_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type);
   --  Setup another path in to the application for submitting results
   --  /result, see On_Connect_Handler in body of this procedure.


   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.My_View.New_Line;
      App.My_View.Put_Line ("Closing application and every connection!");

      App.My_Exit.Disabled;

      Gnoga.Application.Multi_Connect.End_Application;
   end On_Exit;


   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      App : constant App_Access := new App_Data;
   begin
      Main_Window.Connection_Data (App);
      App.My_View.Create (Main_Window);

--          App.My_Done.Create(App.My_View);
--          App.My_Done.Text("DONE.");
--          App.My_Done.On_Click_Handler( On_Done'Unrestricted_Access );

      App.My_Exit.Create (App.My_View, "Exit Application");
      App.My_Exit.On_Click_Handler (On_Exit'Unrestricted_Access);

      App.My_View.Horizontal_Rule;

      App.My_Widget.Create (App.My_View);
      App.My_Widget.Widget_Form.Action ("/result");
   end On_Connect;

    Last_Parameters : Gnoga.Types.Data_Map_Type;
    procedure On_Post (URI        : String;
                       Parameters : in out Gnoga.Types.Data_Map_Type) is
        Package DM renames Gnoga.Types.Data_Maps;
    begin
--        Last_Parameters.Assign( Parameters );
--            := Parameters;
        for C in Parameters.Iterate loop
            Declare
                -- Creates a sequence of values, seperated by Unit_Seperator.
                Function Replacement(Key, New_Part: String) return String is
                    Unit_Separator : Constant Character := Character'Val( 31 );
                    Current        : Constant String := Last_Parameters(Key);
                Begin
                    Return Result : Constant String :=
                      Current & Unit_Separator & New_Part;
                End Replacement;

                Function "="(Left, Right : String) return Boolean
                  renames Ada.Strings.Equal_Case_Insensitive;

                Key     : String  renames DM.Key(C);
                Element : String  renames DM.Element(C);
                Has_Key : Boolean renames Last_Parameters.Contains( Key );
            Begin
                if not Has_Key then
                    Last_Parameters.Include(Key, New_Item => Element);
                else
                    Last_Parameters.Replace(
                       New_Item => Replacement(Key, Element),
                       Key      => Key
                      );
                end if;
                Ada.Text_IO.Put_Line (":::" & Key & " = " & Element);
            End;
        end loop;
    end On_Post;


    procedure On_Post_Request
      (URI                 : in String;
       Accepted_Parameters : out Ada.Strings.Unbounded.Unbounded_String
      ) is
        Function "+"(Right : String) return Ada.Strings.Unbounded.Unbounded_String
          renames Ada.Strings.Unbounded.To_Unbounded_String;
    begin
      Accepted_Parameters:= --Ada.Strings.Unbounded.Null_Unbounded_String;
          +"Message";
    end On_Post_Request;

    procedure On_Post_File
      (URI       : in String;
       File_Name : in String;
       Temp_Name : in String
      ) is null;

   procedure On_Result_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is

        Function "*"(Left : Character; Right : Natural) Return String is
            ( String'(1..Right => Left) );

        --On_Post_Request_Handler
        --Post_Request_Event

--          P_List       : Gnoga.Types.Data_Map_Type renames Last_Parameters; --:= Gnoga.Types.Data_Maps.Empty_Map;
        EMail_View   : Gnoga.Gui.View.View_Access:= new Gnoga.Gui.View.View_Type;
--          WIN  : Gnoga.Gui.Base.Pointer_To_Base_Class renames Main_Window.Get_View;
--          VIEW : Gnoga.Gui.View.View_Type renames
--            Gnoga.Gui.View.View_Access'(
--               if WIN in Null
--               then new Gnoga.Gui.View.View_Type
--               else     Gnoga.Gui.View.View_Access(Main_Window.Get_View)
--              ).all;

--          Function Parameters_to_JSON( Input : in out Gnoga.Types.Data_Map_Type ) return NSO.JSON.Instance'Class
--            renames NSO.JSON.Parameters_to_JSON;

--          Function Parse_Text_Map( Input : in out Gnoga.Types.Data_Map_Type ) return NSO.JSON.Instance'Class is
--              Use NSO.JSON;
--
--              Function Handle_Value( Value: String ) return Instance'Class is
--                  Unit_Separator : Constant Character := Character'Val( 31 );
--                  US_STR         : Constant String    := (1 => Unit_Separator);
--                  First_Index    : Natural := Value'First;
--                  Last_Index     : Natural := Ada.Strings.Fixed.Index(
--                     Pattern => US_STR,
--                     Source  => Value,
--                     From    => First_Index
--                    );
--              Begin
--                  if Last_Index not in Positive then
--                      Return Make( Value );
--                  else
--                      Return Result : Array_Object := Array_Object(Make_Array) do
--                          COLLECT_ELEMENTS:
--                          loop
--                              declare
--                                  Marker : Constant Natural:=
--                                    (if Last_Index in Positive
--                                     then Positive'Pred(Last_Index)
--                                     else Value'last
--                                    );
--                                  Subtype Slice is Positive range
--                                    First_Index..Marker;
--                                  Data : String renames Value(Slice);
--                              begin
--                                  Array_Object(Result).Append( Make(Data) );
--                                  exit COLLECT_ELEMENTS when Slice'Last = Value'Last;
--                                  First_Index:= Last_Index + US_STR'Length;
--                              end;
--                              Last_Index := Ada.Strings.Fixed.Index(
--                                 Pattern => US_STR,
--                                 Source  => Value,
--                                 From    => First_Index
--                                );
--                          end loop COLLECT_ELEMENTS;
--                      End return;
--                  end if;
--              End Handle_Value;
--
--              Function New_Object return Object_Object is
--                  ( Object_Object(Make_Object) ) with Inline;
--          Begin
--              Return Result : Object_Object := New_Object do
--                  For Item in Input.Iterate loop
--                      Declare
--                          Function Parse_Key( Key, Value : String ) return NSO.JSON.Object_Object is
--
--                              Function Last_Index(Bracket : Character) return Natural is
--                                (Ada.Strings.Fixed.Index(
--                                   Source  => Key,
--                                   Pattern => (1 => Bracket),
--                                   From    => Key'Last,
--                                   Going   => Ada.Strings.Backward
--                                  )
--                                );
--
--                              Function First_Index(Bracket : Character) return Natural is
--                                (Ada.Strings.Fixed.Index(
--                                   Source  => Key,
--                                   Pattern => (1 => Bracket),
--                                   From    => Key'First,
--                                   Going   => Ada.Strings.Forward
--                                  )
--                                );
--
--                              Last_Close : Natural renames First_Index(']');
--                              Last_Open  : Natural renames First_Index('[');
--
--                          Begin
--                              if    Last_Open = Last_Close then -- only when both don't exist.
--                                  Return Result : Object_Object := New_Object do
--                                      Result.Value(
--                                         Name   => Key,
--                                         Value  => Handle_Value( Value )
--                                        );
--                                      Ada.Text_IO.Put_Line( "===> " & (Result.To_String) & Value );
--                                  end return;
--                              elsif Last_Open < Last_Close then
--                                  Return Result : Object_Object := New_Object do
--                                      Result.Value(
--                                         Name  => Key(Natural'Succ(Last_Open)..Natural'Pred(Last_Close)),
--                                         Value => Parse_Key(Key => Key(Key'First..Natural'Pred(Last_Open)), Value => Value)
--                                        );
--                                  End return;
--                              else
--                                  raise NSO.JSON.Parse_Error with "Bad indexing: '"
--                                    & Key & "'.";
--                              end if;
--                          End Parse_Key;
--
--                          K : String renames Gnoga.Types.Data_Maps.Key(Item);
--                          V : String renames Gnoga.Types.Data_Maps.Element(Item);
--                          O : Object_Object renames Parse_Key(K, V);
--                      Begin
--                        Result.Include( O );
--                      End;
--                  end loop;
--
--                  -- Clear the parameters.
--                  Last_Parameters.Clear;
--              End return;
--          End Parse_Text_Map;

    Begin
--          View.Dynamic;
--          Result_View.Create(Main_Window);

        EMail_View.Dynamic;
        EMail_View.Create( Main_Window );

--          EMail_View.Put_Line( '-' * 80 );
--          EMail_View.Put_Line( "Message = " & Main_Window.Form_Parameter("Message") );
--          --On_Post( Main_Window.Location.URL, P_List );
--          --EMail_View.Put_Line( NSO.JSON.To_String( Parse_Text_Map(P_List) ));
--          for C in P_List.Iterate loop
--              EMail_View.Put_Line(
--                 Gnoga.Types.Data_Maps.Key (C) & " = '" &
--                 Gnoga.Types.Data_Maps.Element (C) & '''
--                );
--          end loop;
--          EMail_View.Put_Line( '-' * 80 );
--          --EMail_View.Put_Line( Parse_Text_Map(P_List).To_String );
--          EMail_View.Put_Line( '-' * 80 );
--
--          EMail_View.Put_Line(Parameters_to_JSON( P_List ).To_String);

        EMAIL:
        Declare
            Table        : Gnoga.Gui.Element.Table.Table_Type renames
              Report_Table( EMail_View.all, Main_Window, Last_Parameters ).All;
        Begin

            --Get_CGI_Key

                Send_report
                  (EMail_View.Outer_HTML
                   --EMail_Doc.Document_Element.Outer_HTML
--                       EMail_Doc.Head_Element.HTML_Tag &
--                      (ASCII.CR, ASCII.LF)            &
--                      EMail_Doc.Body_Element.HTML_Tag
                  );
        End EMAIL;

        -- Clear the parameters.
        Last_Parameters.Clear;
    End On_Result_Connect;


--     procedure On_Result_Connect
--       (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
--        Connection  : access
--          Gnoga.Application.Multi_Connect.Connection_Holder_Type)
--     is
--  --        pragma Unreferenced (Connection);
--  --        --  Since there will be no interactions with page once displayed there
--  --        --  is no need to setup any data to associate with the main window.
--  --
--                App : constant App_Access := App_Access(Main_Window.Connection_Data);
--        Result_View : constant Gnoga.Gui.View.View_Access :=
--          new Gnoga.Gui.View.View_Type;
--     begin
--        Result_View.Dynamic;
--        --  By marking the View dynamic it will be deallocated by Main_Window
--        --  when it finalizes.
--        Result_View.Create (Main_Window);
--          --App.My_Done.Place_Inside_Bottom_Of( Element.Class(Main_Window) );
--
--          FORM_DATA:
--          Declare
--              k : Constant String := "K!";
--              Name       : String renames Main_Window.Form_Parameter ("Name");
--              Message    : String renames Main_Window.Form_Parameter ("Message");
--              Conditions : String renames Main_Window.Form_Parameter ("Conditions");
--              Observer   : String renames Main_Window.Form_Parameter ("Observer");
--              CRLF : Constant String := (ASCII.CR, ASCII.LF);
--          Begin
--  --              Ada.Text_IO.Put_Line( (1..20 => ':') &
--  --              Gnoga.Server.Connection.Execute_Script(
--  --                 ID     => Main_Window.Connection_ID,
--  --                 Script => --"params"
--  --                           "var alertText = ' '; "            & --CRLF &
--  --                           "for (property in params) {"       & --CRLF &
--  --                           "   alertText += property + ':' "  &
--  --                                "+ params[property]+'; ';"   & --CRLF &
--  --                           "} "                               & --CRLF &
--  --                           "params.toSource();"                & --CRLF &
--  --                             ""
--  --
--  --  --"''+params;"
--  --  --               "params['" & Name & "'];"
--  --                ));
--
--  --------------------------------------------------------------------------------
--  --------------------------------------------------------------------------------
--  --------------------------------------------------------------------------------
--
--  --              FORM_PARAMETER_TEST:
--  --              Declare
--  --                  Use NSO.Types;
--  --
--  --                  Package Parameter_List renames NSO.Types.String_Map;
--  --  --                  is new Ada.Containers.Indefinite_Ordered_Maps(
--  --  --  --                     "<"          => ,
--  --  --  --                     "="          => ,
--  --  --                     Key_Type     => String,
--  --  --                     Element_Type => String
--  --  --                    );
--  --
--  --                  Function Get_Parameters(Object : NSO.JSON.Instance'Class) return Parameter_List.Map is
--  --                      Use Parameter_List, NSO.JSON;
--  --                      Temp : Instance'Class := Object;
--  --                  Begin
--  --                      Return Result : Map := Empty_Map do
--  --                          Declare
--  --                              Procedure Add_Item(Key : String; Value : NSO.JSON.Instance'Class) is
--  --                              begin
--  --                                  Result.Include(New_Item => -Value, Key => Key);
--  --                              end Add_Item;
--  --
--  --                              Procedure Do_It is new NSO.JSON.Apply
--  --                                ( On_Object => Add_Item );
--  --                          Begin
--  --                              Do_It( Temp );
--  --                          End;
--  --                      End return;
--  --                  End Get_Parameters;
--  --
--  --
--  --                  FP : String renames
--  --                    Gnoga.Server.Connection.Execute_Script(
--  --                       ID     => Main_Window.Connection_ID,
--  --                       Script => "JSON.stringify( params )"-- "params.toSource();"
--  --                      );
--  --                  Params : String renames FP;
--  --  --                    FP( Positive'Succ(FP'First)..Positive'Pred(FP'Last) );
--  --                  PS : Aliased String_Stream:= +(Params);
--  --              Begin
--  --                  Ada.Text_IO.Put_Line((1..20 => ':') & Params & (1..20 => ':'));
--  --                  DECLARE
--  --                      Item : NSO.JSON.Instance'Class :=
--  --                        NSO.JSON.Instance'Class'Input( PS'Access );
--  --                      Data : NSO.Types.Report_Objects.Report_Map.Map
--  --                        renames NSO.Types.Report_Objects.Report(Data => Item);
--  --                      Procedure Print(Cursor : Parameter_List.Cursor) is
--  --                          Value : String renames Parameter_List.Element( Cursor );
--  --                          Key   : String renames Parameter_List.Key( Cursor );
--  --                      Begin
--  --                          Ada.Text_IO.Put_Line(Key & (' ',':',ASCII.HT) & Value);
--  --                      End Print;
--  --                  Begin
--  --                      Ada.Text_IO.Put_Line("JSON CREATED.");
--  --                      Get_Parameters(Item).Iterate( Print'Access );
--  --                      --                    Report_Objects.Report
--  --                      Ada.Text_IO.Put_Line( NSO.Types.Report_Objects.Report(Data) );
--  --
--  --  --                      For X in Data.Iterate loop
--  --  --                          Ada.Text_IO.Put_Line( NSO.Types.Report_Objects.Report(X) );
--  --  --                      End loop;
--  --
--  --                  End;
--  --
--  --              End FORM_PARAMETER_TEST;
--
--
--              Result_View.Put_Line ("Name : " & Name);
--              Result_View.Put_Line ("Message : " & Message);
--              Result_View.Put_Line ("Conditions : " & Conditions);
--
--              EMAIL:
--              Declare
--                  ---------------------------------------------
--                  -- GET PARAMETER-DATA AND GENERATE REPORTS --
--                  ---------------------------------------------
--                  Package GO renames NSO.JSON.Gnoga_Object;
--                  Package RO renames NSO.Types.Report_Objects;
--
--                  Function Shim_Parameters (O : NSO.JSON.Gnoga_Object.Form_Object)
--                    return NSO.JSON.Gnoga_Object.JSON_Class is
--                      (NSO.JSON.Gnoga_Object.Get_JSON(O));
--
--  --                  Function JSON_TO_HTML_Report is new NSO.JSON.Gnoga_Object.Report_View(
--  --                       Form       => NSO.JSON.Gnoga_Object.Form_Object,
--  --                       Params     => NSO.JSON.Gnoga_Object.JSON_Class,
--  --                       Report     => NSO.Types.Report_Objects.Report_Map.Map,
--  --                       View       => String,
--  --                       Submission => Shim_Parameters,
--  --                       Parameters => NSO.Types.Report_Objects.Report,
--  --                       Processing => NSO.Types.Report_Objects.Report
--  --                      );
--  --
--  --                  Function Observation_Report_TO_HTML is new NSO.JSON.Gnoga_Object.Report_View(
--  --                       Form       => NSO.JSON.Gnoga_Object.Form_Object,
--  --                       Params     => NSO.JSON.Gnoga_Object.JSON_Class,
--  --                       Report     => NSO.Types.Report_Objects.Observation_Report.Report_Map.Map,
--  --                       View       => String,
--  --                       Submission => NSO.JSON.Gnoga_Object.Parameters,
--  --                       Parameters => NSO.Types.Report_Objects.Observation_Report.Report,
--  --                       Processing => NSO.Types.Report_Objects.Observation_Report.Report
--  --                      );
--
--                  Package Reports is new NSO.JSON.Gnoga_Object.Generic_Reporter(
--                     Form       => NSO.JSON.Gnoga_Object.Form_Object,
--                     Params     => NSO.JSON.Gnoga_Object.JSON_Class,
--                     Parameters => NSO.JSON.Gnoga_Object.Parameters,
--                     This       => NSO.JSON.Gnoga_Object.As_Form(Main_Window)
--                    );
--
--                  Function Observation_Table is new Reports.Generate_Report(
--                     Report     => RO.Observation_Report.Report_Map.Map,
--                     View       => String,
--                     Get_Report => RO.Observation_Report.Report,
--                     Processing => RO.Observation_Report.Report
--                    );
--
--                  Function Weather_Table is new Reports.Generate_Report(
--                     Report     => RO.Weather_Report.Report_Map.Map,
--                     View       => String,
--                     Get_Report => RO.Weather_Report.Report,
--                     Processing => RO.Weather_Report.Report
--                    );
--
--                  Use NSO.Types;
--  --                  Parameter_Data  : String renames
--  --                    Gnoga.Server.Connection.Execute_Script(
--  --                       ID     => Main_Window.Connection_ID,
--  --                       Script => "JSON.stringify( params )"-- "params.toSource();"
--  --                      );
--  --                  Parameter_Stream: Aliased String_Stream:= +(Parameter_Data);
--  --                  Parameter_JSON  : NSO.JSON.Instance'Class :=
--  --                        NSO.JSON.Instance'Class'Input( Parameter_Stream'Access );
--  --                  Weather_Report  : NSO.Types.Report_Objects.Weather_Report.Report_Map.Map
--  --                     renames NSO.Types.Report_Objects.Weather_Report.Report(Parameter_JSON);
--  --  --                  Weather_Table   : String
--  --  --                     renames --NSO.Types.Report_Objects.Report(Weather_Report);
--  --  --                  JSON_TO_HTML_Report( NSO.JSON.Gnoga_Object.As_Form(Main_Window) );
--  --  --
--  --  --                  Observation_Table   : String
--  --  --                     renames
--  --  --                    Observation_Report_TO_HTML( NSO.JSON.Gnoga_Object.As_Form(Main_Window) );
--  --
--  --                  Insturment_Table : String := "";
--
--
--                  Procedure Create_Row(
--                     Left, Right : in     String;
--                     Table       : in out Gnoga.Gui.Element.Table.Table_Type'Class
--                    ) is
--                      use Gnoga.Gui.Element.Table;
--                      row  : constant Table_Row_Access    := new Table_Row_Type;
--                      col1 : constant Table_Column_Access := new Table_Column_Type;
--                      col2 : constant Table_Column_Access := new Table_Column_Type;
--                  Begin
--                      row.Dynamic;
--                      col1.Dynamic;
--                      col2.Dynamic;
--
--                      row.Create (Table);
--                      col1.Create (row.all, Left );
--                      col2.Create (row.all, Right);
--                  End Create_Row;
--
--                  procedure Create_Title(
--                     Title  : in     String;
--                     Table  : in out Gnoga.Gui.Element.Table.Table_Type'Class
--                    ) is
--                      use Gnoga.Gui.Element.Table;
--                      Head : constant Table_Header_Access := new Table_Header_Type;
--                      Row  : constant Table_Row_Access    := new Table_Row_Type;
--                      Col  : constant Table_Heading_Access:= new Table_Heading_Type;
--                  begin
--                      head.Dynamic;
--                      row.Dynamic;
--                      col.Dynamic;
--
--                      head.Create(Table);
--                      row.Create (Head.all);
--                      col.Create (row.all, Title, Column_Span => 2 );
--                  end;
--
--                  EMail_Window : Gnoga.Gui.Window.Window_Type;
--                  EMail_View   : Gnoga.Gui.View.View_Access:= new Gnoga.Gui.View.View_Type;
--                  EMail_Doc    : Gnoga.Gui.Document.Document_Access:= new Gnoga.Gui.Document.Document_Type;
--                  Table        : Gnoga.Gui.Element.Table.Table_Type;
--                  Done_Button  : Gnoga.Gui.Element.Common.Button_Type;
--  --                  renames App_Access (Main_Window.Connection_Data).My_Done;
--              Begin
--                  EMail_View.Dynamic;
--                  Table.Dynamic;
--  --                  Ada.Text_IO.Put_Line(
--  --                     --NSO.JSON.Gnoga_Object.As_Form(Main_Window).Parameters.To_String
--  --                     Observation_Report_TO_HTML( NSO.JSON.Gnoga_Object.As_Form(Main_Window) )
--  --                    );
--
--                  TRY_CATCHING_INTERMITTENT_ERROR:
--                  Declare
--                      Function Make_ID( Number : Positive ) return String is
--                          Package IT is new Ada.Text_IO.Integer_IO( Positive );
--                          Maximum : Constant := 8;
--                          Temp    : String(1..12);
--                      Begin
--                          IT.Put(To => Temp, Item => Number, Base => 16);
--
--                          EXTRACT_HEX:
--                          Declare
--                              Use Ada.Strings, Ada.Strings.Fixed;
--                              Start : Natural renames Natural'Succ(Index(Temp, "#", Forward));
--                              Stop  : Natural renames Natural'Pred(Index(Temp, "#", Backward));
--                              Value : String  renames Temp(Start..Stop);
--                          Begin
--                              Return Result : String (1..Maximum):=
--                                (1..Maximum - Value'Length => '0') & Value do
--                                  ID_For_Email:= Integer'Succ(ID_For_Email);
--                              End return;
--                          End EXTRACT_HEX;
--                      End Make_ID;
--
--                      Raw_ID : Constant String := Make_ID( ID_For_Email );
--                  Begin
--                      -- Manually setting a run-time ID does not fix the intermittent errors.
--                      -- I get STORAGE_ERROR, PROGRAM_ERROR (adjust/finalize raising),
--                      -- GNOGA.SERVER.CONNECTION.SCRIPT_ERROR, GNOGA.SERVER.CONNECTION.CONNECTION_ERROR,
--                      -- and "CONSTRAINT_ERROR : bad input for 'Value: "undefined""
--                      -- Why?
--                      EMail_View.Create( Main_Window,    ID => "EMAIL" & Raw_ID );
--                      Table.Create     ( EMail_View.all, ID => "TABLE" & Raw_ID );
--                  Exception
--                      when E : Others =>
--                          Ada.Text_IO.Put_Line( "EXCEPTION HAPPENED!!" );
--                          Ada.Text_IO.Put_Line("Name: "& Ada.Exceptions.Exception_Name(E));
--                          Ada.Text_IO.Put_Line("Info: "& Ada.Exceptions.Exception_Information(E));
--                          PRINT_TRACE:
--                          For K of Ada.Exceptions.Traceback.Tracebacks(E) loop
--                              Ada.Text_IO.Put(' ' & System.Address_Image(
--                                   Ada.Exceptions.Traceback.STBE.PC_For(K)
--                                 )
--                                );
--                          End loop PRINT_TRACE;
--                  End TRY_CATCHING_INTERMITTENT_ERROR;
--
--
--                  Table.Style(Name => "width",        Value => "50%");
--                  Table.Style(Name => "margin",       Value => "auto");
--                  Table.Style(Name => "clear",        Value => "both");
--                  Table.Style(Name => "position",     Value => "absolute");
--                  Table.Style(Name => "top",          Value => "53%");
--                  Table.Style(Name => "left",         Value => "50%");
--                  Table.Style(Name => "margin-right", Value => "-50%");
--                  Table.Style(Name => "transform",    Value => "translate(-50%, -50%)");
--                  Table.Border(Width => "thick");
--
--                  Create_Title( "Observation Report",          Table );
--                  Create_Row( "Name",       Name,              Table );
--                  Create_Row( "Observer",   Observer,          Table );
--                  Create_Row( "Message",    Message,           Table );
--                  Create_Row( "Conditions", Conditions,        Table );
--                  Create_Row( "Weather Report", Weather_Table, Table );
--                  Create_Row( "Observation Report", Observation_Table, Table );
--
--                  Send_report
--                    (EMail_View.Outer_HTML
--                     --EMail_Doc.Document_Element.Outer_HTML
--  --                       EMail_Doc.Head_Element.HTML_Tag &
--  --                      (ASCII.CR, ASCII.LF)            &
--  --                      EMail_Doc.Body_Element.HTML_Tag
--                    );
--              End EMAIL;
--          end FORM_DATA;
--
--
--      end On_Result_Connect;

    use all type Config.Pascal_String;
begin
--      INI.Print( INI.Parameters.Parameters );
--      for X in INI.Section_to_Map(INI.Parameters.Parameters, "ROSA").Iterate loop
--          Ada.Text_IO.Put_Line( NSO.Types.String_Map.Key(X) & ' '
--                              & NSO.Types.String_Map.Element(X) );
--      end loop;


   Gnoga.Application.Title ("NSO/SP Report Tool");

   Gnoga.Application.HTML_On_Close ("Application ended.");

   Gnoga.Application.Multi_Connect.Initialize;

   Gnoga.Application.Multi_Connect.On_Connect_Handler
     (Event => On_Connect'Unrestricted_Access,
      Path  => "default");

   Gnoga.Application.Multi_Connect.On_Connect_Handler
     (Event => On_Result_Connect'Unrestricted_Access,
      Path  => "result");

 Gnoga.Server.Connection.On_Post_Handler         (On_Post'Unrestricted_Access);
-- Gnoga.Server.Connection.On_Post_Request_Handler (On_Post_Request'Unrestricted_Access);
--     Gnoga.Server.Connection.On_Post_File_Handler
--       (On_Post_File'Unrestricted_Access);


   Gnoga.Application.Multi_Connect.Message_Loop;
   --  With a Multi_Connect application it is possible to have different
   --  URL paths start different Connection Event Handlers. This allows
   --  for the creation of Web Apps that appear as larger web sites or as
   --  multiple applications to the user. Setting Path to "default" means
   --  that any unmatched URL path will start that event handler. A URL path
   --  is the path following the host and port. For example:
   --  http://localhost:8080/test/me
   --  The Path would be "/test/me". In Gnoga the path can even appear to be
   --  a file name "/test/me.html". However if you have a file of the same
   --  name in the html directory it will be served instead.

   --Gnoga.Application.Multi_Connect.Message_Loop;

--      Ada.Text_IO.Put_Line ("Counter : " & Natural'Image (Config.DL));
--      Ada.Text_IO.Put_Line ("Message : " & (+Config.Host));
--        Config.DL := Config.DL + 1;
--        --Config.Host ( "HOST" );
--

--      Ada.Text_IO.Put_Line( "HOST:" & (+Config.Host)  );
--      Ada.Text_IO.Put_Line( "USER:" & (+Config.User)  );
--      Ada.Text_IO.Put_Line( "PASS:" & (+Config.Password)  );

    Ada.Text_IO.Put_Line("Done.");
end Main;

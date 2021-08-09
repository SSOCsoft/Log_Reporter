Pragma Ada_2012;

With
Gnoga.Gui.Base
;

Package NSO.JSON.Gnoga_Object is --with Elaborate_Body is
    Use Gnoga.Gui.Base;

    Subtype JSON_Class is NSO.JSON.Instance'Class;

    -- Returns the JSON format of the form parameters from the given object.
    -- NOTE: This method does NOT work with multi-select selections; when
    --       such is attempted, later values overwrite the former values.
    Function Get_JSON( Object : Base_Type'Class ) return JSON_Class;


    Type Form_Object is new Base_Type with private;
    Function As_Form    (Object : Base_Type'Class) return Form_Object;
    Function Parameters (Object : Form_Object) return JSON_Class;


    -- REPORT_VIEW
    -- This function ties together four types:
    --  (1) A Form-type, which is the ultimate data-source;
    --  (2) The Params-type, which represents the data submitted by the form;
    --  (3) The Report-type, which is the filtered/processed Params; and
    --  (4) The View-type, which is the rendering-media for the report.
    --
    -- As an example, consider the situation of the daily-log, within this setup
    -- the form has Weather-reports which must be processed (filtered/colated)
    -- into an actual report. After this, the report is processed into HTML and
    -- sent in an email.
    --
    -- Process: [HTML_FORM] -> [PARAMETERS] -> [REPORT_OBJECT] -> [HTML_VIEW].
    Generic
        Type Form  (<>) is new Form_Object with private;
        Type Params(<>) is limited private;
        Type Report(<>) is limited private;
        Type View  (<>) is limited private;

        -- Retrieve the submitted form data.
        with Function Submission(Object : Form  ) return Params is <>;

        -- Filter data and [re]construct the report.
        with Function Parameters(Object : Params) return Report is <>;

        -- Transform the report-data into some viewable data.
        with Function Processing(Object : Report) return View   is <>;
    Function Report_View (Object : Form_Object'Class) Return View;





    -- GENERIC_REPORTER
    --
    Generic
        Type Form  (<>) is new Form_Object with private;
        Type Params(<>) is limited private;

        -- Retrieve the submitted form data.
        with Function Parameters(Object : Form)   return Params is <>;

        This : in     Form_Object'Class;
    Package Generic_Reporter is

        Original_Parameters : Constant Params := Parameters( Form(This) );

        Generic
            -- The particular report we are getting.
            Type Report(<>) is limited private;

            -- The type for the final display.
            Type View  (<>) is limited private;

            -- Filter the data and [re]construct the report.
            with Function Get_Report(Object : Params:= Original_Parameters) return Report   is <>;

            -- Transform the report-data into some viewable data.
            with Function Processing(Object : Report:= Get_Report) return View is <>;
        Function Generate_Report return View;

    End Generic_Reporter;


Private

    Type Form_Object is new Base_Type with null record;

End NSO.JSON.Gnoga_Object;

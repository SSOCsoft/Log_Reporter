With
NSO.Types.Report_Objects.Observation_Report,
NSO.Types.Report_Objects.Weather_Report,
Gnoga.Gui.Element.Form,
Gnoga.Gui.Element.Common,
Gnoga.Gui.Base,
Gnoga.Gui.View;

-------------------
--  REPORT_FORM  --
--------------------------------------------------------------------------------
-- This package defines the main form through which the reports are generated --
-- and sent. The type My_Widget_Type is a Gnoga view and defines the elements --
-- of the page. Its components are:                                           --
--   * OBSERVATION_DIV: The <DIV> containing an observation-report's data.    --
--   * WEATHER_DIV: The <DIV> containing a weather-report's data.             --
--   * WIDGET_FORM: The <FORM> by which data is submitted.                    --
--   * NAME_INPUT:                                                            --
--   * STAT_INPUT:                                                            --
--   * OPER_INPUT: The drop-down for selecting the OPERATOR.                  --
--   * MESSAGE:                                                               --
--   * MY_SUBMIT:                                                             --
--                                                                            --
--                                                                            --
--                                                                            --
--------------------------------------------------------------------------------
Package Report_Form is

  -- This package contains the main report-page, which is displayed to collect
  -- reports from operators.


   -------------------------------------------------------------------------
   --  My_Widget_Type
   -------------------------------------------------------------------------
   --  In this tutorial we are going to create a custom Gnoga View that is a
   --  composite of other elements so that we can reuse the view in the
   --  future.

   type My_Widget_Type is new Gnoga.Gui.View.View_Type with
      record
         Observation_Div : NSO.Types.Report_Objects.Observation_Report.Observation_Report;--Gnoga.Gui.Element.Common.DIV_Type;
         Weather_Div : NSO.Types.Report_Objects.Weather_Report.Weather_Report;--Gnoga.Gui.Element.Common.DIV_Type;
         Widget_Form : Gnoga.Gui.Element.Form.Form_Type;
           Name_Input  : Gnoga.Gui.Element.Form.Text_Type;
           Stat_Input  : Gnoga.Gui.Element.Form.Selection_Type;
           Oper_Input  : Gnoga.Gui.Element.Form.Selection_Type;
         Message     : Gnoga.Gui.Element.Form.Text_Area_Type;
         My_Submit   : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

   overriding
   procedure Create  (View    : in out My_Widget_Type;
                      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
                      ID      : in     String  := "");

--      type My_Widget_Type is new Gnoga.Gui.View.View_Type with
--        record
--           Widget_Form : Gnoga.Gui.Element.Form.Form_Type;
--           Name_Input  : Gnoga.Gui.Element.Form.Text_Type;
--           Message     : Gnoga.Gui.Element.Form.Text_Area_Type;
--           My_Submit   : Gnoga.Gui.Element.Form.Submit_Button_Type;
--        end record;

End Report_Form;

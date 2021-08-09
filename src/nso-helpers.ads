With
Gnoga.Gui.Element.Form,
Ada.Streams,
NSO.Types,
INI;

Package NSO.Helpers is

    Generic
        Type Options is (<>);
    Procedure Add_Discrete(
       Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
       Parent : in out Gnoga.Gui.Element.Form.Selection_Type'Class
      );

    Procedure Add_Vector(
       Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
       Parent : in out Gnoga.Gui.Element.Form.Selection_Type'Class;
       Input  : in     NSO.Types.String_Vector.Vector;
       Prefix : in     String:= ""
      );

    Procedure Add_Map(
       Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
       Parent : in out Gnoga.Gui.Element.Form.Selection_Type'Class;
       Input  : in     NSO.Types.String_Map.Map;
       Prefix : in     String:= ""
      );

    -- Adds the sections given by INPUT as groups; grouped by the section-name.
    Procedure Add_Sections(
       Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
       Parent : in out Gnoga.Gui.Element.Form.Selection_Type'Class;
       Fields : in     NSO.Types.String_Vector.Vector;
       File   : in     INI.Instance;
       Prefix : in     String:= ""
      );

    Procedure Add_Section_Groups(
       Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
       Parent : in out Gnoga.Gui.Element.Element_Type'Class; --Gnoga.Gui.Element.Form.Selection_Type'Class;
       Fields : in     NSO.Types.String_Vector.Vector;
       File   : in     INI.Instance;
       Name   : in     String:= ""
      );

    Function Trim( Input : String ) return String;

    Function Unescape_JSON_String( Input : String ) Return String;


    Subtype UPPER_CASE   is Character range 'A'..'Z';
    Subtype LOWER_CASE   is Character range 'a'..'z';
    Subtype NUMERIC      is Character range '0'..'9';
    Subtype ALPHANUMERIC is Character
      with Static_Predicate => ALPHANUMERIC in UPPER_CASE|LOWER_CASE|NUMERIC;

    -- Given "Item" & "Data" produces "<Item>Data</Item>".
    Function HTML_Tag(Name, Text : String) return String
      with Pre => Name'Length in Positive and
                  (for all C of Name => C in ALPHANUMERIC);

    Function HTML_Tag(Name, Text, Attribute, Value : String) return String
      with Pre => Name'Length in Positive and
                  (for all C of Name => C in ALPHANUMERIC) and
                  Attribute'Length in Positive and
                  (for all C of Attribute => C in ALPHANUMERIC) and
                  Value'Length in Positive and
                  (for all C of Value => C /= '"');


    Type    Nullable_Character  is access all Character;
    Subtype Character_Reference is not null Nullable_Character;
    Function Next( Stream : not null access NSO.Types.Root_Stream_Class )
      return Character_Reference with Inline;
End NSO.Helpers;

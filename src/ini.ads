With
Ada.Streams,
Ada.Strings.Less_Case_Insensitive,
Ada.Strings.Equal_Case_Insensitive,
Ada.Containers.Indefinite_Ordered_Maps;

Package INI with Preelaborate, Elaborate_Body is
    Type Value_Type is ( vt_String, vt_Float, vt_Integer, vt_Boolean );


    Type Instance(Convert : Boolean) is private;

    Function Exists( Object : in     Instance;
                     Key    : in     String;
                     Section: in     String:= ""
                   ) return Boolean;


    -- Return the type of the associated value.
    Function Value( Object : in     Instance;
                    Key    : in     String;
                    Section: in     String:= ""
                  ) return Value_Type
      with Pre => Exists(Object, Key, Section);

    -- Return the value associated with the key in the indicated section.
    Function Value( Object : in     Instance;
                    Key    : in     String;
                    Section: in     String:= ""
                  ) return String
      with Pre => Exists(Object, Key, Section);
    Function Value( Object : in     Instance;
                    Key    : in     String;
                    Section: in     String:= ""
                  ) return Float
      with Pre => Exists(Object, Key, Section);
    Function Value( Object : in     Instance;
                    Key    : in     String;
                    Section: in     String:= ""
                  ) return Integer
      with Pre => Exists(Object, Key, Section);
    Function Value( Object : in     Instance;
                    Key    : in     String;
                    Section: in     String:= ""
                  ) return Boolean
      with Pre => Exists(Object, Key, Section);

    -- Associates a value with the given key in the indicated section.
    Procedure Value( Object : in out Instance;
                     Key    : in     String;
                     Value  : in     String;
                     Section: in     String:= ""
                   )
      with Post => Exists(Object, Key, Section);
    Procedure Value( Object : in out Instance;
                     Key    : in     String;
                     Value  : in     Float;
                     Section: in     String:= ""
                   )
      with Post => Exists(Object, Key, Section);
    Procedure Value( Object : in out Instance;
                     Key    : in     String;
                     Value  : in     Integer;
                     Section: in     String:= ""
                   )
      with Post => Exists(Object, Key, Section);
    Procedure Value( Object : in out Instance;
                     Key    : in     String;
                     Value  : in     Boolean;
                     Section: in     String:= ""
                   )
      with Post => Exists(Object, Key, Section);


    -- This value sets the Convert discriminant for the object that is generated
    -- by the 'Input attribute.
    Default_Conversion : Boolean := False;

    Empty : Constant Instance;
Private
    Type Value_Object( Kind : Value_Type; Length : Natural ) ;

    Function "ABS"( Object : Value_Object  ) return String;
    Function "="(Left, Right : Value_Object) return Boolean;

    Package Object_Package is
        procedure Value_Output(
           Stream : not null access Ada.Streams.Root_Stream_Type'Class;
           Item   : in  Value_Object
          ) is null;

        function Value_Input(
           Stream : not null access Ada.Streams.Root_Stream_Type'Class
          ) return Value_Object;
    End Object_Package;

    Type Value_Object( Kind : Value_Type; Length : Natural ) is record
        case Kind is
            when vt_String  => String_Value : String(1..Length):= (Others=>' ');
            when vt_Float   => Float_Value  : Float            := 0.0;
            when vt_Integer => Integer_Value: Integer          := 0;
            when vt_Boolean => Boolean_Value: Boolean          := False;
        end case;
    end record;
--        with Input  => Object_Package.Value_Input,
--             Output => Object_Package.Value_Output;

    Package KEY_VALUE_MAP is new Ada.Containers.Indefinite_Ordered_Maps(
--         "="          => ,
       "<"          => Ada.Strings.Less_Case_Insensitive,
       Key_Type     => String,
       Element_Type => Value_Object
      );

    Function "="(Left, Right : KEY_VALUE_MAP.Map) return Boolean;
    Package KEY_SECTION_MAP is new Ada.Containers.Indefinite_Ordered_Maps(
       "="          => "=",
       "<"          => Ada.Strings.Less_Case_Insensitive,
       Key_Type     => String,
       Element_Type => KEY_VALUE_MAP.Map
      );


    procedure INI_Output(
       Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : in  Instance
      );

    function INI_Input(
       Stream : not null access Ada.Streams.Root_Stream_Type'Class
      ) return Instance;

    Type Instance(Convert : Boolean) is new KEY_SECTION_MAP.Map
      with null record
      with Input => INI_Input, Output => INI_Output;


    overriding
    function Copy (Source : Instance) return Instance is
        ( Source );

    Empty : Constant Instance:=
      (KEY_SECTION_MAP.map with Convert => True, others => <>);
End INI;

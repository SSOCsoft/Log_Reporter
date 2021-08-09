With
Ada.Containers.Indefinite_Holders,
Ada.IO_Exceptions,
Ada.Strings.Fixed;

Package Body INI is
    use type Ada.Containers.Count_Type;
    Package String_Holder is new Ada.Containers.Indefinite_Holders( String );


    Function "=" (Left, Right : String) return Boolean
      renames Ada.Strings.Equal_Case_Insensitive;

    Function "="(Left, Right : KEY_VALUE_MAP.Map) return Boolean is
      (if Left.Length /= Right.Length then False else
         (for all Item in Left.Iterate =>
                Right.Contains(KEY_VALUE_MAP.Key(Item)) and then
                KEY_VALUE_MAP.Element( Item ) = Right(KEY_VALUE_MAP.Key( Item ))
      ));




        Generic
            Target : Character;
            Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Function Read_Until return String;
        Function Read_until return String is
        Begin
            Declare
                C : Character renames Character'Input( Stream );
            Begin
                Return (if C = Target then "" else C & Read_until);
            End;
        exception
            when Ada.IO_Exceptions.End_Error => Return "";
        End Read_until;



        Function "="(Left, Right : Value_Object) return Boolean is
          (if Left.Kind /= Right.Kind then False else
             (case Left.Kind is
                  when vt_String  => Left.String_Value  = Right.String_Value,
                  when vt_Float   => Left.Float_Value   = Right.Float_Value,
                  when vt_Integer => Left.Integer_Value = Right.Integer_Value,
                  when vt_Boolean => Left.Boolean_Value = Right.Boolean_Value
             )
          );

    Function "ABS"( Object : Value_Object ) return String is
      (case Object.Kind is
           when vt_String  =>
             (if (for some C of Object.String_Value => C = '"') then
                raise Program_Error with "Qoute cannot be embedded at this time."
              else Object.String_Value),
           when vt_Float   => Float'Image  ( Object.Float_Value  ),
           when vt_Integer => Integer'Image( Object.Integer_Value),
           when vt_Boolean => Boolean'Image( Object.Boolean_Value)
      );

    package body Object_Package is
    function Value_Input(
       Stream : not null access Ada.Streams.Root_Stream_Type'Class
      ) return Value_Object is

        Subtype Integer_Characters is Character
          with Static_Predicate => Integer_Characters in '0'..'9';
        Subtype Float_Characters is Character
          with Static_Predicate => Float_Characters in '.'|Integer_Characters;

        Function Dequote ( Input : String ) Return String is

            Function Delimit(
               Input, Working : String := ""
              ) return String is
                Search: constant String:= """""";
                Index : Natural renames Ada.Strings.Fixed.Index(
                   Source  => Input,
                   Pattern => Search,
                   From    => 1
                  );

                Subtype Head is Natural range Input'First..Positive'Pred(Index);
                Subtype Tail is Natural range Input'First+Search'Length..Input'Last;
            Begin
                If Index not in Positive then
                    Return Working & Input;
                else
                    Return Delimit( Input(Tail), Working & Input(Head) & '"');
                end if;
            End;

            Subtype Inner_Range is Natural range
              Positive'Succ(Input'First) .. Positive'Pred(Input'Last);
            Temp : String( Input'Range ) := Input;
            Index_Input, Index_Temp : Natural := Input'First;
            Begin
                if Input'Length not in Positive then
                    return "";
                elsif Input(Input'Last) = '"' and Input(Input'First) = '=' then
                    Return Delimit( Inner_Range );
                else
                    Return Input;
            end if;
        End Dequote;

            Function Trim(Input : String) return String is
                Function Trim_Left(Input : String) return String is
                    Index : Positive := Input'First;
                Begin
                    If Input'Length not in Positive then
                        Return "";
                    Else
                        while Index <= Input'Last and Input(Index) not in ' '..'~' loop
                            Index:= Positive'Succ( Index );
                        end loop;
                        Return Input(Index..Input'Last);
                    end if;
                End;

                Function Trim_Right( Input : String ) return String is
                    Index : Natural := Input'Last;
                Begin
                    If Input'Length not in Positive then
                        Return "";
                    Else
                        while Index >= Input'First and Input(Index) not in ' '..'~' loop
                            Index:= Positive'Pred( Index );
                        end loop;
                        Return Input(Input'First..Index);
                    End if;
                End;

            Begin
                Return Trim_Right( Trim_Left( Input ) );
            end Trim;



       Function Get_Value is new Read_until( ASCII.CR, STREAM );
        Value : String renames Get_Value;
    Begin
        --Return (vt_Boolean, 0, Boolean_Value => Value = "T" or Value = "TRUE");
        if Value = "T" or Value = "TRUE" or Value = "F" or Value = "FALSE" then
            Return (vt_Boolean, 0, Boolean_Value => Value = "T" or Value = "TRUE");
--          elsif (for all C of Value => C in Integer_Characters) then
--              Return ( vt_Integer, 0, Integer_Value => Integer'Value(Value) );
--          elsif (for all C of Value => C in Float_Characters) then
--              Return ( vt_Float, 0, Float_Value => Float'Value(Value) );
        else
            Declare
                Text : String renames Dequote( Trim( Value ) );
            Begin
                Return ( vt_String, Text'Length, String_Value => Text );
            End;
        end if;
    End Value_Input;

    End Object_Package;

    Generic
        Item : Value_Object;
    Procedure Assignment( Object : in out Instance;
                          Key    : in     String;
                          Section: in     String);

    Procedure Assignment( Object : in out Instance;
                          Key    : in     String;
                          Section: in     String
                         ) is
    Begin
        Declare
            -- This RENAME raises CONSTRAINT_ERROR if the section does not exist.
            Sec : KEY_VALUE_MAP.Map renames Object(Section);
        Begin
            -- This indexing  raises CONSTRAINT_ERROR when the key does not exist.
            Sec(Key):= Item;
        exception
            when Constraint_Error =>
                -- Create a new key, with the item associated.
                Sec.Include(Key => Key, New_Item => Item);
        End;
    exception
        when Constraint_Error =>
            -- Create a new section, with a new key that has item associated.
            Declare
                New_Section : KEY_VALUE_MAP.Map := KEY_VALUE_MAP.Empty_Map;
            Begin
                New_Section.Include(Key => Key, New_Item => Item);
                Object.Include(Key => Section, New_Item => New_Section);
            End;
    End Assignment;


    Function Exists( Object : in     Instance;
                     Key    : in     String;
                     Section: in     String:= ""
                   ) return Boolean is
      ( Object.Contains(Section) and then Object(Section).Contains(Key) );


    -- Return the type of the associated value.
    Function Value( Object : in     Instance;
                    Key    : in     String;
                    Section: in     String:= ""
                  ) return Value_Type is
      (Object(Section)(Key).Kind);



    -- Return the value associated with the key in the indicated section.
    Function Value( Object : in     Instance;
                    Key    : in     String;
                    Section: in     String:= ""
                  ) return String is
      (Object(Section)(Key).String_Value);
    Function Value( Object : in     Instance;
                    Key    : in     String;
                    Section: in     String:= ""
                  ) return Float is
      (Object(Section)(Key).Float_Value);
    Function Value( Object : in     Instance;
                    Key    : in     String;
                    Section: in     String:= ""
                  ) return Integer is
      (Object(Section)(Key).Integer_Value);
    Function Value( Object : in     Instance;
                    Key    : in     String;
                    Section: in     String:= ""
                  ) return Boolean is
      (Object(Section)(Key).Boolean_Value);

    -- Associates a value with the given key in the indicated section.
    Procedure Value( Object : in out Instance;
                     Key    : in     String;
                     Value  : in     String;
                     Section: in     String:= ""
                   ) is
        Item : Constant Value_Object :=
          (Kind => vt_String, Length => Value'Length, String_Value => Value);

        Procedure Assign is new Assignment(Item);
    begin
        Assign(Object, Key, Section);
    end Value;

    Procedure Value( Object : in out Instance;
                     Key    : in     String;
                     Value  : in     Float;
                     Section: in     String:= ""
                   ) is

        Item : Constant Value_Object :=
          (Kind => vt_Float, Length => 0, Float_Value => Value);
        Procedure Assign is new Assignment(Item);
    begin
        Assign( Object, Key, Section );
    end Value;

    Procedure Value( Object : in out Instance;
                     Key    : in     String;
                     Value  : in     Integer;
                     Section: in     String:= ""
                    ) is
        item : Constant Value_Object :=
          (Kind => vt_Integer, Length => 0, Integer_Value => Value);
        Procedure Assign is new Assignment(Item);
    begin
        Assign( Object, Key, Section );
    end Value;

    Procedure Value( Object : in out Instance;
                     Key    : in     String;
                     Value  : in     Boolean;
                     Section: in     String:= ""
                    ) is
        Item : Constant Value_Object :=
          (Kind => vt_Boolean, Length => 0, Boolean_Value => Value);
        Procedure Assign is new Assignment(Item);
    begin
        Assign( Object, Key, Section );
    end Value;




    Procedure INI_Output(
       Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : in  Instance
      ) is
    Begin
        for Section in Item.Iterate loop
            Declare
                Key : String renames KEY_SECTION_MAP.Key( Section );
                Val : KEY_VALUE_MAP.Map renames KEY_SECTION_MAP.Element(Section);
            Begin
                if Key /= "" then
                    String'Write( Stream, '[' & Key & ']' );
                    String'Write(Stream, ASCII.CR & ASCII.LF);
                end if;

                for KVP in Val.Iterate loop
                    Declare
                        K : String renames KEY_VALUE_MAP.Key( KVP );
                        V : Value_Object renames KEY_VALUE_MAP.Element( KVP );
                    Begin
                        String'Write       (Stream, K);
                        Character'Write    (Stream, '=');
                        Value_Object'Write (Stream, V);
                        String'Write       (Stream, ASCII.CR & ASCII.LF);
                    End;
                end loop;
                String'Write       (Stream, ASCII.CR & ASCII.LF);
            End;
        end loop;
    End INI_Output;

    Function INI_Input(
       Stream : not null access Ada.Streams.Root_Stream_Type'Class
      ) return Instance is
        Section_Name : String_Holder.Holder:= String_Holder.To_Holder("");
        Finished     : Boolean := False;

        Function Get_Name  is new Read_until( ']', Stream );
        Function Get_Key   is new Read_until( '=', Stream );
        Function Get_Value is new Read_until( ASCII.CR, Stream );

        -- Gets the next graphic character in the stream, discarding all the
        -- non-graphic characters; sets FINISHED to true on END_ERROR.
        Function Get_Non_Blank return Character is
        Begin
            Return Result : Character do
                loop
                    Character'Read(Stream, Result);
                    exit when Result in ' '..'~';
                end loop;
            exception
                when Ada.IO_Exceptions.End_Error => Finished:= True;
            end return;
        End Get_Non_Blank;

    Begin
        Return  Result : Instance(Convert => Default_Conversion) do
            Result.Include(Section_Name.Element, KEY_VALUE_MAP.Empty_Map);
            Loop
                Declare
                    Procedure Set_Name is
                        Name    : String renames Get_Name;
                        Default : KEY_VALUE_MAP.Map;
                    Begin
                        Section_Name.Replace_Element( Name );
                        Result.Include(Section_Name.Element, Default);
                    End Set_Name;

                    C : Constant Character := Get_Non_Blank;
                begin
                    exit when Finished;
                    case C is
                    when '['    =>
                        Set_Name;
                    when others =>
                        Declare
                            Key   : String renames "&"(C, Get_Key);
                            Value : Constant Value_Object:=
                              Object_Package.Value_Input(Stream);
                        Begin
                            Result(Section_Name.Element).Include(Key, Value);
                        End;
                    end case;
                end;
            end loop;
        end return;
    End INI_Input;


End INI;

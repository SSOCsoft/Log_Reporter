with
--NSO.Helpers,
NSO.Types,
Interfaces,
Ada.Strings.Unbounded,
Ada.Characters.Latin_1,
Ada.Tags.Generic_Dispatching_Constructor,
Ada.Characters.Conversions;
with Ada.Containers;
--  NSO.Types;

Package Body NSO.JSON is
    use Ada.Characters.Conversions, Ada.Characters;

    Procedure Apply( Object : in Instance'Class ) is
    Begin
        if Object in Null_Object then
            On_Null;
        elsif Object in True_Object or Object in False_Object then
            On_Boolean( Object in True_Object );
        elsif Object in String_Object then
            On_String( String_Object(Object).Element.Element );
        elsif Object in Number_Object then
            On_Number( Number_Object(Object).Element );
        elsif Object in Array_Object then
            Declare
                Procedure Operate( Cursor : Value_Array.Cursor ) is
                    Idx : Natural renames Value_Array.To_Index(Cursor);
                    Val : Instance'Class renames Value_Array.Element(Cursor);
                Begin
                    On_Array( Idx, Val );
                End Operate;
            Begin
                Array_Object(Object).Element.Iterate( Operate'Access );
            End;
        elsif Object in Object_Object then
            Declare
                Procedure Operate( Cursor : Name_Value_Pairs.Cursor ) is
                    Key : String renames Name_Value_Pairs.Key(Cursor);
                    Val : Instance'Class renames Name_Value_Pairs.Element(Cursor);
                Begin
                    On_Object( Key, Val );
                End Operate;
            Begin
                Object_Object(Object).Element.Iterate( Operate'Access );
            End;
        end if;
    End Apply;


    Subtype ASCII_Control_Character is Character
      with Static_Predicate => ASCII_Control_Character in
        Latin_1.NUL..Latin_1.US|Latin_1.DEL;

    Subtype ISO_6429_Control_Character is Character
      with Static_Predicate => ISO_6429_Control_Character in
        Latin_1.Reserved_128..Latin_1.APC;

    Subtype Whitespace is Character
      with Static_Predicate => Whitespace in
        Latin_1.HT|Latin_1.Space|Latin_1.No_Break_Space;

    Subtype Control_Character is Character
      with Static_Predicate => Control_Character in
        ASCII_Control_Character | ISO_6429_Control_Character;

    Package Constructors is
        Use Ada.Streams, NSO.Types;
--          Use Ada.Strings.Unbounded;

        Type Parameter( Stream : not null access Root_Stream_Class ) is record
--              Stream : --not null access NSO.Types.Root_Stream_Class;
--              not null access Ada.Streams.Root_Stream_Type'Class;
            Buffer : Nullable_Character := Null;
            --Unbounded_String := To_Unbounded_String("");
        end record;


--          Type Parameter is record
--              Stream : --not null access NSO.Types.Root_Stream_Class;
--              not null access Ada.Streams.Root_Stream_Type'Class;
--              Buffer : Nullable_Character := Null;
--              --Unbounded_String := To_Unbounded_String("");
--          end record;

        Function Expect( C : Character; From : not null access Parameter ) return Boolean is
          (if From.Buffer /= Null then From.Buffer.All = C) with Inline;
        Function Expect( S : String; From : not null access Parameter ) return Boolean is
          (for some C of S => Expect(C, From)) with Inline;
--          Function Expect( S : String; From : not null access Parameter ) return Character
--            with Inline;

        function Constructor (Params : not null access Parameter) return Null_Object
          with Pre => Expect( 'n', Params );
        function Constructor (Params : not null access Parameter) return False_Object
          with Pre => Expect( 'f', Params );
        function Constructor (Params : not null access Parameter) return True_Object
          with Pre => Expect( 't', Params );
        function Constructor (Params : not null access Parameter) return String_Object
          with Pre => Expect( '"', Params );
        function Constructor (Params : not null access Parameter) return Number_Object
          with Pre => Expect( "-0123456789", Params );
        function Constructor (Params : not null access Parameter) return Array_Object
          with Pre => Expect( '[', Params );
        function Constructor (Params : not null access Parameter) return Object_Object
          with Pre => Expect( '{', Params );


--      Generic
--          with Function Next( Stream : not null access Ada.Streams.Root_Stream_Type'Class ) return Character is <>;
        Function Value_Constructor(Stream : not null access Root_Stream_Type'Class) return Instance'Class;
        Function Value_Constructor(Params : not null access Parameter) return Instance'Class;
    Private

        -- Consumes characters from the stream until a non-whitespace character is encountered.
        Function Consume_Whitespace( Stream : not null access Root_Stream_Type'Class ) return Character
          with Post => Consume_Whitespace'Result not in Whitespace;
        Function Consume_Whitespace( P : not null access Parameter ) return Character
          with Pre  => P.Buffer = Null,
               Post => Consume_Whitespace'Result not in Whitespace;


        -- This returns the tail of a string.
        Function "-"( Input : String ) return String is
          ( Input(Positive'Succ(Input'First)..Input'Last) )
          with Inline, Pure_Function;

        -- Asserts that the next element in the stream is the given character.
        -- Raises PARSE_ERROR if it does not.
        Generic
            Context : Parameter;
        Function Expect_Character( Element : Character ) return Boolean with Inline;

        -- Asserts that the next elemente in the stream are those of the given string.
        -- Raises PARSE_ERROR if it does not.
        Generic
            Context : Parameter;
        Procedure Expect_String( Element : String ) with Inline;

        -- This function ensures theat the Value is the next item in the stream,
        -- it accounts for an unused buffer, as well as checks the buffer's own
        -- content if it is used.
        Procedure Check (Param : not null access Parameter; Value : String)
          with Pre => Value'Length in Positive;

        -- If Params.Buffer is empty, it fills it and returns the result,
        -- otherwise it simply returns the contents of the buffer.
        --
        -- NOTE:	This will not consume current contents of the buffer, but
        --	may consume a character from the stream.
        Function Buffer(Params : in out Parameter) Return Character
          with Inline;
        Function Buffer(Params : not null access Parameter) Return Character
          with Inline;

    End Constructors;

    Package Body Constructors is
        Use NSO.Types;

        Function Next_Character( Stream : not null access Root_Stream_Class )
           return Reference_Character is
              ( New Character'(Character'Input(Stream)) ) with Inline;

        Function Make_Param( Stream : not null access Ada.Streams.Root_Stream_Type'Class ) return Parameter is
        Begin
            Return Result : Constant Parameter :=
              ( Buffer => Next_Character( Stream ),
                Stream => Stream );
        End Make_Param;






        Function Buffer(Params : not null access Parameter) Return Character is
            ( Buffer( Params.All ) );

        Function Buffer(Params : in out Parameter) Return Character is
            Empty : Constant Boolean := Params.Buffer = Null;
        Begin
            Return Result : Constant Character :=
              (if not Empty then Params.Buffer.All
               else Consume_Whitespace(Params.Stream)) do
                if Empty then
                    Params.Buffer := New Character'( Result );
                end if;
            end return;
        End Buffer;

        Function Value_Constructor(Params : not null access Parameter) return Instance'Class is
            -- If Params.Buffer is empty, it fills it and returns the result,
            -- otherwise it simply returns the contents of the buffer.
            Function Buffer return Character with Inline is
                Empty : Constant Boolean := Params.Buffer = Null;
            Begin
                Return Result : Constant Character :=
                  (if not Empty then Params.Buffer.All
                   else Consume_Whitespace(Params.Stream)) do
                    if Empty then
                        Params.Buffer := New Character'( Result );
                    end if;
                end return;
            End Buffer;

            C : Character renames Buffer;
        Begin
            -- Here we switch on the character in the buffer, constructing the
            -- appropriate object.
            Return Result : Constant Instance'Class :=
              (case C is
                   when '{' => Object_Object'(Constructor(Params)),
                   when '[' => Array_Object' (Constructor(Params)),
                   when '0'..'9'
                      | '-' => Number_Object'(Constructor(Params)),
                   when '"' => String_Object'(Constructor(Params)),
                   when 't' => True_Object'  (Constructor(Params)),
                   when 'f' => False_Object' (Constructor(Params)),
                   when 'n' => Null_Object'  (Constructor(Params)),
                   when others => raise Parse_Error
                     with "'" & C & "' is an invalid character."
              );
        End Value_Constructor;


        Function Value_Constructor(Stream : not null access Root_Stream_Type'Class) return Instance'Class is
            use Constructors;
            Param : Aliased Parameter:= Make_Param( Stream );
            C     : Character renames Param.Buffer.All;
        Begin
            Return Result : Constant Instance'Class := Value_Constructor(Param'Unchecked_Access);
--                (case C is
--                     when '{' => Object_Object'(Constructor(Param'Access)),
--                     when '[' => Array_Object' (Constructor(Param'Access)),
--                     when '0'..'9'
--                        | '-' => Number_Object'(Constructor(Param'Access)),
--                     when '"' => String_Object'(Constructor(Param'Access)),
--                     when 't' => True_Object'  (Constructor(Param'Access)),
--                     when 'f' => False_Object' (Constructor(Param'Access)),
--                     when 'n' => Null_Object'  (Constructor(Param'Access)),
--                     when others => raise Parse_Error
--                       with "'" & C & "' is an invalid character."
--                );
        End Value_Constructor;


        Function Expect_Character( Element : Character ) return Boolean is
            Item : Character renames Character'Input(Context.Stream);
        Begin
            Return Result : Constant Boolean := Item = Element
              or else raise Parse_Error with
                "'"&Element&"' expected, but '" &Item& "' found.";
        End Expect_Character;


        Procedure Expect_String( Element : String ) is
            Function Expect is new Expect_Character(Context);
            Result : Constant Boolean :=
              (for all C of Element => Expect(C));
        Begin
            Null;
        End Expect_String;


        Function Consume_Whitespace( Stream : not null access Root_Stream_Type'Class ) return Character is
        Begin
            Return Result : Character do
                Loop
                    Result:= Character'Input( Stream );
                    exit when Result not in Whitespace;
                End Loop;
            End Return;
        End Consume_Whitespace;


        Function Consume_Whitespace( P : not null access Parameter ) return Character is
          (Consume_Whitespace(P.Stream));

--          Begin
--              Return Result : Character do
--                  Loop
--                      Result:= Character'Input( P.Stream );
--                      exit when Result not in Whitespace;
--                  End Loop;
--              End Return;
--          End Consume_Whitespace;


        Procedure Check (Param : not null access Parameter; Value : String) is
            Procedure Expect is new Expect_String(Param.all);
        Begin
            -- Handle the empty-buffer by matching the next non-whitespace
            -- character with the head of the given string.
            if Param.Buffer = Null then
                Declare
                    Element : Character renames Value(Value'First);
                    Item    : Character renames Consume_Whitespace(Param);
                Begin
                    if Element /= Item then
                        Raise Parse_Error with
                          "'"&Element&"' expected, but '" &Item& "' found.";
                    end if;
                End;

                -- Handle the tail.
                Expect( -Value );
            -- Handle the case where the buffer matches the head of the string.
            elsif Param.Buffer.All = Value(Value'First) then
                -- Handle the tail.
                Expect( -Value );
            else -- Buffer not the expected value.
                Raise Parse_Error with
                  "'"& Value(Value'First) &"' expected, but '"
                     & Param.Buffer.All   & "' found.";
            end if;
        End Check;



        function Constructor (Params : not null access Parameter) return Null_Object is
            Procedure Expect is new Expect_String(Params.all);
        Begin
            Return Result : Constant Null_Object := (Value_Type => VK_Null) do
                Check( Params, "null" );
            End return;
        End Constructor;


        function Constructor (Params : not null access Parameter) return False_Object is
            Procedure Expect is new Expect_String(Params.all);
        Begin
            Return Result : Constant False_Object := (Value_Type => VK_False) do
                Check( Params, "false");
            End return;
        End Constructor;


        function Constructor (Params : not null access Parameter) return True_Object is
            Procedure Expect is new Expect_String(Params.all);
        Begin
            Return Result : Constant True_Object := (Value_Type => VK_True) do
                Check( Params, "true");
            End return;
        End Constructor;


        function Constructor (Params : not null access Parameter) return String_Object is
            Use Ada.Strings.Unbounded, String_Holder;
            Working : Unbounded_String := To_Unbounded_String("");
            Escape  : Boolean          := False;

            Subtype Escape_Character is Character
              with Static_Predicate =>  Escape_Character in
                '"' | '\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' | 'u';

            Procedure Append( Item : Character ) with Inline is
            Begin
                Append( New_Item => Item, Source => Working );
            End Append;
        Begin
            Check( Params, (1 => '"') );

            Return Result : String_Object :=
              (Value_Type => VK_String, Element => Empty_Holder) do

                -- Read the string from the stream.
                READ_STRING:
                Loop
                    Declare
                        C : Character renames Character'Input( Params.Stream );
                    Begin
                        -- Here we handle all escapes, except for the hex-code,
                        -- which we will do later, due to it intrinsically being
                        -- referencial to points outside Latin-1.
                        -- NOTE: If we wanted a wholly conformant implementation
                        --	 then we need a string-form of Unicode.
                        if Escape then
                            Escape:= False;
                            case Escape_Character'(C) is
                            when '"' => Append( Latin_1.Quotation );
                            when '\' => Append( Latin_1.Reverse_Solidus );
                            when '/' => Append( Latin_1.Solidus );
                            when 'b' => Append( Latin_1.BS );
                            when 'f' => Append( Latin_1.FF );
                            when 'n' => Append( Latin_1.LF );
                            when 'r' => Append( Latin_1.CR );
                            when 't' => Append( Latin_1.HT );
                            when 'u' => -- Four-hex escape-value next.
                                Append( New_Item => "\u", Source => Working );
                            end case;
                        elsif C = '\' then
                            Escape:= True;
                        elsif C = '"' then
                            Exit READ_STRING;
                        else
                            Append( C );
                        end if;
                    End;
                End Loop READ_STRING;

                -- Here we handle the hex-value escape-codes; because we use a
                -- plain Latin-1 string, we have to
                HANDLE_HEX_ESCAPES:
                Declare
                    Procedure Hex_Fixup(
                       Item  : in out Unbounded_String;
                       Index : in     Positive
                      ) with Pre => Index in 1..Length(Working)-4 is
                        Low  : Constant Positive := Index+2;
                        High : Constant Positive := Index+5;
                        Hex_Value : String renames Slice(Working, Low, High);
                        Chr_Value : Interfaces.Unsigned_32 renames
                          Interfaces.Unsigned_32'Value("16#"& Hex_Value &'#');
                        Temp : Constant Wide_Wide_Character :=
                          Wide_Wide_Character'Val( Chr_Value );
                        Ch   : Character renames To_Character(Temp,Latin_1.NUL);
                    Begin
                        if Is_Character( Temp ) then
                            -- Replace the slash.
                            Replace_Element(Item, Index, Ch);
                            -- Delete the remaining escape sequence.
                            Delete(Item, Positive'Pred(Low), High);
                        else
                            -- Delete the entire sequence.
                            Delete(Item, Index, High);
                        end if;
                    End Hex_Fixup;

                    Index : Natural:= Natural'First;
                Begin
                    HEX_LOOP:
                    Loop
                        Index:= Ada.Strings.Unbounded.Index(Working, "\u");
                        Exit HEX_LOOP when Index not in Positive;
                        Hex_Fixup( Working, Index );
                    End Loop HEX_LOOP;
                End HANDLE_HEX_ESCAPES;

                Result.Element.Replace_Element( To_String(Working) );
            End return;
        End Constructor;


        function Constructor (Params : not null access Parameter) return Number_Object is
            Function Next return Character is
              (Character'Input( Params.Stream )) with Inline;

            Function Leading_0( Is_Negative : Boolean ) return Boolean;

            Generic
                Terminator : in out Character;
            Function Digit_Sequence return String;

            Function Get_Fraction return String;
            Function Get_Exponent return String;

            Subtype Digit is Character range '0'..'9';
            Subtype Number_Character is Character
              with Static_Predicate => Number_Character in
                Digit|'+'|'-'|'.'|'e'|'E';
            Subtype Digit_1_to_9 is Digit range '1'..'9';

            Function Leading_0( Is_Negative : Boolean ) return Boolean is
                Item : Character renames Params.Buffer.All;
            Begin
                if Is_Negative then
                    Item:= Next;
                end if;

                if Item not in Digit then
                    Raise Parse_Error with
                    "Digit expected, but '"& Item &"' found.";
                end if;

                Return Item = '0';
            End Leading_0;


            Function Digit_Sequence return String is
                C : Character renames Next;
            Begin
                if C not in Digit then
                    Terminator:= C;
                    Return "";
                else
                    Return C & Digit_Sequence;
                end if;
            End Digit_Sequence;

            Negative : Constant Boolean := Buffer(Params) = '-';
            C        : Character renames Params.Buffer.All;
            Lead_0   : Boolean   renames Leading_0(Negative);

            Function Sequence is new Digit_Sequence( C );
            Function Get_Exponent return String is
              (if C not in 'e'|'E' then
                   Raise Parse_Error with
                    "'E' or 'e' expected, but '"& C &"' found."
               else 'E' & Sequence
              );

            Function Get_Fraction return String is
              (if C /= '.' then Raise Parse_Error with
                    "'.' expected, but '"& C &"' found."
               else '.' & Sequence
              );

            Function Get_Tail return String is
              (case Number_Character'(C) is
                   when 'e' | 'E' => Get_Exponent,
                   when '.'       => Get_Fraction & Get_Tail,
                   when others    => ""
              );
--                  Buffer : Character renames Params.Buffer.All;
--              Begin
--                  if Buffer = '.' then
--                      declare
--                          Fraction : String renames Get_Fraction;
--                      begin
--                          Return Fraction &
--                          (if Buffer not in 'e'|'E' then "" else Get_Exponent);
--                      end;
--                  elsif Buffer in 'e' | 'E' then
--                      Return Get_Exponent;
--                  else
--                      Return "";
--                  end if;
--              End Get_Tail;

            Function Get_Head return String is
              (if Leading_0(Negative) then "0" else C & Sequence);

        Begin
            Return Result : Number_Object := (Value_Type => VK_Number, Element => 0.0) do
                Declare
                    Head : String renames Get_Head;
                    Tail : String renames Get_Tail;
                Begin
                    Result.Element:= Number'Value( Head & Tail );
                End;
            End return;
        End Constructor;

--            (raise Program_Error with "Unilplemented.");

        function Constructor (Params : not null access Parameter) return Array_Object is
            use Value_Array;
            Function Next(P : not null access Root_Stream_Type'Class:= Params.Stream) return Character
              renames Consume_Whitespace;

        Begin
            Check( Params, (1 => '[') );

            Return Result : Array_Object := (Value_Type => VK_Array, Element => Empty_Vector) do
                -- Read a comma-separated list of name-value paris separated with colon.
                READ_VALUE:
                Loop

                    Declare
                        P : Aliased Parameter :=
                          (Buffer => New Character'(Next), Stream => Params.Stream);
                        C : Character renames P.Buffer.All;
                        Function Value(Params : not null access Parameter := P'Access)
                          return Instance'class
                            renames Value_Constructor;
                    Begin
                        Case C is
                            -- Here we need to contend with the array ending.
                            -- (Esp. a possibly empty array.)
                            when ']' => exit READ_VALUE;

                            -- Here we handle a separator by re-starting the read,
                            -- discarding the comma from the stream.
                            when ',' => Null;

                            -- At this point, we have ensured that no non-value
                            -- characters are in the stream, and may now get the
                            -- actual value.
                            when Others =>
                                Result.Element.Append( Value );
                        end case;
                    End;
                End Loop READ_VALUE;
            End return;
        End Constructor;


        function Constructor (Params : not null access Parameter) return Object_Object is
            use Name_Value_Pairs;
            Function Next(P : not null access Root_Stream_Type'Class:= Params.Stream) return Character
              renames Consume_Whitespace;

        Begin
            Check( Params, (1 => '{') );

            Return Result : Object_Object := (Value_Type => VK_Object, Element => Empty_Map) do
                -- Read a comma-separated list of values.
                READ_NAME_VALUE_PAIR:
                Loop
                    Declare
                        P : Aliased Parameter :=
                          (Buffer => New Character'(Next), Stream => Params.Stream);
                        C : Character renames P.Buffer.All;
                        Function Value(Params : not null access Parameter := P'Access)
                          return Instance'class
                            renames Value_Constructor;
                    Begin
                        Case C is
                            -- Here we need to contend with the array ending.
                            -- (Esp. a possibly empty array.)
                            when '}' => exit READ_NAME_VALUE_PAIR;

                            -- Here we handle a separator by re-starting the read,
                            -- discarding the comma from the stream.
                            when ',' => Null;

                            -- At this point, we have ensured sequence-marker
                            -- characters have been handled.
                            -- actual value.
                            when '"' =>
                                READ_NAME:
                                Declare
                                    String_Value : Constant String_Object :=
                                      Constructor( P'Access );
                                    Name : String renames
                                      String_Value.Element.Element;
                                Begin
                                    C:= Next(Params.Stream);
                                    if C /= ':' then
                                        raise Parse_Error with
                                          "':' expected, but '" &C& "' found.";
                                    else
                                        P.Buffer:= Null;
                                    end if;

                                    READ_VALUE:
                                    Declare
                                        Value : Instance'Class renames
                                          JSON_Class_Input( P.Stream );
                                    Begin
                                        Result.Element.Include( Name, Value );
                                    End READ_VALUE;
                                End READ_NAME;
                            when others =>
                                Raise Parse_Error with
                                  "'"& '"' &"' expected, but '"
                                     &  C  & "' found.";
                        end case;
                    End;
                End Loop READ_NAME_VALUE_PAIR;
            end return;
        End Constructor;

        --  (raise Program_Error with "Unilplemented.");

    End Constructors;

--  generic
--     type T (<>) is abstract tagged limited private;
--     type Parameters (<>) is limited private;
--     with function Constructor (Params : not null access Parameters) return T
--       is abstract;
--  function Ada.Tags.Generic_Dispatching_Constructor
--    (The_Tag : Tag;
--     Params  : not null access Parameters) return T'Class;

    ------------
    --  NULL  --
    ------------

    Function "-"(Object : Null_Object) return String is ("null");

    Function Value(Object : Null_Object) return Wide_String is ("null");

    Overriding
    Function Kind(Object : Null_Object) return Value_Kind is (Object.Value_Type);

    -------------
    --  FALSE  --
    -------------

    Function "-"(Object : False_Object) return String is ("false");

    Function Value( Object : False_Object ) return Boolean is (False);
    Function Value(Object : False_Object) return Wide_String is ("false");

    Overriding
    Function Kind(Object : False_Object) return Value_Kind is (Object.Value_Type);

    ------------
    --  TRUE  --
    ------------

    Function "-"(Object : True_Object) return String is ("true");

    Function Value( Object : True_Object ) return Boolean is (True);
    Function Value(Object : True_Object) return Wide_String is ("true");

    Overriding
    Function Kind(Object : True_Object) return Value_Kind is (Object.Value_Type);

    --------------
    --  STRING  --
    --------------

    Function "-"(Object : String_Object) return String is
      (Object.Element.Element);

    Function Value(Object : String_Object) return Wide_Wide_String is ( "IMPLEMENT ME"); --Object.Element.Element );

    Overriding
    Function Kind(Object : String_Object) return Value_Kind is (Object.Value_Type);

    --------------
    --  NUMBER  --
    --------------

    Function "-"(Object : Number_Object) return String is
        Image : String renames Number'Image( Object.Element );
        First : Constant Positive:= Image'First;
        Blank : Constant Boolean:= Image(First) = ' ';
    Begin
        Return (if not Blank then Image
                else Image(Positive'Succ(First)..Image'Last)
               );
    End "-";


    Function Value(Object : Number_Object) return Number is (Object.Element);

    Overriding
    Function Kind(Object : Number_Object) return Value_Kind is (Object.Value_Type);

    -------------
    --  ARRAY  --
    -------------

    Function "-"(Object : Array_Object) return String is
        Use Ada.Strings.Unbounded;

        Function As_String( List : Value_Array.Vector ) return Unbounded_String is
        Begin
            Return Result : Unbounded_String := To_Unbounded_String("") do
                Declare
                    Last : Value_Array.Extended_Index renames List.Last_Index;
                    Procedure Get_Strings( Cursor : Value_Array.Cursor ) is
                        Use Value_Array;
                        Item  : Instance'Class renames Element(Cursor);
                        Value : String renames "-"( Item );
                        Finished : Constant Boolean := To_Index(Cursor) = Last;
                    Begin
                        Append(New_Item => Value, Source => Result);
                        if not Finished then
                            Append(New_Item => ',', Source => Result);
                        end if;
                    End Get_Strings;
                Begin
                    List.Iterate( Get_Strings'Access );
                End;
            End return;
        End As_String;

    Begin
        Return '[' & To_String(As_String(Object.Element)) & ']';
    End;

    Procedure Append ( Object : in out Array_Object; Value : Instance'Class ) is
    Begin
        Object.Element.Append( Value );
    End Append;

    Procedure Prepend( Object : in out Array_Object; Value : Instance'Class ) is
    Begin
        Object.Element.Prepend( Value );
    End Prepend;

    Function Value(Object : Array_Object) return Value_Array.Vector is (Object.Element);

    Overriding
    Function Kind(Object : Array_Object) return Value_Kind is (Object.Value_Type);


    --------------
    --  OBJECT  --
    --------------

    Function "-"(Object : Object_Object) return String is
        Use Ada.Strings.Unbounded;

        Function As_String( List : Name_Value_Pairs.Map ) return Unbounded_String is
        Begin
            Return Result : Unbounded_String := To_Unbounded_String("") do
                if Integer(List.Length) not in Positive then
                    return;
                end if;

                Declare
                    Last_Key  : String renames List.Last_Key;
                    Procedure Get_Strings( Cursor : Name_Value_Pairs.Cursor ) is
                        Package NVP renames Name_Value_Pairs;

                        Key   : String renames NVP.Key( Cursor );
                        Value : Instance'Class renames NVP.Element(Cursor);
                        Finished : Constant Boolean := Key = Last_Key;
                    Begin
                        Append(New_Item => Key,    Source => Result);
                        Append(New_Item => ':',    Source => Result);
                        Append(New_Item => -Value, Source => Result);
                        if not Finished then
                            Append(New_Item => ',', Source => Result);
                        end if;
                    End Get_Strings;
                Begin
                    List.Iterate( Get_Strings'Access );
                End;
            End return;
        End As_String;

    Begin
        Return '{' & To_String(As_String(Object.Element)) & '}';
    End "-";

    Function Value(Object : Object_Object) return Name_Value_Pairs.Map is (Object.Element);

    Function Value( Object : Object_Object; Name : String        ) return Instance'Class is
        ( Object.Element( Name ) );
--          ( Object.Element( To_Wide_Wide_String(Name) ) );
--      Function Value( Object : Object_Object; Name : String_Object ) return Instance is
--          ( Object.Element(Name.Value) );
    Generic
        Type A_Type(<>) is private;
        with Function Make( X : A_Type ) Return Instance'Class is <>;
    Procedure Generic_Value(Object : in out Object_Object; Name : String; Value : A_Type )
      with Inline;

    Procedure Generic_Value(Object : in out Object_Object; Name : String; Value : A_Type ) is
    Begin
        Object.Element.Include( New_Item => Make( Value ), Key => Name );
    End Generic_Value;

    Function Make_Integer( X : Integer ) return Instance'Class is
        ( Make( Number(X) ) );

    Function Make_Float( X : Float ) return Instance'Class is
        ( Make( Number(X) ) );

    Procedure Value_Instance is new Generic_Value( Integer, Make_Integer );
    Procedure Value_Instance is new Generic_Value( Float,   Make_Float   );
    Procedure Value_Instance is new Generic_Value( Boolean );
    Procedure Value_Instance is new Generic_Value( String  );
    Procedure Value_Instance is new Generic_Value( Number  );

    Procedure Value(Object : in out Object_Object; Name : String; Value : Boolean )
      renames Value_Instance;
    Procedure Value(Object : in out Object_Object; Name : String; Value : Integer )
      renames Value_Instance;
    Procedure Value(Object : in out Object_Object; Name : String; Value : Float   )
      renames Value_Instance;
    Procedure Value(Object : in out Object_Object; Name : String; Value : String  )
      renames Value_Instance;
    Procedure Value(Object : in out Object_Object; Name : String; Value : Number  )
      renames Value_Instance;

    Procedure Value(Object : in out Object_Object; Name : String; Value : Instance'Class ) is
    Begin
        if Object.Element.Contains( Name ) then
            Object.Element(Name):= Value;
        else
            Object.Element.Include( New_Item => Value, Key => Name );
        end if;
    End Value;



    Overriding
    Function Kind(Object : Object_Object) return Value_Kind is (Object.Value_Type);

    -----------------
    -- CONVERSIONS --
    -----------------
    Function "+"(Right : String) return Wide_Wide_String
        renames To_Wide_Wide_String;
    Function "+"(Right : Wide_Wide_String) return String_Holder.Holder is
        ( String_Holder.To_Holder( To_String(Right) ) ); --renames String_Holder.To_Holder;
    Function "+"(Right : String) return  String_Holder.Holder is
        ( +Wide_Wide_String'(+Right) ) with Inline;

    ----------
    -- MAKE --
    ----------
    Function Make                    return Instance'Class is
      ( Null_Object'(Value_Type => VK_Null) );
    Function Make ( Item : String  ) return Instance'Class is
      ( String_Object'(Value_Type => VK_String, Element => +Item) );
    Function Make ( Item : Number  ) return Instance'Class is
      ( Number_Object'(Value_Type => VK_Number, Element =>  Item) );
    Function Make ( Item : Boolean ) return Instance'Class is
      (if Item then True_Object' (Value_Type => VK_True )
       else         False_Object'(Value_Type => VK_False) );

    Function Make_Array( Length  : Natural:=   0;
                         Default : Number := 0.0
                       ) return Instance'Class is
        Use Ada.Containers;
        Count : Constant Count_Type:= Count_Type(Length);
        Value : Constant Number_Object:= Number_Object'(VK_Number, Default );
        Function Constructor return Array_Object with Inline is
        Begin
            Return Result : Array_Object :=
              Array_Object'(Value_Type => VK_Array,
                            Element => Value_Array.Empty_Vector) do
                Result.Element.Reserve_Capacity( Count );
                Result.Element.Append( Value,    Count );
            End return;
        End Constructor;
    Begin
        Return Result : Constant Instance'Class := Constructor;
    End Make_Array;

    Function Make_Object return Instance'Class is
        Use Name_Value_Pairs;
    Begin
        Return Result : Instance'Class:=
          Object_Object'( Value_Type => VK_Object, Element => Empty_Map );
    End Make_Object;


  procedure JSON_Class_Output(
     Stream : not null access Ada.Streams.Root_Stream_Type'Class;
     Item   : in Instance'Class) is
    Begin
        null;
    End JSON_Class_Output;

  Function JSON_Class_Input
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
    return Instance'Class --is
        renames Constructors.Value_Constructor;

--  --          Function Next return Character is
--  --          Begin
--  --              Return Result : Character do
--  --                  Loop
--  --                      Result:= Character'Input( Stream );
--  --                      Exit when Result not in Whitespace
--  --                            and Result not in Control_Character;
--  --                  End Loop;
--  --              end return;
--  --          End Next;
--    Begin
--          declare
--              Use Constructors;
--              Param : Aliased Parameter:=
--                Parameter'(Buffer => New Character'(Next), Stream => Stream);
--              C     : Character renames Param.Buffer.All;
--          begin
--              Return Result : Instance'Class :=
--                (case C is
--                     when '{' => Object_Object'(Constructor(Param'Access)),
--                     when '[' => Array_Object' (Constructor(Param'Access)),
--                     when '0'..'9'
--                        | '-' => Number_Object'(Constructor(Param'Access)),
--                     when '"' => String_Object'(Constructor(Param'Access)),
--                     when 't' => True_Object'  (Constructor(Param'Access)),
--                     when 'f' => False_Object' (Constructor(Param'Access)),
--                     when 'n' => Null_Object'  (Constructor(Param'Access)),
--                     when others => raise Parse_Error
--                                     with "'" & C & "' is an invalid character."
--                );
--          end;
--    End JSON_Class_Input;

    Function Constant_Reference(Object : in Object_Object;
                                Key    : in String
                               ) return String is
    Begin
        Do_Index:
        Declare
            Index : Instance'Class renames Object.Element(Key);
        Begin
            Return Result : Constant String :=
              (if Index in String_Object then
                      -String_Object( Index )
               else To_String( Index ));
        End Do_Index;
--      Exception
--          when Name_Error => Raise Bad_Index;
    End Constant_Reference;

    Function Array_Constant(Object : in Array_Object;
                             Key    : in Natural
                            ) return Instance'Class is
    Begin
        Return Object.Element(Key);
--      Exception
--          when Name_Error => Raise Bad_Index;
    End Array_Constant;



    Function "**"(Left : Instance'Class; Right : String ) return String is
      (if Left in Object_Object and then Object_Object(Left).Element.Contains(Right)
       then To_String( Object_Object(Left).Element(Right) )
       else ""
      );

    Function "**"(Left : Instance'Class; Right : Natural ) return Instance'Class is
      (if Left in Array_Object and then Natural(Array_Object(Left).Element.Length) > Right
       then Array_Object(Left).Element(Right)
       else Null_Object'(Value_Type => VK_Null)
      );

    Procedure Include(Object : in out Object_Object; Name : String; Value : Instance'Class ) is
    Begin
        Object.Element.Include(Key => Name, New_Item => Value);
    End Include;


    Procedure Include(Left : in out Object_Object; Right : in Object_Object ) is
        Package NVP renames Name_Value_Pairs;
    Begin
        For C in Right.Element.Iterate loop
            Left.Element.Include(New_Item => NVP.Element(C), Key => NVP.Key(C));
        end loop;
    End Include;

    Function Exists(Object : in out Object_Object; Name : String) return Boolean is
      ( Object.Element.Contains(Name) );

End NSO.JSON;

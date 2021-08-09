With
INI,
Ada.Streams,
Ada.Containers.Indefinite_Vectors,
Ada.Containers.Indefinite_Holders,
Ada.Containers.Indefinite_Ordered_Maps,
Ada.Containers.Indefinite_Ordered_Sets,
Ada.Strings.Less_Case_Insensitive,
Ada.Strings.Equal_Case_Insensitive;

Package NSO.Types with Preelaborate is
    Package String_Vector is new Ada.Containers.Indefinite_Vectors(
       Element_Type => String,
       Index_Type   => Positive,
       "="          => Ada.Strings.Equal_Case_Insensitive
      );


    package String_Holder is new Ada.Containers.Indefinite_Holders(
       Element_Type => String,
       "="          => Ada.Strings.Equal_Case_Insensitive
      );


    Package String_Map is new Ada.Containers.Indefinite_Ordered_Maps(
       "<"          => Ada.Strings.Less_Case_Insensitive,
       "="          => Ada.Strings.Equal_Case_Insensitive,
       Key_Type     => String,
       Element_Type => String
      );

    package String_Set is new Ada.Containers.Indefinite_Ordered_Sets(
       "<"          => Ada.Strings.Less_Case_Insensitive,
       "="          => Ada.Strings.Equal_Case_Insensitive,
       Element_Type => String
      );


    Type Bit_Vector is Array(Natural range <>) of Boolean
      with Component_Size => 1, Default_Component_Value => False;

    Type Direction  is (E, NE, N, NW, W, SW, S, SE);
    Type Sky        is (Clear, Partly_Cloudy, Mostly_Cloudy, Overcast);
    Type Reports    is (Weather);

--      --
--      Type Instrument is (FIRS, ROSA, ZYLA, SPINOR, HSG);
--
--      -- Spectral Band
--      --
--      -- Note: One Angstrom is 0.1 nm,
--      Type S_Band     is (
--         Å03500, Å03933, Å04170, Å04503, Å05890, Å06300,
--         Å06563, Å08540, Å10830
--        );
--
--
--      H_Alpha : Constant S_Band := Å06563;
--      Ca_II_K : Constant S_Band := Å03933;
--      G_Band  : Constant S_Band := Å04503;



    -- String_Stream allows uses a string to buffer the underlying stream,
    -- it may be initialized with content from a string or a given length for
    -- the string underlying the stream.
    --
    -- This is intended for the construction and consumption of string-data
    -- using stream-operations
    Type String_Stream(<>) is new Ada.Streams.Root_Stream_Type with Private;

    Subtype Root_Stream_Class is Ada.Streams.Root_Stream_Type'Class;

    -- Create a String_Stream.
    Function "+"( Length : Natural ) return String_Stream;
    Function "+"( Text   : String  ) return String_Stream;
    Function "+"( Length : Natural ) return not null access Root_Stream_Class;
    Function "+"( Text   : String  ) return not null access Root_Stream_Class;

    -- Retrieve the remaining string-data; the (POSITION..DATA'LENGTH) slice.
    Function "-"( Stream : String_Stream ) return String;

    -- Retrieve the string-data; the (1..DATA'LENGTH) slice.
    Function Data(Stream : String_Stream ) return String;

Private
    Pragma Assert( Ada.Streams.Stream_Element'Size = String'Component_Size );

    Overriding
    procedure Read
      (Stream : in out String_Stream;
       Item   :    out Ada.Streams.Stream_Element_Array;
       Last   :    out Ada.Streams.Stream_Element_Offset);

    Overriding
    procedure Write
      (Stream : in out String_Stream;
       Item   :        Ada.Streams.Stream_Element_Array);

    Type String_Stream(Length : Ada.Streams.Stream_Element_Count) is
      new Ada.Streams.Root_Stream_Type with record
        Data     : Ada.Streams.Stream_Element_Array(1..Length);
        Position : Ada.Streams.Stream_Element_Count;
    End record;
End NSO.Types;

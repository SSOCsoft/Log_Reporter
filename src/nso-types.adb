With
Ada.Unchecked_Conversion,
Ada.IO_Exceptions;

Package Body NSO.Types is
    Use Ada.Streams;

    -------------------
    --  INITALIZERS  --
    -------------------

    Function From_String( Text   : String  ) return String_Stream
      with Inline, Pure_Function;
    Function Buffer     ( Length : Natural ) return String_Stream
      with Inline, Pure_Function;


    --------------
    --  R E A D --
    --------------

    Procedure Read
      (Stream : in out String_Stream;
       Item   :    out Ada.Streams.Stream_Element_Array;
       Last   :    out Ada.Streams.Stream_Element_Offset) is
        Use Ada.IO_Exceptions, Ada.Streams;
    Begin
        -- When there is a read of zero, do nothing.
        -- When there is a read beyond the buffer's bounds, raise an exception.
        --	Note: I've used two cases here-
        --		1) when the read is greater than the buffer,
        --		2) when the read would go beyond the buffer.
        -- Finally, read the given amount of data and update the position.
        if Item'Length = 0 then
            null;
        elsif Item'Length > Stream.Data'Length then
            Raise End_Error with "Request is larger than the buffer's size.";
        elsif Stream_Element_Offset'Pred(Stream.Position)+Item'Length > Stream.Data'Length then
            Raise End_Error with "Buffer will over-read.";
        else
            Declare
                Subtype Selection is Stream_Element_Offset range
                  Stream.Position..Stream.Position+Stream_Element_Offset'Pred(Item'Length);
            Begin
                Item(Item'Range):= Stream.Data(Selection);
                Stream.Position:= Stream_Element_Offset'Succ(Selection'Last);
                Last:= Selection'Last;--Stream.Position;
            End;
        end if;
    End Read;


    -----------------
    --  W R I T E  --
    -----------------

    Procedure Write
      (Stream : in out String_Stream;
       Item   :        Ada.Streams.Stream_Element_Array) is
    Begin
        Declare
            Subtype Selection is Stream_Element_Offset range
              Stream.Position..Stream.Position+Stream_Element_Offset'Pred(Item'Length);
        Begin
            Stream.Data(Selection):= Item(Item'Range);
            Stream.Position:= Stream_Element_Offset'Succ(Selection'Last);
        End;
    End Write;


    ----------------------------------
    --  INITALIZER IMPLEMENTATIONS  --
    ----------------------------------

    -- Create a buffer of the given length, zero-filled.
    Function Buffer( Length : Natural ) return String_Stream is
        Len : Constant Ada.Streams.Stream_Element_Offset :=
          Ada.Streams.Stream_Element_Offset(Length);
    Begin
        Return Result : Constant String_Stream:=
          (Root_Stream_Type with
           Position =>  1,
           Data     => (1..Len => 0),
           Length   =>  Len
          );
    End Buffer;

    -- Create a buffer from the given string.
    Function From_String( Text : String ) return String_Stream is
        Use Ada.Streams;

        Subtype Element_Range is Stream_Element_Offset range
          Stream_Element_Offset(Text'First)..Stream_Element_Offset(Text'Last);
        Subtype Constrained_Array  is Stream_Element_Array(Element_Range);
        Subtype Constrained_String is String(Text'Range);

        Function Convert is new Ada.Unchecked_Conversion(
           Source => Constrained_String,
           Target => Constrained_Array
          );
    Begin
        Return Result : Constant String_Stream:=
          (Root_Stream_Type with
           Position => Element_Range'First,
           Data     => Convert( Text ),
           Length   => Text'Length
          );
    End From_String;


    -- Classwide returning renames, for consistancy/overload.
    Function To_Stream( Text   : String  ) return Root_Stream_Class is
        ( From_String(Text) ) with Inline, Pure_Function;
    Function To_Stream( Length : Natural ) return Root_Stream_Class is
        ( Buffer(Length)    ) with Inline, Pure_Function;


    ----------------------------
    --  CONVERSION OPERATORS  --
    ----------------------------

    -- Allocating / access-returning initalizing operations.
    Function "+"( Length : Natural ) return not null access Root_Stream_Class is
      ( New Root_Stream_Class'(To_Stream(Length)) );
    Function "+"( Text   : String  ) return not null access Root_Stream_Class is
      ( New Root_Stream_Class'(To_Stream(Text))   );

    -- Conversion from text or integer to a stream; renaming of the initalizers.
    Function "+"( Text   : String  ) return String_Stream renames From_String;
    Function "+"( Length : Natural ) return String_Stream renames Buffer;

    -- Convert a given Stream_Element_Array to a String.
    Function "-"( Data   : Ada.Streams.Stream_Element_Array ) Return String is
        Subtype Element_Range is Natural range
          Natural(Data'First)..Natural(Data'Last);
        Subtype Constrained_Array  is Stream_Element_Array(Data'Range);
        Subtype Constrained_String is String(Element_Range);

        Function Convert is new Ada.Unchecked_Conversion(
           Source => Constrained_Array,
           Target => Constrained_String
          );

    Begin
        Return Convert( Data );
    End "-";


    ----------------------
    --  DATA RETRIEVAL  --
    ----------------------

    Function "-"( Stream : String_Stream ) return String is
    Begin
        Return -Stream.Data(Stream.Position..Stream.Length);
    End "-";


    Function Data(Stream : String_Stream ) return String is
    Begin
        Return -Stream.Data;
    End Data;


End NSO.Types;

-- The Config package provides configuration parameters via GNAT's ability to
-- see Shared_Passive objects as a persistant value; unfortunately, this is not
-- usable for variable-length/unconstrained types (eg String). In order to use
-- the ability anyway we implement Pascal-style strings, which are 256-bytes and
-- store the length in the first byte.
--
-- The unary-plus operator is for converting between String and Pascal_String.
-- The Procedure/Function pairs are for setting/retriving the value of the named
-- configuration parameter and are associated with objects in the PRIVATE part.
Package Config with Shared_Passive is

    ---------------------
    -- CONSTANT VALUES --
    ---------------------

    -- A string-object is 256 Bytes.
    String_Object_Size : Constant := 256;


    -----------
    -- TYPES --
    -----------

    -- This type is the length-byte for the string, it ranges from 0 to 255.
    Type Natural_255 is range Natural'First..Natural'Pred(String_Object_Size)
     with Size => 8, Object_Size => 8;

    -- A length-prefixed string.
    Type Pascal_String is private;


    -- Conversion functions.
    Function "+"( Item : Pascal_String ) return String;
    Function "+"( Item : String ) return Pascal_String
      with Pre => Item'Length in 0..Natural'Pred(String_Object_Size);

    ------------
    -- VALUES --
    ------------

    Function  Host return Pascal_String;
    Procedure Host( Item : String )
      with Pre => Item'Length in Natural_255;

    Function  User return Pascal_String;
    Procedure User( Item : String )
      with Pre => Item'Length in Natural_255;

    Function  Password return Pascal_String;
    Procedure Password( Item : String )
      with Pre => Item'Length in Natural_255;

Private
    -- A string of 255 bytes in length.
    Subtype Max_String_255 is String(1..255);

    -- A String of up to 255 bytes, indicated by a prefixed length byte.
    -- We use the initalization-value of length 0 and all other bytes ASCII.NUL.
    -- Further, we specify to the compiler that this object is exactly sized as
    -- 2,048 Bits (8*256 Bytes) and that the size-in-RAM should be so as well.
    Type Pascal_String is record
        Length : Natural_255   := 0;
        Text   : Max_String_255:= (others => ASCII.NUL);
    end record
    with Size => 8*String_Object_Size, Object_Size => 8*String_Object_Size;


    mailhost: Pascal_String:= (
       Length => 4,
       Text => "HOST" & (5..Max_String_255'Last => <>)
      );
    mailuser: Pascal_String:= (
       Length => 4,
       Text => "USER" & (5..Max_String_255'Last => <>)
      );
    mailpassword: Pascal_String:= (
       Length => 4,
       Text => "PASS" & (5..Max_String_255'Last => <>)
      );

End Config;

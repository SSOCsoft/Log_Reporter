Package Body Config is

    -- Return the slice (1..Item.LENGTH).
    Function "+"( Item : Pascal_String ) return String is
    Begin
        Return Result : Constant String := (Item.Text(1..Natural(Item.Length)));
    End "+";

    -- Return Item concatenated with the padding to make the result-length 255.
    Function "+"( Item : String ) return Pascal_String is
        Subtype Tail is Natural range
          Natural'Succ(Item'Length)..Max_String_255'Last;
    Begin
        Return Result : Constant Pascal_String :=
          (Text   => Item &
                    (Tail => ASCII.NUL),
           Length => Item'Length);
    End "+";


    ---------
    -- GET --
    ---------

    function  Host     return Pascal_String is (mailhost);
    function  User     return Pascal_String is (mailuser);
    function  Password return Pascal_String is (mailpassword);


    ---------
    -- SET --
    ---------

    procedure Host( Item : String )     is
    Begin
        mailhost:= +Item;
    End;


    procedure User( Item : String )     is
    Begin
        mailuser:= +Item;
    End;



    procedure Password(Item : String)   is
    Begin
        mailpassword:= +Item;
    End;


End Config;

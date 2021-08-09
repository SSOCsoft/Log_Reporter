Pragma Ada_2012;

With
Interfaces,
Ada.Streams,
Ada.Wide_Wide_Characters.Unicode,
Ada.Containers.Indefinite_Holders,
Ada.Containers.Indefinite_Ordered_Maps,
Ada.Containers.Indefinite_Vectors;



Package NSO.JSON with Elaborate_Body is

    Type Value_Kind is ( VK_Object, VK_Array, VK_String, VK_Number,
                         VK_True,   VK_False, VK_Null );

    Type Instance(<>) is tagged;

    Function "**"(Left : Instance'Class; Right : String ) return String;
    Function "**"(Left : Instance'Class; Right : Natural) return Instance'Class;

    procedure JSON_Class_Output(
       Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : in Instance'Class);
    function JSON_Class_Input(
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Instance'Class;

    For Instance'Class'Input  use JSON_Class_Input;
    For Instance'Class'Output use JSON_Class_Output;

    Type Instance(Value_Type: Value_Kind) is abstract tagged null record;
--      with
--    Class'Input => JSON_Class_Input, Class'Output => JSON_Class_Output;

    Function To_String( Object : Instance'Class ) return String;
    Function "-" ( Object : Instance ) return String is Abstract;
    Function Kind( Object : Instance'Class ) return Value_Kind;
    Function Kind( Object : Instance ) return Value_Kind is abstract;

    Package Value_Array is new Ada.Containers.Indefinite_Vectors
      (Natural, Instance'Class);

    Package Name_Value_Pairs is new Ada.Containers.Indefinite_Ordered_Maps(
--             "<"          => ,
--             "="          => ,
           Key_Type     => String,
           Element_Type => Instance'Class
          );

    ------------
    --  NULL  --
    ------------

    Type Null_Object is new Instance( Value_Type => VK_Null ) with private;


    -------------
    --  FALSE  --
    -------------

    Type False_Object is new Instance( Value_Type => VK_False ) with private;
    Function Value( Object : False_Object ) return Boolean;

    ------------
    --  TRUE  --
    ------------

    Type True_Object is new Instance( Value_Type => VK_True ) with private;
    Function Value( Object : True_Object ) return Boolean;

    --------------
    --  STRING  --
    --------------

    Type String_Object is new Instance( Value_Type => VK_String ) with private;
--    Function Value(  )

    --------------
    --  NUMBER  --
    --------------

    Subtype Number is Interfaces.IEEE_Float_64 range Interfaces.IEEE_Float_64'Range;
    Type Number_Object is new Instance( Value_Type => VK_Number ) with private;
    Function Value( Object : Number_Object ) return Number;

    -------------
    --  ARRAY  --
    -------------

    Type Array_Object is new Instance( Value_Type => VK_Array ) with private
      with Constant_Indexing => Array_Constant;

    Procedure Append ( Object : in out Array_Object; Value : Instance'Class );
    Procedure Prepend( Object : in out Array_Object; Value : Instance'Class );

    Function Array_Constant(Object : in Array_Object;
                            Key    : in Natural
                            ) return Instance'Class;

    --------------
    --  OBJECT  --
    --------------

    Type Object_Object is new Instance( Value_Type => VK_Object ) with private
      with Constant_Indexing => Constant_Reference;
    Function  Value(Object : in     Object_Object; Name : String        ) return Instance'Class;
    Procedure Value(Object : in out Object_Object; Name : String; Value : Boolean );
    Procedure Value(Object : in out Object_Object; Name : String; Value : Integer );
    Procedure Value(Object : in out Object_Object; Name : String; Value : Float   );
    Procedure Value(Object : in out Object_Object; Name : String; Value : String  );
    Procedure Value(Object : in out Object_Object; Name : String; Value : Number  );
    Procedure Value(Object : in out Object_Object; Name : String; Value : Instance'Class );

    Function Exists(Object : in out Object_Object; Name : String) return Boolean;

    Function Constant_Reference(Object : in Object_Object;
                                Key    : in String
                               ) return String;

    -- Adds the Right object to the Left, merging the two.
    Procedure Include(Left : in out Object_Object; Right : in Object_Object );

    -- Associates a JSON Instance with the given name.
    Procedure Include(Object : in out Object_Object; Name : String; Value : Instance'Class );

    ----------
    -- Make --
    ----------

    Function Make                    return Instance'Class;
    Function Make ( Item : String  ) return Instance'Class;
    Function Make ( Item : Number  ) return Instance'Class;
    Function Make ( Item : Boolean ) return Instance'Class;

    -- Make_Array is *NOT* overloaded because the defaults would make calls
    -- anbiguous, considering the NULL object's parameterless constructor.
    Function Make_Array( Length  : Natural:=   0;
                         Default : Number := 0.0
                       ) return Instance'Class;

    -- Because there are no sensible method for populating a dictionary via
    -- parameter-calls, making an Object likewise cannot overload with make.
    Function Make_Object return Instance'Class;


    -- Apply is intended to help in the processing of a JSON-classed object.
    Generic
        with Procedure On_Null is null;
        with Procedure On_Boolean(                  Value : in Boolean ) is null;
        with Procedure On_String (                  Value : in String  ) is null;
        with Procedure On_Number (                  Value : in Number  ) is null;
        with procedure On_Array  ( Index : Natural; Value : in Instance'Class ) is null;
        with procedure On_Object ( Key   : String;  Value : in Instance'Class ) is null;
    Procedure Apply( Object : in Instance'Class );


    Bad_Index,
    Parse_Error : Exception;

Private

    package String_Holder is new Ada.Containers.Indefinite_Holders(
       Element_Type => String
      );


    ----------------------------------
    --  STRING CONVERSION OPERATOR  --
    ----------------------------------

    Overriding
    Function "-"(Object : Null_Object) return String;

    Overriding
    Function "-"(Object : True_Object) return String;

    Overriding
    Function "-"(Object : False_Object) return String;

    Overriding
    Function "-"(Object : String_Object) return String;

    Overriding
    Function "-"(Object : Number_Object) return String;

    Overriding
    Function "-"(Object : Array_Object) return String;

    Overriding
    Function "-"(Object : Object_Object) return String;

    Function To_String(Object : Instance'Class) return String is
      ( -Object );

    -----------------------
    --  OBJECT INDEXING  --
    -----------------------
--      Function Constant_Reference(Object : in Object_Object;
--                                  Key    : in String
--                                 ) return String;

--      Constant_Reference

    ------------------
    --  FULL VIEWS  --
    ------------------

    Type Null_Object   is new Instance( Value_Type => VK_Null   ) with null Record;
    Type False_Object  is new Instance( Value_Type => VK_False  ) with null record;
    Type True_Object   is new Instance( Value_Type => VK_True   ) with null record;
    Type String_Object is new Instance( Value_Type => VK_String ) with record
        Element : String_Holder.Holder;
    end record;
    Type Number_Object is new Instance( Value_Type => VK_Number ) with record
        Element : Number;
    end record;
    Type Array_Object is new Instance( Value_Type => VK_Array ) with record
        Element : Value_Array.Vector;
    end record;
    Type Object_Object is new Instance( Value_Type => VK_Object ) with Record
        Element : Name_Value_Pairs.Map;
    end record;
--      with Constant_Indexing => Element.Constant_Reference;
--         Variable_Indexing => Element.Reference;


    ---------------
    --  K I N D  --
    ---------------

    Function Kind( Object : Instance'Class ) return Value_Kind is
        (Object.Value_Type);

    Overriding
    Function Kind(Object : Null_Object) return Value_Kind;

    Overriding
    Function Kind(Object : False_Object) return Value_Kind;

    Overriding
    Function Kind(Object : True_Object) return Value_Kind;

    Overriding
    Function Kind(Object : String_Object) return Value_Kind;

    Overriding
    Function Kind(Object : Number_Object) return Value_Kind;

    Overriding
    Function Kind(Object : Array_Object) return Value_Kind;

    Overriding
    Function Kind(Object : Object_Object) return Value_Kind;

    --------------------------------
    --  Additional support types  --
    --------------------------------

           Type Nullable_Character  is Access All Character;
        Subtype Reference_Character is Not Null Nullable_Character;

End NSO.JSON;

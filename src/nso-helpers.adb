With
INI.Section_to_Map,
Ada.Containers,
Ada.Text_IO,
Ada.Characters.Conversions,
Ada.Characters.Latin_1,
Ada.Strings.Fixed,
Ada.Strings.Unbounded,

Gnoga.Gui.Element.Form.Fieldset;

Package Body NSO.Helpers is
    Use NSO.Types;

    Procedure Add_Discrete(
       Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
       Parent : in out Gnoga.Gui.Element.Form.Selection_Type'Class
      ) is
        Use Gnoga.Gui.Element.Form;
        Items : Array(Options'Range) of Option_Type;
    Begin
        For Index in Items'Range Loop
            declare
                Image   : String renames Options'Image(Index);
                Element : Option_Type renames Items(Index);
            begin
                Element.Create
                  (
                   --ID        => "OPTION_" & Image,
                   Form      => Form,
                   Selection => Parent,
                   Value     => Image,
                   Text      => Image,
                   Selected  => False,
                   Disabled  => False
                  );
            end;
        End loop;
    End Add_Discrete;

    Procedure Add_Vector(
       Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
       Parent : in out Gnoga.Gui.Element.Form.Selection_Type'Class;
       Input  : in     String_Vector.Vector;
       Prefix : in     String:= ""
      ) is
        Use Gnoga.Gui.Element.Form;
        Items : Array(Input.First_Index..Input.Last_Index) of Option_Type;
    Begin
        For Index in Items'Range Loop
            declare
                This    : String renames Input(Index);
                Image   : String renames "&"(Prefix, This);
                Element : Option_Type renames Items(Index);
            begin
                Element.Create
                  (
                   --ID        => "OPTION_" & Image,
                   Form      => Form,
                   Selection => Parent,
                   Value     => Image,
                   Text      => This,
                   Selected  => False,
                   Disabled  => False
                  );
            end;
        End loop;
    End Add_Vector;

    Procedure Add_Map(
       Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
       Parent : in out Gnoga.Gui.Element.Form.Selection_Type'Class;
       Input  : in     String_Map.Map;
       Prefix : in     String:= ""
      ) is
        Use Gnoga.Gui.Element.Form, NSO.Types.String_Map, Ada.Containers;
        Items   : Array(1..Input.Length) of Option_Type;
        Current : Count_Type := Items'First;
    Begin
        For KVP in Input.Iterate loop
            declare
                This    : String renames Element(KVP);
                Image   : String renames Key(KVP);
                Element : Option_Type renames Items(Current);
            begin
                Element.Create
                  (
                   --ID        => "OPTION_" & Image,
                   Form      => Form,
                   Selection => Parent,
                   Value     => Image, -- Value is the value of the selection; which should be KEY.
                   Text      => This,  -- Text is the display's value, which should be the ELEMENT.
                   Selected  => False,
                   Disabled  => False
                  );
            end;
            Current:= Count_Type'Succ(Current);
        end loop;
    End Add_Map;

    Procedure Add_Sections(
       Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
       Parent : in out Gnoga.Gui.Element.Form.Selection_Type'Class;
       Fields : in     NSO.Types.String_Vector.Vector;
       File   : in     INI.Instance;
       Prefix : in     String:= ""
      ) is
        Use Gnoga.Gui.Element.Form;
        Sections : Array(1..Natural(Fields.Length)) of Option_Group_Type;
    Begin

        For S in Sections'Range loop
            Declare
                Group : Option_Group_Type renames Sections(S);
                Name  : String renames Fields(S);
            Begin
                Group.Create(
                   Form      => Form,
                   Selection => Parent,
                   Label     => Name
                  );
                Declare
                    Function "-"(Right : String_Map.Cursor) return String is
                        E : String renames String_Map.Element(Right);
                        First : Constant Natural:= E'First;
                        Last  : Constant Natural:= E'Last;
                    Begin
                        Return (if E(First) = '"' and E(Last) = '"'
                                then E(Natural'Succ(First)..Natural'Pred(Last))
                                else E
                               );
                    End "-";


                    Mapping : String_Map.Map renames
                      INI.Section_to_Map(File, Name);
                    Options : Array(1..Natural(Mapping.Length)) of Option_Type;
                    Cursor  : Natural:= Options'First;
                    use String_Map;
                Begin
                    For KVP in Mapping.Iterate loop
                        Options(Cursor).Create(
                           Form      => Form,
                           Selection => Group,
                           Value     => Name & '.' & (-KVP),
                           Text      => (-KVP)
                          );
                        Cursor:= Natural'Succ( Cursor );
                    End loop;
                End;
            End;
        End loop;

    End Add_Sections;

    Procedure Add_Section_Groups(
       Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
       Parent : in out Gnoga.Gui.Element.Element_Type'Class;--Form.Selection_Type'Class;
       Fields : in     NSO.Types.String_Vector.Vector;
       File   : in     INI.Instance;
       Name   : in     String:= ""
      ) is
        Function Base_ID return String is
            (if Name'Length in Positive then Name&'.' else"");

        Use Gnoga.Gui.Element.Form, Gnoga.Gui.Element.Form.Fieldset;
        Sections : Array(1..Natural(Fields.Length)) of Selection_Type;
        Container: Fieldset_Type;
        Caption  : String renames Trim( Name );
    Begin
        Container.Create(Parent => Parent);--, ID => Caption);
        if Caption'Length in Positive then
            Container.Put_Legend(Value => Caption);
        end if;

        For S in Sections'Range loop
            MAKE_GROUP:
            Declare
                Group : Selection_Type renames Sections(S);
                Name  : String         renames Fields(S);
                Label : Label_Type;
            Begin
                Group.Create
                  (
                   ID        => Base_ID & Name,
                   Form      => Form,
                   Name      => Name,
                   Multiple_Select => True,
                   Visible_Lines   => 5
                  );
                Label.Create
                  (
                   Form      => Form,
                   Label_For => Group,
                   Content   => "<span style=""Display:Block; Clear:Both;"">" & Name & "</span>"
                  );
                Label.Style(Name => "display", Value => "inline-block");
                Label.Margin(Right => "5em");
                --Label.Vertical_Align(Gnoga.Gui.Element.Top);
                POPULATE_POTIONS:
                Declare
                    Function "-"(Right : String_Map.Cursor) return String is
                        E : String renames String_Map.Element(Right);
                        First : Constant Natural:= E'First;
                        Last  : Constant Natural:= E'Last;
                    Begin
                        Return (if E(First) = '"' and E(Last) = '"'
                                then E(Natural'Succ(First)..Natural'Pred(Last))
                                else E
                               );
                    End "-";


                    Mapping : String_Map.Map renames
                      INI.Section_to_Map(File, Name);
                    Options : Array(1..Natural(Mapping.Length)) of Option_Type;
                    Cursor  : Natural:= Options'First;
                    use String_Map;
                Begin
                    For KVP in Mapping.Iterate loop
                        Options(Cursor).Create(
                           Form      => Form,
                           Selection => Group,
                           Value     => Name & '.' & (-KVP),
                           Text      => (-KVP)
                          );
                        Cursor:= Natural'Succ( Cursor );
                    End loop;
                End POPULATE_POTIONS;
                Group.Place_Inside_Bottom_Of( Label );
                Label.Place_Inside_Bottom_Of(Container);
            End MAKE_GROUP;
            Container.Place_Inside_Bottom_Of(Parent);
        End loop;

    End ADD_SECTION_GROUPS;


    Function Unescape_JSON_String( Input : String ) Return String is
        Use Ada.Strings.Unbounded, Ada.Characters;
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
        -- For handling escaped characters, I've separated things into two
        -- portions: First, we handle the single-character escapes; Second,
        -- we handle the hex-code escapes.
        For C of Input loop
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
            else
                Append( C );
            end if;
        End Loop;

        Handle_Hex:
        Declare
            Hex_Escape : Constant String:= "\u";
            Location   : Natural := Index( Working, Hex_Escape );
--              Function WChar return Wide_Wide_Character is
--                  ( Wide_Wide_Character'Pos( Wide_Wide_Integer'Va ) );
        Begin
--              while Location in Positive loop
            Declare
                Subtype String_4 is String(1..4);
                Chunk : String_4            renames Slice(Working, Location, Location+3);
--                  Pos   : Long_Long_Integer   renames Long_Long_Integer'Value( "16#" & Chunk & '#' );
--                  WC    : Wide_Wide_Character renames Wide_Wide_Character'Val( Pos );
            Begin
                Ada.Text_IO.Put_Line( "CHUNK: " & Chunk );
--                  Append(Ada.Characters.Conversions.To_Character( WC ));
            End;



--                  Ada.Characters.Conversions.To_Character
--                    (
--
--                    );
--              end loop;
        End Handle_Hex;



        Return Ada.Strings.Unbounded.To_String( Working );
    End Unescape_JSON_String;


    Function Next( Stream : not null access NSO.Types.Root_Stream_Class )
      return Character_Reference is
      ( New Character'( Character'Input(Stream) ) );

    -- Given "X", produces "<X>".
    Function HTML_Open(Name : String) return String is
      ( '<' & Name & '>' ) with Inline;

    -- Produces: <Name Attribute="Value">
    Function HTML_Open(Name, Attribute, Value: String) return String is
      ( '<' & Name &' '& Attribute & "=""" & Value & """>" ) with Inline;


    -- Given "X", produces "</X>"; uses HTML_Open for bracket-consistency.
    Function HTML_Close(Name : String) return String is
      ( HTML_Open('/' & Name) ) with Inline;

    Function HTML_Tag(Name, Text : String) return String is
        ( HTML_Open(Name) & Text & HTML_Close(Name) );

    Function HTML_Tag(Name, Text, Attribute, Value : String) return String is
        ( HTML_Open(Name, Attribute, Value) & Text & HTML_Close(Name) );

    Function Trim( Input : String ) return String is
        Use Ada.Strings;
    Begin
        Return Fixed.Trim( Side => Both, Source => Input );
    End Trim;


End NSO.Helpers;

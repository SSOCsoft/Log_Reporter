Pragma Ada_2012;

With
Gnoga.Types,
Ada.Strings.Fixed,
Ada.Strings.Equal_Case_Insensitive,
Ada.Containers.Indefinite_Multiway_Trees;

Function NSO.JSON.Parameters_to_JSON( Input : in out Gnoga.Types.Data_Map_Type )
    return NSO.JSON.Instance'Class is

    -- PT is a package for the construction of a textual-tree from the submitted
    -- values; we use this because the values, whether indexed or not, form a
    -- tree structure.
    --
    -- The values of Input are such that the common portions of the text-string
    -- may be rooted in a common object, with the root-object holding all values
    -- which have been submitted.
    --
    -- EXAMPLE:
    --  *Input: ("Item[Color]":"Brown"; "Item[Number]":"3"; "Name":"Steve")
    --  *Output: {"Item" : { "Color" : "Brown", "Number" : "3"}
    --            "Name" : "Steve"
    --           }
    Package PT is New Ada.Containers.Indefinite_Multiway_Trees
      (Element_Type => String,
       "="          => Ada.Strings.Equal_Case_Insensitive
      );

    -- This function creates a JSON object from the traversal of the tree we
    -- created from the passed parameters.
    Function From_Tree( Input : in PT.Tree ) return NSO.JSON.Object_Object;

    -- This function creates a tree from the traversal of the parameters we
    -- obtain from the submission of the Form data.
    Function To_Tree  ( Input : in Gnoga.Types.Data_Map_Type ) return PT.Tree;

    -- Return either a single String value, or an array of values delimited by
    -- the Unit_Seperator control-character.
    -- Note: The parameter 'Value' is the string associated with a given key.
    Function Handle_Value( Value: String ) return Instance'Class;


    ---------------------
    -- IMPLEMENTATIONS --
    ---------------------

    Function To_Tree( Input : in Gnoga.Types.Data_Map_Type ) return PT.Tree is
        Open   : Constant Character := '[';
        Close  : Constant Character := ']';
        Result : PT.Tree := PT.Empty_Tree;

        -- We use this procedure to track our position within the tree, as the
        -- creation of trees from the indexed-keys lends itself to a recursive
        -- implementation. We default 'Current' to the tree's root, but with
        -- subsequent non-defaulted parameters so that it may only be used with
        -- named parameter-association; the recursive calls should not use the
        -- named parameter association.
        Procedure To_Tree( Current    : PT.Cursor:= Result.Root;
                           Key, Value : String
                          ) is

            Function First_Index(Bracket : Character) return Natural is
              (Ada.Strings.Fixed.Index(
                  Source  => Key,
                  Pattern => (1 => Bracket),
                  From    => Key'First,
                  Going   => Ada.Strings.Forward
                 )
              ) with Inline;

            First_Open  : Natural renames First_Index( Open  );
            First_Close : Natural renames First_Index( Close );
            This        : PT.Cursor;
        Begin

            -- There's something before the index.
            -- So we create a node for it, then pass the remainder recursively.
            if First_Open > Key'First then
                Result.Insert_Child(
                   Parent   => Current,
                   Before   => PT.No_Element,
                   New_Item => Key(Key'First..First_Open-1),
                   Position => This
                  );
                To_Tree( This, Key(First_Open..key'Last), Value );

            -- There's no indexing at all.
            -- We create a node for this, the key of the pair, then we create
            -- a node directly under it for the value of the pair.
            elsif First_Open = First_Close then -- no indexing,
                Result.Insert_Child(
                   Parent   => Current,
                   Before   => PT.No_Element,
                   New_Item => Key,
                   Position => This
                  );
                Result.Insert_Child(
                   Parent   => This,
                   Before   => PT.No_Element,
                   New_Item => Value,
                   Position => This
                  );

            -- The 'normal' case.
            -- We create a node for this name, then we check to see if this is
            -- the final index: if it is, then we create a child-node for the
            -- associated value; if not, recursively call with the next index.
            elsif First_Open < First_Close then
                Result.Insert_Child(
                   Parent   => Current,
                   Before   => PT.No_Element,
                   New_Item => Key(First_Open+1..First_Close-1),
                   Position => This
                  );
                if First_Close = Key'Last then
                    Result.Insert_Child(
                       Parent   => This,
                       Before   => PT.No_Element,
                       New_Item => Value,
                       Position => This
                      );
                else
                    To_Tree(This, Key(First_Close+1..Key'Last), Value);
                end if;

            -- Malformed indexing.
            -- The only way we get First_Open > First_Close is to have a key
            -- containing a close-index prior to an open-index; this may be due
            -- to a bad form-input naming or creation. Check the names.
            else
                raise NSO.JSON.Parse_Error with "Bad indexing: '" & Key & "'.";
            end if;
        End To_Tree;

    Begin
        -- Here we operate on the Key-Value Pairs that are the parameters being
        -- passed from the form submission; we are using the default parameter
        -- for CURRENT so that the root-node assocites with the first call into
        -- the procedure.
        for KVP in Input.Iterate loop
            To_Tree(
               Key   => Gnoga.Types.Data_Maps.Key(KVP),
               Value => Gnoga.Types.Data_Maps.Element(KVP)
              );
        end loop;

        Return Result;
    End To_Tree;

    Function From_Tree( Input : PT.Tree ) return NSO.JSON.Object_Object is

        -- We need four values to return from the KIND function, these were
        -- chosen to coorespond to the four cases we have to deal with.
        Subtype Value_Style is Value_Kind
          with Static_Predicate => Value_Style in
            VK_Null | VK_String | VK_Array | VK_Object;

        -- If all children are leaves then:
        --  0 children    -> This is a leaf            -> VK_Null.
        --  1 child       -> child is a value          -> VK_String.
        --  more children -> children are an array     -> VK_Array.
        -- Otherwise:
        --  This is an object w/ more processing to do -> VK_Object.
        --
        -- WARNING: These are NOT the actual parsed-types, but rather used to
        -- indicate what sort of structure we are dealing with in the tree.
        Function Kind(Position : in PT.Cursor) return Value_Style with Inline is
            Children           : constant Natural :=
              Natural(PT.Child_Count(Position));
            Are_Children_Leaves: constant Boolean :=
              (for all Child in Input.Iterate_Children(Position) =>
                   PT.Is_Leaf(Child) );
        Begin
            Return (if not Are_Children_Leaves then VK_Object
                    else (case Children is
                          when 0      => VK_Null,
                          when 1      => VK_String,
                          when others => VK_Array
                     )
                   );
        End Kind;

        -- Given a position, and a current-object, we traverse the tree in order
        -- to create the cooresponding object.
        Procedure  Make_Instance(
           Position : in     PT.Cursor;
           Current  : in out Object_Object
          ) is

            Style : Value_Style renames Kind( Position );
        Begin
            Declare
                Text   : String renames PT.Element(Position);
                Result : Instance'Class:=
                  (case Style is
                       when VK_Array  => Make_Array,
                       when VK_String => Handle_Value( PT.Element(PT.First_Child(Position)) ),
                       when VK_Null   => Handle_Value( PT.Element(Position) ),
                       when VK_Object => (if   Current.Exists(Text)
                                          then Current.Value(Text)
                                          else Make_Object
                                         )
                  );
            Begin
                case Style is
                    -- We should already have created the node from a prior call.
                    when VK_Null   => Return;

                    -- All we need to do is let the call to value below
                    -- associate the result above with this node's text.
                    when VK_String => null;

                    -- Add the children-nodes to the result-array's data.
                    when VK_Array  =>
                        declare
                            Object : Array_Object renames Array_Object(Result);
                            Item   : PT.Cursor:= PT.First_Child(Position);
                        begin
                            loop
                                Object.Append( Handle_Value(PT.Element(Item)) );
                                PT.Next_Sibling(Item);
                                exit when not PT.Has_Element( Item );
                            end loop;
                        end;

                    -- Recursively call this function for child-nodes.
                    when VK_Object =>
                        declare
                            Object : Object_Object renames Object_Object(Result);
                            Item   : PT.Cursor:= PT.First_Child(Position);
                        begin
                            loop
                                Make_Instance(Item, Object);
                                PT.Next_Sibling(Item);
                                exit when not PT.Has_Element( Item );
                            end loop;
                        end;
                end case;

                -- Associate the result-object with this node's textual-value.
                Current.Value(Text, Result);
            End;
        End Make_Instance;

    begin
        -- We create an empty object, and then use it as our 'current' in our
        -- calls to Make_Instance, thus constructing our JSON object.
        Return Result : Object_Object:= Object_Object(NSO.JSON.Make_Object) do
            For Child in Input.Iterate_Children(Input.Root) loop
                Make_Instance(Child, Result);
            End loop;
        End return;
    end From_Tree;

    Function Handle_Value( Value: String ) return Instance'Class is
        Unit_Separator : Constant Character := Character'Val( 31 );
        US_STR         : Constant String    := (1 => Unit_Separator);
        First_Index    : Natural := Value'First;
        Last_Index     : Natural :=
          Ada.Strings.Fixed.Index(
             Pattern => US_STR,
             Source  => Value,
             From    => First_Index
            );
    Begin
        if Last_Index not in Positive then
            Return Make( Value );
        else
            Return Result : Array_Object := Array_Object(Make_Array) do
                COLLECT_ELEMENTS:
                loop
                    declare
                        Marker : Constant Natural:=
                          (if Last_Index in Positive
                           then Positive'Pred(Last_Index)
                           else Value'last
                          );
                        Subtype Slice is Positive range First_Index..Marker;
                        Data : String renames Value(Slice);
                    begin
                        Array_Object(Result).Append( Make(Data) );
                        exit COLLECT_ELEMENTS when Slice'Last = Value'Last;
                        First_Index:= Last_Index + US_STR'Length;
                        Last_Index := Ada.Strings.Fixed.Index(
                           Pattern => US_STR,
                           Source  => Value,
                           From    => First_Index
                          );
                    end;
                end loop COLLECT_ELEMENTS;
            End return;
        end if;
    End Handle_Value;

Begin
    Return Result : Constant Instance'Class:= From_Tree( To_Tree( Input ) ) do
        Input.Clear;
    End return;
End NSO.JSON.Parameters_to_JSON;

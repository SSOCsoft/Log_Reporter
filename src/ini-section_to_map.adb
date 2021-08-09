Function INI.Section_to_Map( Object : in     Instance;
                             Section: in     String:= ""
                            ) return NSO.Types.String_Map.Map is
    Use NSO.Types.String_Map;
Begin
    Return Result : Map do
        For Item in Object(Section).Iterate loop
            Declare
                Key   : String       renames KEY_VALUE_MAP.Key( Item );
                Value : Value_Object renames KEY_Value_MAP.Element(Item);
                Image : Constant String := --renames "ABS"(Value);
                  (if Value.Kind = vt_String then Value.String_Value
                   else ABS Value);
            Begin
                Result.Include(
                   Key      => Key,
                   New_Item => Image
                  );
            End;
        end loop;
    Exception
        when CONSTRAINT_ERROR => null;
    End return;
End INI.Section_to_Map;

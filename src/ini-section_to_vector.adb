Function INI.Section_to_Vector( Object : in     Instance;
                                Section: in     String:= ""
                              ) return NSO.Types.String_Vector.Vector is
    Use NSO.Types.String_Vector;
Begin
    Return Result : Vector do
        For Item in Object(Section).Iterate loop
            Declare
                Key   : String       renames KEY_VALUE_MAP.Key( Item );
                Value : Value_Object renames KEY_Value_MAP.Element(Item);
                Image : String renames -- "ABS"(Value);
                  String'(if Value.Kind = vt_String then Value.String_Value
                          else ABS Value);
            Begin
                Result.Append( Image );
            End;
        end loop;
    Exception
        when CONSTRAINT_ERROR => null;
    End return;
End INI.Section_to_Vector;

Function INI.Section_Vector( Object : in     Instance )
   return NSO.Types.String_Vector.Vector is
    use NSO.Types.String_Vector;
Begin
    Return Result : Vector := Empty_Vector do
        For Section in Object.Iterate loop
            Declare
                Key : String renames INI.KEY_SECTION_MAP.KEY(Section);
            Begin
                if Key'Length in Positive then
                    Result.Append( Key );
                end if;
            End;
        end loop;
    End return;
End INI.Section_Vector;

With
Ada.Text_IO;

Procedure INI.Print(Object : Instance) is
    Procedure DEBUG_Print( Object : Value_Object ) is
    Begin
        Ada.Text_IO.Put_Line(if Object.Kind = vt_String
                             then Object.String_Value
                             else ABS Object );
    End DEBUG_Print;

    Procedure DEBUG_Print( Object : KEY_VALUE_MAP.Map ) is
        Package KVM renames KEY_VALUE_MAP;
    Begin
        For Item in Object.Iterate loop
            Ada.Text_IO.Put( "    " & KVM.Key(Item) & '=' );
            DEBUG_Print( KVM.Element(Item) );
        End loop;
    End DEBUG_Print;

    Package KSM renames KEY_SECTION_MAP;
Begin
    For Section in Object.Iterate Loop
        Ada.Text_IO.Put_Line( KSM.Key(Section) );
        DEBUG_Print( KSM.Element(Section) );
    End Loop;
End INI.Print;

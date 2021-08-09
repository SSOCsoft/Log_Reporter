With
NSO.Types;

-- Section_to_Vector takes a section by name, and returns
-- the children by value, converted to string if necessary.
Function INI.Section_to_Vector( Object : in     Instance;
                                Section: in     String:= ""
                              ) return NSO.Types.String_Vector.Vector;

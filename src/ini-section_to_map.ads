With
NSO.Types;

-- Section_to_Map takes a section by name, and returns
-- the map of keys to the children by value, converted to string if necessary.
Function INI.Section_to_Map( Object : in     Instance;
                             Section: in     String:= ""
                           ) return NSO.Types.String_Map.Map;

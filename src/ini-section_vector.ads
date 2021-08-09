With
NSO.Types;

-- Section_Vector returns the names of the sections; empty-name excluded.
Function INI.Section_Vector( Object : in     Instance )
   return NSO.Types.String_Vector.Vector;

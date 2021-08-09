Pragma Ada_2012;

With
Gnoga.Types;

--------------------------
--  PARAMETERS_TO_JSON  --
--------------------------------------------------------------------------------
-- This function takes the parameters collected from an HTML Form and returns --
-- a result of the JSON object. The parameters on the form should be indexed  --
-- via square brackets, in the form of "BASE[INDEX_A][INDEX_B]" and values    --
-- are text-encoded as either plain-strings or as an array of strings using   --
-- the ASCII Unit_Seperator character (DEC 31, HEX 1F).                       --
--                                                                            --
-- NOTE: This function resets the Input to an empty map.                      --
--------------------------------------------------------------------------------
Function NSO.JSON.Parameters_to_JSON( Input : in out Gnoga.Types.Data_Map_Type )
    return NSO.JSON.Instance'Class;

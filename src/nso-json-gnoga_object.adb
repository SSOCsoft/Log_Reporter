Pragma Ada_2012;

With
Ada.Unchecked_Conversion,
NSO.Types,
--NSO.Types.Report_Objects,
Gnoga.Server.Connection;

Package Body NSO.JSON.Gnoga_Object is


    JS_Make_Path        : Constant String :=
      "let pathing = (str) => "&
      " {return Array.from(str).reduceRight( (accumulator, current) =>"&
      "  {switch (current){"&
      "   case '[' : accumulator.push(''); break;"&
      "   case ']' : break;"&
      "     default: accumulator[accumulator.length-1] = current+accumulator[accumulator.length-1];"&
      "   };"&
      "  return accumulator; },['']).reverse();"&
      " }; /*pathing*/";

    JS_Get_Parameters   : Constant String:=
      "let collect = () => {"&
      " let object = {};"&
      " new URLSearchParams( new URL( document.location.href ).search ).forEach((value, key) => {"&
      "   /* Reflect.has in favor of: object.hasOwnProperty(key) */"&
      "   if(!Reflect.has(object, key)){"&
      "     object[key] = value;"&
      "      return;"&
      "   }"&
      "   if(!Array.isArray(object[key])){"&
      "      object[key] = [object[key]];"&
      "   }"&
      "   object[key].push(value);"&
      "  });"&
      " return object;"&
      "}; /* collect */";

    JS_Build_Object     : constant String:=
      "let build_obj = (kvp) => {"&
      "  let object = {};"&
      "  Object.entries(kvp).forEach(([key, value]) => {"&
      "    let keyset = pathing( key ).reduce( (acc, cur, x, arr) =>"&
      "    {const exists  = Reflect.has(acc,cur);"&
      "     const is_last = (x == arr.length-1);"&
      "     if (exists)"&
      "       {if (is_last) { acc[cur].push(value); } else {return acc[cur];}}"&
      "     else"&
      "       {if (is_last) { acc[cur] = []; acc[cur].push(value);} else { acc[cur]={}; }};"&
      "     return acc[cur];"&
      "    },object);"&
      "  });"&
      " return object;"&
      "}; /* build_obj */";

    JS_Normalize_Object : Constant String:=
      "let normalize = (o) => {"&
      "  let object = {};"&
      "  Object.entries(o).forEach(([key, value]) => {"&
      "    if (Array.isArray(value) && (value.length == 1)) { object[key] = value[0]; }"&
      "    else                                             { object[key] = normalize(value);};"&
      "   },o);"&
      "  return object;"&
      "}; /* Normalize */";

    JS_Parameter_Object : Constant String :=
      --"{"&
         JS_Make_Path &
         JS_Get_Parameters &
         JS_Build_Object &
         JS_Normalize_Object &
      "  JSON.stringify( normalize(build_obj(collect())) );"&
      "";--"}";

    Function Get_JSON(Object : Base_Type'Class) return JSON_Class is
      (JSON_Class'Class'Input(
          NSO.Types."+"(
             Gnoga.Server.Connection.Execute_Script(
                ID     => Object.Connection_ID,
                Script => "JSON.stringify( params )"
               ) -- Returns the String that is the JSON form-parameters.
            ) -- Uses the given String as a Stream.
         ) -- Reads the JSON from the given Stream.
      ); -- Returns the JSON.

    Function Parameters (Object : Form_Object) return JSON_Class is
      (JSON_Class'Class'Input(
          NSO.Types."+"(
             Gnoga.Server.Connection.Execute_Script(
                ID     => Object.Connection_ID,
                Script =>
			JS_Parameter_Object
--  			"object = {};"&
--  			"new URLSearchParams( new URL( document.location.href )"&
--  			".search ).forEach((value, key) => {"&
--  			"    /* Reflect.has in favor of: object.hasOwnProperty(key) */ "&
--  			"    if(!Reflect.has(object, key)){"&
--  			"        object[key] = value;"&
--  			"        return;"&
--  			"    }"&
--  			"    if(!Array.isArray(object[key])){"&
--  			"        object[key] = [object[key]];"&
--  			"    }"&
--  			"    object[key].push(value);"&
--  			"});"&
--  			"JSON.stringify( object );"
               ) -- Returns the String that is the JSON form-parameters.
            ) -- Uses the given String as a Stream.
         ) -- Reads the JSON from the given Stream.
      ); -- Returns the JSON.



--      Generic
--          Type Params(<>) is limited private;
--          Type Report(<>) is limited private;
--          Type View  (<>) is limited private;
--
--          with Function Get(Object : Params) return Report is <>;
--          with Function Get(Object : Report) return View   is <>;
    Function Report_View (Object : Form_Object'Class) Return View is
--          Function Submission(Object : Form  ) return Params renames Get;
--          Function Parameters(Object : Params) return Report renames Get;
--          Function Processing(Object : Report) return View   renames Get;

        Form_Data : Form renames Form( Object );
    Begin
        Return Result : Constant View :=
          Processing( Parameters( Submission( Object => Form_Data ) ) );
    End Report_View;

    Function As_Form    (Object : Base_Type'Class) return Form_Object is
        Function Convert is new Ada.Unchecked_Conversion(
           Source => Base_Type,
           Target => Form_Object
          );

        Base : Base_Type   renames Base_Type( Object );
    Begin
        -- Note: For completeness this should handle upwards-conversion, too.
        -- TODO: Add a RENAMES view-conversion branch if the parameter descends from Form_Object.
        Return Convert( Base );
    End As_Form;


    Package Body Generic_Reporter is
        Function Generate_Report return View is
            ( Processing );
    End Generic_Reporter;

End NSO.JSON.Gnoga_Object;

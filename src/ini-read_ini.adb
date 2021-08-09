With
Ada.Text_IO.Text_Streams;

Function INI.Read_INI(File_Name : String) return INI.Instance is
    Function Open_File(File_Name : String) return Ada.Text_IO.File_Type is
        use Ada.Text_IO;
    Begin
        Return Result : File_Type do
            Ada.Text_IO.Open(
               File => Result,
               Mode => In_File,
               Name => File_Name
              );
        end return;
    End Open_File;

    INI_File   : Ada.Text_IO.File_Type := Open_File(File_Name);
    INI_Stream : Ada.Text_IO.Text_Streams.Stream_Access renames
      Ada.Text_IO.Text_Streams.Stream( File => INI_File );
    Use INI;
Begin
    Return Result : Constant Instance:= Instance'Input( INI_Stream ) do
        Ada.Text_IO.Close( INI_File );
    End return;
End INI.Read_INI;

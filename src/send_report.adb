With
NSO.Helpers,
Ada.Text_IO,
Config, INI,
GNAT.Sockets.Server,
GNAT.Sockets.SMTP.Client.Synchronous;

Procedure Send_Report(Text : String:= ""; Params : INI.Instance:= INI.Empty) is
    use all type GNAT.Sockets.SMTP.Client.Mail;
    Use GNAT.Sockets.SMTP.Client, GNAT.Sockets.Server;

    DEBUG   : Constant Boolean:= False;
    EMAIL   : Constant Boolean:= True;

    Recpipant : Constant String:=
      (if DEBUG
       then "<efish@nmsu.edu>"      -- Developer's e-mail address.
       else "report_ops@sp.nso.edu" -- The reporting mail-list; note: SP.NSO.EDU
      );

    Function Test_Message return String is
        Use NSO.Helpers;
        CRLF   : Constant String := (ASCII.CR, ASCII.LF);

        -- Wraps the given text in CRLF.
        Function "+"(Text : String) return String is
          (CRLF & Text & CRLF) with Inline;
        Function "+"(Left, Right: String) return String is
          (CRLF & Left & (+Right)) with Inline;

        MaxWide: Constant String := "maxwidth: 120em;";
        Head   : Constant String := HTML_Tag("head", "");
        Content: Constant String := HTML_Tag("body", Text, "Style", MaxWide);
--          Prolog : Constant String := "<HTML><head></head><body style=""maxwidth: 120em;"">";
--          Epilog : Constant String := "</body></HTML>";
--          Function As_HTML(HTML_Body : String) return String is
--            ( Prolog & CRLF & HTML_Body & CRLF & Epilog) with Inline;
    Begin
        Return Result : Constant String := HTML_Tag("HTML", Head + Content) do
          --As_HTML(Text) do
            if DEBUG then
                Ada.Text_IO.Put_Line( Result );
            end if;
        End return;
    End;


    Message : Mail renames Create
      (Mime     => "text/html",
       Subject  => "EMAIL-REPORT" & (if DEBUG then "    [DEBUG]" else ""),
       From     => "<LVTT@NSO.EDU>",
       To       => Recpipant,
       Contents => Test_Message
--         Cc       => ,
--         Bcc      => ,
--         Date     =>
      );
   Buffer  : Constant := 1024 * 2;
   Factory : aliased Connections_Factory;
   Server  : aliased Connections_Server (Factory'Access, 0);
   Client  : Connection_Ptr :=
                new SMTP_Client
                    (  Listener     => Server'Unchecked_Access,
                       Reply_Length => Buffer,
                       Input_Size   => 80,
                       Output_Size  => Buffer
                    );

    -- This procedure **DOES NOT** work!
    Procedure Async_Send is
        use type Config.Pascal_String;
    Begin
        Set_Credentials (SMTP_Client (Client.all),
                         User     => +Config.User,
                         Password => +Config.Password
                        );
        Send( SMTP_Client (Client.all), Message );

        Connect(
           Listener =>  Server,
           Client   =>  Client,
           Host     => +Config.Host,
           Port     =>  GNAT.Sockets.SMTP.SMTP_Port
          );
    end Async_Send;


    -- This procedure **WORKS**!!
    Procedure Synch_Send is
        use GNAT.Sockets.SMTP.Client.Synchronous, Config;
    Begin
        Send(
           Server   =>  Server,
           Host     => +Config.Host,      -- Currently: "mail.nso.edu"
           Message  =>  Message,
           User     => +Config.User,      -- User of the mail-system.
           Password => +Config.Password,  -- Their password.
           Timeout  =>  10.0
          );
    end Synch_Send;
Begin
    if EMAIL and Text /= "" then
        Synch_Send;
    end if;
End Send_Report;

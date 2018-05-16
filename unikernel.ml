open Lwt.Infix

module Main (S: Mirage_stack_lwt.V4) = struct

  let report_and_close flow pp e message =
      let ip, port = S.TCPV4.dst flow in
      Logs.warn
        (fun m -> m "closing connection from %a:%d due to error %a while %s"
            Ipaddr.V4.pp_hum ip port pp e message);
      S.TCPV4.close flow

  let rec echo flow how_many start_at=
    let dst, dst_port = S.TCPV4.dst flow in
    Logs.info (fun f -> f "new tcp connection from IP %s on port %d"
              (Ipaddr.V4.to_string dst) dst_port);
    S.TCPV4.read flow >>= function
    | Ok `Eof -> Logs.info (fun f -> f "Closing connection!"); Lwt.return_unit
    | Error e -> Logs.warn (fun f -> f "Error reading data from established connection: %a" S.TCPV4.pp_error e);
                            Lwt.return_unit
    | Ok (`Data b) ->
      Logs.info (fun f -> f "read: %d bytes:\n%s" (Cstruct.len b) (Cstruct.to_string b));

      let solution =
            let equationElements = String.split_on_char ' ' (Cstruct.to_string b) in
              let aStr = List.nth equationElements 0 in
              let eqStr = List.nth equationElements 1 in
              let bStr = List.nth equationElements 2 in
                  let a = Pervasives.int_of_string aStr in
                  let b = Pervasives.int_of_string bStr in
                      let solution = match eqStr with
                        | "+" -> a + b
                        | "-" -> a - b
                        | "*" -> a * b
                        | "/" -> a / b
                       in
                        Pervasives.string_of_int solution
      in

      let make_chars how_many start_at =
        let buf = Io_page.(to_cstruct (get 1)) in
        let output = (String.sub (solution ^ solution) start_at how_many) ^ "\n" in
        Cstruct.blit_from_string output 0 buf 0 (String.length output);
        Cstruct.set_len buf (String.length output)
      in

      S.TCPV4.write flow (make_chars (String.length solution) start_at) >>= function
      | Ok () -> echo flow (String.length solution) ((start_at + 1) mod (String.length solution))
      | Error e -> report_and_close flow S.TCPV4.pp_write_error e "writing in Echo"


  let start s =
    let port = Key_gen.port () in
    S.listen_tcpv4 s ~port (fun flow -> echo flow 4 0);
    S.listen s

end

open Lwt.Infix
open H2
module Client = H2_lwt_unix.Client

let connection_handler =
  let error_handler _ ?request _ _ = ignore request in
  let request_handler _client_address request_descriptor =
    let request = Reqd.request request_descriptor in
    let request_body = Reqd.request_body request_descriptor in
    let response_string = request.target in
    let rec respond () =
      Body.schedule_read
        request_body
        ~on_eof:(fun () ->
          let response =
            Response.create
              ~headers:
                (Headers.of_list [ "content-type", "application/octet-stream" ])
              `OK
          in
          let response_body =
            Reqd.respond_with_streaming request_descriptor response
          in
          let rec f () =
            Printf.eprintf "Sending: %s\n%!" response_string;
            Body.write_string response_body response_string;
            Lwt_unix.sleep 1.0 >>= fun () -> f ()
          in
          Lwt.async (fun () -> f ()))
        ~on_read:(fun _request_data ~off:_ ~len:_ -> respond ())
    in
    respond ()
  in
  H2_lwt_unix.Server.create_connection_handler
    ?config:None
    ~request_handler
    ~error_handler

let error_handler _ = assert false

let make_request connection path () =
  let response_handler notify_response_received _response response_body =
    let rec read_response () =
      Body.schedule_read
        response_body
        ~on_eof:(fun () -> Lwt.wakeup_later notify_response_received ())
        ~on_read:(fun bigstring ~off ~len ->
          let response_fragment = Bytes.create len in
          Bigstringaf.blit_to_bytes
            bigstring
            ~src_off:off
            response_fragment
            ~dst_off:0
            ~len;
          Printf.eprintf
            "        Receiving: %s\n%!"
            (Bytes.to_string response_fragment);
          read_response ())
    in
    read_response ()
  in
  let request =
    Request.create
      `GET
      path
      ~scheme:"http"
      ~headers:Headers.(add_list empty [ ":authority", "localhost" ])
  in
  let response_received, notify_response_received = Lwt.wait () in
  let response_handler = response_handler notify_response_received in
  let request_body =
    Client.request connection request ~error_handler ~response_handler
  in
  Body.close_writer request_body;
  response_received

let () =
  let main =
    let port = 8080 in
    let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
    Lwt_io.establish_server_with_client_socket listen_address connection_handler
    >>= fun _server ->
    Lwt_unix.getaddrinfo
      "localhost"
      (string_of_int port)
      [ Unix.(AI_FAMILY PF_INET) ]
    >>= fun addresses ->
    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr >>= fun () ->
    Client.create_connection ~error_handler socket >>= fun connection ->
    Lwt.choose
      [ make_request connection "FOO" ()
      ; (Lwt_unix.sleep 6.0 >>= fun () -> make_request connection "BAR" ())
      ]
  in
  Lwt_main.run main

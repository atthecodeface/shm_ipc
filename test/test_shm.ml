open Lwt

(*f trace - use with trace __POS__ *)
let trace pos = 
    let (a,b,c,d) = pos in
    Printf.printf "trace:%s:%d:%d:%d\n%!" a b c d

let create_server _ =
  let (shm, data, server) = Shm_ipc.create_server "TestSever" 1 "/tmp/a.blob" 12345 65536L in
  let data_ba = Shm_ipc.Shm.data_ba data in
  Printf.printf "Blah size %d\n" (Bigarray.Array1.dim data_ba);
  server

let create_client _ =
  let (shm, data, client) = Shm_ipc.create_client "/tmp/a.blob" 12345 65536L in
  let data_ba = Shm_ipc.Shm.data_ba data in
  Printf.printf "Blah size %d\n" (Bigarray.Array1.dim data_ba);
  client

let server_thread s msg : unit -> unit Lwt.t =
  let iter = ref 0 in
  let poll_n _ =
    trace __POS__;
    let n = !iter in
    iter := !iter + 1;
    if (n=5) then (ignore (Shm_ipc.Ipc.Server.send s 0 msg));
    let quit  = (
        let (rc,msg,client) = Shm_ipc.Ipc.Server.poll s (1000) in
        match rc with 
        | Shm_ipc.Ipc.Timeout -> false
        | Shm_ipc.Ipc.Message -> (Printf.printf "server message\n"; true )
        | _ -> (Printf.printf "server other\n"; true)
      )
    in
    if quit then None else (Some ())
  in
  let stream = Lwt_stream.from_direct poll_n in
  fun _ -> Lwt_stream.iter_s (fun _ -> Lwt_main.yield ()) stream

let client_thread s : unit -> unit Lwt.t =
  let iter = ref 0 in
  let poll_n _ =
    trace __POS__;
    let n = !iter in
    iter := !iter + 1;
    let quit = (
        let (rc,msg) = Shm_ipc.Ipc.Client.poll s (1000) in
        match rc with 
        | Shm_ipc.Ipc.Timeout -> false
        | Shm_ipc.Ipc.Message -> (Printf.printf "client message\n"; Shm_ipc.Ipc.Client.send s msg ; true )
        | _ -> (Printf.printf "client other\n"; true)
      ) in
    if quit then None else (Some ())
  in 
  let stream = Lwt_stream.from_direct poll_n in
  fun _ -> Lwt_stream.iter_s (fun _ -> Lwt_main.yield ()) stream

let _ =
  let s = create_server () in
  let c = create_client () in
  let msgs = [Shm_ipc.Ipc.Server.msg_alloc s 10;Shm_ipc.Ipc.Server.msg_alloc s 20;Shm_ipc.Ipc.Server.msg_alloc s 30;] in
  List.iteri (fun i m -> Printf.printf "Message %d size %d\n" i (Bigarray.Array1.dim (Shm_ipc.Ipc.msg_ba m))) msgs ;
  trace __POS__;
  let st = server_thread s (List.hd msgs) in
  trace __POS__;
  let ct = client_thread c in
  trace __POS__;
  Lwt_main.run (Lwt.join [st (); ct ()]);
  Shm_ipc.Ipc.Client.stop c;
  let rc = Shm_ipc.Ipc.Server.shutdown s (100*1000) in
  Printf.printf "Server shutdown returned %d\n" rc;
  ()



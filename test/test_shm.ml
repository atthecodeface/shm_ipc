open Lwt

(*f trace - use with trace __POS__ *)
let trace pos = 
    let (a,b,c,d) = pos in
    Printf.printf "trace:%s:%d:%d:%d\n%!" a b c d

let create_server _ =
  let (shm, data, server) = Shm_ipc.create_server "TestSever" 1 "/tmp/a.blob" 12345 65536L in
  let data_ba = Shm_ipc.Shm.data_ba data in
  Printf.printf "Blah size %d address %0Lx\n" (Bigarray.Array1.dim data_ba) (Shm_ipc.Ba.address data_ba);
  server

let create_client _ =
  let (shm, data, client) = Shm_ipc.create_client "/tmp/a.blob" 12345 65536L in
  let data_ba = Shm_ipc.Shm.data_ba data in
  let data_ba2 = Shm_ipc.Ba.retype Bigarray.float32 Bigarray.c_layout data_ba in
  Printf.printf "Blah size %d address %0Lx address %0Lx\n" (Bigarray.Array1.dim data_ba) (Shm_ipc.Ba.address data_ba) (Shm_ipc.Ba.address data_ba2);
  client

let server_thread s msg =
  let iter = ref 0 in
  let timeout_callback _ = 
    let n = !iter in
    iter := !iter + 1;
    if (n=5) then (ignore (Shm_ipc.Ipc.Server.send s 0 msg));
    Some ()
  in
  let msg_callback client msg = 
    Printf.printf "server message from client %d at iteration %d\n" client !iter;
    None
  in
  let stream = Lwt_stream.from_direct (Shm_ipc.server_thread_poll s 1000 timeout_callback msg_callback) in
  fun _ -> Lwt_stream.iter_s (fun _ -> Lwt_main.yield ()) stream

let client_thread s =
  let iter = ref 0 in
  let timeout_callback _ = 
    iter := !iter + 1;
    Some ()
  in
  let msg_callback msg = 
    Printf.printf "client message from server at iteration %d\n" !iter;
    ignore (Shm_ipc.Ipc.Client.send s msg);
    None
  in
  let stream = Lwt_stream.from_direct (Shm_ipc.client_thread_poll s 1000 timeout_callback msg_callback) in
  fun _ -> Lwt_stream.iter_s (fun _ -> Lwt_main.yield ()) stream

let _ =
  let s = create_server () in
  let c = create_client () in
  let msgs = [Shm_ipc.Ipc.Server.msg_alloc s 10;Shm_ipc.Ipc.Server.msg_alloc s 20;Shm_ipc.Ipc.Server.msg_alloc s 30;] in
  List.iteri (fun i m -> let msg_ba = (Shm_ipc.Ipc.msg_ba m) in Printf.printf "Message %d size %d address %0Lx\n" i (Bigarray.Array1.dim msg_ba) (Shm_ipc.Ba.address msg_ba)) msgs ;
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



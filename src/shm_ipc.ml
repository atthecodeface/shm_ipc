(** Copyright (C) 2018,  Gavin J Stark.  All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * @file   shm_ipc.ml
 * @brief  OCAML bindings for C stubs for SHM
 *
 *)


(*a Types *)
(*t t_ba_char *)
type t_ba_char     = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(*a Modules
  Shm - the base shared memory module
  Ipc - the client/server interprocess communication
  Ba  - Bigarray manipulation (reshaping particularly)
  Mbf - Message buffer format (like Google protobuf, but in-memory so no compressing data)
 *)
(*m Shm *)
module Shm = struct
  (*t Exceptions *)
  exception DataErr of string

  (*t Abstract types for C-level things - t_s, t_c, t_msg *)
  type t_shm
  type t_shm_data

  (*f external C stub function declarations *)
  external _shm_init :       unit -> t_shm = "shm_c_init"
  external _shm_data_alloc : t_shm -> string -> int -> int64 -> int -> t_shm_data = "shm_c_data_alloc"
  external _shm_is_null       : t_shm        -> bool = "shm_c_is_null"
  external _shm_data_is_null  : t_shm_data   -> bool = "shm_c_is_null"
  external _shm_data_as_ba    : ('a,'b) Bigarray.kind -> 'c Bigarray.layout -> t_shm_data -> ('a, 'b, 'c) Bigarray.Array1.t = "shm_c_data_as_ba"

  (*f init - initialize the system *)
  let init _ = _shm_init ()

  (*f data_alloc - Allocate/connect to shared memory *)
  let data_alloc shm ?create:(create=0) filename key size = 
    let d = _shm_data_alloc shm filename key size create in
    if _shm_data_is_null d then raise (DataErr "Failed to allocate data");
    d

  (*f data_ba - Get Bigarray of the data *)
  let data_ba shm_data = _shm_data_as_ba Bigarray.char Bigarray.c_layout shm_data

end

(*m Ipc *)
module Ipc = struct
  (*t t_event *)
  type t_event =
    | Shutdown
    | Timeout
    | Message

  (*t Abstract types for C-level things - t_s, t_c, t_msg *)
  type t_s
  type t_c
  type t_msg

  (*f external C stub function declarations *)
  external _shm_msg_ba          : t_msg -> t_ba_char = "shm_c_msg_data_as_ba"

  (*m Server submodule *)
  module Server = struct

    (*t Exceptions *)
    exception ServerErr of string

    (*f external C stub function declarations *)
    external _shm_ipcs_is_null  : t_s  -> bool = "shm_c_is_null"
    external _shm_server_create     : ('a, 'b, 'c) Bigarray.Array1.t -> string -> int -> t_s = "shm_c_server_create"
    external _shm_server_send_msg   : t_s -> int -> t_msg -> int            = "shm_c_server_send_msg"
    external _shm_server_poll       : t_s -> int -> (t_event * t_msg * int) = "shm_c_server_poll"
    external _shm_server_shutdown   : t_s -> int  -> int                    = "shm_c_server_shutdown"
    external _shm_msg_alloc         : t_s -> int -> t_msg = "shm_c_msg_alloc"
    external _shm_msg_free          : t_s -> t_msg -> unit = "shm_c_msg_free"

    (*f create - create a server (must be done prior to clients connecting...) *)
    let create ba name max_clients =
      let s = _shm_server_create ba name max_clients in
      if _shm_ipcs_is_null s then raise (ServerErr "Failed to create server");
      s

    (*f send - sends a message to a client *)
    let send s c msg = _shm_server_send_msg s c msg

    (*f shutdown - shutdown the server with a timeout to wait for clients to acknowledge *)
    let shutdown s timeout = _shm_server_shutdown s timeout

    (*f msg_alloc - allocate a message from the shared memory *)
    let msg_alloc s size = _shm_msg_alloc s size

    (*f msg_free - free a previously allocated message from the shared memory *)
    let msg_free s msg = _shm_msg_free s msg

    (*f poll - poll for an event (shutdown, message from any client, etc) with a timeout *)
    let poll s timeout = _shm_server_poll s timeout

    (*f All done *)
  end

  (*m Client submodule *)
  module Client = struct

    (*t Exceptions *)
    exception ClientErr of string

    (*f external C stub function declarations *)
    external _shm_ipcc_is_null  : t_c  -> bool = "shm_c_is_null"
    external _shm_client_start    : ('a, 'b, 'c) Bigarray.Array1.t -> t_c                  = "shm_c_client_start"
    external _shm_client_poll       : t_c -> int -> (t_event * t_msg) = "shm_c_client_poll"
    external _shm_client_send_msg   : t_c -> t_msg -> int= "shm_c_client_send_msg"
    external _shm_client_stop     : t_c -> unit                  = "shm_c_client_stop"

    (*f start - start a client; requires server to be running *)
    let start ba  =
      let s = _shm_client_start ba in
      if _shm_ipcc_is_null s then raise (ClientErr "Failed to connect client to server");
      s

    (*f send - send a message from the client start a client; requires server to be running *)
    let send t msg = _shm_client_send_msg t msg

    (*f poll - poll for an event (shutdown, message from the server, etc) with a timeout *)
    let poll t timeout = _shm_client_poll t timeout

    (*f stop - stops the the client*)
    let stop t = _shm_client_stop t

    (*f All done *)
  end

  (* msg_ba - get Bigarray for the contents of a message *)
  let msg_ba msg = _shm_msg_ba msg

  (*f All done *)
end

(*m Ba
 *)
module Ba = 
struct
  (*f external C stub function declarations *)
  external _shm_ba_retype  : ('a,'b) Bigarray.kind -> 'c Bigarray.layout -> ('d, 'e, 'f) Bigarray.Array1.t-> ('a, 'b, 'c) Bigarray.Array1.t = "shm_c_ba_retype"
  external _shm_ba_address  : ('a, 'b, 'c) Bigarray.Array1.t -> int64 = "shm_c_ba_address"

  let retype  kind layout ba = _shm_ba_retype kind layout ba
  let address ba             = _shm_ba_address ba

  (*f All done *)
end

(*a Helper functions *)
(*f create_server - initialize Shm allocate data, and create server *)
let create_server name max_clients filename key size=
  let shm  = Shm.init () in
  let data = Shm.data_alloc shm ~create:1 filename key size in
  let data_ba = Shm.data_ba data in
  let server = Ipc.Server.create data_ba name max_clients in
  (shm, data, server)

(*f create_client - initialize Shm allocate data, and create server *)
let create_client filename key size =
  let shm  = Shm.init () in
  let data = Shm.data_alloc shm ~create:0 filename key size in
  let data_ba = Shm.data_ba data in
  let client = Ipc.Client.start data_ba in
  (shm, data, client)

(*f server_thread_poll - polling thread, for use with Lwt_stream
  let server_thread server timeout t_cb m_cb = 
    let stream = Lwt_stream.from_direct (server_thread_poll server timeout t_cb m_cb) in
    fun _ -> Lwt_stream.iter_s (fun _ -> Lwt_main.yield ()) stream
 *)
let server_thread_poll server timeout timeout_callback msg_callback =
  fun _ -> (
    let (rc,msg,client) = Ipc.Server.poll server timeout in
    match rc with 
    | Ipc.Timeout -> timeout_callback ()
    | Ipc.Message -> msg_callback client msg
    | _ -> (Printf.printf "server other\n"; None)
  )

(*f client_thread_poll - polling thread, for use with Lwt_stream
  let client_thread client timeout t_cb m_cb = 
    let stream = Lwt_stream.from_direct (client_thread_poll client timeout t_cb m_cb) in
    fun _ -> Lwt_stream.iter_s (fun _ -> Lwt_main.yield ()) stream
 *)
let client_thread_poll client timeout timeout_callback msg_callback =
  fun _ -> (
    let (rc,msg) = Ipc.Client.poll client timeout in
    match rc with 
    | Ipc.Timeout -> timeout_callback ()
    | Ipc.Message -> msg_callback msg
    | _ -> (Printf.printf "client other\n"; None)
  )


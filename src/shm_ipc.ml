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

(*a Shm module *)
module Shm = struct
  type t_ipc_event =
    | Shutdown
    | Timeout
    | Message
  exception ShmData of string
  exception ShmServer of string
  exception ShmClient of string
  type t_ba_char     = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  type t_shm
  type t_shm_data
  type t_shm_ipc_s
  type t_shm_ipc_c
  type t_shm_ipc_msg

  (*f external C stub function declarations *)
  external _shm_init :       unit -> t_shm = "shm_c_init"
  external _shm_data_alloc : t_shm -> string -> int -> int64 -> int -> t_shm_data = "shm_c_data_alloc"
  external _shm_is_null       : t_shm        -> bool = "shm_c_is_null"
  external _shm_data_is_null  : t_shm_data   -> bool = "shm_c_is_null"
  external _shm_ipcs_is_null  : t_shm_ipc_s  -> bool = "shm_c_is_null"
  external _shm_ipcc_is_null  : t_shm_ipc_c  -> bool = "shm_c_is_null"
  external _shm_data_as_ba    : t_shm_data -> t_ba_char = "shm_c_data_as_ba"
  external _shm_server_create     : ('a, 'b, 'c) Bigarray.Array1.t -> string -> int -> t_shm_ipc_s = "shm_c_server_create"
  external _shm_server_send_msg   : t_shm_ipc_s -> int -> t_shm_ipc_msg -> int = "shm_c_server_send_msg"
  external _shm_server_poll       : t_shm_ipc_s -> int -> (t_ipc_event * t_shm_ipc_msg * int) = "shm_c_server_poll"
  external _shm_server_shutdown   : t_shm_ipc_s -> int  -> int                = "shm_c_server_shutdown"
  external _shm_client_start    : ('a, 'b, 'c) Bigarray.Array1.t -> t_shm_ipc_c                  = "shm_c_client_start"
  external _shm_client_poll       : t_shm_ipc_c -> int -> (t_ipc_event * t_shm_ipc_msg) = "shm_c_client_poll"
  external _shm_client_send_msg   : t_shm_ipc_c -> t_shm_ipc_msg -> int= "shm_c_client_send_msg"
  external _shm_client_stop     : t_shm_ipc_c -> unit                  = "shm_c_client_stop"
  external _shm_msg_alloc       : t_shm_ipc_s -> int -> t_shm_ipc_msg = "shm_c_msg_alloc"
  external _shm_msg_free        : t_shm_ipc_s -> t_shm_ipc_msg -> unit = "shm_c_msg_free"
  external _shm_msg_ba          : t_shm_ipc_msg -> t_ba_char = "shm_c_msg_data_as_ba"

  let init _ = _shm_init ()

  let data_alloc shm ?create:(create=0) filename key size = 
    let d = _shm_data_alloc shm filename key size create in
    if _shm_data_is_null d then raise (ShmData "Failed to allocate data");
    d

  let data_ba shm_data = _shm_data_as_ba shm_data

  let server_create ba name max_clients =
    let s = _shm_server_create ba name max_clients in
    if _shm_ipcs_is_null s then raise (ShmServer "Failed to create server");
    s

  let server_send s c msg = _shm_server_send_msg s c msg

  let server_shutdown s timeout = _shm_server_shutdown s timeout

  let server_msg_alloc s size = _shm_msg_alloc s size

  let server_msg_free s msg = _shm_msg_free s msg

  let server_poll s timeout = _shm_server_poll s timeout

  let msg_ba msg = _shm_msg_ba msg

  let client_start ba  =
    let s = _shm_client_start ba in
    if _shm_ipcc_is_null s then raise (ShmClient "Failed to connect client to server");
    s
    
  let client_send s msg = _shm_client_send_msg s msg

  let client_poll s timeout = _shm_client_poll s timeout

  let client_stop s = _shm_client_stop s
    
end

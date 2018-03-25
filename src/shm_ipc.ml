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
(*t t_ba_* *)
type t_ba_char   = (char,  Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type t_ba_int32  = (int32, Bigarray.int32_elt,         Bigarray.c_layout) Bigarray.Array1.t
type t_ba_int64  = (int64, Bigarray.int64_elt,         Bigarray.c_layout) Bigarray.Array1.t
type t_ba_float  = (float, Bigarray.float32_elt,       Bigarray.c_layout) Bigarray.Array1.t
type t_ba_double = (float, Bigarray.float64_elt,       Bigarray.c_layout) Bigarray.Array1.t

(*a Useful functions *)
let sfmt = Printf.sprintf

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
    external _shm_client_stop       : t_c -> unit                  = "shm_c_client_stop"
    external _shm_msg_alloc         : t_c -> int -> t_msg = "shm_c_msg_alloc"
    external _shm_msg_free          : t_c -> t_msg -> unit = "shm_c_msg_free"

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

    (*f msg_alloc - allocate a message from the shared memory *)
    let msg_alloc t size = _shm_msg_alloc t size

    (*f msg_free - free a previously allocated message from the shared memory *)
    let msg_free t msg = _shm_msg_free t msg

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
  external _shm_ba_retype      : ('a,'b) Bigarray.kind -> 'c Bigarray.layout -> ('d, 'e, 'f) Bigarray.Array1.t-> ('a, 'b, 'c) Bigarray.Array1.t = "shm_c_ba_retype"
  external _shm_ba_retype_sub  : ('a,'b) Bigarray.kind -> 'c Bigarray.layout -> ('d, 'e, 'f) Bigarray.Array1.t -> int -> int -> ('a, 'b, 'c) Bigarray.Array1.t = "shm_c_ba_retype_sub"
  external _shm_ba_address  : ('a, 'b, 'c) Bigarray.Array1.t -> int64 = "shm_c_ba_address"

  (*f retype_sub Bigarray.kind Bigarray.layout Bigarray.Array1.t ofs len - ofs and len are in source ba terms *)
  let retype_sub  kind layout ba ofs len = _shm_ba_retype_sub kind layout ba ofs len

  (*f retype Bigarray.kind Bigarray.layout Bigarray.Array1.t - retype the whole bigarray *)
  let retype      kind layout ba = _shm_ba_retype kind layout ba

  (*f address - get data address of the BA (for debug for now...) *)
  let address     ba             = _shm_ba_address ba

  (*f All done *)
end

(*m Mbf

  Mbfs consist of entries each of which starts with a key/type int32:

    type is bottom 3 bits
    rest are key

    type 0 => value is 4 bytes
    type 1 => value is 8 bytes
    type 2 => value is (4 byte length)::data of length + padding to next 4-byte boundary
    type 3 => value is (4 byte ofs, 4 byte len) to data within the mbf
 *)
module Mbf =
struct
  (*t exceptions *)
  exception BadMsg of string

  (*t type 't' *)
  type t = t_ba_char

  (*f round_up_to *)
  let round_up_to n k =
    (k + (n-1)) land (-n)

  (*m Make module - make an mbf *)
  module Make =
    struct
      (*t t_elt *)
      type t_elt =
        | Callback      of (bool-> int -> unit)
        | Assoc         of ((string -> int) * ((string * t_elt) list))
        | List          of t_elt list
        | String        of string
        | Float         of float
        | Double        of float
        | Int           of int
        | Int32         of int32
        | Int64         of int64
        | ArrayFloat    of float array
        | ArrayDouble   of float array
        | ArrayInt32    of int32 array
        | ArrayInt64    of int64 array
        | Blob          of (bool -> int -> int -> int) (* fill -> ofs -> max_size -> act_size function *)

      (*f int64_of_double *)
      let int64_of_double v = Int64.bits_of_float v

      (*f int64_of_float *)
      let int64_of_float v  = Int64.of_int32 (Int32.bits_of_float v)

      (*f size_inline ofs k - store the key+type as int32 then data keeping alignment of next to 4-bytes *)
      let size_inline ofs k ?size:(size=1) len =
        let ofs = ofs+4 in
        let ofs = (round_up_to size ofs) in
        ofs + (round_up_to 4 (len*size))

      (*f size - work out the size in bytes to store key 'k' -> element 't' at offset 'ofs' *)
      let rec size ofs k t =
        match t with
        | Callback f -> (f false ofs; ofs)
        | List l    -> (
          let (last_ofs,_) = List.fold_left (fun (ofs,i) e -> ((size ofs i e), i+1)) (ofs+8,0) l in
          last_ofs
        )
        | Assoc (map,al)  -> (
          let add_map_element ofs (n,e) =
            let k = map n in
            size ofs k e
          in
          let last_ofs = List.fold_left add_map_element (ofs+8) al in
          last_ofs
        )
        | String s  -> (size_inline ofs k (4+(String.length s)))
        | Float _   -> (size_inline ofs k 4)
        | Double _  -> (size_inline ofs k 8)
        | Int32 _   -> (size_inline ofs k 4)
        | Int64 _   -> (size_inline ofs k 8)
        | Int _     -> (size_inline ofs k 4)
        | ArrayFloat a -> (size_inline ofs k ~size:4 (Array.length a))
        | ArrayInt32 a -> (size_inline ofs k ~size:4 (Array.length a))
        | ArrayDouble a -> (size_inline ofs k ~size:8 (Array.length a))
        | ArrayInt64 a  -> (size_inline ofs k ~size:8 (Array.length a))
        | Blob f        -> (size_inline ofs k (f false (ofs+8) 0))

      (*f write_int_n - write an N byte integer (as an int64) little endian *)
      let write_int_n n (mbf:t_ba_char) max_sz ofs d =
        if (ofs>=0) && ((ofs+n)<=max_sz) then (
          let rec write_byte i d =
            if (i>=n) then () else (
              mbf.{ofs+i} <- Char.chr (0xff land (Int64.to_int d));
              write_byte (i+1) Int64.(shift_right d 8)
            ) in
          (write_byte 0 d);
        );
        ofs+n

      (*f write_int_of_4 - write a 4 byte integer (as an int64) little endian *)
      let write_int_of_4 = write_int_n 4

      (*f write_key_type - write key 'k' of type 't' (0 to 3) and return ofs *)
      let write_key_type mbf max_sz ofs k t =
        let data = Int64.(logor (shift_left (of_int k) 3) (of_int t)) in
        write_int_of_4 mbf max_sz ofs data

      (*f write_key_data - write key 'k' and 4 or 8 bytes of int64 data and return ofs *)
      let write_key_data mbf max_sz ofs k size v =
        let t = if (size=4) then 0 else 1 in
        let data = Int64.(logor (shift_left (of_int k) 3) (of_int t)) in
        let ofs = write_int_of_4 mbf max_sz ofs data in
        let ofs = write_int_of_4 mbf max_sz ofs v in
        let ofs = if size<=4 then ofs else (
          write_int_of_4 mbf max_sz ofs (Int64.shift_right v 32)
        ) in
        ofs

      (*f write_key_data_array - write key 'k' and 4 or 8 bytes of each array element and return ofs *)
      let write_key_data_array mbf max_sz ofs k size f a =
        let data = Int64.(logor (shift_left (of_int k) 3) 2L) in
        let ofs = write_int_of_4 mbf max_sz ofs data in
        let len = Array.length a in
        let padding = if size<=4 then 0 else (ofs+4) land 4 in
        let byte_size = len * size + padding in
        let ofs = write_int_of_4 mbf max_sz ofs (Int64.of_int byte_size) in
        let ofs = ofs + padding in
        let write_element ofs e =
          let e = f e in
          let ofs = write_int_of_4 mbf max_sz ofs e in
          let ofs = if (size<=4) then ofs else (write_int_of_4 mbf max_sz ofs (Int64.shift_right e 32)) in
          ofs
        in
        Array.fold_left write_element ofs a

      (*f write - write key 'k' -> element 't' at offset 'ofs' in mbf with max size max_sz*)
      let rec write mbf max_sz ofs k t =
        match t with
        | Callback f -> (f true ofs; ofs)
        | List l    -> (
          let write_element (ofs,i) e =
            ((write mbf max_sz ofs i e), i+1)
          in
          let (last_ofs,_) = List.fold_left write_element (ofs+8,0) l in
          let len = last_ofs - (ofs+8) in
          let ofs = write_key_type  mbf max_sz ofs k 2 in
          let _ = write_int_of_4  mbf max_sz ofs (Int64.of_int len) in
          last_ofs
        )
        | Assoc (map,al)  -> (
          let write_map_element ofs (n,e) =
            let k = map n in
            write mbf max_sz ofs k e
          in
          let last_ofs = List.fold_left write_map_element (ofs+8) al in
          let len = last_ofs - (ofs+8) in
          let ofs = write_key_type  mbf max_sz ofs k 2 in
          let _ = write_int_of_4  mbf max_sz ofs (Int64.of_int len) in
          last_ofs
        )
        | String s  -> (
          let sl = String.length s in
          let ofs = write_key_type mbf max_sz ofs k 2 in
          let ofs = write_int_of_4  mbf max_sz ofs (Int64.of_int sl) in
          let write_char i c =
            mbf.{ofs+i} <- c
          in
          if (ofs+sl<max_sz) then String.iteri write_char s;
          ofs + (round_up_to 4 sl)
        )
        | Float v   -> (write_key_data mbf max_sz ofs k 4 (int64_of_float v))
        | Double v  -> (write_key_data mbf max_sz ofs k 8 (int64_of_double v))
        | Int32 v   -> (write_key_data mbf max_sz ofs k 4 (Int64.of_int32 v))
        | Int64 v   -> (write_key_data mbf max_sz ofs k 8 v)
        | Int v     -> (write_key_data mbf max_sz ofs k 4 (Int64.of_int v))
        | ArrayFloat a  -> (write_key_data_array mbf max_sz ofs k 4 int64_of_float a)
        | ArrayInt32 a  -> (write_key_data_array mbf max_sz ofs k 4 Int64.of_int32 a)
        | ArrayDouble a -> (write_key_data_array mbf max_sz ofs k 8 int64_of_double a)
        | ArrayInt64 a  -> (write_key_data_array mbf max_sz ofs k 8 (fun x->x) a)
        | Blob f        -> (
          let len = f true (ofs+8) max_sz in
          let ofs = write_key_type  mbf max_sz ofs k 2 in
          let ofs = write_int_of_4  mbf max_sz ofs (Int64.of_int len) in
          ofs + len
        )

      (*f All done *)
    end
  (*t t_data - internal data *)
  type t_data = 
    | Value of int64
    | Arr   of (int * int) (* offset, length *)

  (*m Value module to convert data to useful values *)
  module Value =
  struct
    (*f str *)
    let str v =
      match v with
      | Value x -> sfmt "%016Lx" x
      | Arr (ofs,len) -> sfmt "(%d,%d)" ofs len

    (*f double_of_int64 *)
    let double_of_int64 v = Int64.float_of_bits v

    (*f float_of_int32 *)
    let float_of_int32 v  = Int32.float_of_bits v

    (*t BadValue exception *)
    exception BadValue

    (*f int64 - used externally and internally  *)
    let int64 w =
      match w with
      | Value v -> v
      | _ -> raise BadValue

    (*f int32 - used externally and internally *)
    let int32 w =
      Int64.to_int32 (int64 w)

    (* as_ofs_len - used internally *)
    let as_ofs_len v =
      match v with
      | Arr (ofs,len) -> (ofs, len)
      | _ -> raise BadValue

    (*f string *)
    let string ba (v:t_data) =
      let (ofs,len) = as_ofs_len v in
      String.init len (fun i -> ba.{ofs+i})

    (*f int *)
    let int w =
      Int64.to_int (int64 w)

    (*f bool *)
    let bool w =
      (int64 w) == 1L

    (*f float *)
    let float w =
      float_of_int32 (int32 w)

    (*f double *)
    let double w =
      double_of_int64 (int64 w)

    (*f rep_double *)
    let rep_double t v =
      let (ofs,len) = as_ofs_len v in
      let arr = Ba.retype_sub Bigarray.float64 Bigarray.c_layout t ofs len in
      arr

    (*f rep_float *)
    let rep_float t v =
      let (ofs,len) = as_ofs_len v in
      let arr = Ba.retype_sub Bigarray.float32 Bigarray.c_layout t ofs len in
      arr

    (*f rep_int32 *)
    let rep_int32 t v =
      let (ofs,len) = as_ofs_len v in
      let arr = Ba.retype_sub Bigarray.int32 Bigarray.c_layout t ofs len in
      arr

    (*f rep_int64 *)
    let rep_int64 t v =
      let (ofs,len) = as_ofs_len v in
      let arr = Ba.retype_sub Bigarray.int64 Bigarray.c_layout t ofs len in
      arr

    (*f array *)
    let array t v =
      let (ofs,len) = as_ofs_len v in (t, ofs, len)

    (*f All done *)
  end

  (*t t_gen_fold_fn *)
  type ('a,'b) t_gen_fold_fn = 'a -> 'b -> 'a

  (*t t_fold_fn - functions of this type are supplied to fold_message to invoke callbacks on particular keys with their data

    Need to add in 'remote' big array thing possibly (or just use RepInt32?)
   *)
  type 'a t_fold_fn =
    | None   
    | String of ('a, string)  t_gen_fold_fn
    | Bool   of ('a, bool)    t_gen_fold_fn
    | Int    of ('a, int)     t_gen_fold_fn
    | Float  of ('a, float)   t_gen_fold_fn
    | Double of ('a, float)   t_gen_fold_fn
    | Int32  of ('a, int32)   t_gen_fold_fn
    | Int64  of ('a, int64)   t_gen_fold_fn
    | RepDouble of ('a, t_ba_double)  t_gen_fold_fn
    | RepFloat  of ('a, t_ba_float)  t_gen_fold_fn
    | RepInt32  of ('a, t_ba_int32)  t_gen_fold_fn
    | RepInt64  of ('a, t_ba_int64)  t_gen_fold_fn
    | Blob      of ('a, (t * int * int))  t_gen_fold_fn

  (*f read_int_n - read an N byte integer (as an int64) little endian *)
  let read_int_n t o n =
    let rec acc_int n acc =
      if (n<0) then acc else (
        acc_int (n-1) Int64.(logor (shift_left acc 8) (of_int (Char.code t.{o+n})))
      ) in
    ((acc_int (n-1) 0L), o+n)

  (*f read_int_of_4 - read a 4 byte integer (as an int64) little endian *)
  let read_int_of_4 t o = read_int_n t o 4

  (*f read_int_of_8 - read an 8 byte integer (as an int64) little endian *)
  let read_int_of_8 t o = read_int_n t o 8

  (*f read_type_and_key - read a key from an mbf as field number/type - 32 bits
  The key is a number plus a type
  The types are:
    0 -> inline 4-byte
    1 -> inline 8-byte
    2 -> inline len + data
    3 -> offset + len
    4-7 reserved
   *)
  let read_type_and_key t o =
    let (k, no)      = read_int_of_4 t o in
    let key          = Int64.(to_int (shift_right k 3)) in
    let field_type   = (Int64.to_int k) land 7 in
    (field_type, key, no)

  (*f read_key_value - read a key/value pair and return key, wire value and new offset

    Returns (key * Value (t_data) * new offset )
  *)
  let read_key_value t o =
    let (field_type, key, no) = read_type_and_key t o in
    match field_type with
    | 0 -> let (value, no)  = read_int_of_4 t no in (key, Value value, no)
    | 1 -> let (value, no)  = read_int_of_8 t no in (key, Value value, no)
    | 2 -> let (size, no)   = read_int_of_4 t no in
           let size = Int64.to_int size in
           (key, Arr (no,size), no+((size+3) land 0x7ffffffc))
    | 3 -> let (ofs, no) = read_int_of_4   t no in
           let (size, no) = read_int_of_4   t no in
           let ofs  = Int64.to_int ofs in
           let size = Int64.to_int size in
           (key, Arr (ofs,size), no)
    | _ -> raise (BadMsg (sfmt "unknown field type %d in mbf typed key" field_type))
                 
  (*f exec_fold_fn - execute the correct kind of fold function *)
  let exec_fold_fn t f acc v =
    match f with
    | None -> acc
    | String  f    -> f acc (Value.string t v)
    | Double f     -> f acc (Value.double v)
    | Float  f     -> f acc (Value.float  v)
    | Bool  f      -> f acc (Value.bool   v)
    | Int   f      -> f acc (Value.int    v)
    | Int32  f     -> f acc (Value.int32  v)
    | Int64  f     -> f acc (Value.int64  v)
    | RepDouble f  -> f acc (Value.rep_double t v)
    | RepFloat f   -> f acc (Value.rep_float t v)
    | RepInt32 f   -> f acc (Value.rep_int32 t v)
    | RepInt64 f   -> f acc (Value.rep_int64 t v)
    | Blob f       -> f acc (Value.array t v)

  (*f fold_message - Fold over an mbf message blob, invoking appropriate function for each element *)
  let fold_message t ?verbose:(verbose=false) fn_of_key acc ofs len =
    let end_of_block = ofs+len in
    let rec fold_over_kv acc ofs =
      if verbose then Printf.printf "Scanning offset %d end %d\n" ofs end_of_block;
      if (ofs>=end_of_block) then (
        acc
      ) else (
        let (k,v,new_ofs) = read_key_value t ofs in
        if verbose then Printf.printf "key %d new_ofs %d value %s\n" k new_ofs (Value.str v);
        let new_acc = exec_fold_fn t (fn_of_key k) acc v in
        fold_over_kv new_acc new_ofs
      )
    in
    fold_over_kv acc ofs

  (*f fold_root_message - Fold over a complete mbf message that is key 0 length N, invoking appropriate function for each element *)
  let fold_root_message t ?verbose:(verbose=false) fn_of_key acc ofs len =
    let (k,v,new_ofs) = read_key_value t ofs in
    if (k<>0) || (new_ofs<=len) then (
        let (_,ofs,len) = Value.array t v in
        if verbose then Printf.printf "Root message offset %d len %d\n" ofs len;
        fold_message t ~verbose:verbose fn_of_key acc ofs len
    ) else (
      raise (BadMsg "Root message block must have key 0 and array value whose length does no go beyond message")
    )

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


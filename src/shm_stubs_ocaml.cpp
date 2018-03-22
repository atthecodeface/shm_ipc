/** Copyright (C) 2017,  Gavin J Stark.  All rights reserved.
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
 * @file          ocaml_quaternion.cpp
 * @brief         Quaternions and methods thereon
 *
 */

/*a Documentation
 * 
 * This is a wrapper around the ATCF library quaternion class
 *
 * It provides ocaml wrappers for the quaternion methods in 'quaternion.h'
 *
 */
/*a Includes
 */
#define CAML_NAME_SPACE 
#include <caml/mlvalues.h>
#define CAML_INTERNALS
#include <caml/bigarray.h>
#undef CAML_INTERNALS
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/threads.h>

#include <stdio.h>

extern "C"
{
#include "shm_stubs_lib.h"
#include "shm_stubs_ipc.h"
}

/*a Defines
 */
static struct custom_operations custom_ops = {
    (char *)"shm_stubs.basic",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default
};
static void finalize_shm(value v)
{
    fprintf(stderr,"\n\n\n******************************************************************************** FINALIZE\n\n\n\n");
    (void) v;
}
static struct custom_operations custom_shm_ops = {
    (char *)"shm_stubs.shm",
    finalize_shm,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default
};
typedef struct t_ocaml_shm_ipc {
    value ba;
    struct shm_ipc *shm_ipc;
    int connected;
    int client;
} t_ocaml_shm_ipc;

#define shm_of_val(v) (*((struct shm **) Data_custom_val(v)))
#define shm_data_of_val(v) (*((struct shm_data **) Data_custom_val(v)))
#define shm_ipc_msg_of_val(v) (*((struct shm_ipc_msg **) Data_custom_val(v)))
#define ocaml_shm_ipc_of_val(v) (*((struct t_ocaml_shm_ipc **) Data_custom_val(v)))
#define val_alloc_of_shm_ipc_msg(msg) ({value r = caml_alloc_custom(&custom_ops, sizeof(struct shm_ipc_msg *), 0, 1); \
            shm_ipc_msg_of_val(r) = msg; r;} )

// Use -D__OCAML_QUATERNION_VERBOSE on compilation to be verbose,
// or uncomment the following
//#define __OCAML_QUATERNION_VERBOSE
#ifdef __OCAML_QUATERNION_VERBOSE
#define VERBOSE fprintf
#else
#define VERBOSE __discard
static void __discard(const void *, ...) {}
#endif

/*a Functions
int 
shm_ipc_client_poll(struct shm_ipc *shm_ipc, int client, int timeout, struct shm_ipc_event *event)

int
shm_ipc_server_poll(struct shm_ipc *shm_ipc, int timeout, struct shm_ipc_event *event)

get client number

list active clients of server

get state of server
 */
/*f shm_c_server_create : ba -> string (name) -> int (max clients) -> shm_ipc
 *
 * Creates an SHM server that can be connected to, using the bigarray provided
 *
 */
extern "C"
CAMLprim value
shm_c_server_create(value ba, value nm, value mc)
{
    CAMLparam3(ba, nm, mc);
    struct caml_ba_array *caml_ba = Caml_ba_array_val(ba);
    uintnat byte_size = caml_ba_byte_size(caml_ba);
    t_ocaml_shm_ipc *ocaml_shm_ipc = (t_ocaml_shm_ipc *)malloc(sizeof(t_ocaml_shm_ipc));
    if ((ocaml_shm_ipc) && (byte_size > shm_ipc_size())) {
        struct shm_ipc *shm_ipc = (struct shm_ipc *)(caml_ba->data);
        struct shm_ipc_server_desc desc;
        desc.name        = String_val(nm);
        desc.max_clients = Long_val(mc);
        shm_ipc_server_init(shm_ipc, &desc);
        caml_register_global_root(&ba);
        ocaml_shm_ipc->ba = ba;
        ocaml_shm_ipc->shm_ipc = shm_ipc;
        ocaml_shm_ipc->connected = 1;
        ocaml_shm_ipc->client = -1;
        value r = caml_alloc_custom(&custom_ops, sizeof(t_ocaml_shm_ipc *), 0, 1);
        ocaml_shm_ipc_of_val(r) = ocaml_shm_ipc;
        CAMLreturn(r);
    } else {
        if (ocaml_shm_ipc) free(ocaml_shm_ipc);
        value r = caml_alloc_custom(&custom_ops, sizeof(t_ocaml_shm_ipc *), 0, 1);
        ocaml_shm_ipc_of_val(r) = NULL;
        CAMLreturn(r);
    }
}

/*f shm_c_server_send_msg : shm_ipc -> int client -> msg -> int
 *
 * sends a message to a client
 *
 */
extern "C"
CAMLprim value
shm_c_server_send_msg(value ipcc, value client, value msg)
{
    CAMLparam3(ipcc, client, msg);
    t_ocaml_shm_ipc *ocaml_shm_ipc = ocaml_shm_ipc_of_val(ipcc);
    struct shm_ipc_msg *shm_msg = shm_ipc_msg_of_val(msg);
    int rc=-1;
    if ((ocaml_shm_ipc->client<0) && (ocaml_shm_ipc->connected)) {
        rc = shm_ipc_server_send_msg(ocaml_shm_ipc->shm_ipc, Long_val(client), shm_msg);
    }
    CAMLreturn(Val_long(rc));
}

/*f shm_c_server_poll     : shm_ipc -> int -> event
 *
 * polls for an event
 *
 */
extern "C"
CAMLprim value
shm_c_server_poll(value ipcc, value timeout)
{
    CAMLparam2(ipcc, timeout);
    t_ocaml_shm_ipc *ocaml_shm_ipc = ocaml_shm_ipc_of_val(ipcc);
    struct shm_ipc_event event;
    int rc;
    rc = SHM_IPC_EVENT_SHUTDOWN;
    value r = caml_alloc_tuple(3);
    if ((ocaml_shm_ipc->client<0) && (ocaml_shm_ipc->connected)) {
        rc = shm_ipc_server_poll(ocaml_shm_ipc->shm_ipc, Long_val(timeout), &event);
    }
    Field(r,0) = Val_long(rc);
    Field(r,1) = Val_long(0);
    Field(r,2) = Val_long(0);
    if (rc==SHM_IPC_EVENT_MESSAGE) {
        Field(r,1) = val_alloc_of_shm_ipc_msg(event.msg);
        Field(r,2) = Val_long(event.client);
    }
    CAMLreturn(r);
}

/*f shm_c_server_shutdown : shm_ipc -> unit
 *
 * Shuts down the server; returns 0 on success, otherwise a failure indication
 *
 */
extern "C"
CAMLprim value
shm_c_server_shutdown(value ipcs, value timeout)
{
    CAMLparam2(ipcs, timeout);
    int rc=-1;
    t_ocaml_shm_ipc *ocaml_shm_ipc = ocaml_shm_ipc_of_val(ipcs);
    if ((ocaml_shm_ipc->client<0) && (ocaml_shm_ipc->connected)) {
        ocaml_shm_ipc->connected = 0;
        caml_remove_global_root(&ocaml_shm_ipc->ba);
        rc = shm_ipc_server_shutdown(ocaml_shm_ipc->shm_ipc, timeout);
    }
    CAMLreturn(Val_long(rc));
}

/*f shm_c_client_start : ba -> shm_ipc
 *
 * Connects to SHM client
 *
 */
extern "C"
CAMLprim value
shm_c_client_start(value ba)
{
    CAMLparam1(ba);
    struct caml_ba_array *caml_ba = Caml_ba_array_val(ba);
    uintnat byte_size = caml_ba_byte_size(caml_ba);
    t_ocaml_shm_ipc *ocaml_shm_ipc = (t_ocaml_shm_ipc *)malloc(sizeof(t_ocaml_shm_ipc));
    if ((ocaml_shm_ipc) && (byte_size > shm_ipc_size())) {
        struct shm_ipc *shm_ipc = (struct shm_ipc *)(caml_ba->data);
        struct shm_ipc_client_desc desc;
        int client_number = shm_ipc_client_start(shm_ipc, &desc);
        if (client_number>=0) {
            caml_register_global_root(&ba);
            ocaml_shm_ipc->ba = ba;
            ocaml_shm_ipc->shm_ipc = shm_ipc;
            ocaml_shm_ipc->connected = 1;
            ocaml_shm_ipc->client = client_number;
        } else {
            free(ocaml_shm_ipc);
            ocaml_shm_ipc = NULL;
        }
        value r = caml_alloc_custom(&custom_ops, sizeof(t_ocaml_shm_ipc *), 0, 1);
        ocaml_shm_ipc_of_val(r) = ocaml_shm_ipc;
        CAMLreturn(r);
    } else {
        if (ocaml_shm_ipc) free(ocaml_shm_ipc);
        value r = caml_alloc_custom(&custom_ops, sizeof(t_ocaml_shm_ipc *), 0, 1);
        ocaml_shm_ipc_of_val(r) = NULL;
        CAMLreturn(r);
    }
}

/*f shm_c_client_send_msg : shm_ipc -> msg -> int
 *
 * sends a message
 *
 */
extern "C"
CAMLprim value
shm_c_client_send_msg(value ipcc, value msg)
{
    CAMLparam2(ipcc, msg);
    t_ocaml_shm_ipc *ocaml_shm_ipc = ocaml_shm_ipc_of_val(ipcc);
    struct shm_ipc_msg *shm_msg = shm_ipc_msg_of_val(msg);
    int rc=-1;
    if ((ocaml_shm_ipc->client>=0) && (ocaml_shm_ipc->connected)) {
        rc = shm_ipc_client_send_msg(ocaml_shm_ipc->shm_ipc, ocaml_shm_ipc->client, shm_msg);
    }
    CAMLreturn(Val_long(rc));
}

/*f shm_c_client_poll     : shm_ipc -> int -> event
 *
 * polls for an event
 *
 */
extern "C"
CAMLprim value
shm_c_client_poll(value ipcc, value timeout)
{
    CAMLparam2(ipcc, timeout);
    t_ocaml_shm_ipc *ocaml_shm_ipc = ocaml_shm_ipc_of_val(ipcc);
    struct shm_ipc_event event;
    int rc;
    rc = SHM_IPC_EVENT_SHUTDOWN;
    value r = caml_alloc_tuple(2);
    if ((ocaml_shm_ipc->client>=0) && (ocaml_shm_ipc->connected)) {
        rc = shm_ipc_client_poll(ocaml_shm_ipc->shm_ipc, ocaml_shm_ipc->client, Long_val(timeout), &event);
    }
    Field(r,0) = Val_long(rc);
    Field(r,1) = Val_long(0);
    if (rc==SHM_IPC_EVENT_MESSAGE) {
        Field(r,1) = val_alloc_of_shm_ipc_msg(event.msg);
    }
    CAMLreturn(r);
}

/*f shm_c_client_stop : shm_ipc -> unit
 *
 * Disconnects client
 *
 */
extern "C"
CAMLprim void
shm_c_client_stop(value ipcc)
{
    CAMLparam1(ipcc);
    t_ocaml_shm_ipc *ocaml_shm_ipc = ocaml_shm_ipc_of_val(ipcc);
    if ((ocaml_shm_ipc->client>=0) && (ocaml_shm_ipc->connected)) {
        ocaml_shm_ipc->connected = 0;
        caml_remove_global_root(&ocaml_shm_ipc->ba);
        shm_ipc_client_stop(ocaml_shm_ipc->shm_ipc, ocaml_shm_ipc->client);
    }
    CAMLreturn0;
}

/*f shm_c_msg_alloc 
 */
extern "C"
CAMLprim value
shm_c_msg_alloc(value ipc, value size)
{
    CAMLparam2(ipc, size);
    t_ocaml_shm_ipc *ocaml_shm_ipc = ocaml_shm_ipc_of_val(ipc);
    struct shm_ipc_msg *shm_ipc_msg = NULL;
    if (ocaml_shm_ipc->connected) {
        shm_ipc_msg = shm_ipc_msg_alloc(ocaml_shm_ipc->shm_ipc, Long_val(size));
    }
    CAMLreturn(val_alloc_of_shm_ipc_msg(shm_ipc_msg));
}

/*f shm_c_msg_data_as_ba
 */
extern "C"
CAMLprim value
shm_c_msg_data_as_ba(value msg)
{
    CAMLparam1(msg);
    struct shm_ipc_msg *shm_msg = shm_ipc_msg_of_val(msg);
    char *base      = shm_msg->data;
    int byte_size = shm_msg->hdr.byte_size - sizeof(struct _shm_ipc_msg_hdr);
    CAMLreturn(caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, (void *)base, byte_size ));
}

/*f shm_c_msg_free
 */
extern "C"
CAMLprim void
shm_c_msg_free(value ipc, value msg)
{
    CAMLparam2(ipc, msg);
    t_ocaml_shm_ipc *ocaml_shm_ipc = ocaml_shm_ipc_of_val(ipc);
    shm_ipc_msg_free(ocaml_shm_ipc->shm_ipc, shm_ipc_msg_of_val(msg));
    CAMLreturn0;
}

/*f shm_c_init : unit -> int
 *
 * Creates a SHM for use with everything else
 *
 */
extern "C"
CAMLprim value
shm_c_init(void)
{
    CAMLparam0();
    VERBOSE(stderr,"SHM init\n");
    struct shm *shm = shm_shm_init(1);
    value r = caml_alloc_custom(&custom_shm_ops, sizeof(struct shm *), 0, 1);
    shm_of_val(r) = shm;
    CAMLreturn(r);
}

/*f shm_c_data_alloc : shm -> string -> int -> int64 -> int -> int
 *
 * Allocated shared memory
 *
 */
extern "C"
CAMLprim value
shm_c_data_alloc(value s, value filename, value shm_key, value byte_size, value create)
{
    CAMLparam5(s, filename, shm_key, byte_size, create);
    struct shm *shm = shm_of_val(s);
    const char *shm_filename = String_val(filename);
    size_t byte_size_64 = Int64_val(byte_size);
    struct shm_data *shm_data = shm_data_alloc(shm, shm_filename, Long_val(shm_key), byte_size_64, Long_val(create));
    value r = caml_alloc_custom(&custom_ops, sizeof(struct shm_data *), 0, 1);
    shm_data_of_val(r) = shm_data;
    CAMLreturn(r);
}

/*f shm_c_is_null : value -> bool
 *
 * Return true if value is NULL
 *
 */
extern "C"
CAMLprim value
shm_c_is_null(value s)
{
    CAMLparam1(s);
    CAMLreturn(Val_bool(*((void **)Data_custom_val(s))==NULL));
}

/*f shm_c_data_as_ba : kind -> layout -> data -> ('a, 'b, 'c) Bigarray.Array1.t
 *
*/
extern "C"
CAMLprim value
shm_c_data_as_ba(value kind, value layout, value r)
{
    CAMLparam3(kind, layout, r);
    struct shm_data *shm_data = shm_data_of_val(r);
    int ba_kind   = Caml_ba_kind_val(kind);
    int ba_layout = Caml_ba_layout_val(layout);
    size_t byte_size = shm_data_byte_size(shm_data) / caml_ba_element_size[ba_kind];
    void *base       = shm_data_base(shm_data);
    CAMLreturn(caml_ba_alloc_dims(ba_kind | ba_layout, 1, base, byte_size ));
}

/*f shm_c_ba_retype : kind -> layout -> ('d, 'e, 'f) Bigarray.Array1.t ->  ('a, 'b, 'c) Bigarray.Array1.t
 *
 * must only retype an _externally_ allocated bigarray or keep it in scope
 *
 */
extern "C"
CAMLprim value
shm_c_ba_retype(value kind, value layout, value ba)
{
    CAMLparam3(kind, layout, ba);
    struct caml_ba_array *data = Caml_ba_array_val(ba);
    int ba_kind   = Caml_ba_kind_val(kind);
    int ba_layout = Caml_ba_layout_val(layout);
    size_t byte_size = caml_ba_byte_size(data);
    size_t num_elements = byte_size / caml_ba_element_size[ba_kind];
    void *base = data->data;
    CAMLreturn(caml_ba_alloc_dims(ba_kind | ba_layout, 1, base, num_elements ));
}

/*f shm_c_ba_retype_sub : kind -> ('d, 'e, 'f) Bigarray.Array1.t -> int ofs -> int len -> ('a, 'b, 'c) Bigarray.Array1.t
 *
 * must only retype an _externally_ allocated bigarray or keep it in scope
 *
 */
extern "C"
CAMLprim value
shm_c_ba_retype_sub(value dest_kind, value dest_layout, value ba, value ofs, value len)
{
    CAMLparam5(dest_kind, dest_layout, ba, ofs, len);
    struct caml_ba_array *src = Caml_ba_array_val(ba);
    size_t src_ofs      = ofs * caml_ba_element_size[(src->flags) & CAML_BA_KIND_MASK];
    size_t byte_size    = len * caml_ba_element_size[(src->flags) & CAML_BA_KIND_MASK];
    size_t last_byte    = src_ofs + byte_size;
    if (last_byte > caml_ba_byte_size(src)) {
        last_byte = caml_ba_byte_size(src);
    }
    size_t num_elements = (last_byte - src_ofs) / caml_ba_element_size[dest_kind];
    void *base = (void *)((char *)src->data + src_ofs);
    int ba_kind   = Caml_ba_kind_val(dest_kind);
    int ba_layout = Caml_ba_layout_val(dest_layout);
    CAMLreturn(caml_ba_alloc_dims(ba_kind | ba_layout, 1, base, num_elements ));
}

/*f shm_c_ba_address : ('a, 'b, 'c) Bigarray.Array1.t -> 
 *
 */
extern "C"
CAMLprim value
shm_c_ba_address(value ba)
{
    CAMLparam1(ba);
    struct caml_ba_array *data = Caml_ba_array_val(ba);
    void *base = data->data;
    CAMLreturn(caml_copy_int64((int64_t) (base)));
}

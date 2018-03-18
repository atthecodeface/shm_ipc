/** Copyright (C) 2015-2016,  Gavin J Stark.  All rights reserved.
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
 * @file          shm_ipc.h
 * @brief         SHM IPC library
 *
 * The SHM IPC library provides a single server, multiple client
 * interprocess communication system using shared memory. It does not
 * provide allocation of the shared memory - shm_support provides
 * wrappers an abstraction for that, if required. It does provide
 * system-optimized mechanisms to reduce CPU cycles and cache traffic
 * to support IPC.
 *
 */

/*a Includes
 */
#include <stdint.h> 

/*a Defines
 */
#define SHM_IPC_MAX_CLIENTS 64
#define MSGS_PER_QUEUE 8

/*a Enumerations
 */
/** SHM_IPC_STATE, state used for client and server
 */
enum {
    /** For clients, indicates client is inactive **/
    SHM_IPC_STATE_INIT,
    /** For clients & servers, indicates alive **/
    SHM_IPC_STATE_ALIVE,
    /** For clients & servers, indicates shutting down **/
    SHM_IPC_STATE_SHUTTING_DOWN,
    /** For the server, once all clients are shut down, needs
     * reinitialization */
    SHM_IPC_STATE_DEAD
};

/** SHM_IPC_EVENT, return type for server and client poll
 */
enum {
    /** Indicates client/server has shutdown **/
    SHM_IPC_EVENT_SHUTDOWN=0,
    /** No event ready within timeout **/
    SHM_IPC_EVENT_TIMEOUT,
    /** Message event; event field will be filled out and valid */
    SHM_IPC_EVENT_MESSAGE,
};

/*a Structures
 */
/*f struct _shm_ipc_msg_data_hdr */ /**
 *
 *  @brief Internal structure for a message queue
 *
 * Internal structure for the header of the message heap
 *
 */
struct _shm_ipc_msg_data_hdr {
    /** Atomically accessed lock to permit any client or the server to
     * allocate or free messages **/
    int  locked;
    /** Free list for the message heap, which must only be accessed when the lock is held. **/
    int  free_list;
};

/*f struct _shm_ipc_msg_hdr */ /**
 *
 * @brief Internal structure for the header of a message heap entry
 *
 * The message heap is implemented as a chain of message heap blocks, each with this header.
 *
 */
struct _shm_ipc_msg_hdr {
    /** Next message heap block in the heap - whether it contains a
     * message or is free; zero for the last block in the heap. **/
    int  next_in_list;
    /** Previous message heap block in the heap - whether it contains
     * a message or is free; zero for the first block in the heap. **/
    int  prev_in_list;
    /** Next free message heap block in the heap; forms the next
     * pointer in the free list chain **/
    int  next_free;
    /** Size in bytes of the message heap block, including this
     * header **/
    int  byte_size;
};

/*f struct _shm_ipc_msg_data */ /**
 *
 *  @brief Internal structure for message heap
 *
 * Structure for the whole of the message heap.
 */
struct _shm_ipc_msg_data {
    /** First message heap block header, chaining to the next
     * internally with its @next_in_list element **/
    struct _shm_ipc_msg_data_hdr hdr;
    /** Data for the contents of the heap
     **/
    char   data[8192-sizeof(struct _shm_ipc_msg_data_hdr)];
};

/*f struct shm_ipc_msg */ /**
 *
 * @brief Message as seen by clients and servers
 *
 * Externally visible structure as seen by clients and servers; the @p
 * hdr must not be touched by these, but the @p data field should be
 * used to contain the message data for the message.
 */
struct shm_ipc_msg {
    /** Message header that the server and client must not
     * touch. Included to make the software simpler, hence faster. **/
    struct _shm_ipc_msg_hdr hdr;
    /** Data for the payload; an actual message instance has a
     * server/client-defined payload size, from the @p
     * shm_ipc_msg_alloc call. **/
    char data[4];
};

/*f struct shm_ipc_client_desc */ /**
 *
 * @brief Client descriptor, to enable a client to register with a server
 *
 * This structure is used in @shm_ipc_client_start calls, to register
 * a new client with the server.
 *
 */
struct shm_ipc_client_desc {
    /** Version number; really this is over-the-top, and is currently
     * not used **/
    int version;
    /** Name of the client, for debugging purposes. Not used
     * currently. **/
    const char *name;
};

/*f struct shm_ipc_server_desc */ /**
 *
 * @brief Server descriptor, for initializing the server
 *
 * This structure is used in @shm_ipc_server_init calls, to register
 * the server.
 *
 */
struct shm_ipc_server_desc {
    /** Version number; really this is over-the-top, and is currently
     * not used **/
    int version;
    /** Maximum number of clients that are permited to connect to the
     * server. Cannot exceed 64, or @p SHM_IPC_MAX_CLIENTS **/
    int max_clients;
    /** Name of the server, for debugging purposes. Not used
     * currently. **/
    const char *name;
};

/*f struct shm_ipc_event */ /**
 *
 *  @brief Structure of an event returned by the @p shm_ipc_..._poll
 *  functions
 *
 * A structure of this type should be allocated (statically, on the
 * stack, etc) for passing to the client or server poll functions; it
 * is filled out if an event occurs.
 *
 */
struct shm_ipc_event {
    /** Filled out by poll routine with the SHM IPC structure **/
    struct shm_ipc *shm_ipc;
    /** Type of event that occured (e.g. @p SHM_IPC_EVENT_MESSGE **/
    int event_type;
    /** Client that the event is from (for server polling) **/
    int client;
    /** Message that was passed to the client/server **/
    struct shm_ipc_msg *msg;
};

/*a External functions
 */
/*f shm_ipc_size */ /**
 *
 * @brief Provide the size of the basic server/client shared memory structure
 *
 * @returns The size of the structure required for the client/server
 * system, so that it may be allocated in shared memory
 *
 * Find the size of shared memory required for the server/client
 * system; this memory has to be accessible by the server and the
 * clients, and it should be cache line aligned. Ideally this is
 * allocated with malloc, for a pthreads approach, or with shm shared
 * memory for separate processes. For the latter, the @p shm_support
 * functions may be useful. Note that malloc will not necessarily
 * provide cache-line aligned memory regions.
 *
 */
int shm_ipc_size(void);

/*f shm_ipc_server_init */ /**
 *
 * @brief Initialize an SHM IPC server
 *
 * @param shm_ipc Storage for shm_ipc structure in memory visible to
 * clients and server, of at least @p shm_ipc_size() bytes
 *
 * @param desc Structure filled out with maximum number of clients, server name, etc
 *
 * Initializes a server with support for up to the supplied maximum number of clients
 *
 * Clients cannot connect to a server until it has been initialized
 */
void shm_ipc_server_init(struct shm_ipc *shm_ipc, const struct shm_ipc_server_desc *desc);

/*f shm_ipc_server_shutdown */ /**
 *
 * @brief Shut down the server and inform clients
 *
 * @returns Something indicating how it shut down
 *
 * @param shm_ipc Previously initialized server structure
 *
 * @param timeout Time to wait for clients to shutdown; use 0 to
 * return immediately, <0 for indefinitely
 *
 * Wait for clients to stop before quitting the server. This will timeout after the
 * given timeout, if that is non-zero.
 *
 */
int shm_ipc_server_shutdown(struct shm_ipc *shm_ipc, int timeout);

/*f shm_ipc_server_poll */ /**
 *
 * @brief Server call to poll for messages, or other events
 *
 * @returns SHM_IPC_EVENT_* enumeration indicating that an event is
 * valid (and @p event is filled out), or that the timeout occurred
 *
 * @param shm_ipc Previously initialized server structure
 *
 * @param timeout Time to wait for events in microseconds; use 0 to
 * return immediately, <0 for indefinitely
 *
 * @param event Structure to be filled out if a valid event has
 * occurred
 *
 * Poll for events from clients on behalf of for the specified amount
 * of time. If a valid event occurs then @p event will be filled out;
 * otherwise (e.g. on timeouts) an appropriate return value is
 * used and @p event is not touched.
 *
 */
int shm_ipc_server_poll(struct shm_ipc *shm_ipc, int timeout, struct shm_ipc_event *event);

/*f shm_ipc_server_send_msg */ /**
 *
 * @brief Send a message from the server to a client
 *
 * @returns Zero on success, non-zero on failure
 *
 * @param shm_ipc Previously initialized server structure
 *
 * @param client Client number to send the message to
 *
 * @param msg Message to send
 *
 * Send a message from the server to a client. The message has to have been allocated with
 * @p shm_ipc_alloc_msg or received from @p shm_ipc_client_poll
 *
 */
int shm_ipc_server_send_msg(struct shm_ipc *shm_ipc, int client, struct shm_ipc_msg *msg);

/*f shm_ipc_msg_alloc */ /**
 *
 * @brief Allocate a messsage from the server message heap
 *
 * @param shm_ipc Previously initialized server structure
 *
 * @param size Size in bytes of the message payload
 *
 * Allocates a message from the server message heap.
 *
 * The allocate/free process is not cheap, and it is preferable to
 * allocate messages at start of day (for the client, usually) and
 * reuse the messages, rather than allocate and free dynamically.
 * 
 */
struct shm_ipc_msg *shm_ipc_msg_alloc(struct shm_ipc *shm_ipc, int size);

/*f shm_ipc_msg_free */ /**
 *
 * @brief Free a messsage back to the server message heap
 *
 * @param shm_ipc Previously initialized server structure
 *
 * @param shm_ipc_msg Message returned by either an @p
 * shm_ipc_msg_alloc or a poll function call
 *
 * Frees the message back to the server message heap, for later
 * allocation.
 *
 * The allocate/free process is not cheap, and it is preferable to
 * allocate messages at start of day (for the client, usually) and
 * reuse the messages, rather than allocate and free dynamically.
 * 
 */
void shm_ipc_msg_free(struct shm_ipc *shm_ipc, struct shm_ipc_msg *shm_ipc_msg);

/*f shm_ipc_client_start */ /**
 *
 * @brief Start the client
 *
 * @returns Client number allocated, or less than zero on failure
 *
 * @param shm_ipc Previously initialized server structure
 *
 * @param desc Filled-out client descriptor
 *
 * Finds a legal, inactive client number within the server, and
 * allocates it and returns that client number.
 *
 * Internally, if the call added client 'n', then the server @p
 * active_client_mask will now have bit 'n' set, and @p total_clients
 * will be incremented
 *
 * The approach is to select a client that is not active, and try to
 * claim that. If that fails, try again.
 *
 * If all clients are busy, or the server is shutting down, then the call will fail.
 *
 */
int shm_ipc_client_start(struct shm_ipc *shm_ipc, const struct shm_ipc_client_desc *desc);

/*f shm_ipc_client_stop */ /**
 *
 * @brief Stop the client
 *
 * @param shm_ipc Previously initialized server structure
 *
 * @param client  Client number to send the message to
 *
 * Stop a client that has previously been started
 *
 * Sets the state to be shutting down, and alerts server.
 *
 */
void shm_ipc_client_stop(struct shm_ipc *shm_ipc, int client);

/*f shm_ipc_client_send_msg */ /**
 *
 * @brief Send a message from a client to the server
 *
 * @returns Zero on success, non-zero on failure
 *
 * @param shm_ipc Previously initialized server structure
 *
 * @param client Client number sending the message
 *
 * @param msg Message to send
 *
 * Send a message from the client to the server. The message has to have been allocated with
 * shm_ipc_alloc_msg or received from shm_ipc_client_poll
 *
 */
int shm_ipc_client_send_msg(struct shm_ipc *shm_ipc, int client, struct shm_ipc_msg *msg);

/*f shm_ipc_client_poll */ /**
 *
 * @brief Client call to poll for messages, server shutdown, or other
 * events
 *
 * @returns SHM_IPC_EVENT_* enumeration indicating that an event is
 * valid (and @p event is filled out), or that the timeout occurred,
 * or possibly that the server has shut down and so the client should
 * also do so
 *
 * @param shm_ipc Previously initialized server structure
 *
 * @param client Client number to send the message to
 *
 * @param timeout Time to wait for events in microseconds; use 0 to
 * return immediately, <0 for indefinitely
 *
 * @param event Structure to be filled out if a valid event has
 * occurred
 *
 * Poll for events on behalf of the @p client for the specified amount
 * of time. If a valid event occurs then @p event will be filled out;
 * otherwise (e.g. on timeouts, or if the server has shut down, or if
 * the client is not correctly active) an appropriate return value is
 * used and @p event is not touched.
 *
 */
int shm_ipc_client_poll(struct shm_ipc *shm_ipc, int client, int timeout, struct shm_ipc_event *event);

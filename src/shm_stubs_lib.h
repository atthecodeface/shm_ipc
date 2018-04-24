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
 * @file          shm_support.h
 * @brief         SHM support library
 *
 * This library abstracts some of the Netronome SHM library calls to
 * make host applications a little simpler.
 *
 * For example, the SHM device opening and closing is wrapped in the
 * library, adding atexit handlers and so on, so that firmware and
 * SHMs are handled cleanly.
 *
 */

/*a Includes
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h> 
#include <stddef.h> 
#include <sys/types.h> 
#include <sys/stat.h> 
#include <fcntl.h>
#include <unistd.h>
#ifdef USE_HUGETLBFS
#include <hugetlbfs.h>
#endif

/*a Types
 */
/*t struct shm_data */
typedef struct shm_data *shm_data_ptr;

/*a Functions
 */
/*f shm_shm_init */
/**
 * @brief Initialize an SHM structure for use, attaching to an SHM if
 * required
 *
 * @param sig_term     Set to 1 if a signal handler for SIGTERM
 * should be installed
 *
 * @returns NULL on error, otherwise an allocated SHM structure
 *
 * Initialize SHM structure, attaching to specified device number
 *
 * Adds atexit handler to shut down SHM cleanly at exit if required.
 *
 */
extern struct shm *shm_shm_init(int sig_term);

/*f shm_shm_shutdown */
/**
 * @brief Shutdown the SHM chip structure, making it available for others
 *
 * @param shm    SHM structure of device to shut down
 *
 * Shutdown the SHM, unloading firmware before closing the device
 * Performs an incremental shutdown of the activated components, and can
 * be performed many times successively without failure
 * Removes the SHM from the list to be shutdown at exit
 *
 */
extern void shm_shm_shutdown(struct shm *shm);

/*f shm_data_alloc */
/**
 *
 * @brief Allocate some shared memory (one area per SHM)
 *
 * @param shm           SHM structure of SHM device to allocate SHM for
 *
 * @param shm_filename  Filename for a shared memory 'lock' file
 *
 * @param shm_key       32-bit key used with filename for SHM 'key'
 *
 * @param size          Size in bytes of memory to create (if create is non-zero)
 *
 * @param create        Non-zero if SHM should be created if it does not exist
 *
 * @returns Number of bytes allocated in shared memory
 *
 * This function allocates shared memory of @p size bytes, using
 * the @p shm_filename and @p shm_key to define a system-wide shared memory
 * handle so that multiple processes may share the same memory.
 *
 * Only a single shared memory is permitted per SHM structure.
 *
 * Generally the server will invoke this call with @p create set to a
 * vaue of 1. Clients then start up, connect to the server, and call
 * this function with create set to 0 (but the rest of the arguments
 * the same, except size is ignored)
 *
 */
extern shm_data_ptr shm_data_alloc(struct shm *shm, const char *shm_filename,
                         int shm_key, size_t size, int create);

/*f shm_data_alloc_huge
 */
/**
 *
 * @brief Allocate huge pages as the shared memory (? one per SHM?)
 *
 * @param shm           SHM structure of SHM device to allocate SHM for
 *
 * @param size          Size in bytes of memory to create (if create is non-zero)
 *
 * @returns Number of bytes allocated in shared memory
 *
 * This function allocates huge pages of @p size bytes if the OS supports it
 *
 */
extern shm_data_ptr
shm_data_alloc_huge(struct shm *shm, size_t byte_size);

/*f shm_data_ptr */
/**
 * @brief Get pointer to SHM data allocated with @p shm_alloc
 *
 * @param shm_data       SHM data structure
 *
 * @returns Pointer to (process virtual) address of shared memory
 * allocated by @p shm_alloc
 *
 * Get SHM data pointer after it has been allocated
 *
 */
extern void *shm_data_base(shm_data_ptr shm_data);

/*f shm_data_byte_size */
/**
 * @brief Get data size of allocated SHM data
 *
 * @param shm_data       SHM data structure
 *
 * @returns Size of SHM allocated
 *
 * Get SHM data size
 *
 */
extern size_t shm_data_byte_size(shm_data_ptr shm_data);

/*f shm_data_physical_address */
/**
 * @brief Get physical address of huge page
 *
 * @param shm            SHM allocated within
 *
 * @param shm_data       SHM data structure
 *
 * @returns 0 on error else a physical address
 *
 * Get SHM data size
 *
 */
extern uint64_t
shm_data_physical_address(struct shm *shm, struct shm_data *shm_data, uint64_t ofs);

/*f shm_shm_close */
/**
 *
 * @brief Close the shared memory corresponding to the SHM device
 *
 * @param shm           SHM structure of SHM device to close SHM for
 *
 * Close the shared memory previousa allocated with @p shm_alloc
 *
 */
extern void shm_shm_close(struct shm *shm);

/*f shm_huge_init */
/**
 *
 * @brief Initialize hugepages management
 *
 * @param shm        SHM structure already initialized
 *
 * @returns True if hugepages in place
 *
 * Prepares hugepages use of SHM
 *
 */
extern int shm_huge_init(struct shm *shm);

/*f shm_huge_malloc */
/**
 *
 * @brief Malloc using hugepages and get pointer to it
 *
 * @param shm        SHM structure already initialized
 *
 * @param ptr        Pointer to store (process virtual) allocated memory ptr in
 *
 * @param byte_size  Byte size to allocate
 *
 * @returns amount of memory allocated
 *
 * Allocate huge pages with @p get_huge_pages, and ensure it is mapped.
 *
 */
extern int shm_huge_malloc(struct shm *shm, void **ptr, size_t byte_size);

/*f shm_huge_physical_address */
/**
 *
 * @brief Find physical address of an offset into a huge malloc region
 *
 * @param shm   SHM structure already initialized
 *
 * @param ptr   Previously shm_huge_malloc pointer
 *
 * @param ofs   Offset from pointer to find address
 *
 * This function for Ubuntu LTS14.04 uses a /proc/self/pagemap hack to
 * find the physical address for a process virtual address
 *
 */
uint64_t
shm_huge_physical_address(struct shm *shm, void *ptr, uint64_t ofs);

/*f shm_huge_free */
/**
 *
 * @brief Free a hugepage allocation
 *
 * @param shm        SHM structure already initialized
 *
 * @param ptr        Huge page allocation previous returned by shm_huge_malloc
 *
 * Free a huge page allocation previous allocated by @p shm_huge_malloc
 */
extern void shm_huge_free(struct shm *shm, void *ptr);


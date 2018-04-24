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
 * @file          shm_support.c
 * @brief         SHM support library
 *
 */

/*a Includes
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h> 
#include <stddef.h> 
#include <string.h> 
#include <sys/types.h> 
#include <sys/stat.h> 
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>
#include <sys/shm.h>
#include <sys/ipc.h>
#include <inttypes.h>
#include "shm_stubs_lib.h"

#ifdef USE_HUGETLBFS
#include <hugetlbfs.h>
#else
/** SHM_HUGETLB is provided by /usr/include/x86_64-linux-gnu/bits/shm.h normally **/
#ifndef SHM_HUGETLB
#define SHM_HUGETLB 0
#endif

/** GHP_DEFAULT is provided by /usr/include/hugetlbfs.h **/
#define GHP_DEFAULT 0
#endif

/*a Functions from /usr/include/hugetlbfs.h
 */
/*f gethugepagesize */
/**
 * Return huge page size of 1MB
 */
#ifndef USE_HUGETLBFS
static int
gethugepagesize(void)
{
    return 1<<20;
}
#endif // USE_HUGETLBFS

/*f get_huge_pages */
/**
 * Fail to return memory
 */
#ifndef USE_HUGETLBFS
static void *
get_huge_pages(size_t size, int flags)
{
    (void)size;(void)flags;
    return NULL;
}
#endif // USE_HUGETLBFS

/*f free_huge_pages */
/**
 * Dummy function doing nothing
 */
#ifndef USE_HUGETLBFS
static void
free_huge_pages(void *ptr)
{
    (void)ptr;
}
#endif // USE_HUGETLBFS

/*a Statics
 */
static struct shm *shm_list;
static int exit_handler_registered=0;

/*a Structures
 */
/** struct pagemap_data
 */
struct pagemap_data {
    int   fd;
    int   page_size;
    long  huge_page_size;
};

/** struct shm_data
 */
struct shm_data {
    struct shm_data *prev;
    struct shm_data *next;
    FILE  *file;
    int   id;
    int   huge;
    void  *data;
    size_t byte_size;
};

/** struct shm
 */
struct shm {
    struct shm *prev;
    struct shm *next;
    struct pagemap_data pagemap;
    struct shm_data *data;
};

/*f exit_handler */
/**
 *
 * Shutdown all SHMs on the list
 *
 */
static void
exit_handler(void)
{
    fprintf(stderr,"Exit handler called\n");
    while (shm_list) {
        fprintf(stderr,"Shutdown %p\n",(void *)shm_list);
        shm_shm_shutdown(shm_list);
    }
}

/*f sigterm_handler
 */
static void
sigterm_handler(int sig)
{
    (void) sig;
    exit(0);
}

/*f shm_link */
/**
 */
static void
shm_link(struct shm *shm)
{
    struct shm *n;
    for (n=shm_list; n; n=n->next) {
        if (n==shm) return;
    }

    shm->next=shm_list;
    if (shm_list) shm_list->prev=shm;
    shm_list = shm;
    shm->prev=NULL;
}

/*f shm_unlink */
/**
 */
static void
shm_unlink(struct shm *shm)
{
    struct shm **shm_ptr;

    for (shm_ptr=&shm_list; *shm_ptr; ) {
        if ((*shm_ptr)==shm) {
            *shm_ptr=shm->next;
            if (shm->next) {
                shm->next->prev=*shm_ptr;
            }
        } else {
            shm_ptr=&((*shm_ptr)->next);
        }
    }
    shm->next=NULL;
    shm->prev=NULL;
}

/*a Huge pages */
/*f shm_huge_init
 */
extern int
shm_huge_init(struct shm *shm)
{
    shm->pagemap.page_size      = getpagesize();
    shm->pagemap.huge_page_size = gethugepagesize();
    shm->pagemap.fd=open("/proc/self/pagemap", O_RDONLY);
    return (shm->pagemap.fd>=0);
}

/*f shm_huge_malloc
 *
 * Malloc using hugepages, and get pointer to it,
 * and return 0 on success
 *
 * @param shm        SHM structure already initialized
 * @param ptr        Pointer to store (virtual) allocated memory ptr in
 * @param byte_size  Byte size to allocate
 *
 */
extern int
shm_huge_malloc(struct shm *shm, void **ptr, size_t byte_size)
{
    int  num_huge_pages;
    long allocation_size;

    fprintf(stderr,"shm_huge_malloc %ld\n", byte_size);
    num_huge_pages = ((byte_size-1)/shm->pagemap.huge_page_size)+1;
    allocation_size = num_huge_pages*shm->pagemap.huge_page_size;

    fprintf(stderr,"num_pages %d allocation_size %ld\n", num_huge_pages, allocation_size);
    *ptr = get_huge_pages(allocation_size,GHP_DEFAULT);
    fprintf(stderr,"got %p\n", *ptr);
    if (*ptr == NULL)
        return 0;

    ((uint64_t *)(*ptr))[0]=0;

    return allocation_size;
}

/*f shm_huge_physical_address
 *
 * Find physical address of an offset into a huge malloc region
 *
 * @param shm   SHM structure already initialized
 * @param ptr   Previously shm_huge_malloc pointer
 * @param ofs   Offset from pointer to find address
 *
 */
extern uint64_t
shm_huge_physical_address(struct shm *shm, void *ptr, uint64_t ofs)
{
    uint64_t linux_pfn, linux_page_data;
    uint64_t addr;
    int err;

    if (shm->pagemap.fd<0) return 0;
    /* Hack around with the internals of the pagemap file
       This is based on DPDK's huge page hacking
    */
    ptr = (void *)(((char *)ptr) + ofs);
    linux_pfn = ((uint64_t)ptr) / shm->pagemap.page_size;
    err = (lseek(shm->pagemap.fd, linux_pfn*sizeof(uint64_t),SEEK_SET)<0);
    if (!err) {
        err=(read(shm->pagemap.fd, &linux_page_data, sizeof(uint64_t))<0);
    }
    if (!err) {
        err=(((linux_page_data>>63)&1)==0); /* page not present */
    }
    if (err) {
        return 0;
    }
    addr = (linux_page_data & (-1LL>>(64-55)))*shm->pagemap.page_size;
    addr += ((uint64_t)ptr) % shm->pagemap.page_size;
    //fprintf(stderr,"Huge page for %p offset %"PRIx64" is %"PRIx64"\n",ptr,ofs,addr);
    return addr;
}

/*f shm_huge_free
 *
 * Free a hugepage allocation
 *
 * @param shm        SHM structure already initialized
 * @param ptr        Huge page allocation previous returned by shm_huge_malloc
 *
 */
extern void
shm_huge_free(struct shm *shm, void *ptr)
{
    (void) shm;
    free_huge_pages(ptr);
}

/*a SHM instance functions
 */
/*f shm_shm_init
 *
 * Initialize SHM structure
 * Returns NULL on error, otherwise an allocated SHM structure
 * Adds atexit handler to shut down SHM cleanly at exit
 *
 * @param sig_term   If non-zero attaches sigterm handler too
 */
extern struct shm *
shm_shm_init(int sig_term)
{
    struct shm *shm;
    shm = malloc(sizeof(struct shm));
    if (!shm) return NULL;

    shm->pagemap.fd = -1;
    shm->data = NULL;

    if (!exit_handler_registered) {
        exit_handler_registered=1;
        atexit(exit_handler);
    }
    if (sig_term) {
        if (signal(SIGTERM, sigterm_handler) == SIG_ERR) {
            fprintf(stderr, "Failed to attach signal handler for SIGTERM\n");
            goto err;
        }
        if (signal(SIGINT, sigterm_handler) == SIG_ERR) {
            fprintf(stderr, "Failed to attach signal handler for SIGINT\n");
            goto err;
        }
    }

    shm_huge_init(shm);

    shm_link(shm);

    return shm;

err:
    shm_shm_shutdown(shm);
    return NULL;
}

/*f shm_shm_close
 */
extern void
shm_shm_close(struct shm *shm)
{
    struct shm_data *d, *d_next;
    for (d=shm->data; d; d=d_next) {
        d_next = d->next;
        if (d->huge) {
            if (d->data) shm_huge_free(shm, d->data);
            d->data = NULL;
        } else {
            shmdt(d->data);
            d->data = NULL;
            if (d->file != NULL) {
                struct shmid_ds shmid_ds;
                shmctl(d->id, IPC_RMID, &shmid_ds);
            }
            if (d->file != NULL) {
                fclose(d->file);
                d->file = NULL;
            }
        }
        free(d);
    }
    shm->data = NULL;
}

/*f shm_shm_shutdown
 *
 * Shutdown the SHM, unloading firmware before closing the device
 * Performs an incremental shutdown of the activated components, and can
 * be performed many times successively without failure
 * Removes the SHM from the list to be shutdown at exit
 *
 * @param shm    SHM structure of device to shut down
 *
 */
void
shm_shm_shutdown(struct shm *shm)
{
    if (!shm) return;
    shm_unlink(shm);
    shm_shm_close(shm);
    free(shm);
}

/*a Shared memory
 */
/*f shm_data_alloc
 */
extern shm_data_ptr
shm_data_alloc(struct shm *shm, const char *shm_filename, int shm_key, size_t byte_size, int create)
{
    int shm_flags;
    key_t key;
    struct shmid_ds shmid_ds;
    struct shm_data *shm_data;

    shm_data = (struct shm_data *)malloc(sizeof(struct shm_data));
    shm_flags = 0x1ff;
    if (create) {
        if (byte_size==0) {
            return NULL;
        }
        shm_flags |= IPC_CREAT;
        shm_data->file = fopen(shm_filename,"w");
        if (!shm_data->file) {
            fprintf(stderr,"Failed to open shm lock file %s\n", shm_filename);
            return NULL;
        }
    } else {
        byte_size = 0;
    }
    key = ftok(shm_filename, shm_key);

    shm_data->id = shmget(key, byte_size, SHM_HUGETLB | shm_flags );
    if (shm_data->id == -1) {
        fprintf(stderr,"Failed to allocate SHM id\n");
        shm_shm_close(shm);
        return NULL;
    }
    if (shmctl(shm_data->id, IPC_STAT, &shmid_ds) != 0) {
        fprintf(stderr,"Failed to find SHM size\n");
        return NULL;
    }
    byte_size = shmid_ds.shm_segsz;
    shm_data->data = shmat(shm_data->id, NULL, 0);
    shm_data->byte_size = byte_size;
    if (create) {
      ((uint64_t *)(shm_data->data))[0]=0;
    }
    return shm_data;
}

/*f shm_data_alloc_huge
 */
extern shm_data_ptr
shm_data_alloc_huge(struct shm *shm, size_t byte_size)
{
    void *data;
    struct shm_data *shm_data;

    byte_size = shm_huge_malloc(shm, &data, byte_size);

    if (byte_size==0) return NULL;

    shm_data = (struct shm_data *)malloc(sizeof(struct shm_data));
    shm_data->file = NULL;
    shm_data->huge = 1;
    shm_data->id = -1;
    shm_data->data = data;
    shm_data->byte_size = byte_size;
    return shm_data;
}

/*f shm_data_base
 *
 * Get SHM data pointer after it has been allocated
 *
 * @param shm_data       SHM data structure
 *
 */
extern void *
shm_data_base(struct shm_data *shm_data)
{
    return shm_data->data;
}

/*f shm_data_physical_address
 *
 * Get SHM data physical address (if huge?)
 *
 * @param shm_data       SHM data structure
 *
 */
extern uint64_t
shm_data_physical_address(struct shm *shm, struct shm_data *shm_data, uint64_t ofs)
{
    return shm_huge_physical_address(shm, shm_data->data, ofs);
}

/*f shm_data_byte_size
 *
 * Get size of SHM data
 *
 * @param shm_data       SHM data structure
 *
 */
extern size_t
shm_data_byte_size(struct shm_data *shm_data)
{
    return shm_data->byte_size;
}


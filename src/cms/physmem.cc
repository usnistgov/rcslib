/* 
The NIST RCS (Real-time Control Systems) 
 library is public domain software, however it is preferred
 that the following disclaimers be attached.

Software Copywrite/Warranty Disclaimer

   This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
public domain. NIST Real-Time Control System software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modified
versions bear some notice that they have been modified.



*/ 

/***********************************************************************
* File: physmem.cc
* Purpose: Provides the member functions for the PHYSMEM_HANDLE class. This is
* a C++ interface for portable access to physical memory.
* NOTES:
*  On some platforms accessing a block of physical memory is a simple as
* intitializing a pointer and then dereferencing it, but accessing specific
* sections of extended memory under DOS or Windows requires changing the
* descriptor tables and therefore circumventing protected mode security.
* Other platforms require the use of mmap() on memory device files to choose
* a section of memory mapped IO.
***********************************************************************/

#if HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"

typedef char *LOCAL_ADDRESS_TYPE;
#else
#include "physmem_no_config.h"
#endif

#include "physmem.hh"		/* class PHYSMEM_HANDLE */

#if  defined(__MSDOS__) && !defined(__WIN32__)
#include "_physmem.h"		/* read_physmem(), write_physmem(), */
				/* create_ptr_to_physmem() */
#endif

#include "rcs_prnt.hh"

#ifdef DEBUG_MEMORY
#include "dbg_mem.h"
#else
#ifndef DEBUG_MALLOC
#define DEBUG_MALLOC malloc
#endif
#ifndef DEBUG_CALLOC
#define DEBUG_CALLOC calloc
#endif
#ifndef DEBUG_REALLOC
#define DEBUG_REALLOC realloc
#endif
#ifndef DEBUG_FREE
#define DEBUG_FREE free
#endif
#endif
// DEBUG_MEMORY

#ifdef USE_MAPMEM
#error "Mapmem does not work any more, get a much older version of RCS lib."
#endif

#ifdef USE_GENPORT
#error "Genport does not work any more, get a much older version of RCS lib."
#endif


class PHYSMEM_INTERNALS 
{
  PHYSMEM_INTERNALS();
  ~PHYSMEM_INTERNALS();
 
  class PHYSMEM_HANDLE *loopback1;

  friend class PHYSMEM_HANDLE;

  LOCAL_ADDRESS_TYPE local_address;
  
#ifdef VXWORKS
  volatile char *bus_address;
#endif

#ifdef lynxosPC
  char smem_name[40];
#endif
  long offset;			/* Operations read and write work use offset */
  long size;
  long address_code;		/* Platform specific address type code. */
  /* (See vme.h for VXWORKS) */

  int isvalid;
  char *temp_buf;
  unsigned long physical_address;
  int using_bit3;
  double total_bytes_moved;
  int enable_byte_counting;
  int error_count;
  class PHYSMEM_HANDLE *loopback2;
private:
  void zero_self(void);

private:
  PHYSMEM_INTERNALS(const PHYSMEM_INTERNALS &);
  PHYSMEM_INTERNALS &operator=(const PHYSMEM_INTERNALS &);

};

PHYSMEM_INTERNALS::PHYSMEM_INTERNALS():
    loopback1(0),
    local_address(0),
#ifdef VXWORKS
    bus_address(0),
#endif
    offset(0),
    size(0),
    address_code(0),
    isvalid(0),
    temp_buf(0),
    physical_address(0),
    using_bit3(0),
    total_bytes_moved(0),
    enable_byte_counting(0),
    error_count(0),
    loopback2(0)
{
  zero_self();
}

PHYSMEM_INTERNALS::~PHYSMEM_INTERNALS()
{
  zero_self();
}

void
PHYSMEM_INTERNALS::zero_self()
{
  local_address=0;
  
#ifdef VXWORKS
  bus_address=0;
#endif

#ifdef lynxosPC
  memset(smem_name,0,sizeof(smem_name));
#endif

  offset=0;			/* Operations read and write work use offset */
  size=0;
  address_code=0;		/* Platform specific address type code. */
  /* (See vme.h for VXWORKS) */

  isvalid=0;
  temp_buf=0;
  physical_address=0;
  using_bit3=0;
  total_bytes_moved=0;
  enable_byte_counting=0;
}

static bool null_internals_error_printed=false;

long 
PHYSMEM_HANDLE::get_size(void)
{
  if(internals)
    {
      return internals->size;
    }
  if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::get_size -- internals == NULL\n");
      null_internals_error_printed=true;
    }
  return(0);
}

void 
PHYSMEM_HANDLE::set_size(long l)
{
  if(l < 0)
    {
      rcs_print_error("PHYSMEM_HANDLE::set_size  -- bad size %ld\n",l);
    }
  if(internals)
    {
      internals->size = l;
    }
  else if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::set_size -- internals == NULL\n");
      null_internals_error_printed=true;
    }
}
 
long 
PHYSMEM_HANDLE::get_offset(void)
{
  if(internals)
    {
      return internals->offset;
    }
  if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::get_offset -- internals == NULL\n");
      null_internals_error_printed=true;
    }
  return(0);
}

void PHYSMEM_HANDLE::set_offset(long l)
{
  if(l < 0)
    {
      rcs_print_error("PHYSMEM_HANDLE::set_offset  -- bad offset %ld\n",l);
    }
 if(internals)
    {
      if(internals->size > 0 && l > internals->size)
	{
	  rcs_print_error("PHYSMEM_HANDLE::set_offset  -- null offset %ld, size=%ld\n",l,internals->size);
	  internals->error_count++;
	  return;
	}
      internals->offset = l;
    }
 else if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::set_offset -- internals == NULL\n"); 
      null_internals_error_printed=true;
    }
}

void PHYSMEM_HANDLE::increment_offset(long l)
{
  if(l < 0)
    {
      rcs_print_error("PHYSMEM_HANDLE::increment_offset  -- bad offset %ld\n",l);
    }
 if(internals)
    {
      if(internals->size > 0 && 
	 ( l > internals->size || l+internals->offset > internals->size))
	{
	  rcs_print_error("PHYSMEM_HANDLE::increment_offset  -- bad offset %ld, current_offset=%ld,size=%ld\n",l,internals->offset,internals->size);
	  internals->error_count++;
	  return;
	}
      internals->offset += l;
    }
 else if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::increment_offset -- internals == NULL\n"); 
      null_internals_error_printed=true;
    }
}


void PHYSMEM_HANDLE::align(unsigned char uc)
{
 if(internals)
    {
      if(internals->size > 0 && 
	 ( uc > internals->size || uc+internals->offset > internals->size))
	{
	  rcs_print_error("PHYSMEM_HANDLE::aling  -- bad offset %d, current_offset=%ld,size=%ld\n",
			  uc,internals->offset,internals->size);
	  internals->error_count++;
	}
      //      unsigned long mod = (internals->offset % uc);
      void *p = (void *) 
      	(((char *) internals->local_address) + internals->offset);
      unsigned long pmod = ((unsigned long)p)%uc;
      //       unsigned long lamod = ((unsigned long)internals->local_address)%uc;
      //       printf("align(%d) offset=0x%lX, mod=%ld, local_address=%p, lamod=%ld,p=%p, pmod=%ld, inc=%u\n",
      // 	     uc,
      // 	     ((unsigned long) internals->offset),
      // 	     mod,
      // 	     internals->local_address,
      // 	     lamod,
      // 	     p,
      // 	     pmod,
      // 	     inc);

      if(pmod != 0)
	{
	  unsigned int inc = (unsigned int) (uc-pmod);
	  internals->offset += inc;
	}
    }
 else if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::increment_offset -- internals == NULL\n"); 
      null_internals_error_printed=true;
    }
}

void PHYSMEM_HANDLE::decrement_offset(long l)
{
  if(l < 0)
    {
      rcs_print_error("PHYSMEM_HANDLE::decrement_offset  -- bad offset %ld\n",l);
    }
 if(internals)
    {
      if(internals->size > 0 && 
	 ( l > internals->size || internals->offset-l < 0))
	{
	  rcs_print_error("PHYSMEM_HANDLE::decrement_offset  -- bad offset %ld, size=%ld\n",l,internals->size);
	  internals->error_count++;
	  return;
	}
      internals->offset -= l;
    }
 else if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::decrement_offset -- internals == NULL\n"); 
      null_internals_error_printed=true;
    }
}

long
PHYSMEM_HANDLE::get_address_code(void)
{
  if(internals)
    {
      return internals->address_code;
    }
  if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::get_address_code -- internals == NULL\n");
      null_internals_error_printed=true;
    }
  return(-1);
}

void PHYSMEM_HANDLE::set_address_code(long l)
{
  if(internals)
    {
      internals->address_code=l;
    }
  else if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::set_address_code -- internals == NULL\n");
      null_internals_error_printed=true;
    }
}

void *
PHYSMEM_HANDLE::get_local_address(void)
{
  if(internals)
    {
      return internals->local_address;
    }
  return(0);
}


unsigned long
PHYSMEM_HANDLE::get_physical_address(void)
{
  if(internals)
    {
      return internals->physical_address;
    }
  if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::get_physical_address -- internals == NULL\n");
      null_internals_error_printed=true;
    }
  return(0);
}

void
PHYSMEM_HANDLE::set_physical_address(unsigned long ul)
{
  if(internals)
    {
      internals->physical_address=ul;
    }
  else if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::set_physical_address -- internals == NULL\n");
      null_internals_error_printed=true;
    }
}
  
char *
PHYSMEM_HANDLE::get_temp_buf(void)
{
  if(internals)
    {
      return internals->temp_buf;
    }
  if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::get_temp_buf -- internals == NULL\n");
      null_internals_error_printed=true;
    }
  return(0);
}

void
PHYSMEM_HANDLE::set_temp_buf(char *x)
{
  if(internals)
    {
      internals->temp_buf= x;
    }
  else if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::set_temp_buf -- internals == NULL\n");
      null_internals_error_printed=true;
    }
}

int
PHYSMEM_HANDLE::get_using_bit3(void)
{
  if(internals)
    {
      return internals->using_bit3;
    }
  if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::get_using_bit3 -- internals == NULL\n");
      null_internals_error_printed=true;
    }
  return(0);
}

void PHYSMEM_HANDLE::set_using_bit3(int i)
{
  if(internals)
    {
      internals->using_bit3 = i;
    }
  else if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::get_using_bit3 -- internals == NULL\n");
      null_internals_error_printed=true;
    }
} 

double PHYSMEM_HANDLE::get_total_bytes_moved(void)
{
  if(internals)
    {
      return internals->total_bytes_moved;
    }
  if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::get_total_bytes_moved -- internals == NULL\n");
      null_internals_error_printed=true;
    }
  return(0);
}
 
void PHYSMEM_HANDLE::set_total_bytes_moved(double d)
{
  if(internals)
    {
      internals->total_bytes_moved = d;
    }
  else if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::get_total_bytes_moved -- internals == NULL\n");
      null_internals_error_printed=true;
    }
} 

void PHYSMEM_HANDLE::increment_total_bytes_moved(double d)
{
  if(internals)
    {
      internals->total_bytes_moved += d;
    }
  else if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::get_total_bytes_moved -- internals == NULL\n");
      null_internals_error_printed=true;
    }
} 

int PHYSMEM_HANDLE::get_enable_byte_counting(void)
{
  if(internals)
    {
      return internals->enable_byte_counting;
    }
  if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::get_enable_byte_counting -- internals == NULL\n");
      null_internals_error_printed=true;
    }
  return(0);
}
void PHYSMEM_HANDLE::set_enable_byte_counting(int i)
{
  if(internals)
    {
      internals->enable_byte_counting = i;      
    } 
  else if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::get_enable_byte_counting -- internals == NULL\n");
      null_internals_error_printed=true;
    }
} 

/* Constructor. */
PHYSMEM_HANDLE::PHYSMEM_HANDLE ():
  internals(0)
{
  internals  = new PHYSMEM_INTERNALS();
  if(internals)
    {
      internals->loopback1 = this;
      internals->loopback2 = this;
      internals->size = 0;
      internals->offset = 0;
      internals->temp_buf = NULL;
      
      internals->local_address = (LOCAL_ADDRESS_TYPE) NULL;
      internals->physical_address = 0;
      internals->using_bit3 = 0;
      internals->isvalid = 1;
      internals->total_bytes_moved = 0;
      internals->enable_byte_counting = 0;
    }
}

#ifdef VXWORKS

extern "C" int rcs_local_to_bus_addr(char *  _local_addr);
extern "C" int rcs_bus_to_local_addr(char * _physical_addr);
extern "C" int rcs_set_bus_addr_code(int);

STATUS rcs_last_bus_local_convert_status=-1;
int rcs_bus_addr_code=0;
char *rcs_lkdup_bus_addr;
char *rcs_lkdup_local_addr;

int
rcs_set_bus_addr_code(int new_code)
{
  rcs_bus_addr_code = new_code;
  return rcs_bus_addr_code;
}

int
rcs_local_to_bus_addr(char *  _local_addr)
{
  rcs_lkdup_bus_addr=0;
  rcs_last_bus_local_convert_status = 
    sysLocalToBusAdrs(rcs_bus_addr_code,
		      _local_addr,
		      &rcs_lkdup_bus_addr);
  if(rcs_last_bus_local_convert_status != OK)
    {
      rcs_print_error("sysLocalToBusAdrs(%d (0x%X),%p,%p) failed: errno =%d : %s\n",
		      rcs_bus_addr_code,rcs_bus_addr_code,
		      ((void *) _local_addr),
		      ((void *) (&rcs_lkdup_bus_addr)),
		      errno,strerror(errno));
    }
  return((int) rcs_lkdup_bus_addr);
}

int
rcs_bus_to_local_addr(char *  _bus_addr)
{
  rcs_lkdup_bus_addr=0;
  rcs_last_bus_local_convert_status = 
    sysBusToLocalAdrs(rcs_bus_addr_code,
		      _bus_addr,
		      &rcs_lkdup_local_addr);
  if(rcs_last_bus_local_convert_status != OK)
    {
      rcs_print_error("sysBusToLocalAdrs(%d (0x%X),%p,%p) failed: errno =%d : %s\n",
		      rcs_bus_addr_code,rcs_bus_addr_code,
		      ((void *) _bus_addr),
		      ((void *) (&rcs_lkdup_local_addr)),
		      errno,strerror(errno));
    }
  return((int) rcs_lkdup_local_addr);
}

#endif

PHYSMEM_HANDLE::PHYSMEM_HANDLE (unsigned long _physical_address,
				long _address_code, long _size):
  internals(0)
{
  internals  = new PHYSMEM_INTERNALS();
  if(internals)
    {
      internals->loopback1 = this;
      internals->loopback2 = this;
      internals->temp_buf = NULL;
      internals->physical_address = _physical_address;
      internals->size = _size;
      internals->address_code = _address_code;
      internals->local_address = (LOCAL_ADDRESS_TYPE) NULL;
      internals->offset = 0;
      internals->using_bit3 = 0;

      if (0 == _physical_address)
	{
	  internals->local_address = (LOCAL_ADDRESS_TYPE) NULL;
	  return;
	}

#ifdef VXWORKS
      /* Normally the address should be in "standard" space and address_code */
      /* should equal VME_AM_STD_USR_DATA. */
      /* and the sysBusToLocal will be used to get the address as seen */
      /* from this board. If the conversion doesn't work the user could replace */
      /* address_code with another code from "vme.h". or determine the correct */
      /* address for this particular board to use and bypass the */
      /* sysBusToLocalAdrs() call by setting address_code to zero. */
      /* If this is done though globmem it can be done by adding the line */
      /* vme_code=??? to the buffer line of the config file. */
      /* local address and put the VME address in a different config file. */
      internals->bus_address = (char *) internals->physical_address;
      if (internals->address_code != 0)
	{
	  if (OK != sysBusToLocalAdrs (internals->address_code,
				       (char *) internals->bus_address,
				       (char **) &internals->local_address))
	    {
	      rcs_print_error ("sysBusToLocalAdrs(%d (0x%X),%p,%p) returned ERROR. %d %s\n",
			       internals->address_code,internals->address_code,
			       ((void*)internals->bus_address),
			       ((void*) (&internals->local_address)),
			       errno, strerror (errno));
	      rcs_print_error ("Try changing vme_code=???");
	      internals->isvalid = 0;
	      return;
	    }
	}
      else
	{
	  internals->local_address = (char *) internals->physical_address;
	}
      internals->isvalid = 1;
      return;
#else
      internals->isvalid = 1;
#endif
      
#ifdef lynxosPC
      SNPRINTF_FUNC ( SNPRINTF_ARGS(internals->smem_name,sizeof(internals->smem_name)), 
		      "rcs_physmem%X", internals->physical_address);

      // Remove any use of this name.
      smem_remove (internals->smem_name);
  
      if ((internals->local_address = (void *)
	   smem_create (internals->smem_name, (char *) internals->physical_address,
			internals->size,
			SM_READ | SM_WRITE)) == NULL)
	{
	  rcs_print_error ("can't get physical memory %X", physical_address);
	}
      return;
#endif

#ifdef LINUX_VME
      {
	int fd = open ("/dev/mem", O_RDWR);
	if (fd < 0)
	  {
	    perror ("open /dev/mem");
	    return;
	  }

	/* map it */
	internals->local_address = (LOCAL_ADDRESS_TYPE) mmap (0, size,
							      PROT_READ | PROT_WRITE,
							      MAP_FILE | MAP_SHARED,
							      fd, physical_address);

	/* close the fd since it's no longer used */
	close (fd);
      }
#endif
#if !defined(__MSDOS__) && !defined(_Windows) && !defined(lynxosPC)
      internals->local_address = (LOCAL_ADDRESS_TYPE) internals->physical_address;
#endif
    }
}

/* Destructor. */
PHYSMEM_HANDLE::~PHYSMEM_HANDLE ()
{
  volatile class PHYSMEM_INTERNALS *_internals = internals;
  internals=0;
  if(!_internals)
    {
      return;
    }
#ifdef lynxosPC
  smem_create ("", (char *) _internals->physical_address, (long int) 0, SM_DETACH);
  smem_remove (_internals->smem_name);

#endif

#ifdef LINUX_VME
  if (_internals->local_address != 0 && physical_address != 0)
    {
      munmap (_internals->local_address, size);
    }
#endif
  
  delete _internals;
  _internals=0;
}


/* Use an ordinary pointer to access memory. */
void
PHYSMEM_HANDLE::set_to_ptr (void *_ptr, long _size)
{
  if(internals)
    {
      if(_ptr == 0 || _size < 1)
	{
	  rcs_print_error("PHYSMEM_HANDLE::set_to_ptr bad ptr %p or size %ld\n",
			  _ptr,_size);
	}
      internals->local_address = (LOCAL_ADDRESS_TYPE) _ptr;
      internals->size = _size;
      internals->offset = 0;
    }
  else if(!null_internals_error_printed)
    {
      rcs_print_error("PHYSMEM_HANDLE::set_to_ptr -- internals == NULL\n");
      null_internals_error_printed=true;
    }
}
 
static int physmem_read_local_address_is_null_error_print_count = 0;

/***********************************************************
* Read _read_size bytes from physical memory to store at _to.
* Returns: 0 for success or -1 for failure.
**********************************************************/
int
PHYSMEM_HANDLE::read (void *_to, long _read_size)
{
  if (NULL == _to)
    {
      rcs_print_error ("PHYSMEM_HANDLE::read _to = NULL.\n");
      return (-1);
    }

  if(0 == internals)
    {
      if(!null_internals_error_printed)
	{
	  rcs_print_error("PHYSMEM_HANDLE::read -- internals == NULL\n");
	  null_internals_error_printed=true;
	}
      return(-1);
    }

  /* Check internals->sizes. */
  if (_read_size + internals->offset > internals->size || internals->offset < 0)
    {
      rcs_print_error
	("PHYSMEM_HANDLE: Can't read %ld bytes at offset %ld from buffer of size %ld.\n",
	 _read_size, internals->offset, internals->size);
      return (-1);
    }

  if (internals->enable_byte_counting)
    {
      internals->total_bytes_moved += _read_size;
    }

#ifdef USE_BIT3
  if (using_bit3)
    {
      if (internals->swap_mode)
	{
	  /* Lock the adaptor */
	  if (internals->bt_use_lock)
	    {
	      internals->bt_status = bt_lock (btd, bt_lock_timeout);
	      if (BT_SUCCESS != internals->bt_status)
		{
		  bt_perror (internals->btd, internals->bt_status,
			     "Could not lock the Bit3 device. ");
		  return (-1);
		}
	    }

	  // Must use character by character copy to ensure bit3 doesn't do stupid
	  // redundant swapping.
	  volatile char *fp = ((char *) internals->local_address) + internals->offset;
	  volatile char *tp = (char *) _to;
	  for (int i = 0; i < _read_size; i++)
	    {
	      *tp = *fp;
	      tp++;
	      fp++;
	    }

	  /* Unlock the adaptor  */
	  if (internals->bt_use_lock)
	    {
	      internals->bt_status = bt_unlock (btd);
	      if (BT_SUCCESS != internals->bt_status)
		{
		  bt_perror (internals->btd, internals->bt_status,
			     "Could not unlock the Bit3 device. ");
		  return (-1);
		}
	    }

	  return 0;
	}
      unsigned int bt_bytes_read;
      bt_devaddr_t transfer_addr = internals->btd_offset + internals->offset;
      internals->bt_status =
	bt_read (internals->btd, _to, transfer_addr, _read_size, &bt_bytes_read);
      if (BT_SUCCESS != internals->bt_status)
	{
	  bt_perror (internals->btd, internals->bt_status, " bt_read failed ");
	  return (-1);
	}
      return (0);
    }
#endif

  /* If local_address has been initialized use it as an ordinary pointer. */
  if (NULL != internals->local_address)
    {
      char *from;
      from = ((char *) internals->local_address) + internals->offset;
      //  printf("_to=%p, from=%p, _read_size=%ld\n",
      // 	     _to,from,_read_size);
      if (_read_size == 2)
	{
	  short *sfrom = (short *) from;
	  short sval;
	  sval = *sfrom;
	  short *sto = (short *) _to;
	  *sto = sval;
	}
      else
	{
	  memcpy (_to, from, _read_size);
	}
      return (0);
    }

  /* include platform specific ways of accessing phsical memory here. */
#if defined(__MSDOS__) && !defined(__WIN32__)
  return (read_physmem (physical_address + internals->offset, _to, _read_size));
#endif
#if defined(WIN32) && !defined(gnuwin32)
#ifdef USE_GENPORT
  if (address_code == NT_ISA_IO_ADDRESS && internals->ioport != 0)
    {
      return internals->read_from_genport (_to, _read_size);
    }
#endif
#endif
  if (!(physmem_read_local_address_is_null_error_print_count % 100000))
    {
      rcs_print_error
	("PHYSMEM_HANDLE: Cannot read from physical memory when local address is NULL.\n");
      rcs_print_error ("(This error has occured %d times.)\n",
		       physmem_read_local_address_is_null_error_print_count +
		       1);
    }
  physmem_read_local_address_is_null_error_print_count++;
  return (-1);
}

static int physmem_write_local_address_is_null_error_print_count = 0;

/***********************************************************
* Write _write_size bytes from memory at _from to physical memory.
* Returns: 0 for success or -1 for failure.
**********************************************************/
int
PHYSMEM_HANDLE::write (void *_from, long _write_size)
{
  if (NULL == _from)
    {
      rcs_print_error ("PHYSMEM_HANDLE:write _from = NULL\n");
      return -1;
    }

  if(0 == internals)
    {
      if(!null_internals_error_printed)
	{
	  rcs_print_error("PHYSMEM_HANDLE::write -- internals == NULL\n");
	  null_internals_error_printed=true;
	}
      return(-1);
    }

  /* Check sizes. */
  if (_write_size + internals->offset > internals->size || internals->offset < 0)
    {
      rcs_print_error
	("PHYSMEM_HANDLE: Can't write %ld bytes at internals->offset %ld from buffer of size %ld.\n",
	 _write_size, internals->offset, internals->size);
      return (-1);
    }
  if (internals->enable_byte_counting)
    {
      internals->total_bytes_moved += _write_size;
    }


#ifdef USE_BIT3
  if (using_bit3)
    {
      if (internals->swap_mode)
	{
	  /* Lock the adaptor */
	  if (internals->bt_use_lock)
	    {
	      internals->bt_status = bt_lock (internals->btd, internals->bt_lock_timeout);
	      if (BT_SUCCESS != internals->bt_status)
		{
		  bt_perror (internals->btd, internals->bt_status,
			     "Could not lock the Bit3 device. ");
		  return (-1);
		}
	    }

	  // Must use character by character copy to ensure bit3 doesn't do stupid
	  // redundant swapping.
	  volatile char *fp = (char *) _from;
	  volatile char *tp = ((char *) internals->local_address) + internals->offset;
	  for (int i = 0; i < _write_size; i++)
	    {
	      *tp = *fp;
	      tp++;
	      fp++;
	    }

	  /* Unlock the adaptor  */
	  if (internals->bt_use_lock)
	    {
	      internals->bt_status = bt_unlock (internals->btd);
	      if (BT_SUCCESS != internals->bt_status)
		{
		  bt_perror (internals->btd, internals->bt_status,
			     "Could not unlock the Bit3 device. ");
		  return (-1);
		}
	    }
	  return 0;
	}
      unsigned int bt_bytes_read;
      bt_devaddr_t transfer_addr = btd_internals->offset + internals->offset;
      internals->bt_status =
	bt_write (internals->btd, _from, transfer_addr, _write_size, &bt_bytes_read);
      if (BT_SUCCESS != internals->bt_status)
	{
	  bt_perror (internals->btd, internals->bt_status, " bt_write failed ");
	  return (-1);
	}
      return (0);
    }
#endif

  /* If local_address has been initialized use it as an ordinary pointer. */
  if(NULL != internals)
    {
      if (NULL != internals->local_address)
	{
	  char *to;
	  to = ((char *) internals->local_address) + internals->offset;
	  //  printf("to=%p, _from=%p, _write_size=%ld\n",
	  // 		 to,_from,_write_size);
	  if (_write_size == 2)
	    {
	      short *sto = (short *) to;
	      short sval = *(short *) _from;
	      *sto = sval;
	    }
	  else
	    {
	      memcpy (to, _from, _write_size);
	    }
	  return (0);
	}
    }

  /* include platform specific ways of accessing phsical memory here. */
#if  defined(__MSDOS__) && !defined(__WIN32__)
  return (write_physmem (physical_address + internals->offset, _from, _write_size));
#endif
#if defined(WIN32) && !defined(gnuwin32)
#ifdef USE_GENPORT
  if (address_code == NT_ISA_IO_ADDRESS && internals->ioport != 0)
    {
      return internals->write_to_genport (_from, _write_size);
    }
#endif
#endif
  if (!(physmem_write_local_address_is_null_error_print_count % 100000))
    {
      rcs_print_error
	("PHYSMEM_HANDLE: Cannot write to physical memory when local address is NULL.\n");
      rcs_print_error ("(This error has occured %d times.)\n",
		       physmem_write_local_address_is_null_error_print_count +
		       1);
    }
  physmem_write_local_address_is_null_error_print_count++;
  return (-1);
}


/***********************************************************
* Write_with_bitwise_and  _write_size bytes from memory at _from to physical memory.
* Returns: 0 for success or -1 for failure.
**********************************************************/
int
PHYSMEM_HANDLE::write_with_bitwise_and (void *_from, long _write_size)
{
  if (NULL == _from)
    {
      rcs_print_error ("PHYSMEM_HANDLE:write _from = NULL\n");
      return -1;
    }

  if(0 == internals)
    {
      if(!null_internals_error_printed)
	{
	  rcs_print_error("PHYSMEM_HANDLE::write_with_bitwise_and -- internals == NULL\n");
	  null_internals_error_printed=true;
	}
      return(-1);
    }

  /* Check sizes. */
  if (_write_size + internals->offset > internals->size || internals->offset < 0)
    {
      rcs_print_error
	("PHYSMEM_HANDLE: Can't write %ld bytes at internals->offset %ld from buffer of size %ld.\n",
	 _write_size, internals->offset, internals->size);
      return (-1);
    }
  if (internals->enable_byte_counting)
    {
      internals->total_bytes_moved += _write_size;
    }


#ifdef USE_BIT3
  rcs_print_error("write_with_bitwise_and not supported for BIT3.\n");
  return -1;
#endif

  /* If local_address has been initialized use it as an ordinary pointer. */
  if (NULL != internals->local_address)
    {
      char *to;
      to = ((char *) internals->local_address) + internals->offset;
      const unsigned char *ucto = (unsigned char *)to;
      const unsigned char *ucfrom = (unsigned char *)_from;      
      const unsigned char *end_to = ucto + _write_size;
      const unsigned char *end_from = ucfrom + _write_size;
      unsigned char *t = (unsigned char *) to;
      unsigned char *f = (unsigned char *) _from;
      while(t < end_to && f < end_from)
	{
	  *t = *t & *f;
	  t++;
	  f++;
	}
      return (0);
    }

  /* include platform specific ways of accessing phsical memory here. */
  rcs_print_error("write_with_and_op not supported for unless local address is used..\n");
  return -1;
}


/***********************************************************
* Write_with_bitwise_or  _write_size bytes from memory at _from to physical memory.
* Returns: 0 for success or -1 for failure.
**********************************************************/
int
PHYSMEM_HANDLE::write_with_bitwise_or (void *_from, long _write_size)
{
  if (NULL == _from)
    {
      rcs_print_error ("PHYSMEM_HANDLE:write _from = NULL\n");
      return -1;
    }

  if(0 == internals)
    {
      if(!null_internals_error_printed)
	{
	  rcs_print_error("PHYSMEM_HANDLE::write_with_bitwise_or -- internals == NULL\n");
	  null_internals_error_printed=true;
	}
      return(-1);
    }

  /* Check sizes. */
  if (_write_size + internals->offset > internals->size || internals->offset < 0)
    {
      rcs_print_error
	("PHYSMEM_HANDLE: Can't write %ld bytes at internals->offset %ld from buffer of size %ld.\n",
	 _write_size, internals->offset, internals->size);
      return (-1);
    }
  if (internals->enable_byte_counting)
    {
      internals->total_bytes_moved += _write_size;
    }


#ifdef USE_BIT3
  rcs_print_error("write_with_bitwise_and not supported for BIT3.\n");
  return -1;
#endif

  /* If local_address has been initialized use it as an ordinary pointer. */
  if (NULL != internals->local_address)
    {
      char *to;
      to = ((char *) internals->local_address) + internals->offset;
      const unsigned char *ucto = (unsigned char *)to;
      const unsigned char *ucfrom = (unsigned char *)_from;      
      const unsigned char *end_to = ucto + _write_size;
      const unsigned char *end_from = ucfrom + _write_size;
      unsigned char *t = (unsigned char *) to;
      unsigned char *f = (unsigned char *) _from;
      while(t < end_to && f < end_from)
	{
	  *t = *t | *f;
	  t++;
	  f++;
	}
      return (0);
    }

  /* include platform specific ways of accessing phsical memory here. */
  rcs_print_error("write_with_and_op not supported for unless local address is used..\n");
  return -1;
}


void
PHYSMEM_HANDLE::memsetf (long _memset_offset, char _byte, long _memset_size)
{
  /* Check sizes. */
  if (_memset_size + _memset_offset > internals->size)
    {
      return;
    }

  /* If local_address has been initialized use it as an ordinary pointer. */
  if (NULL != internals->local_address)
    {
      char *temp_addr;
      temp_addr = ((char *) internals->local_address) + _memset_offset;
      memset (temp_addr, _byte, _memset_size);
      return;
    }
  else
    {
      /* Since local address is not initialized use temp_buf and write to
         access the physical memory in an platform specific way. */
      if (NULL == internals->temp_buf)
	{
	  internals->temp_buf = (char *) DEBUG_MALLOC (internals->size);
	}
      if (NULL != internals->temp_buf)
	{
	  if (_memset_size + _memset_offset <= internals->size)
	    {
	      memset (internals->temp_buf, _byte, _memset_size);
	      unsigned long old_offset;
	      old_offset = internals->offset;
	      internals->offset = _memset_offset;
	      write (internals->temp_buf, _memset_size);
	      internals->offset = old_offset;
	    }
	  else
	    {
	      memset (internals->temp_buf, _byte, internals->size - _memset_offset);
	      unsigned long old_offset;
	      old_offset = internals->offset;
	      internals->offset = _memset_offset;
	      write (internals->temp_buf, internals->size - internals->offset);
	      internals->offset = old_offset;
	    }
	}
    }
}



int
PHYSMEM_HANDLE::clear_memory ()
{
  /* If local_address has been initialized use it as an ordinary pointer. */
  if (NULL != internals->local_address)
    {
      memset (internals->local_address, 0, internals->size);
      return (0);
    }
  else
    {
      /* Since local address is not initialized use internals->temp_buf and write to
         access the physical memory in an platform specific way. */
      if (NULL == internals->temp_buf)
	{
	  internals->temp_buf = (char *) DEBUG_MALLOC (internals->size);
	}
      if (NULL == internals->temp_buf)
	{
	  return (-1);
	}
      memset (internals->temp_buf, 0, internals->size);
      unsigned long old_offset;
      old_offset = internals->offset;
      internals->offset = 0;
      if (-1 == write (internals->temp_buf, internals->size))
	{
	  internals->offset = old_offset;
	  return (-1);
	}
      internals->offset = old_offset;
    }
  return (0);
}

int
PHYSMEM_HANDLE::valid ()
{
  if(internals)
    {
      return internals->isvalid;
    }
  return(0);
}


int
PHYSMEM_HANDLE::get_error_count ()
{
  if(internals)
    {
      return internals->error_count;
    }
  return(-1);
}




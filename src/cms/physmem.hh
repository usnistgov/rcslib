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
* File: physmem.hh
* Purpose: Define the PHYSMEM_HANDLE class which creates objects that represent
*  a portable interface to sections of physical memory.
* NOTES:
*  On some platforms accessing a block of physical memory is a simple as
* intitializing a pointer and then dereferencing it, but accessing specific
* sections of extended memory under DOS or Windows requires changing the
* descriptor tables and therefore circumventing protected mode security.
* Other platforms require the use of mmap on memory device files to choose
* a section of memory mapped IO.
***********************************************************************/

#ifndef PHYSMEM_HH
#define PHYSMEM_HH

#ifdef  EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <stddef.h>		/* size_t */

#ifdef EXTERN_C_STD_HEADERS
}
#endif

class PHYSMEM_INTERNALS;

class PHYSMEM_HANDLE
{
public:
  PHYSMEM_HANDLE ();		/* Constructor for blank handle. */
#if defined(USE_BIT3) && defined(WIN32)
#error Bit3 no longer supported.
  PHYSMEM_HANDLE (void *_btd_ptr, unsigned long _btd_offset, long _size,
		  int _swap_mode);
#endif

  /* Constructor to access memory starting at _physical_address, for _size
     bytes. Some platform implementations distinguish between various types of
     addresses using the address code. */
  PHYSMEM_HANDLE (unsigned long _physical_address,
		  long _address_code, long _size);
  
  ~PHYSMEM_HANDLE ();	/* Destructor */
  
  int read (void *_to, long _read_size);	/* Read _read_size bytes and store */
  /* at _to */
  int write (void *_from, long _write_size);	/* Write _write_size bytes */
  int write_with_bitwise_and (void *_from, long _write_size);	/* Write _write_size bytes */
  int write_with_bitwise_or (void *_from, long _write_size);	/* Write _write_size bytes */
  /* using data at _from */

  void set_to_ptr (void *_ptr, long size);	/* Use the physical memory at _ptr. */

  void memsetf (long offset, char _byte, long _memset_size);
  int clear_memory (void);
  int valid (void);
  long get_size(void);
  void set_size(long l);
  long get_offset(void);
  void set_offset(long l);
  void increment_offset(long l);
  void decrement_offset(long l);
  long get_address_code(void);
  void set_address_code(long l);
  unsigned long get_physical_address(void);
  void set_physical_address(unsigned long ul);
  char *get_temp_buf(void);
  void set_temp_buf(char *x);
  int get_using_bit3(void);
  void set_using_bit3(int i);  
  double get_total_bytes_moved(void);
  void set_total_bytes_moved(double d);
  void increment_total_bytes_moved(double d);
  int get_enable_byte_counting(void);
  void set_enable_byte_counting(int i);
  void *get_local_address();
  void align(unsigned char uc);
  int get_error_count();

protected:
  volatile class PHYSMEM_INTERNALS *internals;
private:
  PHYSMEM_HANDLE(const PHYSMEM_HANDLE &);
  PHYSMEM_HANDLE &operator=(const PHYSMEM_HANDLE &);

};

#endif

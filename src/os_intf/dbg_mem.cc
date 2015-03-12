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


#if HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"

#else

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#endif

#include "dbg_mem.h"		// debug_malloc,debug_free
#include "rcs_prnt.hh"		// rcs_print
#include "linklist.hh"		// RCS_LINKED_LIST

RCS_LINKED_LIST *dbg_mem_list = NULL;

int log_debug_mem_list = 0;
int print_debug_mem_calls = 0;
int next_log_debug_id = 0;

struct DEBUG_MEM_ENTRY
{
public:
  void *address;
  const char *file;
  int line;
  size_t size;
  int id;
};



void
enable_debug_memory ()
{
  rcs_print_debug (PRINT_MISC, "enable_debug_print\n");
  log_debug_mem_list = 1;
  print_debug_mem_calls = 1;
  next_log_debug_id = 0;
}

void
disable_debug_memory ()
{
  rcs_print_debug (PRINT_MISC, "disable_debug_print\n");
  log_debug_mem_list = 0;
  print_debug_mem_calls = 0;
  next_log_debug_id = 0;
}


void
clear_dbg_mem_list ()
{
  rcs_print_debug (PRINT_MISC, "clear_dbg_mem_list\n");
  log_debug_mem_list = 0;
  next_log_debug_id = 0;
  if (NULL != dbg_mem_list)
    {
      delete dbg_mem_list;
      dbg_mem_list = NULL;
    }
}

void
print_dbg_mem_list ()
{
  rcs_print ("\n");
  if (NULL == dbg_mem_list)
    {
      rcs_print ("!DBGMEM! Debug memory list is NULL.\n");
      return;
    }
  rcs_print ("!DBGMEM! \tAddress \tsize \tid \tFile:line\n");
  DEBUG_MEM_ENTRY *dbg_mem_entry =
    (DEBUG_MEM_ENTRY *) dbg_mem_list->get_head ();
  while (NULL != dbg_mem_entry)
    {
      rcs_print ("!DBGMEM! \t%p \t%d(0x%X) \t%d \t%s:%d\n",
		 dbg_mem_entry->address, dbg_mem_entry->size,
		 dbg_mem_entry->size, dbg_mem_entry->id, dbg_mem_entry->file,
		 dbg_mem_entry->line);
      dbg_mem_entry = (DEBUG_MEM_ENTRY *) dbg_mem_list->get_next ();
    }
}

void
add_dbg_mem_list (const char *file, int line, void *address, size_t sz)
{
  int orig_log_debug_mem_list = log_debug_mem_list;
  int orig_print_debug_mem_calls = print_debug_mem_calls;
  DEBUG_MEM_ENTRY dbg_mem_entry;
  dbg_mem_entry.address = address;
  dbg_mem_entry.size = sz;
  dbg_mem_entry.file = file;
  dbg_mem_entry.line = line;
  dbg_mem_entry.id = next_log_debug_id++;
  log_debug_mem_list = 0;
  print_debug_mem_calls = 0;
  if (NULL == dbg_mem_list)
    {
      dbg_mem_list = new RCS_LINKED_LIST ();
    }
  dbg_mem_list->store_at_tail (&dbg_mem_entry, sizeof (DEBUG_MEM_ENTRY), 1);
  log_debug_mem_list = orig_log_debug_mem_list;
  print_debug_mem_calls = orig_print_debug_mem_calls;
}


int
delete_dbg_mem_list (void *address)
{
  int address_found = 1;
  int orig_log_debug_mem_list = log_debug_mem_list;
  int orig_print_debug_mem_calls = print_debug_mem_calls;
  if (NULL != dbg_mem_list)
    {
      address_found = 0;
      log_debug_mem_list = 0;
      print_debug_mem_calls = 0;
      DEBUG_MEM_ENTRY *dbg_mem_entry =
	(DEBUG_MEM_ENTRY *) dbg_mem_list->get_head ();
      while (NULL != dbg_mem_entry)
	{
	  if (dbg_mem_entry->address == address)
	    {
	      dbg_mem_list->delete_current_node ();
	      address_found = 1;
	      break;
	    }
	  dbg_mem_entry = (DEBUG_MEM_ENTRY *) dbg_mem_list->get_next ();
	}
      log_debug_mem_list = orig_log_debug_mem_list;
      print_debug_mem_calls = orig_print_debug_mem_calls;
    }
  return address_found;
}


void *
debug_malloc (const char *file, int line, size_t s)
{
  void *ret = 0;
  void *stored_addr = 0;
  if (log_debug_mem_list)
    {
      ret = malloc (s + 64);
      if (0 == ret)
	{
	  fprintf (stderr, "Out of memory\n");
	  exit (255);
	}
      memset (ret, 0xEE, s + 64);
      strcpy ((char *) ret, "RCSlib.debug_memory_string");
      *((size_t *) ((char *) ret + 28)) = s;
      char *endofmemarea = ((char *) ret) + 32 + s;
      strcpy (endofmemarea, "RCSlib.debug_memory_string");
      stored_addr = ret;
      add_dbg_mem_list (file, line, stored_addr, s);
      char *cret = ((char *) ret) + 32;
      ret = cret;
    }
  else
    {
      ret = malloc (s);
      if (0 == ret)
	{
	  fprintf (stderr, "Out of memory\n");
	  exit (255);
	}
      memset (ret, 0xEE, s);
    }
  if (print_debug_mem_calls)
    {
      rcs_print ("%s:%d %p  = malloc(%d) stored_addr=%p\n", file, line, ret,
		 s, stored_addr);
    }
  return ret;
}

void *
debug_calloc (const char *file, int line, size_t nelem, size_t elsize)
{
  void *ret = 0;
  void *stored_addr = 0;

  if (log_debug_mem_list)
    {
      ret = calloc (nelem + 64 / elsize, elsize);
      if (0 == ret)
	{
	  fprintf (stderr, "Out of memory\n");
	  exit (255);
	}
      strcpy ((char *) ret, "RCSlib.debug_memory_string");
      *((size_t *) ((char *) ret + 28)) = (elsize * nelem);
      char *endofmemarea = ((char *) ret) + 32 + (elsize * nelem);
      strcpy (endofmemarea, "RCSlib.debug_memory_string");
      stored_addr = ret;
      add_dbg_mem_list (file, line, stored_addr, nelem * elsize);
      char *cret = ((char *) ret) + 32;
      ret = cret;
    }
  else
    {
      ret = calloc (nelem, elsize);
    }
  if (print_debug_mem_calls)
    {
      rcs_print ("%s:%d %p  = calloc(%d,%d), stored_addr=%p\n", file, line,
		 ret, nelem, elsize, stored_addr);
    }
  return (void *) ret;
}

void *
debug_realloc (const char *file, int line, void *ptr, size_t s)
{
  void *orig_ptr = ((char *) ptr) - 32;
  int address_found = 0;
  void *stored_addr = 0;
  if (log_debug_mem_list && orig_ptr != 0)
    {
      address_found = delete_dbg_mem_list (orig_ptr);
      if (!address_found)
	{
	  rcs_print_error
	    ("debug_realloc(%s,%d,%p,%d) %p address not on debug mem list.\n",
	     file, line, ptr, s, orig_ptr);
	}
    }
  if (!address_found)
    {
      orig_ptr = ptr;
    }
  void *new_ptr = NULL;
  if (0 == ptr)
    {
      new_ptr = malloc (s + 64);
      if (0 == new_ptr)
	{
	  fprintf (stderr, "Out of memory\n");
	  exit (255);
	}
      memset (new_ptr, 0xEE, s + 64);
    }
  else
    {
      size_t s2 = *((size_t *) (((char *) orig_ptr) + 28));
      if (((int) orig_ptr) < 32 || orig_ptr == 0)
	{
	  rcs_print_error ("%s:%d Bad memory to free %p.\n",
			   file, line, orig_ptr);
	  new_ptr = malloc (s + 64);
	  memset (new_ptr, 0xEE, s + 64);
	}
      else
	{
	  if (address_found)
	    {
	      char *endofmemarea2 = ((char *) orig_ptr) + 32 + s2;
	      if (strcmp
		  ((const char *) orig_ptr, "RCSlib.debug_memory_string"))
		{
		  rcs_print_error
		    ("%s:%d debug_free detected possible  memory overwrite before area at %p\n",
		     file, line, orig_ptr);
		}
	      else if (strcmp (endofmemarea2, "RCSlib.debug_memory_string"))
		{
		  rcs_print_error
		    ("%s:%d debug_free detected possible  memory overwrite after area at %p\n",
		     file, line, orig_ptr);
		}
	    }
	  new_ptr = realloc (orig_ptr, s + 64);
	  if (0 == new_ptr)
	    {
	      fprintf (stderr, "Out of memory\n");
	      exit (255);
	    }
	}
    }
  if (log_debug_mem_list)
    {
      strcpy ((char *) new_ptr, "RCSlib.debug_memory_string");
      *((size_t *) ((char *) new_ptr + 28)) = s;
      char *endofmemarea = ((char *) new_ptr) + 32 + s;
      strcpy (endofmemarea, "RCSlib.debug_memory_string");
      stored_addr = new_ptr;
      add_dbg_mem_list (file, line, stored_addr, s);
      char *cret = ((char *) new_ptr) + 32;
      new_ptr = cret;
    }
  if (print_debug_mem_calls)
    {
      rcs_print ("%s:%d %p = realloc(%p,%d) stored_addr=%p\n", file, line,
		 new_ptr, orig_ptr, s, stored_addr);
    }
  return new_ptr;
}

void
debug_free (const char *file, int line, void *ptr)
{
  int address_found = 0;
  void *orig_ptr = ((char *) ptr) - 32;
  if (print_debug_mem_calls)
    {
      rcs_print ("%s:%d free(%p) orig_ptr=%p\n", file, line, ptr, orig_ptr);
    }
  if (log_debug_mem_list)
    {
      address_found = delete_dbg_mem_list (orig_ptr);
      if (!address_found)
	{
	  rcs_print_error
	    ("debug_free(%s,%d,%p) %p address not on debug mem list.\n", file,
	     line, ptr, orig_ptr);
	  return;
	}
    }
  if (!address_found)
    {
      orig_ptr = ptr;
    }
  else
    {
      size_t s = *((size_t *) (((char *) ptr) - 4));
      if (((int) ptr) < 32 || ptr == 0)
	{
	  rcs_print_error ("%s:%d Bad memory to free %p.\n", file, line, ptr);
	  return;
	}
      char *endofmemarea = ((char *) orig_ptr) + 32 + s;
      if (strcmp ((const char *) orig_ptr, "RCSlib.debug_memory_string"))
	{
	  rcs_print_error
	    ("%s:%d debug_free detected possible  memory overwrite before area at %p\n",
	     file, line, orig_ptr);
	}
      else if (strcmp (endofmemarea, "RCSlib.debug_memory_string"))
	{
	  rcs_print_error
	    ("%s:%d debug_free detected possible  memory overwrite after area at %p\n",
	     file, line, orig_ptr);
	}
      else
	{
	  memset (orig_ptr, 0xDD, s + 64);
	}
    }
  if (NULL != orig_ptr)
    {
      free (orig_ptr);
    }
}

#if 0


void *operator
new (size_t s)
{
  return DEBUG_MALLOC (s);
}

void operator
delete (void *ptr)
{
  DEBUG_FREE (ptr);
}

#endif

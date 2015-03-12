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

/*
File: autokey.c

The purpose of this file is to provide funtions to
 generate keys usable for cnum's
from unique strings and a previously allocated shared memory table.
*/

#if HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "rcs_defs.hh"
#endif

#include "autokey.h"
#include "_timer.h"
#ifndef NO_STDIO_SUPPORT
#include "rcs_prnt.hh"
#endif


#define AUTOKEY_WRITE_OK (1)

static int
autokey_entry_compare (struct AUTOKEY_TABLE_ENTRY *entry, const char *name,
		       unsigned int chksum)
{
  int i;
  if (0 == entry || 0 == name)
    {
      return -1;
    }
  if (chksum != entry->chksum)
    {
      return (chksum > entry->chksum ? 1 : -1);
    }
  for (i = 0; i < AUTOKEY_NAMELENGTH; i++)
    {
      if (name[i] != entry->name[i])
	{
	  return (name[i] > entry->name[i] ? 1 : -1);
	}
      if (0 == name[i])
	{
	  break;
	}
    }
  return (0);
}

static int
autokey_entry_write (struct AUTOKEY_TABLE_ENTRY *entry, const char *name,
		     int chksum)
{
  int i;
  int name_end_hit = 0;
  if (0 == entry || 0 == name)
    {
      return -1;
    }
  for (i = 0; i < AUTOKEY_NAMELENGTH; i++)
    {
      if (!name_end_hit)
	{
	  entry->name[i] = name[i];
	}
      else
	{
	  entry->name[i] = 0;
	}
      if (0 == name[i])
	{
	  name_end_hit = 1;
	}
    }
  entry->chksum = chksum;
  return 0;
}

static int
autokey_entry_check (struct AUTOKEY_TABLE_ENTRY *entry)
{
  unsigned int chksum = 0;
  int i;
  if (0 == entry)
    {
      return 0;
    }
  if (!entry->name[0])
    {
      return 0;
    }
  for (i = 0; i < AUTOKEY_NAMELENGTH && entry->name[i] != 0; i++)
    {
      chksum += (unsigned int) entry->name[i];
    }
  return (chksum == entry->chksum);
}

int
autokey_getkey (void *table, int max, const char *name)
{
  int ktable_index;
  struct AUTOKEY_TABLE_ENTRY *ktable = (struct AUTOKEY_TABLE_ENTRY *) table;
  unsigned int chksum = 0;
  int i;
  int tries;
  int table_full;
  if (0 == table || max < 1 || 0 == name)
    {
#ifndef NO_STDIO_SUPPORT
      rcs_print_error ("Bad parameters to autokey_getkey(%p,%d,%p)\n",
		       table, max, name);
#endif
      return -1;
    }

  if (name[0] == 0)
    {
#ifndef NO_STDIO_SUPPORT
      rcs_print_error ("Bad name for autokey_getkey()\n");
#endif
      return -1;
    }

  for (i = 0; i < AUTOKEY_NAMELENGTH && name[i] != 0; i++)
    {
      chksum += (unsigned int) name[i];
    }


  /* Check to see if this name is already in the table if so return its entry 
     number. */
  for (ktable_index = 0; ktable_index < max; ktable_index++)
    {
      if (!autokey_entry_compare (&(ktable[ktable_index]), name, (int) chksum))
	{
	  rcs_print ("autkey found  %d for %s already in table\n", 
		     ktable_index,
		     name);
	  return ktable_index;
	}
    }

  tries = 0;

  do
    {
      /* Search the table for and empty or invalid entry and try to claim it. */
      // bug reported by xshr_001@163.com on  Aug, 12 2005 fixed here. 
      tries++;

      table_full = 1;
      for (ktable_index = 0; ktable_index < max; ktable_index++)
	{
	  if (!autokey_entry_check (&(ktable[ktable_index])))
	    {
	      /* Empty or invalid table entry let's claim it. */
	      autokey_entry_write (&(ktable[ktable_index]), name, chksum);
	      table_full = 0;
	      break;
	    }
	}
      if (table_full)
	{
	  rcs_print_error
	    ("autokey table is full no room for %s in table of size %d\n",
	     name, max);
	  return -1;
	}

      /* Since we have no mutual exclusion we need to sleep for 10 milliseconds
         and check to see that it is still there. */
      esleep (0.01);

      if (!autokey_entry_compare (&(ktable[ktable_index]), name, chksum))
	{
	  rcs_print ("autkey adding %d for %s\n", ktable_index, name);
	  return ktable_index;
	}
    }
  while (tries < 100);
#ifndef NO_STDIO_SUPPORT
  rcs_print_error ("autokey_getkey: timed out\n");
#endif
  return -1;
}


int
autokey_releasekey (void *table, int max, const char *name, int key)
{
  int chksum = 0;
  int i;
  struct AUTOKEY_TABLE_ENTRY *ktable = (struct AUTOKEY_TABLE_ENTRY *) table;

  if (0 == table || max < 1 || 0 == name || key >= max)
    {
#ifndef NO_STDIO_SUPPORT
      rcs_print_error ("Bad parameters to autokey_releasekey(%p,%d,%p,%d)\n",
		       table, max, name, key);
#endif
      return -1;
    }

  if (name[0] == 0)
    {
#ifndef NO_STDIO_SUPPORT
      rcs_print_error ("Bad name for autokey_releasekey()\n");
#endif
      return -1;
    }

  chksum = 0;

  for (i = 0; i < AUTOKEY_NAMELENGTH && name[i] != 0; i++)
    {
      chksum += (unsigned int) name[i];
    }
  if (autokey_entry_compare (&(ktable[key]), name, (int) chksum))
    {
#ifndef NO_STDIO_SUPPORT
      rcs_print_error
	("autokey_releasekey entry in table for %s no longer matches key so it will not be released.\n",
	 name);
#endif
      return (-1);
    }

  for (i = 0; i < AUTOKEY_NAMELENGTH && name[i] != 0; i++)
    {
      ktable[key].name[i] = 0;
    }
  ktable[key].chksum = 0;
  return 0;
}

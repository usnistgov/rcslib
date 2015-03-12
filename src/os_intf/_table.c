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

#include "rcs_defs.hh"

#ifndef NO_STDIO
#include <stdio.h>		/* NULL */
#endif

#include <stddef.h>		/* sizeof(), size_t */
#include <stdlib.h>		/* malloc() */
#include <string.h>		/* memcpy() */
#include "_table.h"
#include "dbg_mem.h"		/* DEBUG_FREE,DEBUG_MALLOC,DEBUG_CALLOC */
#include "rcs_prnt.hh"

int
table_new (_RCS_TABLE * table, size_t dsize)
{
  int div, rem;

  table->table = NULL;
  table->dsize = dsize;
  /* align data to multiple of sizeof(unsigned long int) */
  div = dsize / sizeof (unsigned long int);
  rem = dsize % sizeof (unsigned long int);
  if (rem)
    {
      table->lsize = div + 1;
    }
  else
    {
      table->lsize = div;
    }
  table->size = 0;
  table->count = 0;
  return 0;
}

int
table_delete (_RCS_TABLE * table)
{
  if (table == NULL)
    {
      return -1;
    }

  if (table->table == NULL)
    {
      return -1;
    }
  else
    {
      DEBUG_FREE (table->table);
      return 0;
    }
}

static unsigned long int *
table_nth (_RCS_TABLE * table, unsigned long int nth)
{
  if (NULL == table)
    {
      return 0;
    }
  if (NULL == table->table)
    {
      return 0;
    }
  return &table->table[nth * (1 + table->lsize)];
}

int
table_add (_RCS_TABLE * table, unsigned long int key, const void *data)
{
  int t;
  _RCS_TABLE old = *table;	/* structure copy */
  unsigned long int *ptr;
  unsigned long int *oldptr;

  /* check for non-zero key */
  if (0 == key)
    {
      return -1;
    }

  if (table == NULL)
    {
      return -1;
    }

  /* if uninitialized, create a table of one and mark the slot free */
  if (table->table == NULL)
    {
      table->size = 1;
      table->table = (unsigned long *) DEBUG_CALLOC (table->size,
						     (1 +
						      table->lsize) *
						     sizeof (unsigned long
							     int));
      table->table[0] = 0;
    }

  /* add entry at first 0 slot or overwrite a matched key */
  for (t = 0; t < table->size; t++)
    {
      ptr = table_nth (table, t);
      if (ptr[0] == 0)
	{
	  ptr[0] = key;
	  memcpy (&ptr[1], data, table->dsize);
	  return 0;
	}
      if (ptr[0] == key)
	{
	  memcpy (&ptr[1], data, table->dsize);
	  table->count = 1;
	  return 0;
	}
    }

  /* if we got here, table must be full-- grow the table,
     and add the entry */
  table->size *= 2;
  table->table = (unsigned long *) DEBUG_CALLOC (table->size,
						 (1 +
						  table->lsize) *
						 sizeof (unsigned long int));
  for (t = 0; t < table->size / 2; t++)
    {
      ptr = table_nth (table, t);
      oldptr = table_nth (&old, t);
      ptr[0] = oldptr[0];
      memcpy (&ptr[1], &oldptr[1], table->dsize);
    }
  if (NULL != old.table)
    {
      DEBUG_FREE (old.table);
    }
  /* add the entry at the first new slot */
  ptr = table_nth (table, table->size / 2);
  ptr[0] = key;
  memcpy (&ptr[1], data, table->dsize);
  /* and initialize the rest */
  for (t = table->size / 2 + 1; t < table->size; t++)
    {
      ptr = table_nth (table, t);
      ptr[0] = 0;
    }
  table->count++;
  return 0;
}

int
table_get (_RCS_TABLE * table, unsigned long int key, void *data)
{
  int t;
  unsigned long int *ptr;

  if (key == 0)
    return -1;

  if (table == NULL)
    {
      return -1;
    }

  for (t = 0; t < table->size; t++)
    {
      ptr = table_nth (table, t);
      if (ptr[0] == key)
	{
	  memcpy (data, &ptr[1], table->dsize);
	  return 0;
	}
    }
  /* got to end with no match-- return invalid flag */
  return -1;
}

int
table_clear (_RCS_TABLE * table, unsigned long int key)
{
  int t;
  unsigned long int *ptr;

  if (key == 0)
    return -1;

  if (table == NULL)
    {
      return -1;
    }

  for (t = 0; t < table->size; t++)
    {
      ptr = table_nth (table, t);
      if (ptr[0] == key)
	{
	  ptr[0] = 0;
	  table->count--;
	  return 0;
	}
    }
  /* got to end with no match-- return invalid flag */
  return -1;
}

int
table_clearall (_RCS_TABLE * table)
{
  int t;
  unsigned long int *ptr;


  if (table == NULL)
    {
      return -1;
    }

  for (t = 0; t < table->size; t++)
    {
      ptr = table_nth (table, t);
      ptr[0] = 0;
    }
  table->size = 0;
  table->count = 0;
  table->dsize = 0;		/* acutal size of each data, in bytes */
  table->lsize = 0;		/* upped size of each data, in longs */
  if (NULL != table->table)
    {
      free (table->table);
      table->table = NULL;	/* ptr to first entry */
    }

  return 0;
}


void
table_print (_RCS_TABLE * table)
{
  int t;
  unsigned long int *ptr;
  unsigned char *cptr;
  int i;

  rcs_print ("*** TABLE PRINT ***\n");
  if (table == NULL)
    {
      return;
    }

  rcs_print ("size = %ld /* # of slots in table */\n", table->size);
  rcs_print ("dsize = %d /* actal size of each data, in bytes */\n",
	     table->dsize);
  rcs_print ("lsize = %d /* upped size of each data, in longs */\n",
	     table->lsize);
  rcs_print ("count = %ld /* # of full slots */\n", table->count);
  for (t = 0; t < table->size; t++)
    {
      ptr = table_nth (table, t);
      if (ptr == 0)
	{
	  continue;
	}
      rcs_print ("%lu : ", ptr[0]);
      cptr = (char *) ptr;
      for (i = 0; i < 32 && i < table->dsize + 4; i++)
	{
	  if (0 == i % 4)
	    {
	      rcs_print (" ");
	    }
	  rcs_print ("%2.2X", cptr[i]);
	}
      rcs_print ("\n");
    }
  fflush (stdout);
}






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


#ifndef TABLE_HH
#define TABLE_HH

#ifdef USE_TEMPLATES

#include "rcs_defs.hh"

extern "C"
{
#ifndef NO_STDIO
#include <stdio.h>		/* NULL */
#endif

}

template < class T > class RCS_TABLE
{
public:
  RCS_TABLE ()
  {
    size = 0;
    table = NULL;
  };
  ~RCS_TABLE ()
  {
    if (table != NULL)
      delete table;
  };
  int add (unsigned long int key, const T & data);
  int get (unsigned long int key, T & data);
  int clear (unsigned long int key);
  void print (void);
private:
  unsigned long int size;
  struct RCS_TABLE_ENTRY
  {
    unsigned long int key;
    T data;
  }
   *table;
};

/* NOTE:  actual code for template member function definitions must
   be included in the header file, since this code does not generate
   any executable when compiled-- it needs to be templated before this
   can occur. */

template < class T > int RCS_TABLE < T >::add (unsigned long int key,
					       const T & data)
{
  int t;
  RCS_TABLE_ENTRY *old;

  /* if uninitialized, create a table of one, and make an entry */
  if (table == NULL)
    {
      size = 1;
      table = new RCS_TABLE_ENTRY;
      table[0].key = key;
      table[0].data = data;
      return 0;
    }

  /* if we got here, table must have been initialized--
     add entry at first 0 slot or overwrite a matched key */
  for (t = 0; t < size; t++)
    {
      if (table[t].key == 0 || table[t].key == key)
	{
	  table[t].key = key;
	  table[t].data = data;
	  return 0;
	}
    }

  /* if we got here, table must be full-- grow the table,
     and add the entry */
  /* NOTE:  I tried realloc() on table, but it appeared that
     the data was corrupted in some places (the third entry, for
     example).  realloc() may screw with the alignment, so I
     took the safe if less efficient route of copying from the
     old table to the new table, and deleting the old. */
  old = table;
  table = new RCS_TABLE_ENTRY[size * 2];
  for (t = 0; t < size; t++)
    {
      table[t].key = old[t].key;
      table[t].data = old[t].data;
    }
  delete old;
  /* add the entry at the first new slot */
  table[size].key = key;
  table[size].data = data;
  /* and initialize the rest */
  for (t = size + 1; t < size * 2; t++)
    {
      table[t].key = 0;
    }
  size *= 2;
  return 0;
}

template < class T > int RCS_TABLE < T >::get (unsigned long int key,
					       T & data)
{
  int t;

  for (t = 0; t < size; t++)
    {
      if (table[t].key == key)
	{
	  data = table[t].data;
	  return 0;
	}
    }
  /* got to end with no match-- return invalid flag */
  return -1;
}

template < class T > int RCS_TABLE < T >::clear (unsigned long int key)
{
  int t;

  for (t = 0; t < size; t++)
    {
      if (table[t].key == key)
	{
	  table[t].key = 0;
	  return 0;
	}
    }
  /* got to end with no match-- return invalid flag */
  return -1;
}

template < class T > void RCS_TABLE < T >::print ()
{
  int t;

  for (t = 0; t < size; t++)
    {
      printf ("%lu\t%X\n", table[t].key, &table[t].data);
    }
}

#else /* no TEMPLATES */

extern "C"
{
#include "_table.h"
}

class RCS_TABLE
{
public:
  RCS_TABLE (size_t dsize);
  ~RCS_TABLE ();
  int add (unsigned long int key, const void *data);
  int get (unsigned long int key, void *data);
  int clear (unsigned long int key);
  void print (void);
private:
    _RCS_TABLE table;
};

#endif /* TEMPLATES  */

#endif /* TABLE_HH */

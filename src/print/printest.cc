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


/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1


#include "rcs_prnt.hh"

#include <stdio.h>		/* printf() */
#include <stdlib.h>		/* exit() */

main ()
{
  int i;
  RCS_LINKED_LIST *print_list;
  char **lines_table;

  set_rcs_print_destination (RCS_PRINT_TO_LIST);

  rcs_print ("abcdefghi");
  rcs_print ("jklmnop");
  rcs_print ("qrstuvwxyz\n");
  rcs_print ("01234565789\nABCDEFGHIJK");
  rcs_print ("LMNOPQRSTUVWXYZ");
  rcs_print ("\nNational\nInstitute\nof\nStandards\nand\nTechnology\n");
  print_list = get_rcs_print_list ();

  if (NULL == print_list)
    {
      printf ("Memory error.\n");
      exit (-1);
    }
  char *string;
  string = (char *) print_list->get_head ();
  i = 0;
  while (NULL != string)
    {
      printf ("line = %d, string = %s\n", i, string);
      string = (char *) print_list->get_next ();
      i++;
    }


  update_lines_table ();

  lines_table = get_rcs_lines_table ();
  if ((NULL == print_list) || (NULL == lines_table))
    {
      printf ("Memory error.\n");
      exit (-1);
    }
  for (i = 0; i < print_list->list_size; i++)
    {
      printf ("line = %d, string = %s\n", i,
	      strip_control_characters (NULL, lines_table[i]));
    }


  rcs_print ("1234");
  rcs_print ("567890\n");
  rcs_print ("\nDepartment\nof\nCommerce\n");
  rcs_print ("The End");

  if (NULL == print_list)
    {
      printf ("Memory error.\n");
      exit (-1);
    }
  string = (char *) print_list->get_head ();
  i = 0;
  while (NULL != string)
    {
      printf ("line = %d, string = %s\n", i, string);
      string = (char *) print_list->get_next ();
      i++;
    }


  update_lines_table ();
  lines_table = get_rcs_lines_table ();

  if ((NULL == print_list) || (NULL == lines_table))
    {
      printf ("Memory error.\n");
      exit (-1);
    }
  for (i = 0; i < print_list->list_size; i++)
    {
      printf ("line = %d, string = %s\n", i,
	      strip_control_characters (NULL, lines_table[i]));
    }

  for (i = 0; i < 1000; i++)
    {
      rcs_print ("node = %d\n", i);
    }


  printf ("%d\n", print_list->list_size);

  update_lines_table ();
  lines_table = get_rcs_lines_table ();

  for (i = 0; i < print_list->list_size; i++)
    {
      printf ("line = %d, string = %s\n", i,
	      strip_control_characters (NULL, lines_table[i]));
    }

}

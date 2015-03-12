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
#include "rcs_defs.hh"
#include <stdlib.h>
#include <string.h>
#endif
// HAVE_CONFIG_H

#include "rcs_prnt.hh"

/* In windows DLLs for Microsoft Visual C++ sscanf is not supported so
use separate_words to parse the string followed by commands like strtod to
convert each word. */
int
separate_words_with_buffer (char ** _dest, int _max, const char * _src,
			    char *word_buffer, size_t buflen)
{
  int i;
  if (0 == _dest || 0 == _src || 0 == word_buffer || buflen < 1)
    {
      rcs_print_error("separate_words_with_buffer bad argument.\n");
      return -1;
    }
  for(i = 0; i < _max ; i++)
    {
      _dest[i]=0;
    }
  if (strlen (_src) > buflen)
    {
      rcs_print_error("separate_words string too long (%s)\n",_src);
      return -1;
    }
  int word_count=0;
  char *cP = word_buffer;
  const char  * const end_buf = word_buffer+buflen;
  bool last_in_word=false;
  strncpy(word_buffer,_src,buflen);
  // printf("cP=%p, word_buffer=%p, end_buf=%p, cP < end_buf=%d, *cP=%c, word_count=%d, _max=%d\n",
  // 	 cP	 ,word_buffer,end_buf, (cP < end_buf),*cP,word_count,_max);
  while(cP < end_buf && *cP && word_count < _max)
    {
      bool in_word = (*cP != ' ') && (*cP != '\t') && (*cP != '\r') && (*cP != '\n');
      // printf("*cP=%c, word_count=%d, in_word=%d\n",
      // 	     *cP,word_count,in_word);
      if(in_word && !last_in_word)
	{
	  _dest[word_count] = cP;
	  word_count++;
	}
      if(!in_word)
	{
	  *cP=0;
	}
      last_in_word=in_word;
      cP++;
    }
  
  while(cP < end_buf) 
    {
      if(*cP ==' ' || *cP == '\t' || *cP =='\r' || *cP =='\n' || *cP=='\n' )
	{
	  *cP=0;
	  break;
	}
      cP++;
    }
  //  printf("_src=%s\n",_src);
  //    printf("word_count=%d\n",word_count);
  //    for(int j=0; j < word_count; j++)
  //      {
  //        printf("word[%d]=%s\n",j,_dest[j]);
  //      }
   if(word_count < _max)
     {
       return word_count+1;
     }
   else
     {
       return _max;
     }
}

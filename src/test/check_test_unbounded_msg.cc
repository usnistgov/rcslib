

#include "nml_test_unbounded_format.hh"
#include "check_test_unbounded_msg.hh"

#include <stdio.h>

static bool
check_ntu_s1(ntu_s1 *ntu_s1P)
{
  if(ntu_s1P->ntu_s1_char_ua)
    {
      if(ntu_s1P->ntu_s1_char_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_char_ua_size_allocated(%d) <  0\n",
		  ntu_s1P->ntu_s1_char_ua_size_allocated);
	  return false;
	}
      if(ntu_s1P->ntu_s1_char_ua_length <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_char_ua_length(%d) <  0\n",
		  ntu_s1P->ntu_s1_char_ua_length);
	  return false;
	}
      if(ntu_s1P->ntu_s1_char_ua_size_allocated <  ntu_s1P->ntu_s1_char_ua_length)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_char_ua_size_allocated(%d) <  ntu_s1P->ntu_s1_char_ua_length(%d)\n",
		  ntu_s1P->ntu_s1_char_ua_size_allocated,ntu_s1P->ntu_s1_char_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_s1P->ntu_s1_char_ua_length-1; i++)
	{
	  if(ntu_s1P->ntu_s1_char_ua[i] != (char) ('0'+(i%10)))
	    {
	      fprintf(stderr, "check_ntu_s1 :  ntu_s1P->ntu_s1_char_ua[%d](%d) != %d\n",
		      i,ntu_s1P->ntu_s1_char_ua[i], ((char) ('0'+(i%10))));
	      return false;
	    }
	}
    }

  if(ntu_s1P->ntu_s1_u_char_ua)
    {
      if(ntu_s1P->ntu_s1_u_char_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_u_char_ua_size_allocated(%d) <  0\n",
		  ntu_s1P->ntu_s1_u_char_ua_size_allocated);
	  return false;
	}
      if(ntu_s1P->ntu_s1_u_char_ua_length <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_u_char_ua_length(%d) <  0\n",
		  ntu_s1P->ntu_s1_u_char_ua_length);
	  return false;
	}
      if(ntu_s1P->ntu_s1_u_char_ua_size_allocated <  ntu_s1P->ntu_s1_u_char_ua_length)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_u_char_ua_size_allocated(%d) <  ntu_s1P->ntu_s1_u_char_ua_length(%d)\n",
		  ntu_s1P->ntu_s1_u_char_ua_size_allocated,ntu_s1P->ntu_s1_u_char_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_s1P->ntu_s1_u_char_ua_length; i++)
	{
	  if(ntu_s1P->ntu_s1_u_char_ua[i] != (unsigned char) ('0'+(i%10)))
	    {
	      fprintf(stderr, "check_ntu_s1 :  ntu_s1P->ntu_s1_u_char_ua[%d](%d) != %d\n",
		      i,ntu_s1P->ntu_s1_u_char_ua[i], ((unsigned char) ('0'+(i%10))));
	      return false;
	    }
	}
    }

  if(ntu_s1P->ntu_s1_short_ua)
    {
      if(ntu_s1P->ntu_s1_short_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_short_ua_size_allocated(%d) <  0\n",
		  ntu_s1P->ntu_s1_short_ua_size_allocated);
	  return false;
	}
      if(ntu_s1P->ntu_s1_short_ua_length <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_short_ua_length(%d) <  0\n",
		  ntu_s1P->ntu_s1_short_ua_length);
	  return false;
	}
      if(ntu_s1P->ntu_s1_short_ua_size_allocated <  ntu_s1P->ntu_s1_short_ua_length)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_short_ua_size_allocated(%d) <  ntu_s1P->ntu_s1_short_ua_length(%d)\n",
		  ntu_s1P->ntu_s1_short_ua_size_allocated,ntu_s1P->ntu_s1_short_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_s1P->ntu_s1_short_ua_length; i++)
	{
	  if(ntu_s1P->ntu_s1_short_ua[i] != (short) i)
	    {
	      fprintf(stderr, "check_ntu_s1 :  ntu_s1P->ntu_s1_short_ua[%d](%d) != %d\n",
		      i,ntu_s1P->ntu_s1_short_ua[i], ((short) i));
	      return false;
	    }
	}
    }

  if(ntu_s1P->ntu_s1_u_short_ua)
    {
      if(ntu_s1P->ntu_s1_u_short_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_u_short_ua_size_allocated(%d) <  0\n",
		  ntu_s1P->ntu_s1_u_short_ua_size_allocated);
	  return false;
	}
      if(ntu_s1P->ntu_s1_u_short_ua_length <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_u_short_ua_length(%d) <  0\n",
		  ntu_s1P->ntu_s1_u_short_ua_length);
	  return false;
	}
      if(ntu_s1P->ntu_s1_u_short_ua_size_allocated <  ntu_s1P->ntu_s1_u_short_ua_length)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_u_short_ua_size_allocated(%d) <  ntu_s1P->ntu_s1_u_short_ua_length(%d)\n",
		  ntu_s1P->ntu_s1_u_short_ua_size_allocated,ntu_s1P->ntu_s1_u_short_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_s1P->ntu_s1_u_short_ua_length; i++)
	{
	  if(ntu_s1P->ntu_s1_u_short_ua[i] != (unsigned short) i)
	    {
	      fprintf(stderr, "check_ntu_s1 :  ntu_s1P->ntu_s1_u_short_ua[%d](%d) != %d\n",
		      i,ntu_s1P->ntu_s1_u_short_ua[i], ((unsigned short) i));
	      return false;
	    }
	}
    }


  if(ntu_s1P->ntu_s1_int_ua)
    {
      if(ntu_s1P->ntu_s1_int_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_int_ua_size_allocated(%d) <  0\n",
		  ntu_s1P->ntu_s1_int_ua_size_allocated);
	  return false;
	}
      if(ntu_s1P->ntu_s1_int_ua_length <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_int_ua_length(%d) <  0\n",
		  ntu_s1P->ntu_s1_int_ua_length);
	  return false;
	}
      if(ntu_s1P->ntu_s1_int_ua_size_allocated <  ntu_s1P->ntu_s1_int_ua_length)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_int_ua_size_allocated(%d) <  ntu_s1P->ntu_s1_int_ua_length(%d)\n",
		  ntu_s1P->ntu_s1_int_ua_size_allocated,ntu_s1P->ntu_s1_int_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_s1P->ntu_s1_int_ua_length; i++)
	{
	  if(ntu_s1P->ntu_s1_int_ua[i] != (int) i)
	    {
	      fprintf(stderr, "check_ntu_s1 :  ntu_s1P->ntu_s1_int_ua[%d](%d) != %d\n",
		      i,ntu_s1P->ntu_s1_int_ua[i], ((int) i));
	      return false;
	    }
	}
    }

  if(ntu_s1P->ntu_s1_u_int_ua)
    {
      if(ntu_s1P->ntu_s1_u_int_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_u_int_ua_size_allocated(%d) <  0\n",
		  ntu_s1P->ntu_s1_u_int_ua_size_allocated);
	  return false;
	}
      if(ntu_s1P->ntu_s1_u_int_ua_length <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_u_int_ua_length(%d) <  0\n",
		  ntu_s1P->ntu_s1_u_int_ua_length);
	  return false;
	}
      if(ntu_s1P->ntu_s1_u_int_ua_size_allocated <  ntu_s1P->ntu_s1_u_int_ua_length)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_u_int_ua_size_allocated(%d) <  ntu_s1P->ntu_s1_u_int_ua_length(%d)\n",
		  ntu_s1P->ntu_s1_u_int_ua_size_allocated,ntu_s1P->ntu_s1_u_int_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_s1P->ntu_s1_u_int_ua_length; i++)
	{
	  if(ntu_s1P->ntu_s1_u_int_ua[i] != (unsigned int) i)
	    {
	      fprintf(stderr, "check_ntu_s1 :  ntu_s1P->ntu_s1_u_int_ua[%d](%d) != %d\n",
		      i,ntu_s1P->ntu_s1_u_int_ua[i], ((unsigned int) i));
	      return false;
	    }
	}
    }

  if(ntu_s1P->ntu_s1_long_ua)
    {
      if(ntu_s1P->ntu_s1_long_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_long_ua_size_allocated(%d) <  0\n",
		  ntu_s1P->ntu_s1_long_ua_size_allocated);
	  return false;
	}
      if(ntu_s1P->ntu_s1_long_ua_length <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_long_ua_length(%d) <  0\n",
		  ntu_s1P->ntu_s1_long_ua_length);
	  return false;
	}
      if(ntu_s1P->ntu_s1_long_ua_size_allocated <  ntu_s1P->ntu_s1_long_ua_length)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_long_ua_size_allocated(%d) <  ntu_s1P->ntu_s1_long_ua_length(%d)\n",
		  ntu_s1P->ntu_s1_long_ua_size_allocated,ntu_s1P->ntu_s1_long_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_s1P->ntu_s1_long_ua_length; i++)
	{
	  if(ntu_s1P->ntu_s1_long_ua[i] != (long) i)
	    {
	      fprintf(stderr, "check_ntu_s1 :  ntu_s1P->ntu_s1_long_ua[%d](%ld) != %ld\n",
		      i,ntu_s1P->ntu_s1_long_ua[i], ((long) i));
	      return false;
	    }
	}
    }

  if(ntu_s1P->ntu_s1_u_long_ua)
    {
      if(ntu_s1P->ntu_s1_u_long_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_u_long_ua_size_allocated(%d) <  0\n",
		  ntu_s1P->ntu_s1_u_long_ua_size_allocated);
	  return false;
	}
      if(ntu_s1P->ntu_s1_u_long_ua_length <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_u_long_ua_length(%d) <  0\n",
		  ntu_s1P->ntu_s1_u_long_ua_length);
	  return false;
	}
      if(ntu_s1P->ntu_s1_u_long_ua_size_allocated <  ntu_s1P->ntu_s1_u_long_ua_length)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_u_long_ua_size_allocated(%d) <  ntu_s1P->ntu_s1_u_long_ua_length(%d)\n",
		  ntu_s1P->ntu_s1_u_long_ua_size_allocated,ntu_s1P->ntu_s1_u_long_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_s1P->ntu_s1_u_long_ua_length; i++)
	{
	  if(ntu_s1P->ntu_s1_u_long_ua[i] != (unsigned long) i)
	    {
	      fprintf(stderr, "check_ntu_s1 :  ntu_s1P->ntu_s1_u_long_ua[%d](%ld) != %ld\n",
		      i,ntu_s1P->ntu_s1_u_long_ua[i], ((unsigned long) i));
	      return false;
	    }
	}
    }


  if(ntu_s1P->ntu_s1_float_ua)
    {
      if(ntu_s1P->ntu_s1_float_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_float_ua_size_allocated(%d) <  0\n",
		  ntu_s1P->ntu_s1_float_ua_size_allocated);
	  return false;
	}
      if(ntu_s1P->ntu_s1_float_ua_length <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_float_ua_length(%d) <  0\n",
		  ntu_s1P->ntu_s1_float_ua_length);
	  return false;
	}
      if(ntu_s1P->ntu_s1_float_ua_size_allocated <  ntu_s1P->ntu_s1_float_ua_length)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_float_ua_size_allocated(%d) <  ntu_s1P->ntu_s1_float_ua_length(%d)\n",
		  ntu_s1P->ntu_s1_float_ua_size_allocated,ntu_s1P->ntu_s1_float_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_s1P->ntu_s1_float_ua_length; i++)
	{
	  if(ntu_s1P->ntu_s1_float_ua[i] != (float) i)
	    {
	      fprintf(stderr, "check_ntu_s1 :  ntu_s1P->ntu_s1_float_ua[%d](%f) != %f\n",
		      i,ntu_s1P->ntu_s1_float_ua[i], ((float) i));
	      return false;
	    }
	}
    }


  if(ntu_s1P->ntu_s1_double_ua)
    {
      if(ntu_s1P->ntu_s1_double_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_double_ua_size_allocated(%d) <  0\n",
		  ntu_s1P->ntu_s1_double_ua_size_allocated);
	  return false;
	}
      if(ntu_s1P->ntu_s1_double_ua_length <  0)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_double_ua_length(%d) <  0\n",
		  ntu_s1P->ntu_s1_double_ua_length);
	  return false;
	}
      if(ntu_s1P->ntu_s1_double_ua_size_allocated <  ntu_s1P->ntu_s1_double_ua_length)
	{
	  fprintf(stderr,"check_ntu_s1 : ntu_s1P->ntu_s1_double_ua_size_allocated(%d) <  ntu_s1P->ntu_s1_double_ua_length(%d)\n",
		  ntu_s1P->ntu_s1_double_ua_size_allocated,ntu_s1P->ntu_s1_double_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_s1P->ntu_s1_double_ua_length; i++)
	{
	  if(ntu_s1P->ntu_s1_double_ua[i] != (double) i)
	    {
	      fprintf(stderr, "check_ntu_s1 :  ntu_s1P->ntu_s1_double_ua[%d](%f) != %f\n",
		      i,ntu_s1P->ntu_s1_double_ua[i], ((double) i));
	      return false;
	    }
	}
    }
  return true;
}


bool
check_test_unbounded_msg(NML_TEST_UNBOUNDED_MSG *ntu_test_msgP)
{
  if(ntu_test_msgP->char_ua)
    {
      if(ntu_test_msgP->char_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->char_ua_size_allocated(%d) <  0\n",
		  ntu_test_msgP->char_ua_size_allocated);
	  return false;
	}
      if(ntu_test_msgP->char_ua_length <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->char_ua_length(%d) <  0\n",
		  ntu_test_msgP->char_ua_length);
	  return false;
	}
      if(ntu_test_msgP->char_ua_size_allocated <  ntu_test_msgP->char_ua_length)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->char_ua_size_allocated(%d) <  ntu_test_msgP->char_ua_length(%d)\n",
		  ntu_test_msgP->char_ua_size_allocated,ntu_test_msgP->char_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_test_msgP->char_ua_length-1; i++)
	{
	  if(ntu_test_msgP->char_ua[i] != (char) ('0'+(i%10)))
	    {
	      fprintf(stderr, "check_test_unbounded_msg :  ntu_test_msgP->char_ua[%d](%d) != %d\n",
		      i,ntu_test_msgP->char_ua[i], ((char) ('0'+(i%10))));
	      return false;
	    }
	}
    }

  if(ntu_test_msgP->u_char_ua)
    {
      if(ntu_test_msgP->u_char_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->u_char_ua_size_allocated(%d) <  0\n",
		  ntu_test_msgP->u_char_ua_size_allocated);
	  return false;
	}
      if(ntu_test_msgP->u_char_ua_length <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->u_char_ua_length(%d) <  0\n",
		  ntu_test_msgP->u_char_ua_length);
	  return false;
	}
      if(ntu_test_msgP->u_char_ua_size_allocated <  ntu_test_msgP->u_char_ua_length)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->u_char_ua_size_allocated(%d) <  ntu_test_msgP->u_char_ua_length(%d)\n",
		  ntu_test_msgP->u_char_ua_size_allocated,ntu_test_msgP->u_char_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_test_msgP->u_char_ua_length; i++)
	{
	  if(ntu_test_msgP->u_char_ua[i] != (unsigned char) ('0'+(i%10)))
	    {
	      fprintf(stderr, "check_test_unbounded_msg :  ntu_test_msgP->u_char_ua[%d](%d) != %d\n",
		      i,ntu_test_msgP->u_char_ua[i], ((unsigned char) ('0'+(i%10))));
	      return false;
	    }
	}
    }

  if(ntu_test_msgP->short_ua)
    {
      if(ntu_test_msgP->short_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->short_ua_size_allocated(%d) <  0\n",
		  ntu_test_msgP->short_ua_size_allocated);
	  return false;
	}
      if(ntu_test_msgP->short_ua_length <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->short_ua_length(%d) <  0\n",
		  ntu_test_msgP->short_ua_length);
	  return false;
	}
      if(ntu_test_msgP->short_ua_size_allocated <  ntu_test_msgP->short_ua_length)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->short_ua_size_allocated(%d) <  ntu_test_msgP->short_ua_length(%d)\n",
		  ntu_test_msgP->short_ua_size_allocated,ntu_test_msgP->short_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_test_msgP->short_ua_length; i++)
	{
	  if(ntu_test_msgP->short_ua[i] != (short) i)
	    {
	      fprintf(stderr, "check_test_unbounded_msg :  ntu_test_msgP->short_ua[%d](%d) != %d\n",
		      i,ntu_test_msgP->short_ua[i], ((short) i));
	      return false;
	    }
	}
    }

  if(ntu_test_msgP->u_short_ua)
    {
      if(ntu_test_msgP->u_short_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->u_short_ua_size_allocated(%d) <  0\n",
		  ntu_test_msgP->u_short_ua_size_allocated);
	  return false;
	}
      if(ntu_test_msgP->u_short_ua_length <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->u_short_ua_length(%d) <  0\n",
		  ntu_test_msgP->u_short_ua_length);
	  return false;
	}
      if(ntu_test_msgP->u_short_ua_size_allocated <  ntu_test_msgP->u_short_ua_length)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->u_short_ua_size_allocated(%d) <  ntu_test_msgP->u_short_ua_length(%d)\n",
		  ntu_test_msgP->u_short_ua_size_allocated,ntu_test_msgP->u_short_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_test_msgP->u_short_ua_length; i++)
	{
	  if(ntu_test_msgP->u_short_ua[i] != (unsigned short) i)
	    {
	      fprintf(stderr, "check_test_unbounded_msg :  ntu_test_msgP->u_short_ua[%d](%d) != %d\n",
		      i,ntu_test_msgP->u_short_ua[i], ((unsigned short) i));
	      return false;
	    }
	}
    }


  if(ntu_test_msgP->int_ua)
    {
      if(ntu_test_msgP->int_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->int_ua_size_allocated(%d) <  0\n",
		  ntu_test_msgP->int_ua_size_allocated);
	  return false;
	}
      if(ntu_test_msgP->int_ua_length <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->int_ua_length(%d) <  0\n",
		  ntu_test_msgP->int_ua_length);
	  return false;
	}
      if(ntu_test_msgP->int_ua_size_allocated <  ntu_test_msgP->int_ua_length)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->int_ua_size_allocated(%d) <  ntu_test_msgP->int_ua_length(%d)\n",
		  ntu_test_msgP->int_ua_size_allocated,ntu_test_msgP->int_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_test_msgP->int_ua_length; i++)
	{
	  if(ntu_test_msgP->int_ua[i] != (int) i)
	    {
	      fprintf(stderr, "check_test_unbounded_msg :  ntu_test_msgP->int_ua[%d](%d) != %d\n",
		      i,ntu_test_msgP->int_ua[i], ((int) i));
	      return false;
	    }
	}
    }

  if(ntu_test_msgP->u_int_ua)
    {
      if(ntu_test_msgP->u_int_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->u_int_ua_size_allocated(%d) <  0\n",
		  ntu_test_msgP->u_int_ua_size_allocated);
	  return false;
	}
      if(ntu_test_msgP->u_int_ua_length <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->u_int_ua_length(%d) <  0\n",
		  ntu_test_msgP->u_int_ua_length);
	  return false;
	}
      if(ntu_test_msgP->u_int_ua_size_allocated <  ntu_test_msgP->u_int_ua_length)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->u_int_ua_size_allocated(%d) <  ntu_test_msgP->u_int_ua_length(%d)\n",
		  ntu_test_msgP->u_int_ua_size_allocated,ntu_test_msgP->u_int_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_test_msgP->u_int_ua_length; i++)
	{
	  if(ntu_test_msgP->u_int_ua[i] != (unsigned int) i)
	    {
	      fprintf(stderr, "check_test_unbounded_msg :  ntu_test_msgP->u_int_ua[%d](%d) != %d\n",
		      i,ntu_test_msgP->u_int_ua[i], ((unsigned int) i));
	      return false;
	    }
	}
    }

  if(ntu_test_msgP->long_ua)
    {
      if(ntu_test_msgP->long_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->long_ua_size_allocated(%d) <  0\n",
		  ntu_test_msgP->long_ua_size_allocated);
	  return false;
	}
      if(ntu_test_msgP->long_ua_length <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->long_ua_length(%d) <  0\n",
		  ntu_test_msgP->long_ua_length);
	  return false;
	}
      if(ntu_test_msgP->long_ua_size_allocated <  ntu_test_msgP->long_ua_length)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->long_ua_size_allocated(%d) <  ntu_test_msgP->long_ua_length(%d)\n",
		  ntu_test_msgP->long_ua_size_allocated,ntu_test_msgP->long_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_test_msgP->long_ua_length; i++)
	{
	  if(ntu_test_msgP->long_ua[i] != (long) i)
	    {
	      fprintf(stderr, "check_test_unbounded_msg :  ntu_test_msgP->long_ua[%d](%ld) != %ld\n",
		      i,ntu_test_msgP->long_ua[i], ((long) i));
	      return false;
	    }
	}
    }

  if(ntu_test_msgP->u_long_ua)
    {
      if(ntu_test_msgP->u_long_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->u_long_ua_size_allocated(%d) <  0\n",
		  ntu_test_msgP->u_long_ua_size_allocated);
	  return false;
	}
      if(ntu_test_msgP->u_long_ua_length <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->u_long_ua_length(%d) <  0\n",
		  ntu_test_msgP->u_long_ua_length);
	  return false;
	}
      if(ntu_test_msgP->u_long_ua_size_allocated <  ntu_test_msgP->u_long_ua_length)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->u_long_ua_size_allocated(%d) <  ntu_test_msgP->u_long_ua_length(%d)\n",
		  ntu_test_msgP->u_long_ua_size_allocated,ntu_test_msgP->u_long_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_test_msgP->u_long_ua_length; i++)
	{
	  if(ntu_test_msgP->u_long_ua[i] != (unsigned long) i)
	    {
	      fprintf(stderr, "check_test_unbounded_msg :  ntu_test_msgP->u_long_ua[%d](%ld) != %ld\n",
		      i,ntu_test_msgP->u_long_ua[i], ((unsigned long) i));
	      return false;
	    }
	}
    }


  if(ntu_test_msgP->float_ua)
    {
      if(ntu_test_msgP->float_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->float_ua_size_allocated(%d) <  0\n",
		  ntu_test_msgP->float_ua_size_allocated);
	  return false;
	}
      if(ntu_test_msgP->float_ua_length <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->float_ua_length(%d) <  0\n",
		  ntu_test_msgP->float_ua_length);
	  return false;
	}
      if(ntu_test_msgP->float_ua_size_allocated <  ntu_test_msgP->float_ua_length)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->float_ua_size_allocated(%d) <  ntu_test_msgP->float_ua_length(%d)\n",
		  ntu_test_msgP->float_ua_size_allocated,ntu_test_msgP->float_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_test_msgP->float_ua_length; i++)
	{
	  if(ntu_test_msgP->float_ua[i] != (float) i)
	    {
	      fprintf(stderr, "check_test_unbounded_msg :  ntu_test_msgP->float_ua[%d](%f) != %f\n",
		      i,ntu_test_msgP->float_ua[i], ((float) i));
	      return false;
	    }
	}
    }


  if(ntu_test_msgP->double_ua)
    {
      if(ntu_test_msgP->double_ua_size_allocated <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->double_ua_size_allocated(%d) <  0\n",
		  ntu_test_msgP->double_ua_size_allocated);
	  return false;
	}
      if(ntu_test_msgP->double_ua_length <  0)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->double_ua_length(%d) <  0\n",
		  ntu_test_msgP->double_ua_length);
	  return false;
	}
      if(ntu_test_msgP->double_ua_size_allocated <  ntu_test_msgP->double_ua_length)
	{
	  fprintf(stderr,"check_test_unbounded_msg : ntu_test_msgP->double_ua_size_allocated(%d) <  ntu_test_msgP->double_ua_length(%d)\n",
		  ntu_test_msgP->double_ua_size_allocated,ntu_test_msgP->double_ua_length);
	  return false;
	}
      for(int i =0 ; i < ntu_test_msgP->double_ua_length; i++)
	{
	  if(ntu_test_msgP->double_ua[i] != (double) i)
	    {
	      fprintf(stderr, "check_test_unbounded_msg :  ntu_test_msgP->double_ua[%d](%f) != %f\n",
		      i,ntu_test_msgP->double_ua[i], ((double) i));
	      return false;
	    }
	}
    }
  for(int i = 0; i < ARRAY_LENI(ntu_test_msgP->s1_a); i++)
    {
      if(!check_ntu_s1(&(ntu_test_msgP->s1_a[i])))
	{
	  fprintf(stderr,"check_test_unbounded_msg : check_ntu_s1(ntu_test_msgP->s1_a[%d]) returned false\n",
		  i);
	  return false;
	}
    }
  return true;
}

static bool
check_ntu_s1(ntu_s1 *ntu_s1P,
	     int len)
{

  if((len != 0) != (ntu_s1P->ntu_s1_char_ua != 0))
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_char_ua=%p\n",
	      len, ntu_s1P->ntu_s1_char_ua);
      return false;
    }
  // if(len != ntu_s1P->ntu_s1_char_ua_length)
//     {
//       fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_char_ua_length=%d\n",
// 	      len, ntu_s1P->ntu_s1_char_ua_length);
//       return false;
//     }
  if((len != 0) != (ntu_s1P->ntu_s1_u_char_ua != 0))
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_u_char_ua=%p\n",
	      len, ntu_s1P->ntu_s1_u_char_ua);
      return false;
    }
  if(len != ntu_s1P->ntu_s1_u_char_ua_length)
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_u_char_ua_length=%d\n",
	      len, ntu_s1P->ntu_s1_u_char_ua_length);
      return false;
    }

  if((len != 0) != (ntu_s1P->ntu_s1_short_ua != 0))
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_short_ua=%p\n",
	      len, ntu_s1P->ntu_s1_short_ua);
      return false;
    }
  if(len != ntu_s1P->ntu_s1_short_ua_length)
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_short_ua_length=%d\n",
	      len, ntu_s1P->ntu_s1_short_ua_length);
      return false;
    }
  if((len != 0) != (ntu_s1P->ntu_s1_u_short_ua != 0))
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_u_short_ua=%p\n",
	      len, ntu_s1P->ntu_s1_u_short_ua);
      return false;
    }
  if(len != ntu_s1P->ntu_s1_u_short_ua_length)
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_u_short_ua_length=%d\n",
	      len, ntu_s1P->ntu_s1_u_short_ua_length);
      return false;
    }


  if((len != 0) != (ntu_s1P->ntu_s1_int_ua != 0))
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_int_ua=%p\n",
	      len, ntu_s1P->ntu_s1_int_ua);
      return false;
    }
  if(len != ntu_s1P->ntu_s1_int_ua_length)
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_int_ua_length=%d\n",
	      len, ntu_s1P->ntu_s1_int_ua_length);
      return false;
    }
  if((len != 0) != (ntu_s1P->ntu_s1_u_int_ua != 0))
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_u_int_ua=%p\n",
	      len, ntu_s1P->ntu_s1_u_int_ua);
      return false;
    }
  if(len != ntu_s1P->ntu_s1_u_int_ua_length)
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_u_int_ua_length=%d\n",
	      len, ntu_s1P->ntu_s1_u_int_ua_length);
      return false;
    }


  if((len != 0) != (ntu_s1P->ntu_s1_long_ua != 0))
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_long_ua=%p\n",
	      len, ntu_s1P->ntu_s1_long_ua);
      return false;
    }
  if(len != ntu_s1P->ntu_s1_long_ua_length)
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_long_ua_length=%d\n",
	      len, ntu_s1P->ntu_s1_long_ua_length);
      return false;
    }
  if((len != 0) != (ntu_s1P->ntu_s1_u_long_ua != 0))
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_u_long_ua=%p\n",
	      len, ntu_s1P->ntu_s1_u_long_ua);
      return false;
    }
  if(len != ntu_s1P->ntu_s1_u_long_ua_length)
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_u_long_ua_length=%d\n",
	      len, ntu_s1P->ntu_s1_u_long_ua_length);
      return false;
    }


  if((len != 0) != (ntu_s1P->ntu_s1_float_ua != 0))
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_float_ua=%p\n",
	      len, ntu_s1P->ntu_s1_float_ua);
      return false;
    }
  if(len != ntu_s1P->ntu_s1_float_ua_length)
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_float_ua_length=%d\n",
	      len, ntu_s1P->ntu_s1_float_ua_length);
      return false;
    }

  if((len != 0) != (ntu_s1P->ntu_s1_double_ua != 0))
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_double_ua=%p\n",
	      len, ntu_s1P->ntu_s1_double_ua);
      return false;
    }
  if(len != ntu_s1P->ntu_s1_double_ua_length)
    {
      fprintf(stderr,"check_ntu_s1 : len=%d and ntu_s1P->ntu_s1_double_ua_length=%d\n",
	      len, ntu_s1P->ntu_s1_double_ua_length);
      return false;
    }
  return true;
}


bool
check_test_unbounded_msg(NML_TEST_UNBOUNDED_MSG *ntu_test_msgP,
			 int len,
			 int ntu_s1_len)
{
  if((len != 0) != (ntu_test_msgP->char_ua != 0))
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->char_ua=%p\n",
	      len, ntu_test_msgP->char_ua);
      return false;
    }
  // if(len != ntu_test_msgP->char_ua_length)
//     {
//       fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->char_ua_length=%d\n",
// 	      len, ntu_test_msgP->char_ua_length);
//       return false;
//     }
  if((len != 0) != (ntu_test_msgP->u_char_ua != 0))
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->u_char_ua=%p\n",
	      len, ntu_test_msgP->u_char_ua);
      return false;
    }
  if(len != ntu_test_msgP->u_char_ua_length)
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->u_char_ua_length=%d\n",
	      len, ntu_test_msgP->u_char_ua_length);
      return false;
    }

  if((len != 0) != (ntu_test_msgP->short_ua != 0))
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->short_ua=%p\n",
	      len, ntu_test_msgP->short_ua);
      return false;
    }
  if(len != ntu_test_msgP->short_ua_length)
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->short_ua_length=%d\n",
	      len, ntu_test_msgP->short_ua_length);
      return false;
    }
  if((len != 0) != (ntu_test_msgP->u_short_ua != 0))
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->u_short_ua=%p\n",
	      len, ntu_test_msgP->u_short_ua);
      return false;
    }
  if(len != ntu_test_msgP->u_short_ua_length)
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->u_short_ua_length=%d\n",
	      len, ntu_test_msgP->u_short_ua_length);
      return false;
    }


  if((len != 0) != (ntu_test_msgP->int_ua != 0))
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->int_ua=%p\n",
	      len, ntu_test_msgP->int_ua);
      return false;
    }
  if(len != ntu_test_msgP->int_ua_length)
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->int_ua_length=%d\n",
	      len, ntu_test_msgP->int_ua_length);
      return false;
    }
  if((len != 0) != (ntu_test_msgP->u_int_ua != 0))
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->u_int_ua=%p\n",
	      len, ntu_test_msgP->u_int_ua);
      return false;
    }
  if(len != ntu_test_msgP->u_int_ua_length)
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->u_int_ua_length=%d\n",
	      len, ntu_test_msgP->u_int_ua_length);
      return false;
    }


  if((len != 0) != (ntu_test_msgP->long_ua != 0))
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->long_ua=%p\n",
	      len, ntu_test_msgP->long_ua);
      return false;
    }
  if(len != ntu_test_msgP->long_ua_length)
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->long_ua_length=%d\n",
	      len, ntu_test_msgP->long_ua_length);
      return false;
    }
  if((len != 0) != (ntu_test_msgP->u_long_ua != 0))
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->u_long_ua=%p\n",
	      len, ntu_test_msgP->u_long_ua);
      return false;
    }
  if(len != ntu_test_msgP->u_long_ua_length)
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->u_long_ua_length=%d\n",
	      len, ntu_test_msgP->u_long_ua_length);
      return false;
    }


  if((len != 0) != (ntu_test_msgP->float_ua != 0))
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->float_ua=%p\n",
	      len, ntu_test_msgP->float_ua);
      return false;
    }
  if(len != ntu_test_msgP->float_ua_length)
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->float_ua_length=%d\n",
	      len, ntu_test_msgP->float_ua_length);
      return false;
    }

  if((len != 0) != (ntu_test_msgP->double_ua != 0))
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->double_ua=%p\n",
	      len, ntu_test_msgP->double_ua);
      return false;
    }
  if(len != ntu_test_msgP->double_ua_length)
    {
      fprintf(stderr,"check_test_unbounded_msg : len=%d and ntu_test_msgP->double_ua_length=%d\n",
	      len, ntu_test_msgP->double_ua_length);
      return false;
    }

  check_ntu_s1(&(ntu_test_msgP->s1),ntu_s1_len);
  
  for(int i=0; i < len && i < ARRAY_LENI(ntu_test_msgP->s1_a); i++)
    {
      check_ntu_s1(&(ntu_test_msgP->s1_a[i]),ntu_s1_len);
    }

  return check_test_unbounded_msg(ntu_test_msgP);
}

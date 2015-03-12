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


#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if defined(ENABLE_PACKED_UP)


#ifdef HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "rcs_defs.hh"
#include <stdlib.h>
#include <string.h>
#endif

#include "dbg_mem.h"		// DEBUG_MALLOC,DEBUG_FREE
#include "cms_types.hh"		// CMS_UPDATE_ERROR
#include "cms.hh"		// class CMS
#include "cms_up.hh"		// class CMS_UPDATER
#include "cms_pup.hh"		// class CMS_PACKED_UPDATER
#include "rcs_prnt.hh"		// rcs_print_error

#if ENABLE_RCS_PACKEDL64
#include "l64.h"
#endif

static bool local_endian_determined =false;
static enum cms_endian_type local_endian=CMS_BIG_ENDIAN;

static enum cms_endian_type determine_local_endian(void)
{
  int itest;
  char *cptr;
  itest=1;
  cptr = (char *) (&itest);
  if(*cptr)
    {
      return CMS_LITTLE_ENDIAN;
    }
  return CMS_BIG_ENDIAN;
}

void store_n_swap_two(char **to, const char *from)
{
  char *t = (*to);
  const char *f = from;
  t[0] = f[1];
  t[1] = f[0];
  (*to) +=2;
}

void store_n_swap_four(char **to, const char *from)
{
  char *t = (*to);
  const char *f = from;
  t[0] = f[3];
  t[1] = f[2];
  t[2] = f[1];
  t[3] = f[0];
  (*to) +=4;
}

void store_n_swap_eight(char **to, const char *from)
{
  char *t = (*to);
  const char *f = from;
  t[0] = f[7];
  t[1] = f[6];
  t[2] = f[5];
  t[3] = f[4];
  t[4] = f[3];
  t[5] = f[2];
  t[6] = f[1];
  t[7] = f[0];
  (*to) +=8;
}

void retrieve_n_swap_two(char *to, char **from)
{
  char *t = to;
  const char *f = (*from);
  t[0] = f[1];
  t[1] = f[0];
  (*from) +=2;
}

void retrieve_n_swap_four(char *to, char **from)
{
  char *t = to;
  const char *f = (*from);
  t[0] = f[3];
  t[1] = f[2];
  t[2] = f[1];
  t[3] = f[0];
  (*from) +=4;
}

void retrieve_n_swap_eight(char *to, char **from)
{
  char *t = to;
  const char *f = (*from);
  t[0] = f[7];
  t[1] = f[6];
  t[2] = f[5];
  t[3] = f[4];
  t[4] = f[3];
  t[5] = f[2];
  t[6] = f[1];
  t[7] = f[0];
  (*from) +=8;
}

CMS_STATUS 
CMS_PACKED_UPDATER::update (bool &x)
{
  int orig_pointer_check_disabled = pointer_check_disabled;
  pointer_check_disabled = 1;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  char c = (x != 0);
  retval = update(c);
  x = (c != 0);
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}


CMS_STATUS 
CMS_PACKED_UPDATER::update (bool *x, unsigned int len)
{
  for(unsigned int i=0; i < len; i++)
    {
      update(x[i]);
    }
  return(status);
}


CMS_STATUS 
CMS_PACKED_UPDATER::update (char &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (char)))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  if(encoding)
    {
      *end_encoded_data = x;
      end_encoded_data++;
    }
  else
    {
      x = *(end_encoded_data);
      end_encoded_data++;
    }
  return(status);
}
	    
CMS_STATUS 
CMS_PACKED_UPDATER::update (unsigned char &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (char)))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  if(encoding)
    {
      *end_encoded_data = (char) x;
      end_encoded_data++;
    }
  else
    {
      x = (unsigned char) *(end_encoded_data);
      end_encoded_data++;
    }
  return(status);
}

CMS_STATUS
CMS_PACKED_UPDATER::update (short int &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (short int)))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  if (sizeof(short int) != 2)
    {
      rcs_print_error("Can't handle sizeof(short int) != 2\n");
      return(status=CMS_UPDATE_ERROR);
    }

  if(encoding)
    {
      if(swap_endian)
	{
	  store_n_swap_two(&end_encoded_data,(char *) &x);
	}
      else
	{
	  memcpy(end_encoded_data,&x,2);
	  end_encoded_data+=2;
	}
    }
  else
    {
      if(swap_endian)
	{
	  retrieve_n_swap_two((char *) &x,&end_encoded_data);
	}
      else
	{
	  memcpy(&x,end_encoded_data,2);
	  end_encoded_data+=2;
	}
    }
  return(status);
}

CMS_STATUS
CMS_PACKED_UPDATER::update (unsigned short int &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (unsigned short int)))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  if (sizeof(unsigned short int) != 2)
    {
      rcs_print_error("Can't handle sizeof(unsigned short int) != 2\n");
      return(status=CMS_UPDATE_ERROR);
    }

  if(encoding)
    {
      if(swap_endian)
	{
	  store_n_swap_two(&end_encoded_data,(char *) &x);
	}
      else
	{
	  memcpy(end_encoded_data,&x,2);
	  end_encoded_data+=2;
	}
    }
  else
    {
      if(swap_endian)
	{
	  retrieve_n_swap_two((char *) &x,&end_encoded_data);
	}
      else
	{
	  memcpy(&x,end_encoded_data,2);
	  end_encoded_data+=2;
	}
    }
  return(status);
}

CMS_STATUS
CMS_PACKED_UPDATER::update (int &x)
{  
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (int)))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  char *xp= (char *) &x;
  long lx;

  if(sizeof(int) == 2)
    {
      lx = (long) x;
      xp = (char *) &lx;
    }
  else if (sizeof(int) != 4)
    {
      rcs_print_error("Can't handle sizeof(int) != 4\n");
      return(status=CMS_UPDATE_ERROR);
    }

  if(encoding)
    {
      if(swap_endian)
	{
	  store_n_swap_four(&end_encoded_data,xp);
	}
      else
	{
	  memcpy(end_encoded_data,xp,4);
	  end_encoded_data+=4;
	}
    }
  else
    {
      if(swap_endian)
	{
	  retrieve_n_swap_four(xp,&end_encoded_data);
	}
      else
	{
	  memcpy(&x,end_encoded_data,4);
	  end_encoded_data+=4;
	}
    }

  if(sizeof(int) == 2)
    {
      if(lx >= 32768 || lx < -32768)
	{
	  rcs_print_error("value for int out of range %ld\n",lx);
	  return(status=CMS_UPDATE_ERROR);
	}
      x = (int) lx;
    }
  return(status);
}

CMS_STATUS
CMS_PACKED_UPDATER::update (unsigned int &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (unsigned int)))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  char *xp= (char *) &x;
  unsigned long ulx;

  if(sizeof(unsigned int) == 2)
    {
      ulx = (unsigned long) x;
      xp = (char *) &ulx;
    }
  else if (sizeof(unsigned int) != 4)
    {
      rcs_print_error("Can't handle sizeof(unsigned int) != 4\n");
      return(status=CMS_UPDATE_ERROR);
    }

  if(encoding)
    {
      if(swap_endian)
	{
	  store_n_swap_four(&end_encoded_data,xp);
	}
      else
	{
	  memcpy(end_encoded_data,xp,4);
	  end_encoded_data+=4;
	}
    }
  else
    {
      if(swap_endian)
	{
	  retrieve_n_swap_four(xp,&end_encoded_data);
	}
      else
	{
	  memcpy(&x,end_encoded_data,4);
	  end_encoded_data+=4;
	}
    }

  if(sizeof(unsigned int) == 2)
    {
      if(ulx >= 65536)
	{
	  rcs_print_error("value for unsigned int out of range %lu\n",ulx);
	  return(status=CMS_UPDATE_ERROR);
	}
      x = (unsigned int) ulx;
    }
  return(status);
}

CMS_STATUS
CMS_PACKED_UPDATER::update(long int &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (long int)))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  char *xp= (char *) &x;
  int xi = (int) x;

#if ENABLE_RCS_PACKEDL64
  char xll[8];
  if(sizeof(long int) == 4 && l64_mode && encoding && data_has_64bit_longs)
    {
      l64_set(xll,x);
    }
#endif

  if(sizeof(long int) == 8 && l64_mode && data_has_64bit_longs)
    {
    }
  else if (sizeof(long int) == 8 && sizeof(int) == 4)
    {
#if defined(INT_MIN) && defined(LONG_MIN) && LONG_MIN < INT_MIN
      if(encoding && x < INT_MIN)
	{
	  rcs_print_warning("CMS_PACKED_UPDATER::update() can not convert 64bit long integer %ld to a 32bit integer which has a minimum value of %ld\n",
			  x,(long) INT_MIN);
	  if(fail_on_overflow)
	    {
	      return(status = CMS_UPDATE_ERROR);
	    }
	  x = INT_MIN/2 + x%(INT_MIN/2);
	  xi = (int) x;
	  rcs_print_warning("CMS_PACKED_UPDATER::update() sending %ld instead.\n", x);
	}

#if defined(INT_MAX) && defined(LONG_MAX) && LONG_MAX > INT_MAX
      if(encoding && x > INT_MAX)
	{
	  rcs_print_warning("CMS_PACKED_UPDATER::update() can not convert 64bit long integer %ld to a 32bit integer which has a maximum value of %ld\n",
			  x,(long) INT_MAX);
	  if(fail_on_overflow)
	    {
	      return(status = CMS_UPDATE_ERROR);
	    }
	  x = INT_MAX/2 + x%(INT_MAX/2);
	  xi = (int) x;
	  rcs_print_warning("CMS_PACKED_UPDATER::update() sending %ld instead.\n", x);
	}
#endif
#endif
      xp = (char *) &xi;
    }
  else if (sizeof(long int) == 4 &&
	   l64_mode && data_has_64bit_longs)
    {
#if ENABLE_RCS_PACKEDL64
      xp = xll;
#endif
    }
  else if (sizeof(long int) != 4)
    {
      rcs_print_error("Can't handle sizeof(long int) of %lu\n",
		      (unsigned long) sizeof(long int));
      return(status=CMS_UPDATE_ERROR);
    }

  
  if(l64_mode && data_has_64bit_longs)
    {  
#if ENABLE_RCS_PACKEDL64
      if(encoding)
	{
	  if(swap_endian)
	    {
	      store_n_swap_eight(&end_encoded_data,xp);
	    }
	  else
	    {
	      memcpy(end_encoded_data,xp,8);
	      end_encoded_data+=8;
	    }
	}
      else
	{
	  if(swap_endian)
	    {
	      retrieve_n_swap_eight(xp,&end_encoded_data);
	    }
	  else
	    {
	      memcpy(xp,end_encoded_data,8);
	      end_encoded_data+=8;
	    }
	  if (sizeof(long int) == 4)
	    {
	        long highpart=0;
		x = l64_get(xll,&highpart);
		if((highpart != 0 && x >= 0) || (highpart != -1 && x < 0))
		  {
		    rcs_print_warning("CMS_PACKED_UPDATER::update() conversion from 64bits to 32bits seemed to lose data. least-significant-32bits=%ld(0x%lX), most-significant-32bits=%ld(0x%lX)\n",
				      x,(unsigned long)x,
				      highpart, (unsigned long)highpart);
		    if(fail_on_overflow)
		      {
			return(status = CMS_UPDATE_ERROR);
		      }		    
		    if(highpart < 0)
		      {
			x = (LONG_MIN/2) + x%(LONG_MIN/2);
		      }
		    else
		      {
			x = (LONG_MAX/2) + x%(LONG_MAX/2);
		      }
		    rcs_print_warning("CMS_PACKED_UPDATER::update() returning 32bit value of %ld\n",x);
		  }
	    }
	}
#endif
    }
  else
    {
      if(encoding)
	{
	  if(swap_endian)
	    {
	      store_n_swap_four(&end_encoded_data,xp);
	    }
	  else
	    {
	      memcpy(end_encoded_data,xp,4);
	      end_encoded_data+=4;
	    }
	}
      else
	{
	  if(swap_endian)
	    {
	      retrieve_n_swap_four(xp,&end_encoded_data);
	    }
	  else
	    {
	      memcpy(xp,end_encoded_data,4);
	      end_encoded_data+=4;
	    }
	  if (sizeof(long int) == 8 && sizeof(int) == 4)
	    {
	      x = xi;
	    }
	}
    }
  return(status);
}
  
CMS_STATUS
CMS_PACKED_UPDATER::update (unsigned long int &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (unsigned long int)))
    {
      return (status=CMS_UPDATE_ERROR);
    }


  char *xp= (char *) &x;
  unsigned int xui = (unsigned int) x;

#if ENABLE_RCS_PACKEDL64
  unsigned char xull[8];
  if(encoding && sizeof(long int) == 4 && l64_mode && data_has_64bit_longs)
    {
      ul64_set(xull,x);
    }
#endif

  if(sizeof(unsigned long int) == 8 && l64_mode && data_has_64bit_longs)
    {
    }
  else if (sizeof(unsigned long int) == 8 && sizeof(unsigned int) == 4)
    {
#if defined(UINT_MAX) && defined(ULONG_MAX) && ULONG_MAX > UINT_MAX
      if(encoding && x > UINT_MAX)
	{
	  rcs_print_warning("CMS_PACKED_UPDATER::update() can not convert 64bit unsigned long integer %lu (0x%lX) to a 32bit unsigned integer which has a maximum value of %lu\n",
			  x,x,(unsigned long) UINT_MAX);
	  if(fail_on_overflow)
	    {
	      return(status = CMS_UPDATE_ERROR);
	    }
	  x = (unsigned long) (UINT_MAX/2) + (x%(UINT_MAX/2));
	  xui = (unsigned int) x;
	  rcs_print_warning("CMS_PACKED_UPDATER::update() sending %u instead.\n", xui);
	}
#else
      rcs_print_error("CMS_PACKED_UPDATER(): packed updater does not support unsigned longs which are 64 bits on this platform.\n");
      return(status=CMS_UPDATE_ERROR);
#endif
      xp = (char *) &xui;
    }
  else if (sizeof(long int) == 4 &&
	   l64_mode && data_has_64bit_longs)
    {
#if ENABLE_RCS_PACKEDL64
      xp = (char *) &xull;
#endif
    }
  else if (sizeof(unsigned long int) != 4)
    {
      rcs_print_error("Can't handle sizeof(unsigned long int) of %lu\n",
		      (unsigned long) sizeof(long int));
      return(status=CMS_UPDATE_ERROR);
    }

  
  if(l64_mode && data_has_64bit_longs)
    {  
#if ENABLE_RCS_PACKEDL64
      if(encoding)
	{
	  if(swap_endian)
	    {
	      store_n_swap_eight(&end_encoded_data,xp);
	    }
	  else
	    {
	      memcpy(end_encoded_data,xp,8);
	      end_encoded_data+=8;
	    }
	}
      else
	{
	  if(swap_endian)
	    {
	      retrieve_n_swap_eight(xp,&end_encoded_data);
	    }
	  else
	    {
	      memcpy(xp,end_encoded_data,8);
	      end_encoded_data+=8;
	    }
	  if (sizeof(unsigned long int) == 4)
	    {
	      unsigned long highpart=0;
	      x = ul64_get(xull,&highpart);
	      if(highpart != 0)
		{
		  rcs_print_warning("CMS::update() -- Conversion from 64 bit unsigned long to 32bit unsigned long seems to have lost data. least-significant-32bits=%lu(0x%lX), most-signifigant-32bits=%lu(0x%lX)\n",
				    x,x,
				    highpart,highpart);
		  if(fail_on_overflow)
		    {
		      return(status = CMS_UPDATE_ERROR);
		    }
		  x = (UINT_MAX)/2 + x%(UINT_MAX/2);
		  rcs_print_warning("CMS::update() returning %lu(0x%lX) instead.\n",x,x);
		}
	    }
	}
#endif
    }
  else
    {
      if(encoding)
	{
	  if(swap_endian)
	    {
	      store_n_swap_four(&end_encoded_data,xp);
	    }
	  else
	    {
	      memcpy(end_encoded_data,xp,4);
	      end_encoded_data+=4;
	    }
	}
      else
	{
	  if(swap_endian)
	    {
	      retrieve_n_swap_four(xp,&end_encoded_data);
	    }
	  else
	    {
	      memcpy(xp,end_encoded_data,4);
	      end_encoded_data+=4;
	    }
	  if (sizeof(long int) == 8 && sizeof(int) == 4)
	    {
	      x = xui;
	    }
	}
    }
  return(status);
}
  

CMS_STATUS
CMS_PACKED_UPDATER::update (float &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (float)))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  char *xp= (char *) &x;
  if (sizeof(float) != 4)
    {
      rcs_print_error("Can't handle sizeof(float) != 4\n");
      return(status=CMS_UPDATE_ERROR);
    }

  if(encoding)
    {
      if(swap_endian)
	{
	  store_n_swap_four(&end_encoded_data,xp);
	}
      else
	{
	  memcpy(end_encoded_data,xp,4);
	  end_encoded_data+=4;
	}
    }
  else
    {
      if(swap_endian)
	{
	  retrieve_n_swap_four(xp,&end_encoded_data);
	}
      else
	{
	  memcpy(&x,end_encoded_data,4);
	  end_encoded_data+=4;
	}
    }
  return(status);
}
  
CMS_STATUS 
CMS_PACKED_UPDATER::update (double &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (double)))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  char *xp= (char *) &x;
  if (sizeof(double) != 8)
    {
      rcs_print_error("Can't handle sizeof(double) != 8\n");
      return(status=CMS_UPDATE_ERROR);
    }

  if(encoding)
    {
      if(swap_endian)
	{
	  store_n_swap_eight(&end_encoded_data,xp);
	}
      else
	{
	  memcpy(end_encoded_data,xp,8);
	  end_encoded_data+=8;
	}
    }
  else
    {
      if(swap_endian)
	{
	  retrieve_n_swap_eight(xp,&end_encoded_data);
	}
      else
	{
	  memcpy(&x,end_encoded_data,8);
	  end_encoded_data+=8;
	}
    }
  return(status);
}
  
CMS_STATUS 
CMS_PACKED_UPDATER::update(
			   __unused_parameter__ long double &)
{
  rcs_print_error("long double not supported\n");
  return(status=CMS_UPDATE_ERROR);
}

CMS_STATUS 
CMS_PACKED_UPDATER::update (char *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (char)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  if(len == 0)
    {
      return(CMS_STATUS_NOT_SET);
    }

  if(encoding)
    {
      memcpy(end_encoded_data,x,len);
      end_encoded_data +=len;
    }
  else
    {
      memcpy(x,end_encoded_data,len);
      end_encoded_data +=len;
    }
  return(status);
}

CMS_STATUS
CMS_PACKED_UPDATER::update (unsigned char *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (unsigned char)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  if(len == 0)
    {
      return(CMS_STATUS_NOT_SET);
    }
  if(encoding)
    {
      memcpy(end_encoded_data,x,len);
      end_encoded_data +=len;
    }
  else
    {
      memcpy(x,end_encoded_data,len);
      end_encoded_data +=len;
    }
  return(status);
}
  
CMS_STATUS
CMS_PACKED_UPDATER::update (short *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (short)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  if(len == 0)
    {
      return(CMS_STATUS_NOT_SET);
    }
  char *xp = (char *) x;
  if (sizeof(unsigned short int) != 2)
    {
      rcs_print_error("Can't handle sizeof(unsigned short int) != 2\n");
      return(status=CMS_UPDATE_ERROR);
    }

  if(encoding)
    {
      if(swap_endian)
	{
	  for(unsigned int i = 0; i < len; i++)
	    {
	      store_n_swap_two(&end_encoded_data,xp);
	      xp+=2;
	    }
	}
      else
	{
	  memcpy(end_encoded_data,xp,2*len);
	  end_encoded_data += 2*len;
	}	  
    }
  else
    {
      if(swap_endian)
	{
	  for(unsigned int i = 0; i < len; i++)
	    {
	      retrieve_n_swap_two(xp,&end_encoded_data);
	      xp+=2;
	    }
	}
      else
	{
	  memcpy(x,end_encoded_data,2*len);
	  end_encoded_data += 2*len;
	}
    }
  return(status);
}
  
CMS_STATUS
CMS_PACKED_UPDATER::update (unsigned short *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (unsigned short)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  if(len == 0)
    {
      return(CMS_STATUS_NOT_SET);
    }
  char *xp = (char *) x;
  if (sizeof(unsigned short int) != 2)
    {
      rcs_print_error("Can't handle sizeof(unsigned short int) != 2\n");
      return(status=CMS_UPDATE_ERROR);
    }

  if(encoding)
    {
      if(swap_endian)
	{
	  for(unsigned int i = 0; i < len; i++)
	    {
	      store_n_swap_two(&end_encoded_data,xp);
	      xp+=2;
	    }
	}
      else
	{
	  memcpy(end_encoded_data,xp,2*len);
	  end_encoded_data += 2*len;
	}	  
    }
  else
    {
      if(swap_endian)
	{
	  for(unsigned int i = 0; i < len; i++)
	    {
	      retrieve_n_swap_two(xp,&end_encoded_data);
	      xp+=2;
	    }
	}
      else
	{
	  memcpy(x,end_encoded_data,2*len);
	  end_encoded_data += 2*len;
	}
    }
  return(status);
}
  
CMS_STATUS
CMS_PACKED_UPDATER::update (int *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (int)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  if(len == 0)
    {
      return(CMS_STATUS_NOT_SET);
    }

  if(sizeof(int) == 2)
    {
      for(unsigned int i=0; i < len; i++)
	{
	  update(x[i]);
	}
      return(status);
    }
  char *xp = (char *) x;
  if (sizeof(int) != 4)
    {
      rcs_print_error("Can't handle sizeof(int) != 4\n");
      return(status=CMS_UPDATE_ERROR);
    }

  if(encoding)
    {
      if(swap_endian)
	{
	  for(unsigned int i = 0; i < len; i++)
	    {
	      store_n_swap_four(&end_encoded_data,xp);
	      xp+=4;
	    }
	}
      else
	{
	  memcpy(end_encoded_data,xp,4*len);
	  end_encoded_data += 4*len;
	}	  
    }
  else
    {
      if(swap_endian)
	{
	  for(unsigned int i = 0; i < len; i++)
	    {
	      retrieve_n_swap_four(xp,&end_encoded_data);
	      xp+=4;
	    }
	}
      else
	{
	  memcpy(x,end_encoded_data,4*len);
	  end_encoded_data += 4*len;
	}
    }
  return(status);
}

CMS_STATUS
CMS_PACKED_UPDATER::update (unsigned int *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (unsigned int)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  if(len == 0)
    {
      return(CMS_STATUS_NOT_SET);
    }
  if(sizeof(unsigned int) == 2)
    {
      for(unsigned int i=0; i < len; i++)
	{
	  update(x[i]);
	}
      return(status);
    }
  char *xp = (char *) x;
  if (sizeof(unsigned int) != 4)
    {
      rcs_print_error("Can't handle sizeof(unsigned int) != 4\n");
      return(status=CMS_UPDATE_ERROR);
    }

  if(encoding)
    {
      if(swap_endian)
	{
	  for(unsigned int i = 0; i < len; i++)
	    {
	      store_n_swap_four(&end_encoded_data,xp);
	      xp+=4;
	    }
	}
      else
	{
	  memcpy(end_encoded_data,xp,4*len);
	  end_encoded_data += 4*len;
	}	  
    }
  else
    {
      if(swap_endian)
	{
	  for(unsigned int i = 0; i < len; i++)
	    {
	      retrieve_n_swap_four(xp,&end_encoded_data);
	      xp+=4;
	    }
	}
      else
	{
	  memcpy(x,end_encoded_data,4*len);
	  end_encoded_data += 4*len;
	}
    }
  return(status);
}

CMS_STATUS 
CMS_PACKED_UPDATER::update (long *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (long)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  if(len == 0)
    {
      return(CMS_STATUS_NOT_SET);
    }
  char *xp = (char *) x;
  unsigned int i=0;

  if(sizeof(long) == 8 && l64_mode && data_has_64bit_longs)
    {
    }
  else if(sizeof(long) == 8 && sizeof(int) == 4
	  && !(l64_mode && data_has_64bit_longs))
    {
      for(i = 0; i < len; i++)
	{
	  update(x[i]);
	}
      return(status);
    }
#if ENABLE_RCS_PACKEDL64
   else if(sizeof(long) == 4
	  && (l64_mode && data_has_64bit_longs))
    {
      for(i = 0; i < len; i++)
	{
	  update(x[i]);
	}
      return(status);
    }
#endif
  else if (sizeof(long) != 4)
    {
      rcs_print_error("Can't handle sizeof(long) = %lu\n",
		      (unsigned long) sizeof(long));
      return(status=CMS_UPDATE_ERROR);
    }

  if(l64_mode && data_has_64bit_longs)
    {
      if(encoding)
	{
	  if(swap_endian)
	    {
	      for(i = 0; i < len; i++)
		{
		  store_n_swap_eight(&end_encoded_data,xp);
		  xp+=8;
		}
	    }
	  else
	    {
	      memcpy(end_encoded_data,xp,8*len);
	      end_encoded_data += 8*len;
	    }	  
	}
      else
	{
	  if(swap_endian)
	    {
	      for(i = 0; i < len; i++)
		{
		  retrieve_n_swap_eight(xp,&end_encoded_data);
		  xp+=8;
		}
	    }
	  else
	    {
	      memcpy(x,end_encoded_data,8*len);
	      end_encoded_data += 8*len;
	    }
	}
    }
  else
    {
      
      if(encoding)
	{
	  if(swap_endian)
	    {
	      for(i = 0; i < len; i++)
		{
		  store_n_swap_four(&end_encoded_data,xp);
		  xp+=4;
		}
	    }
	  else
	    {
	      memcpy(end_encoded_data,xp,4*len);
	      end_encoded_data += 4*len;
	    }	  
	}
      else
	{
	  if(swap_endian)
	    {
	      for(i = 0; i < len; i++)
		{
		  retrieve_n_swap_four(xp,&end_encoded_data);
		  xp+=4;
		}
	    }
	  else
	    {
	      memcpy(x,end_encoded_data,4*len);
	      end_encoded_data += 4*len;
	    }
	}
    }
  return(status);
}

CMS_STATUS
CMS_PACKED_UPDATER::update (unsigned long *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (unsigned long)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  if(len == 0)
    {
      return(CMS_STATUS_NOT_SET);
    }
  char *xp = (char *) x;
  unsigned int i=0;

  if(sizeof(unsigned long) == 8 && l64_mode && data_has_64bit_longs)
    {
    }
  else if(sizeof(unsigned long) == 8 && sizeof(int) == 4
	  && !(l64_mode && data_has_64bit_longs))
    {
      for(i = 0; i < len; i++)
	{
	  update(x[i]);
	}
      return(status);
    }
#if ENABLE_RCS_PACKEDL64
   else if(sizeof(unsigned long) == 4
	  && (l64_mode && data_has_64bit_longs))
    {
      for(i = 0; i < len; i++)
	{
	  update(x[i]);
	}
      return(status);
    }
#endif
  else if (sizeof(unsigned long) != 4)
    {
      rcs_print_error("Can't handle sizeof(unsigned long) = %lu\n",
		      (unsigned long) sizeof(unsigned long));
      return(status=CMS_UPDATE_ERROR);
    }

  if(l64_mode && data_has_64bit_longs)
    {
      if(encoding)
	{
	  if(swap_endian)
	    {
	      for(i = 0; i < len; i++)
		{
		  store_n_swap_eight(&end_encoded_data,xp);
		  xp+=8;
		}
	    }
	  else
	    {
	      memcpy(end_encoded_data,xp,8*len);
	      end_encoded_data += 8*len;
	    }	  
	}
      else
	{
	  if(swap_endian)
	    {
	      for(i = 0; i < len; i++)
		{
		  retrieve_n_swap_eight(xp,&end_encoded_data);
		  xp+=8;
		}
	    }
	  else
	    {
	      memcpy(x,end_encoded_data,8*len);
	      end_encoded_data += 8*len;
	    }
	}
    }
  else
    {
      
      if(encoding)
	{
	  if(swap_endian)
	    {
	      for(i = 0; i < len; i++)
		{
		  store_n_swap_four(&end_encoded_data,xp);
		  xp+=4;
		}
	    }
	  else
	    {
	      memcpy(end_encoded_data,xp,4*len);
	      end_encoded_data += 4*len;
	    }	  
	}
      else
	{
	  if(swap_endian)
	    {
	      for(i = 0; i < len; i++)
		{
		  retrieve_n_swap_four(xp,&end_encoded_data);
		  xp+=4;
		}
	    }
	  else
	    {
	      memcpy(x,end_encoded_data,4*len);
	      end_encoded_data += 4*len;
	    }
	}
    }
  return(status);
}

CMS_STATUS
CMS_PACKED_UPDATER::update (float *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (float)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  if(len == 0)
    {
      return(CMS_STATUS_NOT_SET);
    }
  char *xp = (char *) x;
  if (sizeof(float) != 4)
    {
      rcs_print_error("Can't handle sizeof(float) != 4\n");
      return(status=CMS_UPDATE_ERROR);
    }

  if(encoding)
    {
      if(swap_endian)
	{
	  for(unsigned int i = 0; i < len; i++)
	    {
	      store_n_swap_four(&end_encoded_data,xp);
	      xp+=4;
	    }
	}
      else
	{
	  memcpy(end_encoded_data,xp,4*len);
	  end_encoded_data += 4*len;
	}	  
    }
  else
    {
      if(swap_endian)
	{
	  for(unsigned int i = 0; i < len; i++)
	    {
	      retrieve_n_swap_four(xp,&end_encoded_data);
	      xp+=4;
	    }
	}
      else
	{
	  memcpy(x,end_encoded_data,4*len);
	  end_encoded_data += 4*len;
	}
    }
  return(status);
}

CMS_STATUS
CMS_PACKED_UPDATER::update (double *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (double)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  if(len == 0)
    {
      return(CMS_STATUS_NOT_SET);
    }
  char *xp = (char *) x;
  if (sizeof(double) != 8)
    {
      rcs_print_error("Can't handle sizeof(double) != 8\n");
      return(status=CMS_UPDATE_ERROR);
    }

  if(encoding)
    {
      if(swap_endian)
	{
	  for(unsigned int i = 0; i < len; i++)
	    {
	      store_n_swap_eight(&end_encoded_data,xp);
	      xp+=8;
	    }
	}
      else
	{
	  memcpy(end_encoded_data,xp,8*len);
	  end_encoded_data += 8*len;
	}	  
    }
  else
    {
      if(swap_endian)
	{
	  for(unsigned int i = 0; i < len; i++)
	    {
	      retrieve_n_swap_eight(xp,&end_encoded_data);
	      xp+=8;
	    }
	}
      else
	{
	  memcpy(x,end_encoded_data,8*len);
	  end_encoded_data += 8*len;
	}
    }
  return(status);
}

CMS_STATUS 
CMS_PACKED_UPDATER::update (
			    __unused_parameter__ long double *, 
			    __unused_parameter__ unsigned int)
{
  rcs_print_error("long double not supported\n");
  return(status=CMS_UPDATE_ERROR);
}
 
void
CMS_PACKED_UPDATER::rewind ()
{
  end_encoded_data = (char *) start_encoded_data;
  if(end_encoded_data == 0)
    {
      rcs_print_error("end_encoded_data is null\n");
      status=CMS_UPDATE_ERROR;
      return;
    }

  if(encoding)
    {
      if(force_endian && endian_to_force != local_endian)
	{
	  data_endian = endian_to_force;
	  swap_endian=true;
	}
      else
	{
	  data_endian = local_endian;
	  swap_endian=false;
	}
      if(endian_in_data)
	{
	  if(l64_mode && sizeof(long) == 8)
	    {
	      data_has_64bit_longs=true;
	      if(data_endian == CMS_BIG_ENDIAN)
		{
		  *end_encoded_data='b';
		}
	      else
		{
		  *end_encoded_data='l';
		}
	    }
	  else
	    {
	      data_has_64bit_longs=false;
	      if(data_endian == CMS_BIG_ENDIAN)
		{
		  *end_encoded_data='B';
		}
	      else
		{
		  *end_encoded_data='L';
		}
	    }
	  end_encoded_data++;
	}
    }
  else
    {
      if(endian_in_data)
	{
	  char endian_char = *end_encoded_data;
	  switch(endian_char)
	    {
	    case 'B':
	      data_endian = CMS_BIG_ENDIAN;
	      data_has_64bit_longs=false;
	      break;
	      
	    case 'L':
	      data_endian = CMS_LITTLE_ENDIAN;
	      data_has_64bit_longs=false;
	      break;

	    case 'b':
	      data_endian = CMS_BIG_ENDIAN;
	      data_has_64bit_longs=true;
	      break;
	      
	    case 'l':
	      data_endian = CMS_LITTLE_ENDIAN;
	      data_has_64bit_longs=true;
	      break;

	    case 0:
	      if(mode != CMS_DECODE_HEADER && mode != CMS_DECODE_QUEUING_HEADER)
		{
		  rcs_print_error("Bad endian char=\\0, mode=%d\n",mode);
		}
	      data_endian = local_endian;
	      break;
	      
	    default:
	      data_endian = local_endian;
	      rcs_print_error("Bad endian char=\'%c\' %d (0x%X), mode=%d\n",
			      endian_char,
			      endian_char,
			      endian_char,
			      mode);
	      break;
	    }
	  end_encoded_data++;
	}
      if(data_endian != local_endian)
	{
	  swap_endian=true;
	}
      else
	{
	  swap_endian=false;
	}
    }
}
	    
int 
CMS_PACKED_UPDATER::get_encoded_msg_size ()
{
  return( (int) (end_encoded_data - ((char *)start_encoded_data)));
}

  
CMS_PACKED_UPDATER::CMS_PACKED_UPDATER (CMS *_cms_parent, bool _l64_mode):
  CMS_UPDATER (_cms_parent,1, 2),
  data_endian(CMS_LITTLE_ENDIAN),
  endian_to_force(CMS_LITTLE_ENDIAN),
  force_endian(false),
  endian_in_data(false),
  swap_endian(false),
  end_encoded_data(0),
  start_encoded_data(0),
  data_has_64bit_longs(false),
  l64_mode(_l64_mode)
{
   rcs_print_debug (PRINT_CMS_CONSTRUCTORS, "creating CMS_PACKED_UPDATER\n");
#if !ENABLE_RCS_PACKEDL64
   if(l64_mode)
     {
       rcs_print_error("l64_mode requested but the library was compiled without support for this.\n");
       status=CMS_UPDATE_ERROR;
       l64_mode=false;
     }
#endif
   if(!local_endian_determined)
     {
       local_endian = determine_local_endian();
       local_endian_determined=true;
     }
   data_endian=local_endian;
   endian_to_force=local_endian;
   force_endian=false;
   endian_in_data=true;
   swap_endian=false;

   encoded_header = DEBUG_MALLOC (neutral_size_factor * sizeof (CMS_HEADER));
   if (encoded_header == NULL)
     {
       rcs_print_error ("CMS:can't malloc encoded_header");
       status = CMS_CREATE_ERROR;
       return;
     }

  encoded_queuing_header = DEBUG_MALLOC (neutral_size_factor * sizeof (CMS_QUEUING_HEADER));
  if (encoded_queuing_header == NULL)
    {
      rcs_print_error ("CMS:can't malloc encoded_queuing_header");
      status = CMS_CREATE_ERROR;
      return;
    }

}

CMS_PACKED_UPDATER::~CMS_PACKED_UPDATER ()
{
  end_encoded_data=0;
  if (NULL != encoded_header)
    {
      DEBUG_FREE (encoded_header);
      encoded_header = NULL;
    }
  if (NULL != encoded_queuing_header)
    {
      DEBUG_FREE (encoded_queuing_header);
      encoded_queuing_header = NULL;
    }
}

int
CMS_PACKED_UPDATER::check_pointer (char * _pointer, long _bytes)
{
  if(end_encoded_data == 0)
    {
      rcs_print_error("end_encoded_data is NULL\n");
      status=CMS_UPDATE_ERROR;
      return -1;
    }
  if(cms_parent == 0)
    {
      rcs_print_error("cms_parent is NULL\n");
      status=CMS_UPDATE_ERROR;
      return -1;
    }
  const char *p1 = end_encoded_data + _bytes;
  const char *p2 = ((const char *)start_encoded_data) + encoded_data_size;
  if(p1 > p2)
    {
      ptrdiff_t pd = end_encoded_data - ((char *) start_encoded_data);
      rcs_print_error("insufficient space to encode/decode message, start_encoded_data=%p,encoded_data_size=%lu(0x%lX), end_encoded_data=%p,_bytes=%ld(0x%lX) (end_encoded_data-start_encoded_data)=%lu(0x%lX)\n",
		      start_encoded_data,
		      (unsigned long) encoded_data_size,
		      (unsigned long) encoded_data_size,
		      end_encoded_data,
		      _bytes,(unsigned long)_bytes,
		      (unsigned long)pd,(unsigned long)pd);
      status=CMS_UPDATE_ERROR;
      return -1;
    }
    if(end_encoded_data < start_encoded_data)
      {
	rcs_print_error("Bad pointers (end_encoded_data=%p) < (start_encoded_data=%p)\n",end_encoded_data,start_encoded_data);
      }
    return (cms_parent->check_pointer (_pointer, _bytes));
}


int
CMS_PACKED_UPDATER::set_mode (CMS_UPDATER_MODE _mode)
{
  mode = _mode;
  switch (mode)
    {
    case CMS_NO_UPDATE:
      break;

    case CMS_ENCODE_DATA:
      start_encoded_data = (char *) encoded_data;
      encoding = 1;
      break;

    case CMS_DECODE_DATA:
      start_encoded_data = (char *) encoded_data;
      encoding = 0;
      break;

    case CMS_ENCODE_HEADER:
      start_encoded_data = (char *) encoded_header;
      encoding = 1;
      break;

    case CMS_DECODE_HEADER:
      start_encoded_data = (char *) encoded_header;
      encoding = 0;
      break;

    case CMS_ENCODE_QUEUING_HEADER:
      start_encoded_data = (char *) encoded_queuing_header;
      encoding = 1;
      break;

    case CMS_DECODE_QUEUING_HEADER:
      start_encoded_data = (char *) encoded_queuing_header;
      encoding = 0;
      break;

    default:
      rcs_print_error ("CMS updater in invalid mode.\n");
      return (-1);
    }
  return (0);
}

//  defined(ENABLE_PACKED_UP)

int
CMS_PACKED_UPDATER::get_pos()
{
  return ((int) (end_encoded_data-start_encoded_data));
}  

#else
#include "rcs_empty_source"
#endif

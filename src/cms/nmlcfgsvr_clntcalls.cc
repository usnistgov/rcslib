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
#endif

#if defined(ENABLE_RCS_NMLCFGSVR)

#ifdef HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "nmlcfgsvr_clntcalls_no_config.h"
#endif

#include "nmlcfgsvr_clntcalls.hh"

#include "cms_types.hh"		// CMS_CONFIG_LINELEN
#include "sokintrf.h"
#include "linklist.hh"
#include "rcs_prnt.hh"
#include "recvn.h"
#include "sendn.h"
#include "tcp_opts.hh"
#include "recvline.h"
#include "sendline.h"
#include "timer.hh"
#include "ntohhton.hh"

static char *last_svrhostport=0;
static char *last_svrhostportdup=0;
static const char *svr_hostname=0;
double last_timeout=-1.0;

static bool nmlcfgsvr_default_domain_set=false;
static char nmlcfgsvr_default_domain[0x200];
static size_t nmlcfgsvr_default_domain_len=0;

static char nmlcfgsvr_connect_sockerrbuf[0x200];

#define DEFAULT_NMLCFGSVR_PORT 11671

static unsigned short nmlcfgsvr_port= DEFAULT_NMLCFGSVR_PORT;

static bool need_new_svr_address=true;

static struct dl_sa *last_svrhost_dl_sa_ptr = 0;

#ifndef VXWORKS
struct hostent *nmlcfgsvr_hostentptr;
#endif

int nmlcfgsvr_use_ipv6 = 0;

static enum NMLCFGSVR_STATUS nmlcfgsvr_connect_status =NMLCFGSVR_STATUS_NOT_SET;


bool hostname_is_ip_address(const char *hostname_to_check)
{
  const char *hcptr = hostname_to_check;
  if(!*hcptr)
    {
      return false;
    }
  while(*hcptr)
    {
      char hc = *hcptr;
      if((hc < '0' || hc > '9') && hc != '.')
	{
	  return false;
	}
      hcptr++;
    }
  return true;
}

static void
setdomain(SOCKET s, double _timeout, 
	  char *recvstring, size_t recvstring_size,
	  char *sendstring, size_t sendstring_size);
  
static SOCKET
nmlcfgsvr_connect(const char *_svrhostport,
		  double _timeout, double *_timeout_ptr,
		  char *recvstring, size_t recvstring_size,
		  char *sendstring, size_t sendstring_size)
{
  int socket_fd;
  int sockerrno;
  const char *sockerrstr;
  struct dl_sa *cli_addr=0;
  char *timeout_string;
  char *colon_location=0;
  char *slash_location=0;
  char *cs_location=0;
  char *portstring=0;

  if(getenv("IPV6") || getenv("NML_IPV6") || getenv("CMS_IPV6") || getenv("NMLCFGSVR_IPV6"))
    {
      nmlcfgsvr_use_ipv6=1;
    }
  if (load_socket_interface () < 0)
    {
      rcs_print_error ("Can't load socket interface.\n");
      return((SOCKET)-1);
    }

  if(0 == last_svrhostport)
    {
      last_svrhostport=strdup(_svrhostport);
      last_svrhostportdup = strdup(last_svrhostport);
      if(!nmlcfgsvr_use_ipv6)
	{
	  colon_location = strchr(last_svrhostportdup,':');
	}
      else
	{
	  colon_location = 0;
	}
      slash_location = strchr(last_svrhostportdup,'/');
      if(colon_location && slash_location && slash_location < colon_location)
	{
	  cs_location = slash_location;
	}
      else if(colon_location)
	{
	  cs_location = colon_location;
	}
      else
	{
	  cs_location = slash_location;
	}
      nmlcfgsvr_port = DEFAULT_NMLCFGSVR_PORT;
      if(cs_location >= last_svrhostportdup)
	{
	  *cs_location =0;
	  portstring=cs_location+1;
	  if(*portstring)
	    {
	      if(*portstring != ':')
		{
		  nmlcfgsvr_port = (unsigned short) strtoul(portstring,0,0);
		}
	      if(_timeout < 0)
		{
		  if(!nmlcfgsvr_use_ipv6)
		    {
		      colon_location = strchr(cs_location+1,':');
		    }
		  else
		    {
		      colon_location = 0;
		    }
		  slash_location = strchr(last_svrhostportdup,'/');
		  if(colon_location && slash_location && slash_location < colon_location)
		    {
		      cs_location = slash_location;
		    }
		  else if(colon_location)
		    {
		      cs_location = colon_location;
		    }
		  else
		    {
		      cs_location = slash_location;
		    }

		  if(cs_location >= last_svrhostportdup)
		    {
		      timeout_string=cs_location+1;
		      if(*timeout_string && *timeout_string != ':')
			{
			  if((*timeout_string >= '0' && 
			      *timeout_string <= '9') ||
			     *timeout_string == '-' ||
			     *timeout_string == '+' )
			    {
			      _timeout= strtod(timeout_string,0);
			    }
			  else
			    {
			      rcs_print_error("Bad timeout_string = %s\n",
					      timeout_string);
			    }
			}
		    }
		}
	    }
	}
      svr_hostname = last_svrhostportdup;
      if(*svr_hostname == 0)
	{
	  svr_hostname = "127.0.0.1";
	}
      need_new_svr_address = true;
    }
  else if(!strcmp(_svrhostport,last_svrhostport))
    {
      if(_timeout< 0)
	{
	  _timeout = last_timeout;
	}
      need_new_svr_address = false;
    }
  else
    {
      free(last_svrhostport);
      if(last_svrhostportdup)
	{
	  free(last_svrhostportdup);
	}
      last_svrhostport=strdup(_svrhostport);
      last_svrhostportdup = strdup(last_svrhostport);
      if(!nmlcfgsvr_use_ipv6)
	{
	  colon_location = strchr(last_svrhostportdup,':');
	}
      else
	{
	  colon_location = 0;
	}
      slash_location = strchr(last_svrhostportdup,'/');
      if(colon_location && slash_location && slash_location < colon_location)
	{
	  cs_location = slash_location;
	}
      else if(colon_location)
	{
	  cs_location = colon_location;
	}
      else
	{
	  cs_location = slash_location;
	}
      nmlcfgsvr_port = DEFAULT_NMLCFGSVR_PORT;
      if(cs_location >= last_svrhostportdup)
	{
	  *cs_location =0;
	  portstring=cs_location+1;
	  if(*portstring)
	    {
	      if(*portstring != ':')
		{
		  nmlcfgsvr_port = (unsigned short) strtoul(portstring,0,0);
		}
	      if(_timeout < 0)
		{
		  if(!nmlcfgsvr_use_ipv6)
		    {
		      colon_location = strchr(cs_location+1,':');
		    }
		  else
		    {
		      colon_location = 0;
		    }
		  slash_location = strchr(last_svrhostportdup,'/');
		  if(colon_location && slash_location && slash_location < colon_location)
		    {
		      cs_location = slash_location;
		    }
		  else if(colon_location)
		    {
		      cs_location = colon_location;
		    }
		  else
		    {
		      cs_location = slash_location;
		    }
		  
		  if(cs_location >= last_svrhostportdup)
		    {
		      timeout_string=cs_location+1;
		      if(*timeout_string && *timeout_string != ':')
			{
			  _timeout= strtod(timeout_string,0);
			}
		    }
		}
	    }
	}
      svr_hostname = last_svrhostportdup;
      if(*svr_hostname == 0)
	{
	  svr_hostname = "127.0.0.1";
	}
      need_new_svr_address = true;
    }
  if(_timeout_ptr)
    {
      *_timeout_ptr=_timeout;
    }
  last_timeout=_timeout;
  if(0 == last_svrhost_dl_sa_ptr)
    {
      last_svrhost_dl_sa_ptr =
	dl_create_sa(svr_hostname,
		     nmlcfgsvr_port,
		     nmlcfgsvr_use_ipv6);
    }
  if(need_new_svr_address)
    {
      dl_sa_set_port(last_svrhost_dl_sa_ptr,nmlcfgsvr_port);
      dl_sa_set_host(last_svrhost_dl_sa_ptr,svr_hostname);
    }
  rcs_print_debug(PRINT_CMS_CONFIG_INFO,"_svrhostport=%s, last_svrhostport=%s, need_new_svr_address=%d, last_svrhostport_sa.host=%s, last_svrhostport_sa.port=%d\n",
		  _svrhostport,last_svrhostport,
		  (int)need_new_svr_address,
		  dl_sa_get_host(last_svrhost_dl_sa_ptr),
		  dl_sa_get_port(last_svrhost_dl_sa_ptr));
  
  socket_fd = (int) dl_tcp_socket (nmlcfgsvr_use_ipv6);
  if (socket_fd < 0)
    {
      sockerrno = dl_get_last_socket_error_int( socket_fd );
      sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,nmlcfgsvr_connect_sockerrbuf,sizeof(nmlcfgsvr_connect_sockerrbuf));
      rcs_print_error ("nmlcfgsvr_connect(%s): Error from socket() (errno = %d:%s)\n",
		       _svrhostport,sockerrno, sockerrstr);
      nmlcfgsvr_connect_status =NMLCFGSVR_CONNECT_FAILED;
      return((SOCKET)-1);
    }
  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "Setting socket options . . . \n");
  if (set_tcp_socket_options (socket_fd) < 0)
    {
      return((SOCKET)-1);
    }

  char *bind_to_host = 
    getenv("NML_BINDTO_HOST");
  cli_addr = dl_create_sa(bind_to_host,0,nmlcfgsvr_use_ipv6);  
  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "Binding (socket_fd=%d,cli_addr=%s port=%d . . . \n",
		   socket_fd,
		   dl_sa_get_host(cli_addr),
		   dl_sa_get_port(cli_addr));
  if (dl_bind (socket_fd, 
	       dl_sa_addr(cli_addr), 
	       dl_sa_len(cli_addr)) < 0)
    {
      sockerrno = dl_get_last_socket_error_int( socket_fd );
      sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,nmlcfgsvr_connect_sockerrbuf,sizeof(nmlcfgsvr_connect_sockerrbuf));
      rcs_print_error ("nmlcfgsvr_connect(%s): bind error %d = %s ,cli_addr=%s cli_addr port=%d\n", 
		       _svrhostport,sockerrno,sockerrstr,
		       dl_sa_get_host(cli_addr),
		       dl_sa_get_port(cli_addr));
      nmlcfgsvr_connect_status =NMLCFGSVR_CONNECT_FAILED;
      if(cli_addr)
	{
	  dl_free_sa(cli_addr);
	  cli_addr=0;
	}
      return((SOCKET)-1);
    }
  rcs_print_debug (PRINT_CMS_CONFIG_INFO, "Connecting . . .\n");
  int connect_ret=0;
  
  connect_ret = dl_connect_in_with_timeout(socket_fd, /* socket */
					   last_svrhost_dl_sa_ptr, /* svr address/port */
					   _timeout, /* timeout */
					   1, /* print errors */
					   1, /* reconnect after connection refused */
					   0, /* errcode ptr */
					   0, /* timedout ptr */
					   0, /* interrupt_op_ptr */
					   nmlcfgsvr_connect_sockerrbuf, /* buffer for error info */
					   sizeof(nmlcfgsvr_connect_sockerrbuf)); /* size of sockerr buf */
  if(connect_ret != 0)
    {
      rcs_print_error("nmlcfgsvr_connect(%s) failed.\n",_svrhostport);
      if(cli_addr)
	{
	  dl_free_sa(cli_addr);
	  cli_addr=0;
	}
      return (SOCKET) -1;
    }
  setdomain(socket_fd,_timeout,
	    recvstring,recvstring_size,
	    sendstring,sendstring_size);
  if(cli_addr)
    {
      dl_free_sa(cli_addr);
      cli_addr=0;
    }
  return(socket_fd);
}

//static char sendstring[CMS_CONFIG_LINELEN];
//static char recvstring[CMS_CONFIG_LINELEN];

static const char * const null_string="(null)";

void
nmlcfgsvr_set_default_domain(const char *_domain)
{
  if(_domain)
    {
      nmlcfgsvr_default_domain_set=true;
      if(strcmp(_domain,"~newnmldomain~"))
	{
	  SNPRINTF_FUNC (
			 SNPRINTF_ARGS(nmlcfgsvr_default_domain,sizeof(nmlcfgsvr_default_domain)),
			 "setdomain %s\r\n",
			 _domain);
	  nmlcfgsvr_default_domain_len=strlen(nmlcfgsvr_default_domain);
	}
      else
	{	    
	  SNPRINTF_FUNC (
			 SNPRINTF_ARGS(nmlcfgsvr_default_domain,sizeof(nmlcfgsvr_default_domain)),
			 "newdomain\r\n");
	  nmlcfgsvr_default_domain_len=strlen(nmlcfgsvr_default_domain);
	}
    }
  else
    {
      memset(nmlcfgsvr_default_domain,0,sizeof(nmlcfgsvr_default_domain));
      nmlcfgsvr_default_domain_len=0;
      nmlcfgsvr_default_domain_set=false;
    }
}
 
static void
setdomain(SOCKET s, double _timeout, 
	  char *recvstring, size_t recvstring_size,
	  char *, size_t)
{
  if(!nmlcfgsvr_default_domain_set)
    {
#if HAVE_GETENV
      char *nmlcfgsvr_domain_env = getenv("NMLCFGSVR_DOMAIN");
      memset(nmlcfgsvr_default_domain,0,sizeof(nmlcfgsvr_default_domain));
      nmlcfgsvr_default_domain_len=0;
      if(nmlcfgsvr_domain_env)
	{
	  if(strcmp(nmlcfgsvr_domain_env,"~newnmldomain~"))
	    {
	      SNPRINTF_FUNC (
			     SNPRINTF_ARGS(nmlcfgsvr_default_domain,sizeof(nmlcfgsvr_default_domain)),
			     "setdomain %s\r\n",
			     nmlcfgsvr_domain_env);
	      nmlcfgsvr_default_domain_len=strlen(nmlcfgsvr_default_domain);
	    }
	  else
	    {	    
	      SNPRINTF_FUNC (
			     SNPRINTF_ARGS(nmlcfgsvr_default_domain,sizeof(nmlcfgsvr_default_domain)),
			     "newdomain\r\n");
	      nmlcfgsvr_default_domain_len=strlen(nmlcfgsvr_default_domain);
	    }
	}	
      /* end of if HAVE_GETENV */
#endif
      nmlcfgsvr_default_domain_set=true;
    }  
  if(nmlcfgsvr_default_domain[0] && 
     strncmp(nmlcfgsvr_default_domain,"newdomain",9) == 0 && 
     s != ((SOCKET) -1))
    {
      if(sendn((int) s,nmlcfgsvr_default_domain,
	       (int) nmlcfgsvr_default_domain_len,0,_timeout) < 0)
	{
	  rcs_print_error
	    ("nmlcfgsvr_clntcalls :  setdomain(%d,%f): send(%d,%s,%d,0,%f) failed,\n",
	     s,_timeout, s,nmlcfgsvr_default_domain,
	     ((int)nmlcfgsvr_default_domain_len),
	     _timeout);
	  return;
	}
      memset(recvstring,0,recvstring_size);
      if(recvline((int) s,recvstring, (int) recvstring_size,0,_timeout,0) < 1)
	{
	  rcs_print_error
	    ("nmlcfgsvr_clntcalls :  setdomain(%d,%f): configuration server timedout or failed to reply\n",
	     s,_timeout);
	  return;
	}
      if(strncmp(recvstring,"OK",2))
	{
	  rcs_print_error("nmlcfgsvr: setdomain(): Server reported error: nmlcfgsvr_default_domain=\"%s\" recvstring=\"%s\"\n",nmlcfgsvr_default_domain,
			  recvstring);
	  return;
	}
      char *crnptr = strchr(recvstring,'\r');
      if(crnptr)
	{
	  *crnptr=0;
	}
      SNPRINTF_FUNC (
		     SNPRINTF_ARGS(nmlcfgsvr_default_domain,sizeof(nmlcfgsvr_default_domain)),
		     "setdomain %s\r\n",
		     recvstring+3);

#if HAVE_SETENV
      setenv("NMLCFGSVR_DOMAIN",recvstring+3,1);
#elif HAVE_PUTENV
      char putenv_buf[200];
      SNPRINTF_FUNC (
		     SNPRINTF_ARGS(putenv_buf,sizeof(putenv_buf)),
		     "NMLCFGSVR_DOMAIN=%s",
		     recvstring+3);
      putenv(putenv_buf);
#endif
      nmlcfgsvr_default_domain_len=strlen(nmlcfgsvr_default_domain);
    }
  if(nmlcfgsvr_default_domain[0] && s != ((SOCKET) -1))
    {
      if(sendn((int) s,nmlcfgsvr_default_domain,
	       (int) nmlcfgsvr_default_domain_len,0,_timeout) < 0)
	{
	  rcs_print_error
	    ("nmlcfgsvr_clntcalls :  setdomain(%d,%f): send(%d,%s,%d,0,%f) failed,\n",
	     s,_timeout, s,nmlcfgsvr_default_domain,
	     ((int) nmlcfgsvr_default_domain_len),
	     _timeout);
	  return;
	}
      memset(recvstring,0,recvstring_size);
      if(recvline((int) s,recvstring, (int) recvstring_size,0,_timeout,0) < 1)
	{
	  rcs_print_error
	    ("nmlcfgsvr_clntcalls :  setdomain(%d,%f): configuration server timedout or failed to reply\n",
	     s,_timeout);
	  return;
	}
      if(strncmp(recvstring,"OK",2))
	{
	  rcs_print_error("nmlcfgsvr: setdomain(): Server reported error: nmlcfgsvr_default_domain=\"%s\" recvstring=\"%s\"\n",nmlcfgsvr_default_domain,
			  recvstring);
	  return;
	}
    }
}
  
static const char *nmlcfgsvr_options_env=0;
static bool nmlcfgsvr_options_env_checked=false;

enum NMLCFGSVR_STATUS
nmlcfgsvr_create(const char *_svrhostport,
		 const char *_bufname,
		 const char *_procname,
		 char *_bufline,
		 char *_procline,
		 char *recvstring,size_t recvstring_size,
		 char *sendstring,size_t sendstring_size,
		 const char *_options,
		 double _timeout,
		 const char *_domainset,
		 bool print_errors
		 )
{
  char *ctype=0;
  const char *options_add="";
  char *colon_loc=0;
  char *slash_loc=0;
  rcs_print_debug (PRINT_CMS_CONFIG_INFO,
		   "nmlcfgsvr_create(%s,%s,%s,%p,%p,%s,%f) called.\n",
		   _svrhostport?_svrhostport:null_string,
		   _bufname?_bufname:null_string,
		   _procname?_procname:null_string,
		   _bufline,
		   _procline,
		   _options?_options:null_string,
		   _timeout);
  SOCKET  s = nmlcfgsvr_connect(_svrhostport,_timeout,&_timeout,
				recvstring,recvstring_size,
				sendstring,sendstring_size);
  if(((int)s) == -1)
    {
      if(((int)nmlcfgsvr_connect_status) < 0)
	{
	  return(nmlcfgsvr_connect_status);
	}
      else
	{
	  return(NMLCFGSVR_CONNECT_FAILED);
	}
    }
  const char *create_string =0;
  if(_options)
    {
      //printf("_options=%s\n",_options);
      create_string=  strstr(_options,"create=");
      //printf("create_string=%s\n",create_string);
      if(create_string)
	{
	  ctype = strdup(create_string+7);
	  colon_loc = ctype;
	  while((*colon_loc >= 'a' && *colon_loc <= 'z') ||
		(*colon_loc >= 'A' && *colon_loc <= 'Z') ||
		(*colon_loc >= '0' && *colon_loc <= '9') ||
		(*colon_loc == '_'))
	    {
	      colon_loc++;
	    }
	  *colon_loc=0;
	}
    }
  if(create_string == 0)
    {
      create_string =  strstr(_svrhostport,":create=");
      //printf("create_string=%s\n",create_string);
      if(create_string)
	{
	  ctype=strdup(create_string+8);
	  colon_loc = strchr(ctype,':');
	  if(colon_loc)
	    {
	      *colon_loc=0;
	    }
	}
    }
  if(create_string == 0)
    {
      create_string =  strstr(_svrhostport,"/create=");
      //printf("create_string=%s\n",create_string);
      if(create_string)
	{
	  ctype=strdup(create_string+8);
	  slash_loc = strchr(ctype,'/');
	  if(slash_loc)
	    {
	      *slash_loc=0;
	    }
	}
    }
  const char *options_string= strstr(_svrhostport,":options=");
  if(options_string)
    {
      options_add=options_string+9;
    }
  else
    {
      options_string= strstr(_svrhostport,":OPTIONS=");
      if(options_string)
	{
	  options_add=options_string+9;
	}
      else
	{
	  options_string= strstr(_svrhostport,"/OPTIONS=");
	  if(options_string)
	    {
	      options_add=options_string+9;
	    }
	  else
	    {
	      options_string= strstr(_svrhostport,"/options=");
	      if(options_string)
		{
		  options_add=options_string+9;
		}
	    }
	}
    }
  if(!nmlcfgsvr_options_env_checked)
    {
      nmlcfgsvr_options_env = getenv("NMLCFGSVR_OPTIONS");
      nmlcfgsvr_options_env_checked=true;
    }
  //printf("ctype=%s\n",ctype);
  if(!ctype)
    {
      if(!_options && !_domainset && (!_procname || _procname[0]) && !options_add && !nmlcfgsvr_options_env)
	{
	  SNPRINTF_FUNC (
			 SNPRINTF_ARGS(sendstring,sendstring_size),
			 "create %s your_address=%s\r\n",
			 _bufname,
			 dl_sa_get_host(last_svrhost_dl_sa_ptr)
			 );
		  }
      else
	{
	  SNPRINTF_FUNC (
			 SNPRINTF_ARGS(sendstring,sendstring_size),
			 "create %s %s %s%s %s %s %s your_address=%s\r\n",
			 _bufname,
			 ((_procname && _procname[0])?_procname:"nonameproc"),
			 (_domainset?"domain=":""),
			 (_domainset?_domainset:""),
			 (_options?_options:""),
			 (options_add?options_add:""),
			 (nmlcfgsvr_options_env?nmlcfgsvr_options_env:""),
			 dl_sa_get_host(last_svrhost_dl_sa_ptr)
			 );
	}
    }
  else
    {
      if(!_options && !_domainset && (!_procname || _procname[0]) && !options_add)
	{
	  SNPRINTF_FUNC (
			 SNPRINTF_ARGS(sendstring,sendstring_size),
			 "%s %s your_address=%s\r\n",ctype,_bufname,
			 dl_sa_get_host(last_svrhost_dl_sa_ptr));
	}
      else
	{
	  SNPRINTF_FUNC (
			 SNPRINTF_ARGS(sendstring,sendstring_size),
			 "%s %s %s %s%s %s %s your_address=%s\r\n",
			 ctype,
			 _bufname,
			 ((_procname && _procname[0])?_procname:"nonameproc"),
			 (_domainset?"domain=":""),
			 (_domainset?_domainset:""),
			 (_options?_options:""),
			 (options_add?options_add:""),
			 dl_sa_get_host(last_svrhost_dl_sa_ptr)
			 );
	}
      free(ctype);
      ctype=0;
    }
  sendn( (int) s,sendstring, (int) strlen(sendstring),0,_timeout);
  memset(recvstring,0,recvstring_size);
  if(recvline( (int) s,recvstring, (int) recvstring_size,0,_timeout,0) < 1)
    {
      if(print_errors)
	{
	  rcs_print_error
	    ("nmlcfgsvr_create(%s,%s,%s ...): configuration server timedout or failed to reply\n",
	     _svrhostport,_bufname,_procname);
	}
       dl_closesocket(s);
      return NMLCFGSVR_MISC_ERROR;
    }
  if(!strncmp(recvstring,"NO",2))
    {
      char *crptr = strchr(recvstring,'\r');
      if(crptr)
	{
	  *crptr=0;
	}
      crptr = strchr(sendstring,'\r');
      if(crptr)
	{
	  *crptr=0;
	}
      if(print_errors)
	{
	  rcs_print_error("nmlcfgsvr_create(%s,%s,%s ...): Server reported error: \"%s\", sendstring=\"%s\"\n",
			  _svrhostport,_bufname,_procname,recvstring,sendstring);
	}
      dl_closesocket(s);
      if(ctype && !strncmp(ctype,"get",3))
	{
	  return NMLCFGSVR_BUFFER_NOT_YET_REGISTERED;
	}
      return NMLCFGSVR_REPORTED_ERROR;
    }
  strncpy(_bufline,recvstring,CMS_CONFIG_LINELEN);
  memset(recvstring,0,recvstring_size);
  if(recvline( (int) s,recvstring, (int) recvstring_size,0,_timeout,0) < 1)
    {
      if(print_errors)
	{
	  rcs_print_error
	    ("nmlcfgsvr_create(%s,%s,%s ...): configuration server timedout or failed to reply\n",
	     _svrhostport,_bufname,_procname);
	}
       dl_closesocket(s);
      return NMLCFGSVR_MISC_ERROR;
    }
  strncpy(_procline,recvstring,CMS_CONFIG_LINELEN);
  dl_closesocket(s);
  return(NMLCFGSVR_CREATE_OK);
}

static const char *no_name_procname = "noname";

bool
nmlcfgsvr_check(const char *_svrhostport,
		const char *_bufname,
		char *recvstring, size_t recvstring_size,
		char *sendstring, size_t sendstring_size,
		const char *_procname,
		enum NMLCFGSVR_STATUS *_sts,
		double _timeout,
		const char *domainset
		)
{  
  SOCKET  s = nmlcfgsvr_connect(_svrhostport,_timeout,&_timeout,
				recvstring,recvstring_size,
				sendstring,sendstring_size);
  if(((int)s) == -1)
    {
      if(_sts)
	{
	  *_sts = nmlcfgsvr_connect_status;
	}
      return false;
    }
  const char *procname_to_send;
  if(_procname)
    {
      procname_to_send = _procname;
    }
  else
    {
      procname_to_send = no_name_procname;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(sendstring,sendstring_size),
		  "get %s %s %s%s\r\n",_bufname,procname_to_send,
		  (domainset?"domain=":""),
		  (domainset?domainset:""));
  sendn( (int) s,sendstring, (int) strlen(sendstring),0,_timeout);
  memset(recvstring,0,recvstring_size);
  if(recvline( (int) s,recvstring, (int) recvstring_size,0,_timeout,0) < 1)
    {
       rcs_print_error
	 ("nmlcfgsvr_check(%s,%s,%s ...): configuration server timedout or failed to reply\n",
	  _svrhostport,_bufname,_procname);
       dl_closesocket(s);
      if(_sts)
	{
	  *_sts = NMLCFGSVR_MISC_ERROR;
	}
      return false;
    }
  if(!strncmp(recvstring,"NO",2))
    {
      dl_closesocket(s);
      if(_sts)
	{
	  *_sts =NMLCFGSVR_BUFFER_NOT_YET_REGISTERED;
	}
      return false;
    }
  memset(recvstring,0,recvstring_size);
  if(recvline( (int) s,recvstring, (int) recvstring_size,0,_timeout,0) < 1)
    {
       rcs_print_error
	 ("nmlcfgsvr_check(%s,%s,%s ...): configuration server timedout or failed to reply\n",
	  _svrhostport,_bufname,_procname);
       dl_closesocket(s);
    }
  dl_closesocket(s);
  return(true);
}      

void
nmlcfgsvr_delete(const char *_svrhostport,
		 const char *_bufname,
		 char *recvstring, size_t recvstring_size,
		 char *sendstring, size_t sendstring_size,
		 const char *_procname,
		 enum NMLCFGSVR_STATUS *_sts,
		 double _timeout
		 )
{  
  SOCKET  s = nmlcfgsvr_connect(_svrhostport,_timeout,&_timeout,
				recvstring,recvstring_size,
				sendstring,sendstring_size);
  if(((int)s) == -1)
    {
      if(_sts)
	{
	  *_sts = nmlcfgsvr_connect_status;
	}
      return;
    }
  const char *procname_to_send;
  if(_procname)
    {
      procname_to_send = _procname;
    }
  else
    {
      procname_to_send = no_name_procname;
    }
  SNPRINTF_FUNC ( SNPRINTF_ARGS(recvstring,recvstring_size),
		  "delete %s %s\r\n",_bufname,procname_to_send);
  sendn( (int) s,recvstring, (int) strlen(recvstring),0,_timeout);
  memset(recvstring,0,recvstring_size);
  if(recvline( (int) s,recvstring, (int) recvstring_size,0,_timeout,0) < 1)
    {
       rcs_print_error
	 ("nmlcfgsvr_delete(%s,%s,%s ...): configuration server timedout or failed to reply\n",
	  _svrhostport,_bufname,_procname);
      if(_sts)
	{
	  *_sts = NMLCFGSVR_MISC_ERROR;
	}
      return;
    }
  if(!strncmp(recvstring,"NO",2))
    {
      dl_closesocket(s);
      if(_sts)
	{
	  *_sts =NMLCFGSVR_BUFFER_NOT_YET_REGISTERED;
	}
      return;
    }
  dl_closesocket(s);
}      


//  defined(ENABLE_NMLCFGSVR)

#else
#include "rcs_empty_source"
#endif

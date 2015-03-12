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

#define CMS_NETWORK_SOURCE

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "nmlcfgsvr_no_config.h"
#endif

#include "nml.hh"
#include "sokintrf.h"
#include "linklist.hh"
#include "rcs_prnt.hh"
#include "cms.hh"
#include "recvn.h"
#include "sendn.h"
#include "tcp_opts.hh"
#include "timer.hh"

#define FSYNC_NUM_MAX 1000
#define DEFAULT_LOCAL_PORT 11671
#define DEFAULT_FILE_SYNC_BASE "nmlcfgsvr_file_sync"

static char *prompt = 0;
static size_t promptlen = 0;
struct PROC_INFO;
static char line[CMS_CONFIG_LINELEN];
static char buffer_name[CMS_CONFIG_LINELEN];
static char process_name[CMS_CONFIG_LINELEN];
static char cmd[CMS_CONFIG_LINELEN];
static const char *my_ip_string = 0;
static char temp_string[2*CMS_CONFIG_LINELEN];

static char *myhostname = 0;
static struct in_addr my_in_addr;

static struct hostent *my_hostent_ptr = 0;
static RCS_LINKED_LIST *buffers_list = 0;
static const char *timeout_infinity_string = "INF";
static RCS_LINKED_LIST *process_defaults_list = 0;
static const char *nmlcfgcheckprocessline =
  "P nmlcfgsvr default REMOTE localhost RW 0 10.0 0 0 stoponconnref";
static double min_check_time = 10.0;
static double nmlcfgsvr_sendn_timeout = 120.0;
static bool debug_nmlcfgsvr = false;
static bool do_buffer_checks = false;
static bool check_fsync_file = true;
static bool do_file_sync = false;
static bool fsync_toggle = false;
static char *saveonexitfile = 0;
static bool bsem_needed_default=false;

// static bool do_net_sync=false;
// static RCS_LINKED_LIST *net_sync_list=0;


int default_buffer_size = 0x400;
int key = DEFAULT_LOCAL_PORT + 1;
int min_key;
int max_key;
int buffer_number = 1;
int port = DEFAULT_LOCAL_PORT + 1;
int max_port;
int min_port;
int start_key = 0;
short start_port = 0;
int max_key_or_port;
int min_key_or_port;
int max_buffer_number;
int min_buffer_number;
const char *file_sync_base = DEFAULT_FILE_SYNC_BASE;
const char *f1_sync_name = 0;
const char *f2_sync_name = 0;
static long local_port = DEFAULT_LOCAL_PORT;
static int blist_mod = 0;
static int p_default_mod = 0;
static bool preserve_process_data = false;

static struct dl_sa *myaddress_ptr = 0;
int global_use_ipv6 = 0;
static bool confirm_write_default=true;

#include <string>
#include <iostream>
#include <sstream>

static void
touch_running_file()
{
  FILE *f = fopen("nmlcfgsvr.running","w");
  if(f) {
    fprintf(f,"nmlcvgsvr is now running.\n");
    fclose(f);
  }
}

struct BUFFER_INFO
{
  BUFFER_INFO ();
  ~BUFFER_INFO ();
  char ClonedBufferName[CMS_CONFIG_LINELEN];
  char BufferName[CMS_CONFIG_LINELEN];
  char BufferHost[CMS_CONFIG_LINELEN];
  char BufferLine[CMS_CONFIG_LINELEN];
  char WordBuffer[CMS_CONFIG_LINELEN];
  char BufLineAdd[CMS_CONFIG_LINELEN];
  size_t size;
  size_t pre_queuemult_size;
  size_t BufferLineLen;
  int queue_len;
  int cnum;
  int successful_checks;
  double last_check_time;
  bool stored_by_startup_file;
  bool neutral;
  bool cloned;
  bool nodist;
  bool last_check_succeeded;
  char *ip_string;
  bool deleted;
  bool resurrected;
  bool isUDP; /* part of patch from blivingston at gdrs.com */
  int port;
  int key;
  int buffer_number;
  class RCS_LINKED_LIST *processes_list;
  struct dl_sa *svr_addr_ptr;
  char *domain;
  int use_ipv6;
  bool bsem_needed;
  double create_time;
  double time_since_create;
  bool confirm_write;
  std::string format_name;
  std::string format_source;
  std::string header;

private:
    BUFFER_INFO (const BUFFER_INFO &);
    BUFFER_INFO & operator= (const BUFFER_INFO &);
};

BUFFER_INFO::BUFFER_INFO ():
size (0),
pre_queuemult_size (0),
BufferLineLen (0),
queue_len (0),
cnum (0),
successful_checks (0),
last_check_time (0),
stored_by_startup_file (false),
neutral (false),
cloned (false),
nodist (false),
last_check_succeeded (false),
ip_string (0),
deleted (false),
resurrected (false),
isUDP (false),
port (0),
key (0),
buffer_number (0),
processes_list (0),
svr_addr_ptr (0), domain (0), 
use_ipv6 (global_use_ipv6),
bsem_needed(bsem_needed_default),
create_time(0),
time_since_create(0),
confirm_write(confirm_write_default)
{
  memset (BufferName, 0, sizeof (BufferName));
  memset (BufferHost, 0, sizeof (BufferHost));
  memset (BufferLine, 0, sizeof (BufferLine));
  memset (WordBuffer, 0, sizeof (WordBuffer));
  memset (BufLineAdd, 0, sizeof (BufLineAdd));

  size = 0;
  pre_queuemult_size = 0;
  BufferLineLen = 0;
  cnum = 0;
  ip_string = 0;
  successful_checks = 0;
  last_check_time = 0;
  create_time = etime();
  neutral = false;
  stored_by_startup_file = false;
  processes_list = 0;
  cloned = false;
  nodist = false;
  last_check_succeeded = false;
  queue_len = 0;
  deleted = false;
  resurrected = false;
  key = 0;
  port = 0;
  buffer_number = 0;
  domain = 0;
  confirm_write=confirm_write_default;
}

struct PROC_INFO
{
  PROC_INFO ();
  ~PROC_INFO ();
  char ProcessName[CMS_CONFIG_LINELEN];
  char ProcessLine[CMS_CONFIG_LINELEN];
  size_t ProcessLineLen;
  bool stored_by_startup_file;
  bool autogenerated;
  double timeout;
  char *ip_string;

private:
    PROC_INFO (const PROC_INFO &);
    PROC_INFO & operator= (const PROC_INFO &);

};

BUFFER_INFO::~BUFFER_INFO ()
{
  if (processes_list)
    {
      PROC_INFO *p = (PROC_INFO *) processes_list->get_head ();
      while (p)
	{
	  delete p;
	  processes_list->delete_current_node ();
	  p = (PROC_INFO *) processes_list->get_next ();
	}
      delete processes_list;
      processes_list = 0;
    }
  if (ip_string)
    {
      free ((void *) ip_string);
      ip_string = 0;
    }
  if(domain)
    {
      free ((void *) domain);
      domain=0;
    }
  if(svr_addr_ptr)
    {
      dl_free_sa(svr_addr_ptr);
      svr_addr_ptr=0;
    }
}


PROC_INFO::PROC_INFO ():
ProcessLineLen (0),
stored_by_startup_file (false),
autogenerated (false), timeout (0.0), ip_string (0)
{
  memset (ProcessName, 0, sizeof (ProcessName));
  memset (ProcessLine, 0, sizeof (ProcessLine));
  ProcessLineLen = 0;
  stored_by_startup_file = false;
  autogenerated = false;
  ip_string = 0;
  timeout = -1;
}

PROC_INFO::~PROC_INFO ()
{
  if (ip_string)
    {
      free (ip_string);
      ip_string = 0;
    }
}

static bool
simple_pattern_match (const char *_pattern, const char *_str)
{
  const char *s = _str;
  char *pattern_dup = strdup (_pattern);
  char *p = pattern_dup;
  const char *pstart = p;

  if(debug_nmlcfgsvr)
    {
      printf("comparing pattern %s with %s\n",
	     pattern_dup, _str);
    }
  if (!strcmp (p, "*"))
    {
      if(debug_nmlcfgsvr)
	{
	  printf("pattern matches anything.\n");
	}
      free (pattern_dup);
      return true;
    }
  while (*p != 0 && *p != '*')
    {
      p++;
    }
  if (p > pstart)
    {
      char oldp = *p;
      *p = 0;
      if ((oldp == '*'?
	   strncmp (pstart, s, p - pstart):
	   strcmp (pstart, s))
	   )
	{
	  if(debug_nmlcfgsvr)
	    {
	      printf("%s does not match %s\n",
		     s, pstart);
	    }
	  free (pattern_dup);
	  return false;
	}
      s += (p - pstart);
      p++;
      pstart = p;
    }
  while (*pstart)
    {
      while (*p != 0 && *p != '*')
	{
	  p++;
	}
      if (p > pstart)
	{
	  if (*p == '*')
	    {
	      *p = 0;
	      const char *new_s = strstr (s, pstart);
	      if (!new_s)
		{
		  if(debug_nmlcfgsvr)
		    {
		      printf("%s does not contain %s\n",
			     s, pstart);
		    }
		  free (pattern_dup);
		  return false;
		}
	      s = new_s + (p - pstart);
	      p++;
	      pstart = p;
	    }
	  else
	    {
	      size_t plen = (size_t) (p - pstart);
	      if (strlen (s) < plen)
		{
		  return false;
		}
	      const char *end_s = s + strlen (s) - plen;
	      if (strcmp (end_s, pstart))
		{
		  if(debug_nmlcfgsvr)
		    {
		      printf("%s does not end with %s\n",
			     s, pstart);
		    }
		  free (pattern_dup);
		  return false;
		}
	      else
		{
		  if(debug_nmlcfgsvr)
		    {
		      printf("%s matches pattern %s\n",
			     _str, _pattern);
		    }
		  free (pattern_dup);
		  return true;
		}
	    }
	}
      else
	{
	  p++;
	  pstart = p;
	}
    }
  if(debug_nmlcfgsvr)
    {
      printf("%s matches pattern %s\n", _str, _pattern);
    }
  free (pattern_dup);
  return (true);
}

class TEMPLATE_INFO
{
public:
  char buffer_name_pattern[0x400];
  char process_name_pattern[0x400];
  char ip_string_pattern[0x400];
  char buffer_line_additions[CMS_CONFIG_LINELEN];
  size_t buffer_size;
  int queue_len;
  bool nuetral;
  char BufferType[CMS_CONFIG_LINELEN];
  bool buffer_size_set;
  bool buffer_line_additions_set;
  bool queue_len_set;
};

RCS_LINKED_LIST *templates_list = 0;

static inline bool
ulong_less_than_long (unsigned long ul, long l)
{
  if (l <= 0)
    {
      return false;
    }
  unsigned long ul2 = (unsigned long) l;
  return (ul < ul2);
}

enum CLIENT_REQUEST_TYPE
{
  INVALID_CLIENT_REQUEST_TYPE = 0,
  GET_CLIENT_REQUEST_TYPE,
  WAIT_CLIENT_REQUEST_TYPE,
  STORE_PROCESS_LINE_CLIENT_REQUEST_TYPE,
  LIST_CLIENT_REQUEST_TYPE,
  DELETE_BUFFER_CLIENT_REQUEST_TYPE,
  LIST_TEMPLATES_CLIENT_REQUEST_TYPE,
  CREATE_BUFFER_CLIENT_REQUEST_TYPE,
  CREATE_EXCLUSIVE_BUFFER_CLIENT_REQUEST_TYPE,
  STORE_BUFFER_LINE_CLIENT_REQUEST_TYPE,
  ADD_TEMPLATE_CLIENT_REQUEST_TYPE,
  SEND_HELP_CLIENT_REQUEST_TYPE,
  LIST_PROCS_FOR_BUF_CLIENT_REQUEST_TYPE,
};

class CLIENT_INFO
{
public:
  CLIENT_INFO ();

  ~CLIENT_INFO()
  {
    if(((long)client_socket) > 0)
      {
	dl_closesocket(client_socket);
	client_socket= ((SOCKET)0);
      }
    if(client_address_ptr)
      {
	dl_free_sa(client_address_ptr);
	client_address_ptr=0;
      }
    if(permanent_domain)
      {
	if(domain_this_line == permanent_domain)
	  {
	    domain_this_line=0;
	  }
	free(permanent_domain);
	permanent_domain=0;
	
      }
    if(domain_this_line)
      {
	free(domain_this_line);
	domain_this_line=0;
      }
    if(ip_string)
      {
	free(ip_string);
	ip_string=0;
      }
    domain=0;
  }

  struct dl_sa *client_address_ptr;
  int use_ipv6;
  SOCKET client_socket;
  char ring_buffer[0x2000];
  char *startptr;
  char *endptr;
  char *wait_for_line;
  bool quit;
  int linenumber;
  int port_to_use;
  bool willstart;
  bool port_to_use_set;
  int set_to_server;
  size_t max_size_from_format;
  char *send_buf;
  size_t send_buf_size;
  char *end_of_sent_data;
  char *end_of_appended_data;
  size_t space_available;
  size_t bytes_sent;
  int bytes_to_send;
  bool have_unsent_data;
  int last_send_error_int;
  int last_send_ret;
  const char *sockerrstr;
  char sockerrbuf[256];
  size_t space_needed;
  size_t add_space;
  size_t old_send_buf_size;
  void *old_send_buf_ptr;
  char *ip_string;
  enum CLIENT_REQUEST_TYPE req_type;
  double timeout;
  char *domain;
  char *permanent_domain;
  char *domain_this_line;
  bool my_ip_string_for_this_client_set;
  char my_ip_string_for_this_client[CMS_CONFIG_LINELEN];
  char ip_subst_buf[CMS_CONFIG_LINELEN];
  bool bsem_needed;
  bool bsem_needed_set;
  std::string format_name;
  std::string format_source;
  std::string header;

  void AppendData (const void *vptr, int n);
  void Send (const void *vptr, int n);
  void Send_Old (void);

private:
    CLIENT_INFO (const CLIENT_INFO & _ci_ref);
    CLIENT_INFO & operator= (const CLIENT_INFO & _ci_ref);
};

void
CLIENT_INFO::AppendData (const void *vptr, int n)
{
  if (send_buf == 0)
    {
      send_buf_size = n + 0x2000 - (n % 0x2000);
      send_buf = (char *) malloc (send_buf_size);
      space_available = send_buf_size - n - 1;
      bytes_to_send = n;
      bytes_sent = 0;
      end_of_sent_data = (char *) send_buf;
      end_of_appended_data = (char *) send_buf;
    }
  else if (ulong_less_than_long ((unsigned long) space_available, n))
    {
      space_needed = n - space_available;
      add_space = (space_needed) + 0x2000 - (space_needed % 0x2000);
      old_send_buf_size = send_buf_size;
      send_buf_size += add_space;
      old_send_buf_ptr = (void *) send_buf;
      send_buf = (char *) realloc (old_send_buf_ptr, send_buf_size);
      end_of_appended_data = (char *) send_buf + bytes_sent + bytes_to_send;
      bytes_to_send += n;
      space_available += add_space - n;
      end_of_sent_data = (char *) send_buf + bytes_sent;
    }
  else
    {
      space_available -= n;
      bytes_to_send += n;
    }
  have_unsent_data = true;
  memcpy (end_of_appended_data, vptr, n);
  end_of_appended_data += n;
}

void
CLIENT_INFO::Send_Old ()
{
  if (!have_unsent_data)
    {
      return;
    }
  if (((long) client_socket) <= 0)
    {
      rcs_print_error ("Bad socket.\n");
      return;
    }
  if (bytes_to_send <= 0)
    {
      rcs_print_error ("bytes_to_send=%d\n", bytes_to_send);
      return;
    }
  if (end_of_sent_data == 0)
    {
      rcs_print_error ("end_of_sent_data=%p\n", end_of_sent_data);
      return;
    }
  last_send_ret = dl_send (client_socket, end_of_sent_data, bytes_to_send, 0);
  if (last_send_ret <= 0)
    {
      last_send_error_int = (int) dl_get_last_socket_error_int ((int) client_socket);
      if (!dl_socket_error_was_would_block
	  ((int) client_socket, last_send_error_int))
	{
	  sockerrstr =
	    dl_get_last_socket_error_string ( (int) client_socket,
					     last_send_error_int, sockerrbuf,
					     sizeof (sockerrbuf));
	  rcs_print_error ("send failed. %d %s\n", last_send_error_int,
			   sockerrstr);
	  quit = true;
	  return;
	}
    }
  else if (last_send_ret < bytes_to_send)
    {
      end_of_sent_data += last_send_ret;
      bytes_to_send -= last_send_ret;
    }
  else
    {
      end_of_sent_data = send_buf;
      end_of_appended_data = (char *) send_buf;
      space_available = send_buf_size;
      bytes_to_send = 0;
      bytes_sent = 0;
      have_unsent_data = false;
    }
}

void
CLIENT_INFO::Send (const void *vptr, int n)
{
  if(debug_nmlcfgsvr)
    {
      printf ("Sending %d bytes (%s) to %s:%d\n",
	      n, (char *) vptr,
	      dl_sa_get_host (client_address_ptr),
	      dl_sa_get_port (client_address_ptr));
    }
  if(my_ip_string_for_this_client_set &&
     strstr((const char *)vptr,my_ip_string) &&
     strcmp(my_ip_string,my_ip_string_for_this_client) &&
     n > 0 && n < ((int) sizeof(ip_subst_buf)))
    {
      memset(ip_subst_buf,0,sizeof(ip_subst_buf));
      char *subst_out_cptr = ip_subst_buf;
      char *subst_out_cptr_end = ip_subst_buf + sizeof(ip_subst_buf);
      const char *subst_in_cptr = (const char *) vptr;
      const char *subst_in_cptr_end = ((const char *) vptr) + n;
      int nleft = n;
      size_t my_ip_string_len =strlen(my_ip_string);
      size_t subst_my_ip_string_len =strlen(my_ip_string_for_this_client);
      int new_n = 0;
      while(subst_in_cptr < subst_in_cptr_end &&
	    subst_out_cptr < subst_out_cptr_end)
	{
	  const char *ip_ptr = 
	    strstr(subst_in_cptr,my_ip_string);
	  if(!ip_ptr)
	    {	    
	      memcpy(subst_out_cptr,
		     subst_in_cptr,
		     nleft);
	      new_n+= nleft;
	      break;
	    }
	  else
	    {
	      long ntocopy= (long) (ip_ptr - subst_in_cptr) ;
	      if(ntocopy > nleft)
		{
		  memcpy(subst_out_cptr,
			 subst_in_cptr,
			 nleft);
		  new_n+= nleft;
		  break;
		}
	      new_n += ntocopy;
	      memcpy(subst_out_cptr,
		     subst_in_cptr,
		     ntocopy);
	      nleft -= (int) ntocopy;
	      nleft -= (int) my_ip_string_len;
	      subst_out_cptr += ntocopy;
	      subst_in_cptr+= ntocopy;
	      memcpy(subst_out_cptr,
		     my_ip_string_for_this_client,
		     subst_my_ip_string_len);
	      subst_in_cptr += my_ip_string_len;
	      subst_out_cptr += subst_my_ip_string_len;
	      new_n += (int) subst_my_ip_string_len;
	    }
	}
      vptr = ip_subst_buf;
      n = new_n;
      if(debug_nmlcfgsvr)
	{
	  printf ("After substitution  %d bytes (%s) to %s:%d\n",
		  n, (char *) vptr,
		  dl_sa_get_host (client_address_ptr),
		  dl_sa_get_port (client_address_ptr));
	}
    }

  if (have_unsent_data)
    {
      Send_Old ();
    }
  if (((long)client_socket) <= 0)
    {
      rcs_print_error ("Bad socket.\n");
      return;
    }
  if (n <= 0)
    {
      rcs_print_error ("n=%d\n", n);
    }
  if (have_unsent_data)
    {
      AppendData (vptr, n);
    }
  else
    {
      last_send_ret = dl_send (client_socket, (char *) vptr, n, 0);
      if (last_send_ret < 0)
	{
	  last_send_error_int = (int) dl_get_last_socket_error_int ( (int) client_socket);
	  if (!dl_socket_error_was_would_block
	      ((int) client_socket, last_send_error_int))
	    {
	      sockerrstr =
		dl_get_last_socket_error_string ((int) client_socket,
						 last_send_error_int,
						 sockerrbuf,
						 sizeof (sockerrbuf));
	      rcs_print_error ("send failed. %d %s\n", last_send_error_int,
			       sockerrstr);
	      quit = true;
	      return;
	    }
	  else
	    {
	      AppendData (vptr, n);
	    }
	}
      else if (last_send_ret > 0 && last_send_ret < n)
	{
	  const char *cptr = (const char *) vptr;
	  cptr += last_send_ret;
	  AppendData (cptr, (n - last_send_ret));
	}
      else if (last_send_ret == 0)
	{
	  AppendData (vptr, n);
	}
    }
}


CLIENT_INFO::CLIENT_INFO ():
client_address_ptr (0),
use_ipv6 (global_use_ipv6),
client_socket (0),
startptr (0),
endptr (0),
wait_for_line (0),
quit (false),
linenumber (0),
port_to_use (0),
willstart (false),
port_to_use_set (false),
set_to_server (0),
max_size_from_format (0),
send_buf (0),
send_buf_size (0),
end_of_sent_data (0),
end_of_appended_data (0),
space_available (0),
bytes_sent (0),
bytes_to_send (0),
have_unsent_data (false),
last_send_error_int (0),
last_send_ret (0),
sockerrstr (0),
space_needed (0),
add_space (0),
old_send_buf_size (0),
old_send_buf_ptr (0),
ip_string (0),
req_type (INVALID_CLIENT_REQUEST_TYPE),
timeout (0), domain (0), permanent_domain (0), domain_this_line (0),
my_ip_string_for_this_client_set(false),
bsem_needed(bsem_needed_default),
bsem_needed_set(false)
{
  memset(my_ip_string_for_this_client,0,sizeof(my_ip_string_for_this_client));
  memset(ip_subst_buf,0,sizeof(ip_subst_buf));
  memset (ring_buffer, 0, sizeof (ring_buffer));
  startptr = endptr = ring_buffer;
  quit = false;
  linenumber = 0;
  willstart = false;
  port_to_use = 0;
  port_to_use_set = false;
  max_size_from_format = 0;
  set_to_server = 0;
  send_buf = 0;
  send_buf_size = 0;
  end_of_sent_data = 0;
  bytes_to_send = 0;
  have_unsent_data = false;
  sockerrstr = 0;
  wait_for_line = 0;
  ip_string = 0;
  req_type = INVALID_CLIENT_REQUEST_TYPE;
  timeout = -1;
  client_address_ptr = dl_create_sa (0, 0, use_ipv6);
}

int fsync_num = -1;

static int last_blist_mod_written = -1;
static int last_p_default_list_mod_written = -1;

static void
write_fsync_file (const char *fname)
{
  FILE *fp = 0;
  BUFFER_INFO *binfo = 0;
  if (!fname)
    {
      return;
    }
  if (last_blist_mod_written == blist_mod &&
      last_p_default_list_mod_written == p_default_mod)
    {
      return;
    }
  last_blist_mod_written = blist_mod;
  last_p_default_list_mod_written = p_default_mod;
  fp = fopen (fname, "w");
  if (!fp)
    {
      fprintf (stderr, "Failed to open %s for writing. -- %d %s\n",
	       fname, errno, strerror (errno));
      return;
    }
  PROC_INFO *p = 0;
  RCS_LINKED_LIST_NODE *temp_current = 0;
  RCS_LINKED_LIST_NODE *temp_current2 = 0;
  fsync_num++;
  if (fsync_num < 1)
    {
      fsync_num = 1;
    }
  if (fsync_num >= FSYNC_NUM_MAX)
    {
      fsync_num = 1;
    }
  
  std::ostringstream temp;
  temp << "#NMLCFGSVR_FSYNC_START " << fsync_num << std::endl;
  fputs (temp.str().c_str(), fp);
  if (buffers_list)
    {
      binfo = (BUFFER_INFO *)
	buffers_list->get_head_with_external_current (&temp_current);
      while (binfo)
	{
	  fputs (binfo->BufferLine, fp);
	  binfo = (BUFFER_INFO *)
	    buffers_list->get_next_with_external_current (&temp_current);
	}
      temp_current = 0;
      temp_current2 = 0;
      binfo = 0;
      if (preserve_process_data)
	{
	  binfo = (BUFFER_INFO *)
	    buffers_list->get_head_with_external_current (&temp_current);
	  while (binfo)
	    {
	      if (binfo->processes_list)
		{
		  p = (PROC_INFO *)
		    binfo->processes_list->
		    get_head_with_external_current (&temp_current2);
		  while (p)
		    {
		      fputs (p->ProcessLine, fp);
		      p = (PROC_INFO *)
			binfo->processes_list->
			get_next_with_external_current (&temp_current2);
		    }
		}
	      binfo = (BUFFER_INFO *)
		buffers_list->get_next_with_external_current (&temp_current);
	    }
	}
    }
  temp_current2 = 0;
  if (process_defaults_list != 0)
    {
      p = (PROC_INFO *)
	process_defaults_list->
	get_head_with_external_current (&temp_current2);
      while (p)
	{
	  fputs (p->ProcessLine, fp);
	  p = (PROC_INFO *)
	    process_defaults_list->
	    get_next_with_external_current (&temp_current2);
	}
    }
  temp.str("");
  temp << "#NMLCFGSVR_FSYNC_END " << fsync_num << std::endl;
  fputs (temp.str().c_str(), fp);
  fflush (fp);
  fclose (fp);
}

static void
write_sync_file ()
{
  if (!f1_sync_name || !f2_sync_name || !do_file_sync)
    {
      return;
    }
  if (last_blist_mod_written == blist_mod &&
      last_p_default_list_mod_written == p_default_mod)
    {
      return;
    }
  if (fsync_toggle)
    {
      write_fsync_file (f1_sync_name);
    }
  else
    {
      write_fsync_file (f2_sync_name);
    }
#if HAVE_SYNC
  sync ();
#endif
  fsync_toggle = !fsync_toggle;
}

#ifndef __unused_parameter
#ifdef __GNUC__
#if (__GNUC__ >= 3 ) && !defined(MS_WINDOWS_API)
#define __unused_parameter__ __attribute__ ((unused))
#else
#define __unused_parameter__
#endif
#else
#define __unused_parameter__
#endif
#endif


#ifdef SIGPIPE
int sigpipe_count = 0;
static void
handle_pipe_error (__unused_parameter__ int id)
{
  sigpipe_count++;
  rcs_print_error ("SIGPIPE intercepted.\n");
}
#endif

bool nmlcfgsvr_quit = false;


static void
handle_sigint (__unused_parameter__ int id)
{
  nmlcfgsvr_quit = true;
  //  rcs_print_error ("SIGINT intercepted.\n");
}

static void
set_ip_strings (CLIENT_INFO * client, BUFFER_INFO * binfo, PROC_INFO * pinfo)
{
  const char *ip_string = my_ip_string;
  if (client && client->ip_string && strcmp(client->ip_string, "127.0.0.1"))
    {
      ip_string = client->ip_string;
    }
  else
    {
      if (client)
	{
	  ip_string = dl_sa_get_host (client->client_address_ptr);
	}
      if (!strcmp (ip_string, "127.0.0.1"))
	{
	  ip_string = my_ip_string;
	}
      if (client)
	{
	  client->ip_string = strdup (ip_string);
	}
    }
  if(binfo)
    {
      if(binfo->ip_string && strcmp(binfo->ip_string,ip_string))
	{
	  free(binfo->ip_string);
	  binfo->ip_string=0;
	}
      if(!binfo->ip_string)
	{
	  binfo->ip_string = strdup (ip_string);
	}
    }

  if(pinfo)
    {
      if(pinfo->ip_string && strcmp(pinfo->ip_string,ip_string))
	{
	  free(pinfo->ip_string);
	  pinfo->ip_string = 0;
	}
      if(!pinfo->ip_string)
	{
	  pinfo->ip_string = strdup (ip_string);
	}
    }
}

static void
get_process_line (CLIENT_INFO * client, 
		  BUFFER_INFO * binfo,
		  const char *_ProcessName, 
		  const char *_BufferName,
		  bool masterserverspawner)
{
  bool procline_found = false;
  PROC_INFO *p;
  if (!binfo || !_ProcessName || !client)
    {
      return;
    }
  const char *process_name_to_use = _ProcessName;
  if (_ProcessName[0] == 0 || strchr (_ProcessName, '=')
      || strchr (_ProcessName, '*'))
    {
      process_name_to_use = "default";
    }
  else
    {
      if (binfo->processes_list != 0)
	{
	  p = (PROC_INFO *) binfo->processes_list->get_head ();
	  while (p != 0 && !procline_found)
	    {
	      if (!strcmp (_ProcessName, p->ProcessName))
		{
		  if (do_file_sync)
		    {
		      write_sync_file ();
		    }
		  client->Send (p->ProcessLine, (int) p->ProcessLineLen);
		  procline_found = true;
		  break;
		}
	      p = (PROC_INFO *) binfo->processes_list->get_next ();
	    }
	}
      if (!procline_found && process_defaults_list != 0)
	{
	  p = (PROC_INFO *) process_defaults_list->get_head ();
	  while (p != 0 && !procline_found)
	    {
	      if (!strcmp (_ProcessName, p->ProcessName))
		{
		  if (do_file_sync)
		    {
		      write_sync_file ();
		    }
		  client->Send (p->ProcessLine, (int)  p->ProcessLineLen);
		  procline_found = true;
		  break;
		}
	      p = (PROC_INFO *) binfo->processes_list->get_next ();
	    }
	}
    }
  const char *timeout_ptr = strstr (line, "timeout=");
  if (timeout_ptr)
    {
      if (!strcmp (timeout_ptr, "timeout=INF"))
	{
	  client->timeout = -1;
	}
      else
	{
	  client->timeout = strtod (timeout_ptr + 8, 0);
	}
    }
  p = new PROC_INFO ();
  p->autogenerated = true;
  binfo->cnum++;
  strcpy (p->ProcessName, process_name_to_use);
  const char *timeout_string = timeout_infinity_string;
  char timeout_buf[40];
  if (client->timeout > 0)
    {
      sprintf(timeout_buf,"%f",client->timeout);
      timeout_string = timeout_buf;
    }
  p->timeout = client->timeout;
  const char *proc_type = "AUTO";
  set_ip_strings (client, 0, p);
  if (client->ip_string && binfo->ip_string &&
      !strcmp (client->ip_string, binfo->ip_string))
    {
      proc_type = "LOCAL";
    }

  char process_line_additions[CMS_CONFIG_LINELEN];
  const char *plineadd_string = strstr (line, "plineadd=");
  char endchar = ' ';
  char *tp = process_line_additions;
  memset (process_line_additions, 0, sizeof (process_line_additions));
  if (plineadd_string)
    {
      plineadd_string += 9;
      if (*plineadd_string == '\"' || *plineadd_string == '\'')
	{
	  endchar = *plineadd_string;
	  plineadd_string++;
	}
      while (*plineadd_string && *plineadd_string != endchar &&
	     tp <
	     process_line_additions + sizeof (process_line_additions))
	{
	  *tp = *plineadd_string;
	  tp++;
	  plineadd_string++;
	}
    }

  if (!masterserverspawner && client->set_to_server != 1)
    {
      if (client)
	{
	  SNPRINTF_FUNC (SNPRINTF_ARGS(p->ProcessLine,sizeof(p->ProcessLine)),
		   "P \t%s \t%s \t%s \t%s \tRW \t0 \t%s \t0 \t%d waitformaster %s \r\n",
			 process_name_to_use, _BufferName, proc_type,
			 client->ip_string, timeout_string, binfo->cnum,
			 process_line_additions);
	}
    }
  else
    {
      int serverspawnnum = 3;
      if (binfo->nodist)
	{
	  serverspawnnum = 0;
	}
      if (client->willstart && !binfo->nodist)
	{
	  serverspawnnum = 2;
	}
      if (client->set_to_server > 0)
	{
	  serverspawnnum = client->set_to_server;
	}
      else if (client->set_to_server == -1)
	{
	  serverspawnnum = 0;
	}
      SNPRINTF_FUNC (SNPRINTF_ARGS(p->ProcessLine,sizeof(p->ProcessLine)),
		     "P \t%s \t%s \tLOCAL \t%s \tRW \t%d \t%s \t1 \t%d %s\r\n",
		     process_name_to_use, _BufferName, client->ip_string,
		     serverspawnnum, timeout_string, binfo->cnum,
		     process_line_additions);
    }
  p->ProcessLineLen = strlen (p->ProcessLine);
  if (client)
    {
      if (do_file_sync)
	{
	  write_sync_file ();
	}
      client->Send (p->ProcessLine, (int) p->ProcessLineLen);
    }
  if (preserve_process_data)
    {
      if (binfo->processes_list == 0)
	{
	  binfo->processes_list = new RCS_LINKED_LIST ();
	}
      binfo->processes_list->store_at_tail (p, sizeof (PROC_INFO), 0);
    }
  else
    {
      delete p;
      p = 0;
    }
}

static bool
buffer_check (BUFFER_INFO * binfo)
{
  if (!binfo)
    {
      return false;
    }
  if (!do_buffer_checks)
    {
      return true;
    }
  if (binfo->nodist)
    {
      return true;
    }
  if(debug_nmlcfgsvr)
    {
      printf("buffer_check(%s)\n", binfo->BufferName);
    }
  double cur_time = etime ();
  binfo->time_since_create = cur_time - binfo->create_time;
  if(debug_nmlcfgsvr)
    {
      printf("buffer_check:time_since_create=%f, min_check_time=%f\n", 
	     binfo->time_since_create,
	     min_check_time);
    }
  if(binfo->time_since_create < min_check_time)
    {
      return true;
    }
  double time_since_last_check = cur_time - binfo->last_check_time;
  if(debug_nmlcfgsvr)
    {
      printf("buffer_check:time_since_last_check=%f, min_check_time=%f\n", 
	     time_since_last_check,
	     min_check_time);
    }
  if (binfo->successful_checks > 0 &&
      binfo->last_check_succeeded && time_since_last_check < min_check_time)
    {
      return true;
    }
#if !defined(DISABLE_RCS_PRINT)
  RCS_PRINT_DESTINATION_TYPE rdest = get_rcs_print_destination ();
#if !defined(DISABLE_RCS_DEBUG_PRINT)
  long rpflags = rcs_print_mode_flags;
  if (rcs_print_mode_flags == 1 && !debug_nmlcfgsvr)
    {
      //set_rcs_print_destination (RCS_PRINT_TO_NULL);
      clear_rcs_print_flag (PRINT_EVERYTHING);
    }
#else
  //  set_rcs_print_destination (RCS_PRINT_TO_NULL);
#endif
#endif

  bool check_succeeded = false;
  NML *nmltemp =
    new NML ((char *) binfo->BufferLine, (char *) nmlcfgcheckprocessline);
  nmltemp->ignore_format_chain = true;
  if (nmltemp->valid ())
    {
      if(debug_nmlcfgsvr)
	{
	  printf("buffer_check(%s): check_succeeded\n", binfo->BufferName);
	}
      check_succeeded = true;
    }
  delete nmltemp;
#if !defined(DISABLE_RCS_DEBUG_PRINT) && !defined(DISABLE_RCS_PRINT)
  set_rcs_print_flag (rpflags);
#endif
#if !defined(DISABLE_RCS_PRINT)
  set_rcs_print_destination (rdest);
#endif
  if (check_succeeded)
    {
      binfo->successful_checks++;
      binfo->last_check_time = etime ();
    }
  binfo->last_check_succeeded = check_succeeded;
  if (!check_succeeded)
    {
      printf ("%s failed buffer_check.\n", binfo->BufferName);
    }
  return check_succeeded;
}

static void
set_deleted_status (BUFFER_INFO * binfo)
{
  if (!binfo->deleted)
    {
      binfo->deleted =
	(NULL != strstr (binfo->BufferLine, "nmlcfgsvr-deleted=true"));
    }
  if (!binfo->deleted)
    {
      binfo->deleted = true;
      blist_mod++;
      if (debug_nmlcfgsvr)
	{
	  printf ("deleting %s\n", binfo->BufferName);
	}
      binfo->BufferLineLen = strlen (binfo->BufferLine);
      if ((binfo->BufferLine[binfo->BufferLineLen] == '\r' ||
	   binfo->BufferLine[binfo->BufferLineLen] == '\n'))
	{
	  binfo->BufferLine[binfo->BufferLineLen] = 0;
	}
      if (binfo->BufferLineLen >= 1 &&
	  (binfo->BufferLine[binfo->BufferLineLen - 1] == '\r' ||
	   binfo->BufferLine[binfo->BufferLineLen - 1] == '\n'))
	{
	  binfo->BufferLine[binfo->BufferLineLen - 1] = 0;
	}
      if (binfo->BufferLineLen >= 2 &&
	  (binfo->BufferLine[binfo->BufferLineLen - 2] == '\r' ||
	   binfo->BufferLine[binfo->BufferLineLen - 2] == '\n'))
	{
	  binfo->BufferLine[binfo->BufferLineLen - 2] = 0;
	}
      strcat (binfo->BufferLine, " nmlcfgsvr-deleted=true\r\n");
      binfo->BufferLineLen = strlen (binfo->BufferLine);
    }
}

static bool
domains_match (BUFFER_INFO * binfo, CLIENT_INFO * client)
{
  if (!binfo || !client)
    {
      return false;
    }
  else if (!binfo->domain && !client->domain)
    {
      return true;
    }
  else if (client->domain && client->domain[0] == '*')
    {
      return true;
    }
  else if (client->domain && client->domain[0] == '!')
    {
      const char *start_d = &(client->domain[1]);
      const char *end_d = &(client->domain[1]);
      while (*start_d && *start_d != ' ' && *start_d != '\r'
	     && *start_d != '\t' && *start_d != '\n')
	{
	  while (*end_d && *end_d != ',' && *end_d != ' ' && *end_d != '\r'
		 && *end_d != '\t' && *end_d != '\n')
	    {
	      end_d++;
	    }
	  if (!binfo->domain && start_d == end_d)
	    {
	      return false;
	    }
	  else if (binfo->domain && start_d != end_d
		   && !strncmp (binfo->domain, start_d, end_d - start_d))
	    {
	      return false;
	    }
	  if (*end_d == ',')
	    {
	      end_d++;
	    }
	  start_d = end_d;
	}
      return true;
    }
  else if (!binfo->domain || !client->domain)
    {
      return false;
    }
  else
    {
      return simple_pattern_match (client->domain, binfo->domain);
    }
}

static void
get_buffer_info (CLIENT_INFO * client)
{
  bool foundit = false;
  BUFFER_INFO *binfo = 0;

  if (client)
    {
      client->req_type = GET_CLIENT_REQUEST_TYPE;
    }
  if (buffers_list)
    {
      binfo = (BUFFER_INFO *) buffers_list->get_head ();
      while (binfo)
	{
	  if (!domains_match (binfo, client))
	    {
	      binfo = (BUFFER_INFO *) buffers_list->get_next ();
	      continue;
	    }
	  if (!binfo->deleted)
	    {
	      if (!strcmp (binfo->BufferName, buffer_name))
		{
		  foundit = true;
		  bool check_succeeded = buffer_check (binfo);
		  if (!check_succeeded)
		    {
		      set_deleted_status (binfo);
		      binfo = 0;
		      foundit = false;
		      blist_mod++;
		      break;
		    }
		  else if (client)
		    {
		      if (do_file_sync)
			{
			  write_sync_file ();
			}
		      binfo->BufferLineLen = (int) strlen (binfo->BufferLine);
		      client->Send (binfo->BufferLine, (int) binfo->BufferLineLen);
		      get_process_line (client, binfo, process_name,
					buffer_name, false);
		    }
		  break;
		}
	    }
	  binfo = (BUFFER_INFO *) buffers_list->get_next ();
	}
    }
  if (!foundit && client && !client->quit)
    {
      if (do_file_sync)
	{
	  write_sync_file ();
	}
      client->Send ("NO\r\n", 4);
    }
}

static void
wait_for_buffer_info (CLIENT_INFO * client)
{
  BUFFER_INFO *binfo = 0;
  bool foundit = false;
  if (client)
    {
      client->req_type = WAIT_CLIENT_REQUEST_TYPE;
    }

  if (buffers_list)
    {
      binfo = (BUFFER_INFO *) buffers_list->get_head ();
      while (binfo)
	{
	  if (!domains_match (binfo, client))
	    {
	      binfo = (BUFFER_INFO *) buffers_list->get_next ();
	      continue;
	    }
	  if (!binfo->deleted)
	    {
	      if (!strcmp (binfo->BufferName, buffer_name))
		{
		  foundit = true;
		  bool check_succeeded = buffer_check (binfo);
		  if (!check_succeeded)
		    {
		      set_deleted_status (binfo);
		      binfo = 0;
		      foundit = false;
		      blist_mod++;
		      break;
		    }
		  else if (client)
		    {
		      if (do_file_sync)
			{
			  write_sync_file ();
			}
		      binfo->BufferLineLen = (int) strlen (binfo->BufferLine);
		      client->Send (binfo->BufferLine, (int) binfo->BufferLineLen);
		      get_process_line (client, binfo, process_name,
					buffer_name, false);
		      if (client->wait_for_line)
			{
			  free (client->wait_for_line);
			  client->wait_for_line = 0;
			}
		    }
		  break;
		}
	    }
	  binfo = (BUFFER_INFO *) buffers_list->get_next ();
	}
    }
  if (!foundit && client && !client->quit)
    {
      if (do_file_sync)
	{
	  write_sync_file ();
	}
      client->wait_for_line = strdup (line);
    }
}

static void
store_process_line (CLIENT_INFO * client)
{
  if (!preserve_process_data)
    {
      if (client)
	{
	  client->Send ("OK\r\n", 4);
	}
      return;
    }
  if (client)
    {
      client->req_type = STORE_PROCESS_LINE_CLIENT_REQUEST_TYPE;
    }

  // WARNING:: process_line has process_name and buffer_name switched
  bool foundit = false;
  if (process_name[0] == 0)
    {
      if (client)
	{
	  if (do_file_sync)
	    {
	      write_sync_file ();
	    }
	  client->Send ("NO\r\n", 4);
	}
      return;
    }
  if (!strcmp (process_name, "default"))
    {
      PROC_INFO *p = new PROC_INFO ();
      strncpy (p->ProcessLine, line, sizeof (p->ProcessLine));
      while ((p->ProcessLine[p->ProcessLineLen - 1] == '\r' ||
	      p->ProcessLine[p->ProcessLineLen - 1] == '\n') &&
	     p->ProcessLineLen > 1)
	{
	  p->ProcessLine[p->ProcessLineLen - 1] = 0;
	  p->ProcessLineLen--;
	}
      strcat (p->ProcessLine, "\r\n");
      p->ProcessLineLen += 2;
      strncpy (p->ProcessName, buffer_name, sizeof (p->ProcessName));
      if (0 == process_defaults_list)
	{
	  p_default_mod++;
	  process_defaults_list = new RCS_LINKED_LIST ();
	}
      if (!client)
	{
	  p->stored_by_startup_file = true;
	}
      p_default_mod++;
      process_defaults_list->store_at_tail (p, sizeof (PROC_INFO), 0);
      if (client)
	{
	  if (do_file_sync)
	    {
	      write_sync_file ();
	    }
	  client->Send ("OK\r\n", 4);
	}
      return;
    }

  if (buffers_list)
    {
      BUFFER_INFO *binfo = (BUFFER_INFO *) buffers_list->get_head ();
      while (binfo)
	{
	  // WARNING:: process_line has process_name and buffer_name switched
	  if (!strcmp (binfo->BufferName, process_name))
	    {
	      foundit = true;
	      PROC_INFO *p = new PROC_INFO ();
	      strncpy (p->ProcessLine, line, sizeof (p->ProcessLine));
	      while ((p->ProcessLine[p->ProcessLineLen - 1] == '\r' ||
		      p->ProcessLine[p->ProcessLineLen - 1] == '\n') &&
		     p->ProcessLineLen > 1)
		{
		  p->ProcessLine[p->ProcessLineLen - 1] = 0;
		  p->ProcessLineLen--;
		}
	      strcat (p->ProcessLine, "\r\n");
	      p->ProcessLineLen += 2;
	      strncpy (p->ProcessName, buffer_name, sizeof (p->ProcessName));
	      if (!client)
		{
		  p->stored_by_startup_file = true;
		}
	      if (0 == binfo->processes_list)
		{
		  binfo->processes_list = new RCS_LINKED_LIST ();
		}
	      binfo->processes_list->store_at_tail (p, sizeof (PROC_INFO), 0);
	      if (client)
		{
		  if (do_file_sync)
		    {
		      write_sync_file ();
		    }
		  client->Send ("OK\r\n", 4);
		}
	      break;
	    }
	  binfo = (BUFFER_INFO *) buffers_list->get_next ();
	}
    }
  if (!foundit && client)
    {
      if (do_file_sync)
	{
	  write_sync_file ();
	}
      client->Send ("NO\r\n", 4);
    }
}

static void
list_buffers (CLIENT_INFO * client)
{
  PROC_INFO *p = 0;
  BUFFER_INFO *binfo = 0;

  if (client)
    {
      client->req_type = LIST_CLIENT_REQUEST_TYPE;
      strcpy (temp_string, "#BEGIN_LIST\r\n");
      client->Send (temp_string, (int) strlen (temp_string));
    }
  if (buffers_list)
    {
      binfo = (BUFFER_INFO *) buffers_list->get_head ();
      while (binfo)
	{
	  if (client)
	    {
	      binfo->BufferLineLen = strlen (binfo->BufferLine);
	      client->Send (binfo->BufferLine, (int) binfo->BufferLineLen);
	    }
	  binfo = (BUFFER_INFO *) buffers_list->get_next ();
	}
      binfo = (BUFFER_INFO *) buffers_list->get_head ();
      while (binfo)
	{
	  if (preserve_process_data && binfo->processes_list)
	    {
	      p = (PROC_INFO *) binfo->processes_list->get_head ();
	      while (p)
		{
		  if (client)
		    {
		      client->Send (p->ProcessLine, (int) p->ProcessLineLen);
		    }
		  p = (PROC_INFO *) binfo->processes_list->get_next ();
		}
	    }
	  binfo = (BUFFER_INFO *) buffers_list->get_next ();
	}
    }
  if (process_defaults_list != 0)
    {
      p = (PROC_INFO *) process_defaults_list->get_head ();
      while (p)
	{
	  if (client)
	    {
	      client->Send (p->ProcessLine, (int) p->ProcessLineLen);
	    }
	  p = (PROC_INFO *) process_defaults_list->get_next ();
	}
    }
  if (client)
    {
      strcpy (temp_string, "#END_LIST\r\n");
      client->Send (temp_string, (int) strlen (temp_string));
    }
}



static void
list_procs_for_buf(CLIENT_INFO * client)
{
  PROC_INFO *p = 0;
  BUFFER_INFO *binfo = 0;

  if (client)
    {
      client->req_type = LIST_PROCS_FOR_BUF_CLIENT_REQUEST_TYPE;
      if(!preserve_process_data)
	{
	  strcpy (temp_string, "Error: Restart nmlcfgsvr with --preserve_process_data required to use this command.\r\n");
	  client->Send (temp_string, (int) strlen (temp_string));
	  return;
	}
      strcpy (temp_string, "#BEGIN_LIST\r\n");
      client->Send (temp_string, (int) strlen (temp_string));
    }
  if (buffers_list)
    {
      binfo = (BUFFER_INFO *) buffers_list->get_head ();
      while (binfo)
	{
	  if (!strcmp(binfo->BufferName,buffer_name) &&
	      preserve_process_data && binfo->processes_list)
	    {
	      p = (PROC_INFO *) binfo->processes_list->get_head ();
	      while (p)
		{
		  if (client)
		    {
		      client->Send (p->ProcessLine, (int) p->ProcessLineLen);
		    }
		  p = (PROC_INFO *) binfo->processes_list->get_next ();
		}
	    }
	  binfo = (BUFFER_INFO *) buffers_list->get_next ();
	}
    }
  if (process_defaults_list != 0)
    {
      p = (PROC_INFO *) process_defaults_list->get_head ();
      while (p)
	{
	  if (client)
	    {
	      client->Send (p->ProcessLine, (int) p->ProcessLineLen);
	    }
	  p = (PROC_INFO *) process_defaults_list->get_next ();
	}
    }
  if (client)
    {
      strcpy (temp_string, "#END_LIST\r\n");
      client->Send (temp_string, (int) strlen (temp_string));
    }
}

static void
save_buffers (const char *fname)
{
  BUFFER_INFO *binfo = 0;
  FILE *fp = 0;
  if (!strcmp (fname, "stdout") || !strcmp (fname, "STDOUT"))
    {
      fp = stdout;
    }
  if (!strcmp (fname, "stderr") || !strcmp (fname, "STDERR"))
    {
      fp = stderr;
    }
  else
    {
      fp = fopen (fname, "w");
    }
  if (!fp)
    {
      fprintf (stderr, "Can't open %s. %d -- %s\n",
	       fname, errno, strerror (errno));
      return;
    }
  PROC_INFO *p;
  strcpy (temp_string, "#BEGIN_LIST\r\n");
  fputs (temp_string, fp);
  if (buffers_list)
    {
      binfo = (BUFFER_INFO *) buffers_list->get_head ();
      while (binfo)
	{
	  fputs (binfo->BufferLine, fp);
	  binfo = (BUFFER_INFO *) buffers_list->get_next ();
	}
      binfo = (BUFFER_INFO *) buffers_list->get_head ();
      if (preserve_process_data)
	{
	  while (binfo)
	    {
	      if (binfo->processes_list)
		{
		  p = (PROC_INFO *) binfo->processes_list->get_head ();
		  while (p)
		    {
		      fputs (p->ProcessLine, fp);
		      p = (PROC_INFO *) binfo->processes_list->get_next ();
		    }
		}
	      binfo = (BUFFER_INFO *) buffers_list->get_next ();
	    }
	}
    }
  if (process_defaults_list != 0)
    {
      p = (PROC_INFO *) process_defaults_list->get_head ();
      while (p)
	{
	  fputs (p->ProcessLine, fp);
	  p = (PROC_INFO *) process_defaults_list->get_next ();
	}
    }
  strcpy (temp_string, "#END_LIST\r\n");
  fputs (temp_string, fp);
}

static void
delete_buffer_info (CLIENT_INFO * client)
{
  bool foundit = false;
  BUFFER_INFO *binfo = 0;
  if (client)
    {
      client->req_type = DELETE_BUFFER_CLIENT_REQUEST_TYPE;
    }
  if (buffers_list)
    {
      binfo = (BUFFER_INFO *) buffers_list->get_head ();
      while (binfo)
	{
	  if (!binfo->deleted)
	    {
	      if (!strcmp (binfo->BufferName, buffer_name))
		{
		  set_deleted_status (binfo);
		  if (client)
		    {
		      if (do_file_sync)
			{
			  write_sync_file ();
			}
		      client->Send ("OK\r\n", 4);
		    }
		  foundit = true;
		  break;
		}
	    }
	  binfo = (BUFFER_INFO *) buffers_list->get_next ();
	}
    }
  if (!foundit && client)
    {
      if (do_file_sync)
	{
	  write_sync_file ();
	}
      client->Send ("NO\r\n", 4);
    }
}

static void
list_templates (CLIENT_INFO * client)
{
  if (client)
    {
      client->req_type = LIST_TEMPLATES_CLIENT_REQUEST_TYPE;
    }
  std::ostringstream temp;
  if (templates_list && client)
    {
      TEMPLATE_INFO *t = (TEMPLATE_INFO *) templates_list->get_head ();
      while (t)
	{
	  temp << "TEMPLATE: buffer_name_pattern=" << t->buffer_name_pattern << "\r\n";
	  if (t->buffer_size_set)
	    {
	      temp << "\t\tsize=" << t->buffer_size << "\r\n";
	    }
	  if (t->queue_len_set)
	    {
	      temp << "\t\tqueue=" << t->queue_len << "\r\n";
	    }
	  if (t->buffer_line_additions_set)
	    {
	      temp << "\t\tblineadd=" <<  t->buffer_line_additions << "\r\n";
	    }
	  t = (TEMPLATE_INFO *) templates_list->get_next ();
	}
    }
  temp << "#END_OF_TEMPLATES_LIST\r\n";
  client->Send (temp.str().c_str(), (int) temp.str().length());
}

static void
create_new_buffer_line (BUFFER_INFO ** binfo, CLIENT_INFO * client)
{
  bool buffer_size_changed = false;
  bool old_buffer_resurrected = false;
  size_t size_needed = 0;
  (*binfo)->size = default_buffer_size;
  (*binfo)->pre_queuemult_size = default_buffer_size;
  const char *sizestr = strstr (line, "size=");
  if (sizestr)
    {
      (*binfo)->size = strtol (sizestr + 5, 0, 0);
      (*binfo)->pre_queuemult_size = (*binfo)->size;
      buffer_size_changed = true;
    }
  memset ((*binfo)->BufLineAdd, 0, sizeof ((*binfo)->BufLineAdd));
  if (templates_list)
    {
      TEMPLATE_INFO *t = (TEMPLATE_INFO *) templates_list->get_head ();
      while (t)
	{
	  if (simple_pattern_match
	      (t->buffer_name_pattern, (*binfo)->BufferName))
	    {
	      if(debug_nmlcfgsvr)
		{
		  printf("Found matching template.\n");
		}
	      if(t->buffer_size < 1)
		{
		  t->buffer_size_set=false;
		}
	      if (t->buffer_size_set &&
		  (!buffer_size_changed || t->buffer_size > (*binfo)->size))
		{
		  buffer_size_changed = true;
		  (*binfo)->size = t->buffer_size;
		}
	      if (t->queue_len_set)
		{
		  (*binfo)->queue_len = t->queue_len;
		}
	      if (t->buffer_line_additions_set)
		{
		  strncat(line, " ",sizeof(line));
		  strncat(line, t->buffer_line_additions,sizeof(line));
		}
	    }
	  t = (TEMPLATE_INFO *) templates_list->get_next ();
	}
    }
  BUFFER_INFO *b = (BUFFER_INFO *) buffers_list->get_head ();
  while (b)
    {
      if (b->deleted)
	{
	  if (!domains_match (b, client))
	    {
	      b = (BUFFER_INFO *) buffers_list->get_next ();
	      continue;
	    }
	  if (!strcmp (b->BufferName, (*binfo)->BufferName))
	    {
	      if (debug_nmlcfgsvr)
		{
		  printf ("resurrecting %s\n", b->BufferName);
		}
	      char *delete_str =
		strstr (b->BufferLine, "nmlcfgsvr-deleted=true");
	      if (delete_str)
		{
		  strcpy (delete_str, "\r\n");
		}
	      else
		{
		  printf
		    ("what happened to the  string \"nmlcfgsvr-deleted=true\" in (%s)\n",
		     b->BufferLine);
		}
	      if (b->ip_string)
		{
		  free (b->ip_string);
		  b->ip_string = 0;
		}
	      delete (*binfo);
	      b->deleted = false;
	      *binfo = b;
	      old_buffer_resurrected = true;
	      (*binfo)->resurrected = true;
	    }
	}
      b = (BUFFER_INFO *) buffers_list->get_next ();
    }
  port = dl_sa_get_port (client->client_address_ptr);
  if (port < 0)
    {
      printf ("port=%d\n", port);
      exit (1);
    }
  const char *clonestr = strstr (line, "clone=");
  if (clonestr)
    {
      const char *cptr = clonestr + 6;
      char *dptr = (*binfo)->ClonedBufferName;
      char c = *cptr;
      while ((c >= 'a' && c <= 'z') ||
	     (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_')
	{
	  c = *cptr;
	  *dptr = c;
	  cptr++;
	  dptr++;
	  if (dptr >=
	      (*binfo)->ClonedBufferName +
	      sizeof ((*binfo)->ClonedBufferName))
	    {
	      break;
	    }
	}
      b = (BUFFER_INFO *) buffers_list->get_head ();
      while (b)
	{
	  if (!domains_match (b, client))
	    {
	      b = (BUFFER_INFO *) buffers_list->get_next ();
	      continue;
	    }
	  if (!strcmp (b->BufferName, (*binfo)->ClonedBufferName))
	    {
	      (*binfo)->size = b->size;
	      (*binfo)->pre_queuemult_size = b->pre_queuemult_size;
	      strcat ((*binfo)->BufLineAdd, b->BufLineAdd);
	      (*binfo)->neutral = b->neutral;
	      (*binfo)->confirm_write = b->confirm_write;
	    }
	  b = (BUFFER_INFO *) buffers_list->get_next ();
	}
    }
  const char *queuestr = strstr (line, "queue=");
  if (queuestr)
    {
      (*binfo)->queue_len = strtol (queuestr + 6, 0, 0);
    }
  if (strstr (line, "nodistribution"))
    {
      (*binfo)->nodist = true;
    }
  else
    {
      (*binfo)->nodist = false;
      if (strstr (line, "udp"))
	{
	  (*binfo)->isUDP = true;
	}
      if (strstr (line, "UDP"))
	{
	  (*binfo)->isUDP = true;
	}
    }
  const char *neutralstr = strstr (line, "neutral=");
  if (neutralstr)
    {
      (*binfo)->neutral = (bool) (strtol (neutralstr + 8, 0, 0) != 0);
    }
  const char *confirm_write_str = strstr (line, "confirm_write=");
  if (confirm_write_str)
    {
      (*binfo)->confirm_write = (bool) (strtol (confirm_write_str + 14, 0, 0) != 0);
    }
  if (client->max_size_from_format > 0)
    {
      size_needed = client->max_size_from_format + 0x400 -
	(client->max_size_from_format % 0x200);
      if (size_needed > (*binfo)->size)
	{
	  (*binfo)->size = size_needed;
	  (*binfo)->pre_queuemult_size = (*binfo)->size;
	}
    }
  if (!old_buffer_resurrected)
    {
      (*binfo)->key = key;
      (*binfo)->buffer_number = buffer_number;
      (*binfo)->port = port;
    }
  else if (old_buffer_resurrected
	   && client->max_size_from_format > (*binfo)->size)
    {
      size_needed = client->max_size_from_format + 0x800 -
	client->max_size_from_format % 0x800;
      if (size_needed > (*binfo)->size)
	{
	  (*binfo)->size = size_needed;
	  (*binfo)->pre_queuemult_size = (*binfo)->size;
	}
      if ((*binfo)->port < 1)
	{
	  (*binfo)->port = port;
	  port++;
	}
      if ((*binfo)->buffer_number < 1)
	{
	  (*binfo)->buffer_number = buffer_number;
	  buffer_number++;
	}
      if ((*binfo)->key < 1)
	{
	  (*binfo)->key = key;
	  key += 2;
	}
    }
  memset ((*binfo)->BufLineAdd, 0, sizeof ((*binfo)->BufLineAdd));
  const char *blinestr = strstr (line, "blineadd=");
  if (blinestr)
    {
      strcat ((*binfo)->BufLineAdd, blinestr + 9);
    }
  set_ip_strings (client, (*binfo), 0);
  int port_to_use = port;
  if (client->port_to_use_set)
    {
      port_to_use = client->port_to_use;
      (*binfo)->port = port_to_use;
    }
  if ((*binfo)->queue_len > 0 && !strstr ((*binfo)->BufLineAdd, "queue"))
    {
      strcat ((*binfo)->BufLineAdd, " queue");
    }
  if ((*binfo)->queue_len > 1)
    {
      (*binfo)->size = (*binfo)->pre_queuemult_size * (*binfo)->queue_len;
      buffer_size_changed = true;
    }

  char *domain_str = strstr (line, "domain=");
  if (domain_str)
    {
      if ((*binfo)->domain)
	{
	  free ((void *) (*binfo)->domain);
	}
      (*binfo)->domain = strdup (domain_str + 7);
    }
  else if (client->domain)
    {
      if ((*binfo)->domain)
	{
	  free ((void *) (*binfo)->domain);
	}
      (*binfo)->domain = strdup (client->domain);
    }
  static char bsem_string[40];
  if (client->bsem_needed_set)
    {
      (*binfo)->bsem_needed = client->bsem_needed;
    }
  if((*binfo)->bsem_needed)
    {
      SNPRINTF_FUNC (SNPRINTF_ARGS(bsem_string,sizeof(bsem_string)),
		     "bsem=%d",((*binfo)->key+1));
    }
  else
    {
      memset(bsem_string,0,sizeof(bsem_string));
    }

  if(client->format_name.length() > 0) {
    (*binfo)->format_name = client->format_name;
  }
  
  std::ostringstream format_info;
  if(!getenv("IGNORE_FORMAT_NAME")) {
    if((*binfo)->format_name.length() > 0)
      {
	format_info << " format_name=" << (*binfo)->format_name << " ";
      }
  }

  (*binfo)->format_source = client->format_source;
  if((*binfo)->format_source.length() > 0)
    {
      format_info << " format_source=" << (*binfo)->format_source << " ";
    }
  (*binfo)->header = client->header;
  if((*binfo)->header.length() > 0)
    {
      format_info << " header=" << (*binfo)->header << " ";
    }
  const char *format_info_cP= format_info.str().c_str();
  SNPRINTF_FUNC ( SNPRINTF_ARGS((*binfo)->BufferLine,sizeof((*binfo)->BufferLine)),
		  "B \t%s \tSHMEM \t%s \t%lu \t%d \t* \t%d \t* \t%d \t%s=%d %s %s %s %s nmlcfgsvr=%s:%ld %s%s %s\r\n",
		  (*binfo)->BufferName, 
		  (*binfo)->ip_string,
		  (unsigned long) (*binfo)->size, 
		  ((*binfo)->neutral ? 1 : 0),
		  (*binfo)->buffer_number, 
		  (*binfo)->key, 
		  ((*binfo)->isUDP ? "UDP" : "TCP"),
		  (*binfo)->port,
		  bsem_string,
		  (*binfo)->BufLineAdd,
		  ((sizeof (long) != 8) ? "packed" : "packedl64"),
		  ((*binfo)->confirm_write? "confirm_write":""),
		  my_ip_string,
		  local_port, 
		  ((*binfo)->domain ? "domain=" : ""),
		  ((*binfo)->domain ? (*binfo)->domain : ""),
		  format_info_cP
		  );
  if (debug_nmlcfgsvr)
    {
      printf ("creating %s\n", (*binfo)->BufferLine);
    }
  if (!old_buffer_resurrected)
    {
      buffer_number++;
      key += 2;
      if (!client->port_to_use_set)
	{
	  port++;
	}
    }
  (*binfo)->BufferLineLen = strlen ((*binfo)->BufferLine);
}

static void
create_buffer_info (CLIENT_INFO * client)
{
  BUFFER_INFO *binfo = 0;
  if (!client)
    {
      return;
    }
  client->req_type = CREATE_BUFFER_CLIENT_REQUEST_TYPE;
  client->willstart = false;
  client->port_to_use_set = false;
  if (strstr (line, "willstart=1"))
    {
      client->willstart = true;
    }
  const char *port_to_use_ptr = strstr (line, "port_to_use=");
  if (port_to_use_ptr)
    {
      client->port_to_use = strtol (port_to_use_ptr + 12, 0, 0);
      if (client->port_to_use > 0)
	{
	  client->port_to_use_set = true;
	}
    }
  else
    {
      port_to_use_ptr = strstr (line, "port=");
      if (port_to_use_ptr)
	{
	  client->port_to_use = strtol (port_to_use_ptr + 5, 0, 0);
	  if (client->port_to_use > 0)
	    {
	      client->port_to_use_set = true;
	    }
	}
    }

  const char *bsem_needed_ptr = strstr (line, "bsem_needed=");
  if (bsem_needed_ptr)
    {
      client->bsem_needed = (bool) (strtol(bsem_needed_ptr + 12, 0, 0) != 0);
      client->bsem_needed_set = true;
    }

  const char *max_size_from_format_ptr =
    strstr (line, "max_size_from_format=");
  if (max_size_from_format_ptr)
    {
      client->max_size_from_format =
	strtoul (max_size_from_format_ptr + 21, 0, 0);
    }
  const char *format_name_ptr =
    strstr (line, "format_name=");
  if (format_name_ptr)
    {
      size_t s = strcspn(format_name_ptr+12," \t\r\n,;:");
      if(s > 0)
	{
	  client->format_name.assign(format_name_ptr+12,s);
	}
    }

  const char *format_source_ptr =
    strstr (line, "format_source=");
  if (format_source_ptr)
    {
      format_source_ptr += 14;
      size_t s = strcspn(format_source_ptr," \t\r\n,;:");
      if(s > 0)
	{
	  client->format_source.assign(format_source_ptr,s);
	}
    }
  const char *header_ptr =
    strstr (line, "header=");
  if (header_ptr)
    {
      header_ptr += 7;
      size_t s = strcspn(header_ptr," \t\r\n,;:");
      if(s > 0)
	{
	  client->header.assign(header_ptr,s);
	}
    }

  const char *set_to_server_ptr = strstr (line, "set_to_server=");
  if (set_to_server_ptr)
    {
      client->set_to_server = strtol (set_to_server_ptr + 14, 0, 0);
      if(client->set_to_server == 2)
	{
	  client->willstart=true;
	}
    }
  const char *timeout_ptr = strstr (line, "timeout=");
  if (timeout_ptr)
    {
      printf ("timeout_ptr=%s\n", timeout_ptr);
      if (!strcmp (timeout_ptr, "timeout=INF"))
	{
	  client->timeout = -1;
	}
      else
	{
	  client->timeout = strtod (timeout_ptr + 8, 0);
	}
    }
  if (buffers_list)
    {
      binfo = (BUFFER_INFO *) buffers_list->get_head ();
      while (binfo)
	{
	  if (!domains_match (binfo, client))
	    {
	      binfo = (BUFFER_INFO *) buffers_list->get_next ();
	      continue;
	    }
	  if (!binfo->deleted)
	    {
	      if (!strcmp (binfo->BufferName, buffer_name))
		{
		  bool check_succeeded = buffer_check (binfo);
		  if (!check_succeeded)
		    {
		      set_deleted_status (binfo);
		      binfo = 0;
		      blist_mod++;
		      break;
		    }
		  else if (client)
		    {
		      if (do_file_sync)
			{
			  write_sync_file ();
			}
		      binfo->BufferLineLen = strlen (binfo->BufferLine);
		      client->Send (binfo->BufferLine, (int) binfo->BufferLineLen);
		      get_process_line (client, binfo, process_name,
					buffer_name, 
					false
					);

		    }
		  return;
		}
	    }
	  binfo = (BUFFER_INFO *) buffers_list->get_next ();
	}
    }
  if (!buffers_list)
    {
      blist_mod++;
      buffers_list = new RCS_LINKED_LIST ();
    }
  binfo = new BUFFER_INFO ();
  dl_sa_copy (&binfo->svr_addr_ptr, client->client_address_ptr);
  binfo->stored_by_startup_file = false;
  if (client->domain)
    {
      binfo->domain = strdup (client->domain);
    }
  else
    {
      binfo->domain = 0;
    }
  binfo->cnum = 0;
  strncpy (binfo->BufferName, buffer_name, sizeof (binfo->BufferName));
  create_new_buffer_line (&binfo, client);
  blist_mod++;
  if (!binfo->resurrected)
    {
      buffers_list->store_at_tail (binfo, sizeof (BUFFER_INFO), 0);
    }
  if (do_file_sync)
    {
      write_sync_file ();
    }
  binfo->BufferLineLen = strlen (binfo->BufferLine);
  client->Send (binfo->BufferLine, (int) binfo->BufferLineLen);
  get_process_line (client, binfo, process_name, buffer_name, true);
}


static void
createnew_buffer_info (CLIENT_INFO * client)
{
  if (!client)
    {
      return;
    }
  client->req_type = CREATE_BUFFER_CLIENT_REQUEST_TYPE;
  client->willstart = false;
  client->port_to_use_set = false;
  if (strstr (line, "willstart=1"))
    {
      client->willstart = true;
    }
  const char *port_to_use_ptr = strstr (line, "port_to_use=");
  if (port_to_use_ptr)
    {
      client->port_to_use = strtol (port_to_use_ptr + 12, 0, 0);
      if (client->port_to_use > 0)
	{
	  client->port_to_use_set = true;
	}
    }
  const char *max_size_from_format_ptr =
    strstr (line, "max_size_from_format=");
  if (max_size_from_format_ptr)
    {
      client->max_size_from_format =
	strtoul (max_size_from_format_ptr + 21, 0, 0);
    }
  const char *set_to_server_ptr = strstr (line, "set_to_server=");
  if (set_to_server_ptr)
    {
      client->set_to_server = strtol (set_to_server_ptr + 14, 0, 0);
    }
  if (buffers_list)
    {
      BUFFER_INFO *binfo_from_list = (BUFFER_INFO *) buffers_list->get_head ();
      while (binfo_from_list)
	{
	  if (!domains_match (binfo_from_list, client))
	    {
	      binfo_from_list = (BUFFER_INFO *) buffers_list->get_next ();
	      continue;
	    }
	  if (!binfo_from_list->deleted)
	    {
	      if (!strcmp (binfo_from_list->BufferName, buffer_name))
		{
		  set_deleted_status (binfo_from_list);
		  blist_mod++;
		  if (do_file_sync)
		    {
		      write_sync_file ();
		    }
		  break;
		}
	    }
	  binfo_from_list = (BUFFER_INFO *) buffers_list->get_next ();
	}
    }
  if (!buffers_list)
    {
      blist_mod++;
      buffers_list = new RCS_LINKED_LIST ();
    }
  BUFFER_INFO *binfo = 0;
  if (0 == binfo)
    {
      binfo = new BUFFER_INFO ();
    }
  dl_sa_copy (&binfo->svr_addr_ptr, client->client_address_ptr);
  binfo->stored_by_startup_file = false;
  if (client->domain)
    {
      binfo->domain = strdup (client->domain);
    }
  else
    {
      binfo->domain = 0;
    }
  binfo->cnum = 0;
  strncpy (binfo->BufferName, buffer_name, sizeof (binfo->BufferName));
  create_new_buffer_line (&binfo, client);
  blist_mod++;
  if (!binfo->resurrected)
    {
      buffers_list->store_at_tail (binfo, sizeof (BUFFER_INFO), 0);
    }
  if (do_file_sync)
    {
      write_sync_file ();
    }
  binfo->BufferLineLen = strlen (binfo->BufferLine);
  client->Send (binfo->BufferLine, (int) binfo->BufferLineLen);
  get_process_line (client, binfo, process_name, buffer_name, true);
}

static void
create_exclusive_buffer_info (CLIENT_INFO * client)
{
  BUFFER_INFO *binfo = 0;
  if (!client)
    {
      return;
    }
  client->req_type = CREATE_EXCLUSIVE_BUFFER_CLIENT_REQUEST_TYPE;
  client->willstart = false;
  client->port_to_use_set = false;
  if (strstr (line, "willstart=1"))
    {
      client->willstart = true;
    }
  const char *port_to_use_ptr = strstr (line, "port_to_use=");
  if (port_to_use_ptr)
    {
      client->port_to_use = strtol (port_to_use_ptr + 12, 0, 0);
      if (client->port_to_use > 0)
	{
	  client->port_to_use_set = true;
	}
    }
  if (buffers_list)
    {
      binfo = (BUFFER_INFO *) buffers_list->get_head ();
      while (binfo)
	{
	  if (!domains_match (binfo, client))
	    {
	      binfo = (BUFFER_INFO *) buffers_list->get_next ();
	      continue;
	    }
	  if (!binfo->deleted)
	    {
	      if (!strcmp (binfo->BufferName, buffer_name))
		{
		  bool check_succeeded = buffer_check (binfo);
		  if (!check_succeeded)
		    {
		      set_deleted_status (binfo);
		      binfo = 0;
		      blist_mod++;
		      break;
		    }
		  else if (client)
		    {
		      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)), "NO\r\n");
		      if (do_file_sync)
			{
			  write_sync_file ();
			}
		      client->Send (temp_string, (int) strlen (temp_string));
		    }
		  return;
		}
	    }
	  binfo = (BUFFER_INFO *) buffers_list->get_next ();
	}
    }
  if (!buffers_list)
    {
      blist_mod++;
      buffers_list = new RCS_LINKED_LIST ();
    }
  binfo = new BUFFER_INFO ();

  dl_sa_copy (&binfo->svr_addr_ptr, client->client_address_ptr);
  binfo->stored_by_startup_file = false;
  binfo->cnum = 0;
  strncpy (binfo->BufferName, buffer_name, sizeof (binfo->BufferName));
  create_new_buffer_line (&binfo, client);
  blist_mod++;
  buffers_list->store_at_tail (binfo, sizeof (BUFFER_INFO), 0);
  if (do_file_sync)
    {
      write_sync_file ();
    }
  binfo->BufferLineLen = strlen (binfo->BufferLine);
  client->Send (binfo->BufferLine, (int) binfo->BufferLineLen);
  get_process_line (client, binfo, process_name, buffer_name, true);
}


static void
checkcreate_buffer_info (CLIENT_INFO * client)
{
  bool orig_do_buffer_checks = do_buffer_checks;
  do_buffer_checks=true;
  create_buffer_info (client);
  do_buffer_checks = orig_do_buffer_checks;
}

static void
checkget_buffer_info (CLIENT_INFO * client)
{
  bool orig_do_buffer_checks = do_buffer_checks;
  do_buffer_checks=true;
  get_buffer_info (client);
  do_buffer_checks = orig_do_buffer_checks;
}

static void
checkwait_buffer_info (CLIENT_INFO * client)
{
  bool orig_do_buffer_checks = do_buffer_checks;
  do_buffer_checks=true;
  wait_for_buffer_info (client);
  do_buffer_checks = orig_do_buffer_checks;
}


static void
store_buffer_line (CLIENT_INFO * client)
{
  char *word[32];
  long words_in_bufferline = 0;
  long buffer_number_on_this_line = -1;
  long key_on_this_line = -1;
  long bsem_key_on_this_line = -1;
  long port_on_this_line = -1;
  BUFFER_INFO *binfo = 0;

  if (client)
    {
      client->req_type = STORE_BUFFER_LINE_CLIENT_REQUEST_TYPE;
    }

  if (!buffers_list)
    {
      blist_mod++;
      buffers_list = new RCS_LINKED_LIST ();
    }
  binfo = new BUFFER_INFO ();
  if (client)
    {
      dl_sa_copy (&binfo->svr_addr_ptr, client->client_address_ptr);
      binfo->stored_by_startup_file = false;
    }
  else
    {
      binfo->stored_by_startup_file = true;
    }
  strncpy (binfo->BufferName, buffer_name, sizeof (binfo->BufferName));
  strncpy (binfo->BufferLine, line, sizeof (binfo->BufferLine));
  binfo->BufferLineLen = strlen (binfo->BufferLine);
  while ((binfo->BufferLine[binfo->BufferLineLen - 1] == '\r' ||
	  binfo->BufferLine[binfo->BufferLineLen - 1] == '\n') &&
	 binfo->BufferLineLen > 1)
    {
      binfo->BufferLine[binfo->BufferLineLen - 1] = 0;
      binfo->BufferLineLen--;
    }
  words_in_bufferline = separate_words_with_buffer (word, 31,
						    binfo->BufferLine,
						    binfo->WordBuffer,
						    sizeof (binfo->
							    WordBuffer));
  if (word[3])
    {
      strcpy (binfo->BufferHost, word[3]);
      if (binfo->BufferHost[0] >= '0' && binfo->BufferHost[0] <= '9')
	{
	  binfo->ip_string = strdup (binfo->BufferHost);
	}
      else
	{
#ifdef HAVE_GETHOSTBYNAME
	  struct hostent *h = gethostbyname (binfo->BufferHost);
	  if (h)
	    {
	      struct in_addr in_addr_temp;
	      in_addr_temp.s_addr = *((int *) (h->h_addr_list[0]));
	      //saddr_temp.sin_addr.sin_family = h->h_addrtype;
	      char *sa = dl_inet_ptr_ntoa (&(in_addr_temp));
	      if (sa)
		{
		  binfo->ip_string = strdup (sa);
		}
	    }
#endif
	}
    }

  if (word[4])
    {
      binfo->size = (size_t) strtoul (word[4], 0, 0);
      if (binfo->size < 128)
	{
	  binfo->size = 128;
	}
    }
  if (word[5])
    {
      long neutral_l = strtol (word[5], 0, 0);
      binfo->neutral = (neutral_l != 0);
    }
  memset (binfo->BufLineAdd, 0, sizeof (binfo->BufLineAdd));
  for (int i = 7; i < words_in_bufferline; i++)
    {
      if (!word[i])
	{
	  break;
	}
      if (debug_nmlcfgsvr)
	{
	  printf ("i=%2.2d \t:word[i]=%s\n", i, word[i]);
	}
      if (word[i][0] >= '0' && word[i][0] <= '9')
	{
	  if (i == 7)
	    {
	      buffer_number_on_this_line = strtol (word[i], 0, 0);
	      if (buffer_number_on_this_line >= 0)
		{
		  if (buffer_number_on_this_line > max_buffer_number)
		    {
		      max_buffer_number = buffer_number_on_this_line;
		    }
		  if (buffer_number_on_this_line < min_buffer_number)
		    {
		      min_buffer_number = buffer_number_on_this_line;
		    }
		}
	      binfo->buffer_number = buffer_number_on_this_line;
	    }
	  else if (i == 9)
	    {
	      key_on_this_line = strtol (word[i], 0, 0);
	      if (key_on_this_line >= 0)
		{
		  if (key_on_this_line > max_key)
		    {
		      max_key = key_on_this_line;
		    }
		  if (key_on_this_line < min_key)
		    {
		      min_key = key_on_this_line;
		    }
		}
	      binfo->key = key_on_this_line;
	    }
	  continue;
	}
      if (!strncmp (word[i], "bsem=", 5) || !strncmp (word[i], "BSEM=", 5))
	{
	  bsem_key_on_this_line = strtol (word[i] + 5, 0, 0);
	  if (bsem_key_on_this_line >= 0)
	    {
	      if (bsem_key_on_this_line > max_key)
		{
		  max_key = bsem_key_on_this_line;
		}
	      if (bsem_key_on_this_line < min_key)
		{
		  min_key = bsem_key_on_this_line;
		}
	    }
	  binfo->bsem_needed=true;
	}

      if (!strncmp (word[i], "TCP=", 4) ||
	  !strncmp (word[i], "tcp=", 4) ||
	  !strncmp (word[i], "udp=", 4) || !strncmp (word[i], "UDP=", 4))
	{
	  port_on_this_line = strtol (word[i] + 4, 0, 0);
	  if (port_on_this_line >= 0)
	    {
	      if (port_on_this_line > max_port)
		{
		  max_port = port_on_this_line;
		}
	      if (port_on_this_line < min_port)
		{
		  min_port = port_on_this_line;
		}
	      binfo->port = port_on_this_line;
	    }
	  continue;
	}
      if (!strncmp (word[i], "STCP=", 5) || !strncmp (word[i], "stcp=", 5))
	{
	  port_on_this_line = strtol (word[i] + 4, 0, 0);
	  if (port_on_this_line >= 0)
	    {
	      if (port_on_this_line > max_port)
		{
		  max_port = port_on_this_line;
		}
	      if (port_on_this_line < min_port)
		{
		  min_port = port_on_this_line;
		}
	    }
	  continue;
	}
      if (!strncmp (word[i], "DOMAIN=", 8) ||
	  !strncmp (word[i], "domain=", 8))
	{
	  if (binfo->domain)
	    {
	      free ((void *) binfo->domain);
	    }
	  binfo->domain = strdup (word[i] + 7);
	}

      if (!binfo->deleted
	  || (strcmp (word[i], "packed") && 
	      strncmp (word[i], "nmlcfgsvr", 9) &&
	      strcmp (word[i], "confirm_write")))
	{
	  if (binfo->BufLineAdd[0] == 0)
	    {
	      strcat (binfo->BufLineAdd, " ");
	    }
	  strcat (binfo->BufLineAdd, word[i]);
	  strcat (binfo->BufLineAdd, " ");
	}
    }
  strcat (binfo->BufferLine, "\r\n");
  if (client && client->domain)
    {
      if (binfo->domain)
	{
	  free (binfo->domain);
	}
      binfo->domain = strdup (client->domain);
    }
  binfo->deleted =
    (NULL != strstr (binfo->BufferLine, "nmlcfgsvr-deleted=true"));
  binfo->BufferLineLen += 2;
  binfo->BufferLineLen = strlen (binfo->BufferLine);
  blist_mod++;

  if (!binfo->deleted)
    {
      bool check_succeeded = buffer_check (binfo);
      if (!check_succeeded)
	{
	  set_deleted_status (binfo);
	}
    }
  if (debug_nmlcfgsvr)
    {
      printf ("stored %s\n", binfo->BufferLine);
    }
  buffers_list->store_at_tail (binfo, sizeof (BUFFER_INFO), 0);
}

static void
add_template (CLIENT_INFO * client)
{
  if (client)
    {
      client->req_type = ADD_TEMPLATE_CLIENT_REQUEST_TYPE;
    }
  TEMPLATE_INFO *t = new TEMPLATE_INFO ();
  strcpy (t->buffer_name_pattern, buffer_name);
  if (0 == templates_list)
    {
      templates_list = new RCS_LINKED_LIST ();
    }
  t->buffer_size = default_buffer_size;
  t->buffer_size_set = false;
  t->queue_len_set = false;
  const char *size_string = strstr (line, "size=");
  if (size_string)
    {
      t->buffer_size_set = true;
      t->buffer_size = strtol (size_string + 5, 0, 0);
      rcs_print_debug (PRINT_MISC, "template buffer_size set to %lu\n",
		       (unsigned long) t->buffer_size);
    }
  const char *queue_string = strstr (line, "queue=");
  if (queue_string)
    {
      t->queue_len_set = true;
      t->queue_len = strtol (queue_string + 6, 0, 0);
      rcs_print_debug (PRINT_MISC, "template buffer_size set to %lu\n",
		       (unsigned long) t->buffer_size);
    }
  const char *blineadd_string = strstr (line, "blineadd=");
  char endchar = ' ';
  char *tb = t->buffer_line_additions;
  memset (t->buffer_line_additions, 0, sizeof (t->buffer_line_additions));
  t->buffer_line_additions_set = false;
  if (blineadd_string)
    {
      blineadd_string += 9;
      if (*blineadd_string == '\"' || *blineadd_string == '\'')
	{
	  endchar = *blineadd_string;
	  blineadd_string++;
	}
      while (*blineadd_string && *blineadd_string != endchar &&
	     tb <
	     t->buffer_line_additions + sizeof (t->buffer_line_additions))
	{
	  *tb = *blineadd_string;
	  tb++;
	  blineadd_string++;
	}
      t->buffer_line_additions_set = true;
    }
  templates_list->store_at_tail (t, sizeof (TEMPLATE_INFO), 0);
  if (client)
    {
      if (do_file_sync)
	{
	  write_sync_file ();
	}
      client->Send ("OK\r\n", 4);
    }
}

static void
send_help (CLIENT_INFO * client)
{
  if (client)
    {
      client->req_type = SEND_HELP_CLIENT_REQUEST_TYPE;
    }

  if (client)
    {
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)), "# NMLCFGSVR Commands:\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)), "create <buffername> <processname>\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)),
	       "\t\tCreate a new buffer line if it does not already exist\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)),
	       "\t\tUse an existing one if it already exists.\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)),
	       "create_exclusive <buffername> <processname>\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)),
	       "\t\tCreate a new buffer line if it does not already exist\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)), "\t\tReturn \"NO\" if it already exists.\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)), "get <buffername> <processname>\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)),
	       "\t\tFind a previously registered buffer line.\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)),
	       "\t\tReturn \"NO\" if it does not already exist.\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)), "delete <buffername> <processname>\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)),
	       "\t\tFind a previously registered buffer line and delete it.\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)),
	       "\t\tReturn \"NO\" if it does not already exist.\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)), "list\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)), "list_procs_for_buf <buffername>\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)),
	       "\t\tList all previously registered buffer lines.\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)), "set_local_ip <Internet Address>\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)),
	       "\t\tSet a default IP or hostname to replace the loopback address.\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)), "quit\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)), "\t\tClose this connection.\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)), "Die!!!\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)),
	       "\t\tKill the server closing all connections.(USE only in emergency)\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
      SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)), "#END_HELP\r\n");
      sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
	     nmlcfgsvr_sendn_timeout);
    }
}

static int linenumber = 0;

static void
int_to_id_string (long id, char *str, char **endstr, size_t maxlen)
{
  if (!str)
    {
      return;
    }
  SNPRINTF_FUNC (SNPRINTF_ARGS(str,maxlen),
		 "%ld.", id);
  if (endstr)
    {
      *endstr = str + strlen (str);
    }
}

static long newdomain_number = 1;
static char newdomainbuf[100];

static void
new_domain_info (CLIENT_INFO * client)
{
  if (!client)
    {
      return;
    }
  strcpy (newdomainbuf, "OK nd.");
  char *end_id = 0;
  int_to_id_string (newdomain_number, newdomainbuf + 6, 
		    &end_id,sizeof(newdomainbuf)-7);
  newdomain_number++;
#if HAVE_GETTIMEOFDAY
  struct timeval tv;
  gettimeofday (&tv, 0);
#endif
  int_to_id_string (dl_sa_get_port (client->client_address_ptr),
		    end_id, &end_id, 
		    (size_t) (newdomainbuf+sizeof(newdomainbuf)-end_id-1));
  //int_to_id_string(client->client_address.sin_addr.s_addr,end_id,&end_id);
  //int_to_id_string(myaddress.sin_port,end_id,&end_id);
  //int_to_id_string(dl_inet_addr(my_ip_string),end_id,&end_id);
#if HAVE_GETTIMEOFDAY
  //int_to_id_string(tv.tv_sec,end_id,&end_id);
  //int_to_id_string(tv.tv_usec,end_id,&end_id);
#endif
  if (client->permanent_domain)
    {
      free (client->permanent_domain);
      client->permanent_domain = 0;
    }
  client->permanent_domain = strdup (newdomainbuf + 3);
  client->domain = client->permanent_domain;
  strcat (end_id, "\r\n");
  client->Send (newdomainbuf, (int) strlen (newdomainbuf));
  printf ("%s\n", newdomainbuf);
}

static void
set_domain_info (CLIENT_INFO * client)
{
  if (!client)
    {
      return;
    }
  if (buffer_name[0] == 0 || buffer_name[0] == '*'
      || strchr (buffer_name, ','))
    {
      rcs_print_error ("Can not set domain when buffer_name=%s\n",
		       buffer_name);
      client->Send ("NO\r\n", 4);
      return;
    }
  if (client->permanent_domain)
    {
      free (client->permanent_domain);
      client->permanent_domain = 0;
    }
  client->permanent_domain = strdup (buffer_name);
  client->domain = client->permanent_domain;
  client->Send ("OK\r\n", 4);
}

static void
process_line (CLIENT_INFO * client)
{
  rcs_print_debug (PRINT_MISC, "Processing line:%s\n", line);
  if (debug_nmlcfgsvr)
    {
      printf ("Processing line:%s\n", line);
      if (client)
	{
	  printf ("\t recieved from %s:%d\n",
		  dl_sa_get_host (client->client_address_ptr),
		  dl_sa_get_port (client->client_address_ptr));
	}
    }
  if (client)
    {
      client->linenumber++;
    }
  else
    {
      linenumber++;
    }
  memset (cmd, 0, sizeof (cmd));
  memset (buffer_name, 0, sizeof (buffer_name));
  memset (process_name, 0, sizeof (process_name));
  if (line[0] == '#')
    {
      memset (line, 0, sizeof (line));
      return;
    }
  if (line[0] == '\r' || line[0] == '\n' || line[0] == 0)
    {
      if (client)
	{
	  if (prompt && promptlen > 0)
	    {
	      sendn ((int) client->client_socket, prompt, (int) promptlen, 0,
		     nmlcfgsvr_sendn_timeout);
	    }
	}
      memset (line, 0, sizeof (line));
      return;
    }
  sscanf (line, "%s %s %s", cmd, buffer_name, process_name);
  rcs_print_debug (PRINT_MISC, "Processing cmd=\"%s\",buffer_name=\"%s\"\n",
		   cmd, buffer_name);
  if (client)
    {
      client->willstart = false;
      client->port_to_use_set = false;
      if (client->domain_this_line)
	{
	  free ((void *) client->domain_this_line);
	  client->domain_this_line = 0;
	}
      char *domain_str = strstr (line, "domain=");
      if (domain_str)
	{
	  client->domain_this_line = strdup (domain_str + 7);
	  client->domain = client->domain_this_line;
	}
      else
	{
	  client->domain = client->permanent_domain;
	}
      char *my_ip_string_for_this_client_str=strstr(line,"your_address=");
      if(my_ip_string_for_this_client_str)
	{
	  client->my_ip_string_for_this_client_set=true;
	  strncpy(client->my_ip_string_for_this_client,
		  my_ip_string_for_this_client_str+13,
		  sizeof(client->my_ip_string_for_this_client));
	  size_t l = strcspn(client->my_ip_string_for_this_client,
				       ", \t\r\n;=");
	  if(l > 0 && l < sizeof(client->my_ip_string_for_this_client))
	    {
	      client->my_ip_string_for_this_client[l] = 0;
	    }
	  if(l < 4)
	    {
	      client->my_ip_string_for_this_client_set=false;
	    }
	  if(!strcmp(client->my_ip_string_for_this_client,my_ip_string))
	    {
	      client->my_ip_string_for_this_client_set=false;
	    }
	  if(!strcmp(client->my_ip_string_for_this_client,"127.0.0.1"))
	    {
	      client->my_ip_string_for_this_client_set=false;
	    }
	}
    }
  if (cmd[0] == '#')
    {
      memset (line, 0, sizeof (line));
      if (client)
	{
	  if (prompt && promptlen > 0)
	    {
	      sendn ((int) client->client_socket, prompt, (int) promptlen, 0,
		     nmlcfgsvr_sendn_timeout);
	    }
	}
      return;
    }
  else if (!strcmp (cmd, "newdomain"))
    {
      new_domain_info (client);
    }
  else if (!strcmp (cmd, "setdomain"))
    {
      set_domain_info (client);
    }
  else if (!strcmp (cmd, "wait"))
    {
      wait_for_buffer_info (client);
    }
  else if (!strcmp (cmd, "check"))
    {
      checkcreate_buffer_info (client);
    }
  else if (!strcmp (cmd, "checkcreate"))
    {
      checkcreate_buffer_info (client);
    }
  else if (!strcmp (cmd, "checkget"))
    {
      checkget_buffer_info (client);
    }
  else if (!strcmp (cmd, "checkwait"))
    {
      checkwait_buffer_info (client);
    }
  else if (!strcmp (cmd, "get"))
    {
      get_buffer_info (client);
    }
  else if (!strcmp (cmd, "new"))
    {
      createnew_buffer_info (client);
    }
  else if (!strcmp (cmd, "create"))
    {
      create_buffer_info (client);
    }
  else if (!strcmp (cmd, "create_exclusive"))
    {
      create_exclusive_buffer_info (client);
    }
  else if (!strcmp (cmd, "delete"))
    {
      delete_buffer_info (client);
    }
  else if (!strcmp (cmd, "list"))
    {
      list_buffers (client);
    }
  else if (!strcmp (cmd, "list_procs_for_buf"))
    {
      list_procs_for_buf (client);
    }
  else if (!strcmp (cmd, "template"))
    {
      add_template (client);
    }
  else if (!strcmp (cmd, "set_local_ip"))
    {
      if (buffer_name[0] != 0)
	{
	  my_ip_string = strdup (buffer_name);
	}
    }
  else if (!strcmp (cmd, "list_templates"))
    {
      list_templates (client);
    }
  else if (!strcmp (cmd, "B") || !strcmp (cmd, "b"))
    {
      store_buffer_line (client);
    }
  else if (!strcmp (cmd, "P") || !strcmp (cmd, "p"))
    {
      store_process_line (client);
    }
  else if (!strcmp (cmd, "help"))
    {
      send_help (client);
    }
  else if (!strcmp (cmd, "enable_checking"))
    {
      do_buffer_checks = true;
      printf ("do_buffer_checks=true\n");
    }
  else if (!strcmp (cmd, "disable_checking"))
    {
      do_buffer_checks = false;
      printf ("do_buffer_checks=false\n");
    }
  else if (!strcmp (cmd, "debug"))
    {
      if (debug_nmlcfgsvr)
	{
	  printf ("NMLCFGSVR: DEBUG OFF\n");
	  debug_nmlcfgsvr = false;
	}
      else
	{
	  printf ("NMLCFGSVR: DEBUG ON\n");
	  debug_nmlcfgsvr = true;
#ifndef DISABLE_RCS_PRINT
	  max_rcs_errors_to_print=-1;
#endif
	}
    }
  else if (!strcmp (cmd, "quit"))
    {
      if (client)
	{
	  client->quit = true;
	}
    }
  else if (!strcmp (cmd, "Die!!!"))
    {
      nmlcfgsvr_quit = true;
      if (client)
	{
	  client->quit = true;
	}
    }
  else
    {
      if (client)
	{
	  SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)), 
		   "#UNRECOGNIZED COMMAND(%s) in line %d:%s\r\n",
		   cmd, client->linenumber, line);
	  rcs_print_error ("#UNRECOGNIZED COMMAND(%s) in line %d:%s\r\n",
		   cmd, client->linenumber, line);
	  if (do_file_sync)
	    {
	      write_sync_file ();
	    }
	  sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
		 nmlcfgsvr_sendn_timeout);
	  SNPRINTF_FUNC (SNPRINTF_ARGS(temp_string,sizeof(temp_string)),
			 "# Enter \"help\" for help.\r\n");
	  sendn ((int) client->client_socket, temp_string, (int) strlen (temp_string), 0,
		 nmlcfgsvr_sendn_timeout);
	}
    }
  if (client)
    {
      if (prompt && promptlen > 0)
	{
	  sendn ((int) client->client_socket, prompt, (int) promptlen, 0,
		 nmlcfgsvr_sendn_timeout);
	}
    }
  memset (line, 0, sizeof (line));
}

static void
process_data (CLIENT_INFO * client)
{
  if (!client)
    {
      return;
    }
  rcs_print_debug (PRINT_MISC,
		   "client->client_socket=%d,client->startptr=%p,client->endptr=%p\n",
		   client->client_socket, client->startptr, client->endptr);

  char *cptr = client->startptr;
  char *lptr = line;
  memset (line, 0, sizeof (line));
  while (cptr < client->ring_buffer + sizeof (client->ring_buffer) - 1
	 && (cptr < client->endptr || client->startptr > client->endptr)
	 && lptr < line + sizeof (line) - 1)
    {
      if (!*cptr || *cptr == 0x4)
	{
	  client->quit = true;
	  break;
	}
      if (*cptr == '\r' || *cptr == '\n')
	{
	  cptr++;
	  lptr = line;
	  continue;
	}
      *lptr = *cptr;
      cptr++;
      lptr++;
      if (*cptr == '\r' || *cptr == '\n')
	{
	  *lptr = 0;
	  if (lptr > line + 1)
	    {
	      process_line (client);
	    }
	  client->startptr = cptr;
	  lptr = line;
	}
      if (client->quit || nmlcfgsvr_quit)
	{
	  break;
	}
    }

  if (cptr == client->ring_buffer + sizeof (client->ring_buffer) - 1 &&
      client->startptr > client->endptr &&
      client->endptr > client->ring_buffer &&
      client->endptr < client->ring_buffer + sizeof (client->ring_buffer) - 1
      && !(client->quit || nmlcfgsvr_quit))
    {
      cptr = client->ring_buffer;
      while (cptr < client->endptr)
	{
	  if (!*cptr || *cptr == 0x4)
	    {
	      client->quit = true;
	      break;
	    }
	  if (*cptr == '\r' || *cptr == '\n')
	    {
	      cptr++;
	      lptr = line;
	      continue;
	    }
	  *lptr = *cptr;
	  cptr++;
	  lptr++;
	  if (*cptr == '\r' || *cptr == '\n')
	    {
	      *lptr = 0;
	      if (lptr > line + 1)
		{
		  process_line (client);
		}
	      client->startptr = cptr;
	      lptr = line;
	    }
	}
    }
}

static char myhostnamebuf[256];
static char fsync_read_buf[256];
static char fsync_end_buf[256];
static int fsync_end_len = 0;

static int
get_fsync_num (const char *f_sync_name)
{
  bool fsync_start_found = false;
  bool fsync_end_found = false;
  int f_num = -1;
  if (!f_sync_name)
    {
      return -1;
    }
  FILE *f = fopen (f_sync_name, "r");
  if (f)
    {
      fsync_end_len = 0;
      while (!feof (f))
	{
	  memset (fsync_read_buf, 0, sizeof (fsync_read_buf));
	  if (fgets (fsync_read_buf, sizeof (fsync_read_buf), f))
	    {
	      if (!fsync_start_found)
		{
		  if (!strncmp (fsync_read_buf, "#NMLCFGSVR_FSYNC_START", 22)
		      && fsync_read_buf[22] != 0 && fsync_read_buf[23] != 0)
		    {
		      f_num = strtol (fsync_read_buf + 23, 0, 0);
		      if (f_num <= 0 || f_num >= FSYNC_NUM_MAX
#ifdef ERANGE
			  || errno == ERANGE
#endif
			)
			{
			  fprintf (stderr,
				   "Bad data from sync file f_num=%d,errno=%d:%s,%s --  %23.23s<> %s\n",
				   f_num, errno, strerror (errno),
				   f_sync_name, fsync_read_buf,
				   fsync_read_buf + 23);
			  f_num = -1;
			  break;
			}
		      SNPRINTF_FUNC ( SNPRINTF_ARGS(fsync_end_buf, sizeof (fsync_end_buf)) ,
				      "#NMLCFGSVR_FSYNC_END %d", f_num);
		      fsync_end_len = (int) strlen (fsync_end_buf);
		      fsync_start_found = true;
		    }
		}
	      else
		{
		  if (!strncmp (fsync_read_buf, fsync_end_buf, fsync_end_len))
		    {
		      fsync_end_found = true;
		      break;
		    }
		}
	    }
	  else
	    {
	      if (errno)
		{
		  fprintf (stderr, "fgets error %d --%s reading %s.\n", errno,
			   strerror (errno), f_sync_name);
		}
	      break;
	    }
	}
      fclose (f);
      if (!fsync_start_found)
	{
	  fprintf (stderr, "Never found #NMLCFGSVR_FSYNC_START in %s\n",
		   f_sync_name);
	  f_num = -1;
	}
      else if (!fsync_end_found)
	{
	  fprintf (stderr, "Never found #NMLCFGSVR_FSYNC_END in %s\n",
		   f_sync_name);
	  f_num = -1;
	}
    }
  return f_num;
}

void
load_fsync_file (const char *fsync_name)
{
  FILE *f = fopen (fsync_name, "r");
  if (!f)
    {
      fprintf (stderr, "Could not open %s : %s\n",
	       fsync_name, strerror (errno));
    }
  bool orig_do_buffer_checks = do_buffer_checks;
  do_buffer_checks = check_fsync_file;
  while (!feof (f))
    {
      fgets (line, sizeof (line), f);
      process_line (0);
    }
  fclose (f);
  printf ("%s loaded.\n", fsync_name);
  do_buffer_checks = orig_do_buffer_checks;
}

static void
print_usage (void)
{
  fprintf (stderr,
	   "nmlcfgsvr usage: [--port <port>] [--localip <Internet Address>] [--startfile <startfile>] [--check] [--debug] [--help] [--startkey <startkey>] [--filesync [<filesyncprefix>]] [--nofsynccheck] [--bsem_needed] [--no_confirm_write_default] [--preserve_process_data]\n");
  exit (1);
}


int
main (int argc, const char **argv)
{
  rcs_print_debug (PRINT_MISC, "NMLCFGSVR[%s] compiled on %s at %s", argv[0],
		   __DATE__, __TIME__);
  const char *startfile = 0;

#ifndef DISABLE_RCS_PRINT
  max_rcs_errors_to_print = -1;
#endif

  int i = 1;

  if (getenv ("IPV6") || getenv ("NML_IPV6") || getenv ("CMS_IPV6"))
    {
      global_use_ipv6 = 1;
    }

  for (i = 1; i < argc; i++)
    {
      if (!strcmp (argv[i], "--port"))
	{
	  if (i < argc - 1)
	    {
	      i++;
	      local_port = strtol (argv[i], 0, 0);
	      key = local_port + 1;
	      port = local_port + 1;
	    }
	  else
	    {
	      print_usage ();
	    }
	  continue;
	}
      else if (!strcmp (argv[i], "--ipv6"))
	{
	  global_use_ipv6 = 1;
	  continue;
	}
      else if (!strcmp (argv[i], "--prompt"))
	{
	  if (i < argc - 1)
	    {
	      i++;
	      prompt = strdup (argv[i]);
	      promptlen = (int) strlen (prompt);
	    }
	  else
	    {
	      print_usage ();
	    }
	  continue;
	}
      else if (!strcmp (argv[i], "--localip"))
	{
	  if (i < argc - 1)
	    {
	      i++;
	      my_ip_string = strdup (argv[i]);
	    }
	  else
	    {
	      print_usage ();
	    }
	  continue;
	}
      else if (!strcmp (argv[i], "--startkey"))
	{
	  if (i < argc - 1)
	    {
	      i++;
	      start_key = strtol (argv[i], 0, 0);
	    }
	  else
	    {
	      print_usage ();
	    }
	  continue;
	}
      else if (!strcmp (argv[i], "--startfile"))
	{
	  if (i < argc - 1)
	    {
	      i++;
	      startfile = strdup (argv[i]);
	      continue;
	    }
	  else
	    {
	      print_usage ();
	    }
	}
      else if (!strcmp (argv[i], "--check"))
	{
	  do_buffer_checks = true;
	  printf ("do_buffer_checks=true\n");
	}
      else if (!strcmp (argv[i], "--nofsynccheck"))
	{
	  check_fsync_file = false;
	  printf ("check_fsync_file=false;\n");
	}
      else if (!strcmp (argv[i], "--filesync"))
	{
	  do_file_sync = true;
	  if (i < argc - 1 && argv[i + 1][0] != '-')
	    {
	      i++;
	      file_sync_base = strdup (argv[i]);
	    }
	}
      else if (!strcmp (argv[i], "--bsem_needed"))
	{
	  bsem_needed_default=true;
	}
      else if (!strcmp (argv[i], "--preserve_process_data"))
	{
	  preserve_process_data=true;
	}
      else if (!strcmp (argv[i], "--no_confirm_write_default"))
	{
	  confirm_write_default=false;
	}
      else if (!strcmp (argv[i], "--debug"))
	{
	  debug_nmlcfgsvr = true;
#ifndef DISABLE_RCS_PRINT
	  max_rcs_errors_to_print=-1;
#endif

	  printf ("NMLCFGSVR: DEBUG ON.\n");
	  continue;
	}
      else if (!strcmp (argv[i], "--minchecktime"))
	{
	  if (i < argc - 1)
	    {
	      i++;
	      min_check_time = strtod (argv[i], 0);
	      printf ("min_check_time=%f\n", min_check_time);
	    }
	  else
	    {
	      print_usage ();
	    }
	  continue;
	}
      else if (!strcmp (argv[i], "--help"))
	{
	  print_usage ();
	  continue;
	}
      else
	{
	  fprintf (stderr, "argument %s not recognized\n", argv[i]);
	  print_usage ();
	}
    }
  if (i < argc)
    {
      if (!strcmp (argv[i], "--debug"))
	{
	  debug_nmlcfgsvr = true;
	  printf ("NMLCFGSVR: DEBUG ON.\n");
	}
    }

  if (getenv ("NMLCFGSVR_IPV6"))
    {
      global_use_ipv6 = 1;
    }

  if (load_socket_interface () < 0)
    {
      rcs_print_error ("Can't load socket interface.\n");
      exit (-1);
    }


#if HAVE_GETENV
  myhostname = getenv ("HOSTNAME");
#else
  myhostname = 0;
#endif

  if (myhostname == 0)
    {
      dl_gethostname (myhostnamebuf, sizeof (myhostnamebuf));
      myhostname = myhostnamebuf;
    }
  else
    {
      myhostname = strdup (myhostname);
    }

  if (!my_ip_string)
    {
      my_ip_string = dl_get_this_host_address_string (global_use_ipv6);
    }

  if (my_ip_string)
    {
      myaddress_ptr = dl_create_sa (my_ip_string,
				    (short) local_port, 
				    global_use_ipv6);
    }
  else if (!my_ip_string &&
	   myhostname && myhostname[0] && strcmp (myhostname, "localhost"))
    {
      myaddress_ptr = dl_create_sa (myhostname, 
				    (short) local_port, 
				    global_use_ipv6);
      my_ip_string = dl_sa_get_host (myaddress_ptr);
    }
  else if (!my_ip_string)
    {
#ifdef VXWORKS
      // Rashmi Changed 
      my_in_addr.s_addr = hostGetByName (myhostname);
#else
      dl_modified_gethostbyname (myhostname, &my_hostent_ptr, 1);
      if (NULL != my_hostent_ptr)
	{
	  my_in_addr.s_addr = *((u_long *) my_hostent_ptr->h_addr_list[0]);
	}
#endif
#if HAVE_GETENV
      my_ip_string = getenv ("NMLCFGSVR_IP_STRING");
#else
      my_ip_string = 0;
#endif
      if (my_ip_string == 0)
	{
	  my_ip_string = dl_inet_ptr_ntoa (&my_in_addr);
	}
      if (my_ip_string != 0)
	{
	  my_ip_string = strdup (my_ip_string);
	}
    }
  unsigned long detected_addr = dl_sa_addr_u_long (myaddress_ptr);
  if (!global_use_ipv6)
    {
      if (detected_addr == htonl (INADDR_ANY) ||
	  detected_addr == htonl (INADDR_LOOPBACK)
#ifdef IN_MULTICAST
	  || IN_MULTICAST (detected_addr)
#endif
#ifdef IN_BADCLASS
	  || IN_BADCLASS (detected_addr)
#endif
	)
	{
	  printf
	    ("The IP address detected or set (%s) appears to be either the loopback address or an invalid address. It is recommended that this program be run with --localip option set to the true unique externally accessable IP address for this system.\n",
	     my_ip_string);
	  if(!strncmp("..::NULL::..",my_ip_string,12)) {
	    my_ip_string = strdup("127.0.0.1");
	  }
	}
    }

  if(my_ip_string)
    {
      const char * my_ip_string_dup = strdup(my_ip_string);
      my_ip_string=my_ip_string_dup;
    }

  rcs_print
    ("Registering server on TCP port %d, my_hostname=%s, my_ip_string=%s.\n",
     dl_sa_get_port (myaddress_ptr), myhostname, my_ip_string);

  if (dl_sa_get_port (myaddress_ptr) == 0 || local_port < 1)
    {
      rcs_print_error ("Bad port number %ld\n", local_port);
      exit (-1);
    }

  if (start_key == 0)
    {
      start_key = (int) ((detected_addr & 0x1FFFFF) << 10 ^ local_port << 15);
      printf
	("If you run other instances of nmlcfgsvr that could interact with processes run on this system or vice-versa,\n it would be safer to set --startkey to a value far enough away from the value used here %d(0x%X) to avoid conflicts.\n",
	 start_key, start_key);
    }

  if (start_port == 0)
    {
      start_port = (short) (local_port + 1);
    }
  min_key = 1 << 30;
  max_key = 0;
  min_port = 1 << 30;
  max_port = 0;
  min_buffer_number = buffer_number;
  max_buffer_number = buffer_number;

  if (startfile)
    {
      FILE *f = fopen (startfile, "r");
      if (!f)
	{
	  fprintf (stderr, "Could not open %s : %s\n",
		   startfile, strerror (errno));
	}
      while (!feof (f))
	{
	  fgets (line, sizeof (line), f);
	  process_line (0);
	}
      fclose (f);
    }

  if (do_file_sync)
    {
      int baselen = (int) strlen (file_sync_base);
      char *f1_sync_name_buf = (char *) malloc (baselen + 5);
      strcpy (f1_sync_name_buf, file_sync_base);
      strcpy (f1_sync_name_buf + baselen, "1.nml");
      f1_sync_name = f1_sync_name_buf;
      char *f2_sync_name_buf = (char *) malloc (baselen + 5);
      strcpy (f2_sync_name_buf, file_sync_base);
      strcpy (f2_sync_name_buf + baselen, "2.nml");
      f2_sync_name = f2_sync_name_buf;
      int f1_num = get_fsync_num (f1_sync_name);
      int f2_num = get_fsync_num (f2_sync_name);
      if (f1_num > 0 &&
	  ((f1_num > f2_num
	   && !(f1_num > 3 * FSYNC_NUM_MAX / 4 && f2_num < FSYNC_NUM_MAX / 4))
	  || (f2_num > 3 * FSYNC_NUM_MAX / 5 && f1_num < FSYNC_NUM_MAX / 4)))
	{
	  load_fsync_file (f1_sync_name);
	  fsync_num = f1_num + 1;
	  fsync_toggle = false;
	  write_sync_file ();
	}
      else if (f2_num > 0)
	{
	  load_fsync_file (f2_sync_name);
	  fsync_num = f1_num + 2;
	  fsync_toggle = true;
	  write_sync_file ();
	}
      else
	{
	  fprintf (stderr, "Can't read either %s or %s\n",
		   f1_sync_name, f2_sync_name);
	}
    }

  if (max_key > start_key &&
      (max_key - start_key < 0x10000 || (min_key - start_key < 0x10000)))
    {
      key = max_key + 1;
    }
  else
    {
      key = start_key;
    }

  if (max_port > start_port &&
      (max_port - start_port < 500 || (min_port - start_port < 500)))
    {
      port = max_port + 1;
    }
  else
    {
      port = start_port;
    }
  buffer_number = max_buffer_number + 1;
  if (debug_nmlcfgsvr)
    {
      printf ("start_key=%d\n", start_key);
      printf ("start_port=%d\n", start_port);
      printf ("min_key=%d\n", min_key);
      printf ("max_key=%d\n", max_key);
      printf ("min_port=%d\n", min_port);
      printf ("max_port=%d\n", max_port);
      printf ("min_buffer_number=%d\n", min_buffer_number);
      printf ("max_buffer_number=%d\n", max_buffer_number);
    }


  SOCKET connection_socket=0;

  if ((long) (connection_socket = dl_tcp_socket (global_use_ipv6)) <= 0)
    {
      rcs_print_error ("socket error: %d -- %s\n", errno, strerror (errno));
      rcs_print_error ("Server can not open stream socket.\n");
      exit (-1);
    }

  if (set_tcp_socket_options ((int) connection_socket) < 0)
    {
      rcs_print_error ("Failed to set socket options.\n");
      exit (-1);
    }
  char *bind_to_host = 
    getenv("NML_BINDTO_HOST");
  struct dl_sa *binding_address_ptr =
    dl_create_sa (bind_to_host, 
		  (short) local_port, 
		  global_use_ipv6);
  if (dl_bind
      (connection_socket, dl_sa_addr (binding_address_ptr),
       dl_sa_len (binding_address_ptr)) < 0)
    {
      rcs_print_error ("bind error: %d -- %s\n", errno, strerror (errno));
      rcs_print_error
	("Server can not bind the connection socket on port %d.\n",
	 dl_sa_get_port (myaddress_ptr));
      exit (-1);
    }
  dl_free_sa (binding_address_ptr);
  rcs_print_debug (PRINT_MISC, "Bound %d to port %d of %s\n",
		   connection_socket,
		   dl_sa_get_port (myaddress_ptr), my_ip_string);

  if (dl_listen (connection_socket, 50) < 0)
    {
      rcs_print_error ("listen error: %d -- %s\n", errno, strerror (errno));
      rcs_print_error ("TCP Server: error on call to listen for port %d.\n",
		       dl_sa_get_port (myaddress_ptr));
      exit (-1);
    }
#ifdef SIGPIPE
  signal (SIGPIPE, handle_pipe_error);
#endif

  signal (SIGINT, handle_sigint);

  touch_running_file();
  rcs_print_debug (PRINT_MISC,
		   "Running NMLCFGSVR on TCP port %d, Connection Socket FD =%d \n",
		   dl_sa_get_port (myaddress_ptr), connection_socket);

  unsigned long bytes_ready;
  unsigned long bytes_recvd;
  int ready_descriptors;
  fd_set read_fd_set, write_fd_set;
  FD_ZERO (&read_fd_set);
  FD_ZERO (&write_fd_set);
  RCS_FD_SET (connection_socket, &read_fd_set);
  SOCKET maxfdpl;
  maxfdpl = connection_socket + 1;
  RCS_LINKED_LIST *clients_list = new RCS_LINKED_LIST ();;
  CLIENT_INFO *current_entry;
  int sockerrno;
  char sockerrbuf[256];
  const char *sockerrstr;
  unsigned long bytes_free_after_end;
  struct timeval select_timeout_timeval;
  struct timeval default_select_timeout_timeval;
  default_select_timeout_timeval.tv_usec = 0;
  default_select_timeout_timeval.tv_sec = 20;
  bool someone_waiting_for_line = false;

  while (!nmlcfgsvr_quit)
    {
      if (!someone_waiting_for_line)
	{
	  ready_descriptors =
	    dl_select ((int) maxfdpl,
		       &read_fd_set,
		       &write_fd_set, (fd_set *) NULL, (timeval *) NULL);
	}
      else
	{
	  select_timeout_timeval = default_select_timeout_timeval;
	  ready_descriptors =
	    dl_select ((int) maxfdpl,
		       &read_fd_set,
		       &write_fd_set, (fd_set *) NULL,
		       (timeval *) & select_timeout_timeval);
	  if (ready_descriptors == 0)
	    {
	      current_entry = (CLIENT_INFO *) clients_list->get_head ();
	      while (NULL != current_entry)
		{
		  if (current_entry->wait_for_line)
		    {
		      strncpy (line, current_entry->wait_for_line,
			       sizeof (line));
		      process_line (current_entry);
		    }
		  current_entry = (CLIENT_INFO *) clients_list->get_next ();
		}
	    }
	}
      if (nmlcfgsvr_quit)
	{
	  break;
	}
      if (ready_descriptors < 0)
	{
	  rcs_print_error
	    ("select(%d, ...) error.(errno = %d | %s)\n",
	     maxfdpl, errno, strerror (errno));
	  continue;
	}
      current_entry = (CLIENT_INFO *) clients_list->get_head ();
      while (NULL != current_entry)
	{
	  if(((long) current_entry->client_socket) < 1)
	    {
	      delete current_entry;
	      clients_list->delete_current_node ();
	      current_entry = (CLIENT_INFO *) clients_list->get_next ();
	      continue;
	    }
	  if (current_entry->have_unsent_data)
	    {
	      current_entry->Send_Old ();
	    }
	  if (dl_fd_isset (current_entry->client_socket, &write_fd_set))
	    {
	      ready_descriptors--;
	    }
	  if (dl_fd_isset (current_entry->client_socket, &read_fd_set))
	    {
	      ready_descriptors--;
	      dl_ioctlsocket_fionread_ulp ((int) current_entry->client_socket,
					   &bytes_ready);
	      if (bytes_ready == 0)
		{
		  rcs_print_debug (PRINT_MISC,
				   "Socket closed by host with IP address %s.\n",
				   dl_sa_get_host (current_entry->
						   client_address_ptr));
		  if(((long)current_entry->client_socket) > 0)
		    {
		      dl_closesocket (current_entry->client_socket);
		      RCS_FD_CLR (current_entry->client_socket, &read_fd_set);
		      current_entry->client_socket= 0;
		    }
		  delete current_entry;
		  clients_list->delete_current_node ();
		  current_entry = (CLIENT_INFO *) clients_list->get_next ();
		  continue;
		}
	      else
		{
		  bytes_free_after_end = (unsigned long) 
		   ( sizeof (current_entry->ring_buffer) -
		    (current_entry->endptr - current_entry->ring_buffer) - 1);
		  if (current_entry->startptr > current_entry->endptr)
		    {
		      bytes_free_after_end = (unsigned long)
			( current_entry->startptr - current_entry->endptr - 1);
		    }
		  if (bytes_free_after_end > 0)
		    {
		      if (bytes_ready > bytes_free_after_end)
			{
			  recvn ((int) current_entry->client_socket,
				 current_entry->endptr,
				 bytes_free_after_end, 0, -1, NULL,1);
			  bytes_ready -= bytes_free_after_end;
			  current_entry->endptr = current_entry->ring_buffer;
			}
		      else
			{
			  bytes_recvd = recvn ((int) current_entry->client_socket,
					       current_entry->endptr,
					       bytes_ready, 0, -1, NULL,1);
			  current_entry->endptr += bytes_recvd;
			  bytes_ready -= bytes_recvd;
			}
		    }
		  if (bytes_ready > 0)
		    {
		      unsigned long bytes_free_at_start = (unsigned long)
			( current_entry->startptr - current_entry->ring_buffer - 1 );

		      if (current_entry->startptr > current_entry->endptr &&
			  current_entry->endptr > current_entry->ring_buffer)
			{
			  bytes_free_at_start = 0;
			}
		      if (bytes_free_at_start > 0)
			{
			  if (bytes_ready > bytes_free_at_start)
			    {
			      recvn ((int) current_entry->client_socket,
				     current_entry->ring_buffer,
				     bytes_free_at_start, 0, -1, NULL,1);
			      bytes_ready -= bytes_free_at_start;
			      current_entry->endptr =
				current_entry->ring_buffer +
				bytes_free_at_start;
			    }
			  else
			    {
			      recvn ((int) current_entry->client_socket,
				     current_entry->ring_buffer,
				     bytes_ready, 0, -1, NULL,1);
			      current_entry->endptr =
				current_entry->ring_buffer + bytes_ready;
			      bytes_ready = 0;
			    }
			}
		    }
		  process_data (current_entry);
		  if (current_entry->quit)
		    {
		      rcs_print
			("Socket closed to host with IP address %s. (quit command recieved.)\n",
			 dl_sa_get_host (current_entry->client_address_ptr));
		      if(((long) current_entry->client_socket) > 0)
			{
			  dl_closesocket (current_entry->client_socket);
			  RCS_FD_CLR (current_entry->client_socket, &read_fd_set);
			  current_entry->client_socket= ((SOCKET)0);
			}
		      clients_list->delete_current_node ();
		    }
		}
	    }
	  else
	    {
	      if(((long) current_entry->client_socket) < 1)
		{
		  delete current_entry;
		  clients_list->delete_current_node ();
		  current_entry = (CLIENT_INFO *) clients_list->get_next ();
		  continue;
		}
	      RCS_FD_SET (current_entry->client_socket, &read_fd_set);
	    }
	  if (current_entry->have_unsent_data)
	    {
	      current_entry->Send_Old ();
	    }
	  if(((long) current_entry->client_socket) < 1)
	    {
	      delete current_entry;
	      clients_list->delete_current_node ();
	      current_entry = (CLIENT_INFO *) clients_list->get_next ();
	      continue;
	    }
	  if (current_entry->have_unsent_data)
	    {
	      RCS_FD_SET (current_entry->client_socket, &write_fd_set);
	    }
	  else
	    {
	      RCS_FD_CLR (current_entry->client_socket, &write_fd_set);
	    }
	  current_entry = (CLIENT_INFO *) clients_list->get_next ();
	}
      if (dl_fd_isset (connection_socket, &read_fd_set)
	  && ready_descriptors > 0)
	{
	  ready_descriptors--;
	  current_entry = new CLIENT_INFO;
	  if (NULL == current_entry)
	    {
	      continue;
	    }
	  int client_address_length;
	  client_address_length =
	    dl_sa_len (current_entry->client_address_ptr);
	  current_entry->client_socket =
	    dl_accept (connection_socket,
		       dl_sa_addr (current_entry->client_address_ptr),
		       &client_address_length);
	  if (((long) current_entry->client_socket) <= 0)
	    {
	      sockerrno =
		dl_get_last_socket_error_int ((int) current_entry->client_socket);
	      sockerrstr =
		dl_get_last_socket_error_string ((int) current_entry->client_socket,
						 sockerrno, sockerrbuf,
						 sizeof (sockerrbuf));
	      rcs_print_error ("accept error = (%d:%s)\n", sockerrno,
			       sockerrstr);
	      delete current_entry;
	      continue;
	    }
	  if (set_tcp_socket_options ((int) current_entry->client_socket) < 0)
	    {
	      rcs_print_error ("Failed to set socket options.\n");
	    }
	  if (make_tcp_socket_nonblocking ((int) current_entry->client_socket) < 0)
	    {
	      rcs_print_error ("Failed to make socket nonblocking.\n");
	    }
	  if (NULL != clients_list)
	    {
	      clients_list->store_at_tail (current_entry,
					   sizeof (CLIENT_INFO), 0);
	    }
	  if (maxfdpl < current_entry->client_socket + 1)
	    {
	      maxfdpl = current_entry->client_socket + 1;
	    }
	  if (prompt && promptlen > 0)
	    {
	      current_entry->Send (prompt, (int) promptlen);
	    }
	  RCS_FD_SET (current_entry->client_socket, &read_fd_set);
	  rcs_print_debug (PRINT_MISC,
			   "Socket opened by host with IP address %s.\n",
			   dl_sa_get_host (current_entry->
					   client_address_ptr));
	}
      else
	{
	  RCS_FD_SET (connection_socket, &read_fd_set);
	}
      current_entry = (CLIENT_INFO *) clients_list->get_head ();
      while (NULL != current_entry)
	{
	  if (current_entry->wait_for_line)
	    {
	      strncpy (line, current_entry->wait_for_line, sizeof (line));
	      process_line (current_entry);
	    }
	  current_entry = (CLIENT_INFO *) clients_list->get_next ();
	}
      someone_waiting_for_line = false;
      current_entry = (CLIENT_INFO *) clients_list->get_head ();
      while (NULL != current_entry)
	{
	  if (current_entry->wait_for_line)
	    {
	      someone_waiting_for_line = true;
	      break;
	    }
	  current_entry = (CLIENT_INFO *) clients_list->get_next ();
	}
      if (0 != ready_descriptors && !nmlcfgsvr_quit)
	{
	  rcs_print_error ("%d descriptors ready but not serviced.\n",
			   ready_descriptors);
	}
    }

  current_entry = (CLIENT_INFO *) clients_list->get_head ();
  while (NULL != current_entry)
    {
      rcs_print
	("Socket closed to host with IP address %s. (Server is exiting.)\n",
	 dl_sa_get_host (current_entry->client_address_ptr));
      delete current_entry;
      clients_list->delete_current_node ();
      current_entry = (CLIENT_INFO *) clients_list->get_next ();
    }
  if(((long) connection_socket) > 0)
    {
      dl_closesocket (connection_socket);
      connection_socket=0;
    }

  unload_socket_interface ();
  if(clients_list)
    {
      CLIENT_INFO *client_info = (CLIENT_INFO *) clients_list->get_head();
      while(client_info)
	{
	  delete client_info;
	  clients_list->delete_current_node();
	  client_info = (CLIENT_INFO *) clients_list->get_next();
	}
      delete clients_list;
      clients_list = 0;
    }
  if (saveonexitfile)
    {
      save_buffers (saveonexitfile);
    }
  if (buffers_list)
    {
      blist_mod++;
      BUFFER_INFO *binfo = (BUFFER_INFO *) buffers_list->get_head();
      while(binfo)
	{
	  delete binfo;
	  buffers_list->delete_current_node();
	  binfo = (BUFFER_INFO *) buffers_list->get_next();
	}
      delete buffers_list;
      buffers_list = 0;
    }
  if (startfile)
    {
      free ((void *) startfile);
      startfile = 0;
    }
  return(0);
}

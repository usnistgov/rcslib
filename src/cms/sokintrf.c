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


#define CMS_NETWORK_SOURCE 1

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"

#if MS_WINDOWS_API
#define USE_WINSOCK
#endif

#else
#include "sokintrf_no_config.h"
#endif
/* HAVE_CONFIG_H */

#  ifndef __NO_INCLUDE_WARN__
#   define __NO_INCLUDE_WARN__ 1
#   define SOKINTRF_SET_NO_INCLUDE_WARN 1
#  endif

#include "sokintrf.h"		/* Forward Fuction Prototypes */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "_timer.h"

#if HAVE_LINUX_SOCKIOS_H
#include <linux/sockios.h>
#endif

#if HAVE_NET_IF_H
#include <net/if.h>
#endif

extern int inet_pton(int af, const char *src, void *dst);

enum SOCKET_AVAILABILITY
{
  SOCKET_AVAIL_NOT_CHECKED,
  SOCKETS_AVAILABLE,
  SOCKETS_NOT_AVAILABLE
};
enum SOCKET_AVAILABILITY socket_availability = SOCKET_AVAIL_NOT_CHECKED;


static int sokintrf_socket_count = 0;

/* Declare Pointers to all the Socket Functions */
#if defined(USE_DYNAMICALLY_LOADED_POINTERS)

#if MS_WINDOWS_API
#ifndef RCS_PASCAL
#define RCS_PASCAL
#endif
typedef int (RCS_PASCAL * PTR_TO_WSAGETLASTERROR) (void);
PTR_TO_WSAGETLASTERROR ptr_to_wsa_getlasterror;
#endif

typedef int (RCS_PASCAL * PTR_TO_SEND) (SOCKET, char *, int,
						int);
extern PTR_TO_SEND ptr_to_send;
#ifdef MSDOS
typedef int (RCS_PASCAL * PTR_TO_SELECT) (int, void *, void *, void *,
						  struct timeval *);
#else
typedef int (RCS_PASCAL * PTR_TO_SELECT) (int, fd_set *, fd_set *,
						  fd_set *, struct timeval *);
#endif
extern PTR_TO_SELECT ptr_to_select;
typedef int (RCS_PASCAL * PTR_TO_RECV) (SOCKET, char *, int,
						int);
extern PTR_TO_RECV ptr_to_recv;
typedef int (RCS_PASCAL * PTR_TO_RECVFROM) (SOCKET, char *,
						    int, int,
						    struct sockaddr *, int *);
extern PTR_TO_RECVFROM ptr_to_recvfrom;
typedef int (RCS_PASCAL * PTR_TO_SENDTO) (SOCKET, char *, int,
						  int, struct sockaddr *,
						  int);
extern PTR_TO_SENDTO ptr_to_sendto;
typedef int (RCS_PASCAL * PTR_TO_SETSOCKOPT) (SOCKET s, int level,
						      int optname,
						      const char *
						      optval, int optlen);
extern PTR_TO_SETSOCKOPT ptr_to_setsockopt;
typedef int (RCS_PASCAL * PTR_TO_IOCTLSOCKET) (SOCKET s,
						       long cmd,
						       unsigned long *
						       argp);
extern PTR_TO_IOCTLSOCKET ptr_to_ioctlsocket;
typedef unsigned short (RCS_PASCAL *
			PTR_TO_NTOHS) (unsigned short netshort);
extern PTR_TO_NTOHS ptr_to_ntohs;
typedef unsigned long (RCS_PASCAL *
		       PTR_TO_NTOHL) (unsigned long netlong);
extern PTR_TO_NTOHL ptr_to_ntohl;
typedef unsigned short (RCS_PASCAL *
			PTR_TO_HTONS) (unsigned short hostshort);
extern PTR_TO_HTONS ptr_to_htons;
typedef unsigned long (RCS_PASCAL *
		       PTR_TO_HTONL) (unsigned long hostlong);
extern PTR_TO_HTONL ptr_to_htonl;
typedef int (RCS_PASCAL * PTR_TO_LISTEN) (SOCKET, int);
extern PTR_TO_LISTEN ptr_to_listen;
typedef int (RCS_PASCAL * PTR_TO_CONNECT) (SOCKET,
						   struct sockaddr *,
						   int);
extern PTR_TO_CONNECT ptr_to_connect;
typedef int (RCS_PASCAL * PTR_TO_CLOSESOCKET) (SOCKET);
extern PTR_TO_CLOSESOCKET ptr_to_closesocket;
typedef SOCKET (RCS_PASCAL * PTR_TO_SOCKET) (int, int, int);
extern PTR_TO_SOCKET ptr_to_socket;
typedef unsigned long (RCS_PASCAL *
		       PTR_TO_INET_ADDR) (char *);
extern PTR_TO_INET_ADDR ptr_to_inet_addr;
typedef char *(RCS_PASCAL *
		       PTR_TO_INET_NTOA) (struct in_addr);
extern PTR_TO_INET_NTOA ptr_to_inet_ntoa;
extern PTR_TO_ACCEPT ptr_to_accept;
typedef int (RCS_PASCAL * PTR_TO_BIND) (SOCKET,
						struct sockaddr *,
						int);
extern PTR_TO_BIND ptr_to_bind;
typedef struct hostent *(RCS_FAR * PTR_TO_GETHOSTBYNAME) (char *);
extern PTR_TO_GETHOSTBYNAME ptr_to_get_hostbyname;
#if MS_WINDOWS_API
typedef int (RCS_FAR * PTR_TO_WSAISFDSET) (SOCKET, fd_set *);
extern PTR_TO_WSAISFDSET ptr_to_wsaisfdset;
#endif

PTR_TO_SOCKET ptr_to_socket = NULL;
PTR_TO_CLOSESOCKET ptr_to_closesocket = NULL;
PTR_TO_ACCEPT ptr_to_accept = NULL;
PTR_TO_BIND ptr_to_bind = NULL;
PTR_TO_CONNECT ptr_to_connect = NULL;
PTR_TO_LISTEN ptr_to_listen = NULL;
PTR_TO_SELECT ptr_to_select = NULL;
PTR_TO_SEND ptr_to_send = NULL;
PTR_TO_SENDTO ptr_to_sendto = NULL;
PTR_TO_RECV ptr_to_recv = NULL;
PTR_TO_RECVFROM ptr_to_recvfrom = NULL;
PTR_TO_INET_ADDR ptr_to_inet_addr = NULL;
PTR_TO_INET_NTOA ptr_to_inet_ntoa = NULL;
PTR_TO_HTONL ptr_to_htonl = NULL;
PTR_TO_HTONS ptr_to_htons = NULL;
PTR_TO_NTOHL ptr_to_ntohl = NULL;
PTR_TO_NTOHS ptr_to_ntohs = NULL;
PTR_TO_SETSOCKOPT ptr_to_setsockopt = NULL;
PTR_TO_IOCTLSOCKET ptr_to_ioctlsocket = NULL;
PTR_TO_GETHOSTBYNAME ptr_to_gethostbyname = NULL;

PTR_TO_WSAISFDSET ptr_to_wsaisfdset = NULL;

#if BUILD_PLATFORM == "autoconf-i686-pc-cygwin"
extern int inet_pton(int af, const char *src, void *dst);
#endif


/* Prototypes for functions only used within this file. */
static SOCKET dl_socket (int domain, int type, int protocol);
static struct sockaddr *dl_create_sockaddr(const char *hostname, short port, size_t *len_ptr, int use_ipv6);
static struct sockaddr *dl_create_broadcast_sockaddr(const char *hostname, short port, size_t *len_ptr, int use_ipv6);
static const char *dl_sockaddr_to_string(const struct sockaddr * addr, char *buf, int buflen, int use_ipv6);
static int dl_sockaddr_get_port(const struct sockaddr * addr,int use_ipv6);
static void dl_sockaddr_set_port(const struct sockaddr * addr,short port, int use_ipv6);

static FARPROC GetProc (char * proc_name);

FARPROC
GetProc (char * proc_name)
{
  FARPROC retval = NULL;
  static char buffer[128];
  char *ptr;
  if (socket_library_instance == NULL)
    {
      return NULL;
    }
  retval = GetProcAddress (socket_library_instance, proc_name);
  if (retval != NULL)
    {
      return retval;
    }
  strcpy (buffer, proc_name);
  ptr = buffer;
  while (*ptr)
    {
      *ptr = (char) toupper (*ptr);
    }
  retval = GetProcAddress (socket_library_instance, proc_name);
  if (retval != NULL)
    {
      return retval;
    }
  ptr = buffer;
  while (*ptr)
    {
      *ptr = (char) tolower (*ptr);
    }
  retval = GetProcAddress (socket_library_instance, buffer);
  if (retval != NULL)
    {
      return retval;

    }
  return NULL;
}

#endif /* USE_DYNAMICALLY_LOADED_POINTERS */

static int socket_interface_count=0;

int
load_socket_interface (void)
{
#ifdef MS_WINDOWS_API
  short winsock_version;
  int wsa_startup_retval;
  WSADATA startup_data;
#endif

  socket_interface_count++;

#ifndef MS_WINDOWS_API
  socket_availability = SOCKETS_AVAILABLE;
  return 0;
#else

  if (SOCKETS_NOT_AVAILABLE == socket_availability)
    {
      return -1;
    }
  if (SOCKETS_AVAILABLE == socket_availability)
    {
      return 0;
    }
  socket_availability = SOCKETS_NOT_AVAILABLE;

#if defined(USE_DYNAMICALLY_LOADED_POINTERS)
#if MS_WINDOWS_API
  socket_library_instance = LoadLibrary ("wsock32.dll");
  if (socket_library_instance == NULL)
    {
      rcs_print_error ("Error on LoadLibrary(" "wsock32.dll" ")\n");
      return -1;
    }
#else
  socket_library_instance = (void *) LoadLibrary ("winsock.dll");
  if (((int) socket_library_instance) < HINSTANCE_ERROR)
    {
      rcs_print_error ("Error %d from LoadLibrary(" "winsock.dll" ")\n",
		       socket_library_instance);
      return -1;
    }
#endif
  ptr_to_wsastartup = (PTR_TO_WSASTARTUP) GetProc ("WSAStartup");
  if (NULL == ptr_to_wsastartup)
    {
      rcs_print_error ("Cannot find WSAStartup function.\n");
      return -1;
    }
  ptr_to_wsacleanup = (PTR_TO_WSACLEANUP) GetProc ("WSACleanup");
  if (NULL == ptr_to_wsacleanup)
    {
      rcs_print_error ("Cannot find WSACleanup function.\n");
      return -1;
    }
  ptr_to_wsagetlasterror = (PTR_TO_WSAGETLASTERROR)
    GetProc ("WSAGetLastError");
  if (NULL == ptr_to_wsagetlasterror)
    {
      rcs_print_error ("Cannot find WSAGetLastError function.\n");
      return -1;
    }
  ptr_to_wsaisfdset = (PTR_TO_WSAISFDSET) GetProc ("__WSAFDIsSet");
  if (NULL == ptr_to_wsaisfdset)
    {
      rcs_print_error ("Cannot find WSAGetLastError function.\n");
      return -1;
    }
  ptr_to_socket = (PTR_TO_SOCKET) GetProc ("socket");
  if (NULL == ptr_to_socket)
    {
      rcs_print_error ("Cannot find socket function.\n");
      return -1;
    }
  ptr_to_closesocket = (PTR_TO_CLOSESOCKET) GetProc ("closesocket");
  if (NULL == ptr_to_closesocket)
    {
      rcs_print_error ("Cannot find closesocket function.\n");
      return -1;
    }
  ptr_to_bind = (PTR_TO_BIND) GetProc ("bind");
  if (NULL == ptr_to_bind)
    {
      rcs_print_error ("Cannot find bind function.\n");
      return -1;
    }
  ptr_to_connect = (PTR_TO_CONNECT) GetProc ("connect");
  if (NULL == ptr_to_connect)
    {
      rcs_print_error ("Cannot find connect function.\n");
      return -1;
    }
  ptr_to_accept = (PTR_TO_ACCEPT) GetProc ("accept");
  if (NULL == ptr_to_accept)
    {
      rcs_print_error ("Cannot find accept function.\n");
      return -1;
    }
  ptr_to_listen = (PTR_TO_LISTEN) GetProc ("listen");
  if (NULL == ptr_to_listen)
    {
      rcs_print_error ("Cannot find listen function.\n");
      return -1;
    }
  ptr_to_select = (PTR_TO_SELECT) GetProc ("select");
  if (NULL == ptr_to_select)
    {
      rcs_print_error ("Cannot find select function.\n");
      return -1;
    }
  ptr_to_send = (PTR_TO_SEND) GetProc ("send");
  if (NULL == ptr_to_send)
    {
      rcs_print_error ("Cannot find send function.\n");
      return -1;
    }
  ptr_to_recv = (PTR_TO_RECV) GetProc ("recv");
  if (NULL == ptr_to_recv)
    {
      rcs_print_error ("Cannot find recv function.\n");
      return -1;
    }
  ptr_to_sendto = (PTR_TO_SENDTO) GetProc ("sendto");
  if (NULL == ptr_to_sendto)
    {
      rcs_print_error ("Cannot find sendto function.\n");
      return -1;
    }
  ptr_to_recvfrom = (PTR_TO_RECVFROM) GetProc ("recvfrom");
  if (NULL == ptr_to_recvfrom)
    {
      rcs_print_error ("Cannot find recvfrom function.\n");
      return -1;
    }
  ptr_to_inet_addr = (PTR_TO_INET_ADDR) GetProc ("inet_addr");
  if (NULL == ptr_to_inet_addr)
    {
      rcs_print_error ("Cannot find inet_addr function.\n");
      return -1;
    }
  ptr_to_inet_ntoa = (PTR_TO_INET_NTOA) GetProc ("inet_ntoa");
  if (NULL == ptr_to_inet_ntoa)
    {
      rcs_print_error ("Cannot find inet_ntoa function.\n");
      return -1;
    }

  ptr_to_htonl = (PTR_TO_HTONL) GetProc ("htonl");
  if (NULL == ptr_to_htonl)
    {
      rcs_print_error ("Cannot find htonl function.\n");
      return -1;
    }
  ptr_to_htons = (PTR_TO_HTONS) GetProc ("htons");
  if (NULL == ptr_to_htons)
    {
      rcs_print_error ("Cannot find htons function.\n");
      return -1;
    }
  ptr_to_ntohl = (PTR_TO_NTOHL) GetProc ("ntohl");
  if (NULL == ptr_to_ntohl)
    {
      rcs_print_error ("Cannot find ntohl function.\n");
      return -1;
    }
  ptr_to_ntohs = (PTR_TO_NTOHS) GetProc ("ntohs");
  if (NULL == ptr_to_ntohs)
    {
      rcs_print_error ("Cannot find ntohs function.\n");
      return -1;
    }
  ptr_to_setsockopt = (PTR_TO_SETSOCKOPT) GetProc ("setsockopt");
  if (NULL == ptr_to_setsockopt)
    {
      rcs_print_error ("Cannot find setsockopt function.\n");
      return -1;
    }
  ptr_to_ioctlsocket = (PTR_TO_IOCTLSOCKET) GetProc ("ioctlsocket");
  if (NULL == ptr_to_ioctlsocket)
    {
      rcs_print_error ("Cannot find ioctlsocket function.\n");
      return -1;
    }
  ptr_to_gethostbyname = (PTR_TO_GETHOSTBYNAME) GetProc ("gethostbyname");
  if (NULL == ptr_to_gethostbyname)
    {
      rcs_print_error ("Cannot find gethostbyname function.\n");
      return -1;
    }

  winsock_version = MAKEWORD (2, 0);
  wsa_startup_retval = (*ptr_to_wsastartup) (winsock_version, &startup_data);
  if (wsa_startup_retval)
    {
      rcs_print_error ("Error %d from WSAStartup.\n", wsa_startup_retval);
      return -1;
    }
  socket_availability = SOCKETS_AVAILABLE;
#else /* USE_DYNAMICALLY_LOADED_POINTERS */
#ifdef USE_WINSOCK
  winsock_version = MAKEWORD (2, 0);
  wsa_startup_retval = WSAStartup (winsock_version, &startup_data);
  if (wsa_startup_retval)
    {
      rcs_print_error ("Error %d from WSAStartup.\n", wsa_startup_retval);
      return -1;
    }
  socket_availability = SOCKETS_AVAILABLE;
  return 0;
#endif
#endif /* USE_DYNAMICALLY_LOADED_POINTERS */

#endif
}

void
unload_socket_interface ()
{
  if(socket_interface_count > 1)
    { 
      socket_interface_count--;
      return;
    }
  else if(socket_interface_count == 1 && sokintrf_socket_count == 0)
    {
#ifdef USE_DYNAMICALLY_LOADED_POINTERS
      if (SOCKETS_AVAILABLE == socket_availability
	  && NULL != ptr_to_wsacleanup && socket_library_instance != NULL)
	{
	  (*ptr_to_wsacleanup) ();
	  FreeLibrary (socket_library_instance);
	}
      socket_library_instance = NULL;
      ptr_to_wsastartup = NULL;
      ptr_to_wsagetlasterror = NULL;
      ptr_to_wsacleanup = NULL;
#else
#if MS_WINDOWS_API
      WSACleanup();
#endif
#endif /* USE_DYNAMICALLY_LOADED_POINTERS */
      socket_availability = SOCKET_AVAIL_NOT_CHECKED;
      socket_interface_count = 0;
    }
}

struct in_addr *dl_create_new_in_addr_struct(void)
{
  return (struct in_addr *) malloc(sizeof(struct in_addr));
}

void 
dl_free_created_in_addr_struct(struct in_addr *inap)
{
  if(!inap)
    {
      rcs_print_error("dl_free_created_in_addr_struct : BAD argument.\n");
      return;
    }
  free(inap);
}


void dl_free_sockaddr(struct sockaddr *sp);

#ifndef VXWORKS
struct hostent *dl_create_new_hostent_struct(void)
{
  return (struct hostent*) malloc(sizeof(struct hostent));
}

void dl_free_created_hostent_struct(struct hostent *hptr)
{
  if(!hptr)
    {
      rcs_print_error("dl_freehostent : BAD argument.\n");
      return;
    }

  free(hptr);
}

int dl_copyhostent(struct hostent *dest, struct hostent *src)
{
  if(0 == dest || 0 == src)
    {
      rcs_print_error("dl_copyhostent:  BAD argument.\n");
      return -1;
    }
  *dest = *src;
  return ((int) sizeof(struct hostent));
}

char *dl_gethostent_h_addr_from_list(struct hostent* hptr, int i)
{
  if(!hptr)
    {
      rcs_print_error("dl_gethostent_h_addr_from_list : BAD argument.\n");
      return 0;
    }
  return (char *) hptr->h_addr_list[i];
}

int dl_gethostent_h_length(struct hostent *hptr)
{
  if(!hptr)
    {
      rcs_print_error("dl_gethostent_h_addr_from_list : BAD argument.\n");
      return 0;
    }
  return hptr->h_length;
}

#endif


#if MS_WINDOWS_API
static int
dl_WSAGetLastError ()
{
#ifndef USE_WINSOCK
  return errno;
#else
#ifdef USE_DYNAMICALLY_LOADED_POINTERS
  if (socket_availability != SOCKETS_AVAILABLE
      || NULL == ptr_to_wsagetlasterror)
    {
      rcs_print_error ("Can not call WSAGetLastError.\n");
      return 0;
    }
  return (*ptr_to_wsagetlasterror) ();
#else
  return WSAGetLastError ();
#endif
#endif
}
#endif

int
dl_fd_isset (SOCKET s, fd_set_ptr set)
{
#if !defined(_WINDOWS) || !defined(USE_DYNAMICALLY_LOADED_POINTERS)
  return ((int) FD_ISSET (s, (fd_set *) set));
#else
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_wsaisfdset)
    {
      rcs_print_error ("Can not call __WSAFDIsSet.\n");
      return 0;
    }
  return (*ptr_to_wsaisfdset) (s,(fd_set *)  set);
#endif
}


int
dl_accept (SOCKET s, struct sockaddr *name, int *plen)
{
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
#if HAVE_SOCKLEN_T_TYPE
  socklen_t accept_addrlen;
  int retval;
  if(plen)
    {
      accept_addrlen = (socklen_t) (*plen);
    }
  retval = accept (s, name, &accept_addrlen);
  if(plen)
    {
      (*plen) = (int) accept_addrlen;
    }
  return retval;
#else
  return( (int) accept(s,name,plen));
#endif
#else
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_accept)
    {
      rcs_print_error ("Cannot call accept ().\n");
      return -1;
    }
  return (*ptr_to_accept) (s, name, plen);
#endif
}


int
dl_bind (SOCKET s, struct sockaddr *addr, int addrlen)
{
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
  return bind (s, addr, addrlen);
#else
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_bind)
    {
      rcs_print_error ("Cannot call bind ().\n");
      return -1;
    }
  return (*ptr_to_bind) (s, addr, addrlen);
#endif
}

int
dl_closesocket (SOCKET s)
{
  if(sokintrf_socket_count > 0)
    {
      sokintrf_socket_count--;
    }
#ifndef USE_WINSOCK
  return close (s);
#else
#ifdef USE_DYNAMICALLY_LOADED_POINTERS
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_closesocket)
    {
      rcs_print_error ("Cannot call closesocket ().\n");
      return -1;
    }
  return (*ptr_to_closesocket) (s);
#else
  return closesocket (s);
#endif
#endif
}

int
dl_ioctlsocket_fionread_ulp(int s, unsigned long *argp)
{
#ifndef USE_WINSOCK
  int temp_i;
#endif
  int ioctl_ret;
  if(argp)
    {
      *argp=0;
    }
#ifdef USE_WINSOCK
  ioctl_ret = ioctlsocket (s, FIONREAD, argp);
  return ioctl_ret;
#else
#ifdef VXWORKS
  ioctl_ret = ioctl(s, FIONREAD, (int) (&temp_i));
#else
  ioctl_ret = ioctl(s, FIONREAD, &temp_i);
#endif
  if(argp)
    {
      if(0 == ioctl_ret)
	{
	  *argp = (unsigned long) temp_i;
	}
      else
	{
	  *argp = 0;
	}
    }
  return ioctl_ret;
#endif
}

int
dl_ioctlsocket_fionread(int s, int *argp)
{
#ifdef USE_WINSOCK
  unsigned long temp_ul;
#endif
  int ioctl_ret;
  if(argp)
    {
      *argp=0;
    }
#ifdef USE_WINSOCK
  ioctl_ret = ioctlsocket (s, FIONREAD, &temp_ul);
  if(argp)
    {
      if(0 == ioctl_ret)
	{
	  *argp = (int) temp_ul;
	}
      else
	{
	  *argp = 0;
	}
    }
#else
#ifdef VXWORKS
  ioctl_ret = ioctl(s, FIONREAD,(int) argp);
#else
  ioctl_ret = ioctl(s, FIONREAD,argp);
#endif
#endif
  return ioctl_ret;
}

#ifndef USE_WINSOCK
int
dl_ioctlsocket(SOCKET s, long cmd, unsigned long *argp);
#endif

int
dl_ioctlsocket(SOCKET s, long cmd, unsigned long *argp)
{
#ifndef USE_WINSOCK
#ifndef VXWORKS
#if defined(MSDOS) && (!defined(MS_WINDOWS_API))
  return tk_ioctl (s, (int) cmd, (int *) argp);
#else
  return ioctl (s, (int) cmd, (caddr_t) argp);
#endif
#else
  return ioctl (s, (int) cmd, (int) argp);
#endif
#else
#ifdef USE_DYNAMICALLY_LOADED_POINTERS
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_ioctlsocket)
    {
      rcs_print_error ("Cannot call ioctlsocket ().\n");
      return -1;
    }
  return (*ptr_to_ioctlsocket) (s, cmd, argp);
#else
  return ioctlsocket (s, cmd, argp);
#endif
#endif
}

#if 0
unsigned long
dl_htonl (unsigned long hostlong)
{
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
  return htonl (hostlong);
#else
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_htonl)
    {
      rcs_print_error ("Cannot call htonl ().\n");
      return hostlong;
    }
  return (*ptr_to_htonl) (hostlong);
#endif
}

unsigned long
dl_ntohl (unsigned long netlong)
{
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
  return ntohl (netlong);
#else
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_ntohl)
    {
      rcs_print_error ("Cannot call ntohl ().\n");
      return netlong;
    }
  return (*ptr_to_ntohl) (netlong);
#endif
}
#endif

unsigned short
dl_htons (unsigned short hostshort)
{
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
  return htons (hostshort);
#else
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_htons)
    {
      rcs_print_error ("Cannot call htons ().\n");
      return hostshort;
    }
  return (*ptr_to_htons) (hostshort);
#endif
}


unsigned short
dl_ntohs (unsigned short netshort)
{
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
  return ntohs (netshort);
#else
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_ntohs)
    {
      rcs_print_error ("Cannot call ntohs ().\n");
      return netshort;
    }
  return (*ptr_to_ntohs) (netshort);
#endif
}

unsigned long
dl_inet_addr (const char *addr_string)
{
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
  return inet_addr (addr_string);
#else
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_inet_addr)
    {
      rcs_print_error ("Cannot call inet_addr ().\n");
      return 0;
    }
  return (*ptr_to_inet_addr) (addr_string);
#endif
}

char *
dl_inet_ptr_ntoa (struct in_addr *in_ptr)
{
  if(!in_ptr)
    {
      rcs_print_error("dl_inet_ptr_ntoa: BAD argument\n");
      return 0;
    }
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
  return (char *) inet_ntoa ( *in_ptr);
#else
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_inet_ntoa)
    {
      rcs_print_error ("Cannot call inet_ntoa ().\n");
      return NULL;
    }
  return (*ptr_to_inet_ntoa) (*in_ptr);
#endif
}

#if 0
int
dl_inet_netof (struct in_addr addr)
{
#if defined(WINDOWS) || defined(WIN32)
  return addr.S_un.S_addr & 0xffffff00;
#else
  return inet_netof (addr);
#endif
}
#endif

int
dl_gethostname (char *hostname, int maxlen)
{
  return gethostname (hostname, maxlen);
}

#if 0
struct in_addr
dl_inet_makeaddr (const int net, const int lna)
{
#if defined(WINDOWS) || defined(WIN32)
  struct in_addr tempAddress;
  tempAddress.S_un.S_addr = ((u_long) net) | ((u_long) lna);
  return tempAddress;
#else
  return (struct in_addr) inet_makeaddr (net, lna);
#endif
}
#endif

int
dl_listen (SOCKET s, int backlog)
{
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
  return listen (s, backlog);
#else
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_listen)
    {
      rcs_print_error ("Cannot call listen ().\n");
      return -1;
    }
  return (*ptr_to_listen) (s, backlog);
#endif
}

int
dl_recv (SOCKET s, char *buf, int size, int flags)
{
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
  return recv (s, buf, size, flags);
#else
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_recv)
    {
      rcs_print_error ("Cannot call recv ().\n");
      return -1;
    }
  return (*ptr_to_recv) (s, buf, size, flags);
#endif
}

int
dl_send (SOCKET s, const void *buf, int size, int flags)
{
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
#ifdef VXWORKS
  return send (s,(void *) buf, size, flags);
#else
  return send (s, buf, size, flags);
#endif
#else
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_send)
    {
      rcs_print_error ("Cannot call recv ().\n");
      return -1;
    }
  return (*ptr_to_send) (s, ((char *)buf), size, flags);
#endif
}

int
dl_recvfrom (SOCKET s, char *buf, int size, int flags,
	     struct sockaddr *from, int *namelen)
{
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
#if HAVE_SOCKLEN_T_TYPE
  socklen_t socklen_t_namelen;
  int retval;
  if(namelen)
    {
      socklen_t_namelen = (socklen_t) (*namelen);
    }
  retval =recvfrom (s, buf, size, flags, from, &socklen_t_namelen);
  if(namelen)
    {
      *namelen = (int) socklen_t_namelen;
    }
  return retval;
#else
  return recvfrom (s, buf, size, flags, from, namelen);
#endif
#else
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_recvfrom)
    {
      rcs_print_error ("Cannot call recvfrom ().\n");
      return -1;
    }
  return (*ptr_to_recvfrom) (s, buf, size, flags, from, namelen);
#endif
}

int
dl_sendto (SOCKET s, char *buf, int size, int flags,
	   struct sockaddr *to, int namelen)
{
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
  return sendto (s, buf, size, flags, to, namelen);
#else
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_sendto)
    {
      rcs_print_error ("Cannot call sendto ().\n");
      return -1;
    }
  return (*ptr_to_sendto) (s, buf, size, flags, to, namelen);
#endif
}

int
dl_select (int maxfd, fd_set_ptr read_fds, fd_set_ptr write_fds,
	   fd_set_ptr except_fds, struct timeval * timeout)
{
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
#ifdef MSDOS
  struct timeval far *temp = timeout;
  return select (maxfd, (fd_set *)  read_fds, (fd_set *) write_fds, (fd_set *) except_fds,
		 ((struct timeval far *) temp));
#else
  return select (maxfd, (fd_set *)read_fds,(fd_set *) write_fds, (fd_set *)except_fds, timeout);
#endif
#else
  /** WARNING : according to linux man page for select() :

  On Linux, timeout is modified to reflect the amount of time not slept; most other  implemen-
  tations  do  not  do this.  This causes problems both when Linux code which reads timeout is
  ported to other operating systems, and when code is ported to Linux  that  reuses  a  struct
  timeval  for  multiple  selects in a loop without reinitializing it.  Consider timeout to be
       undefined after select returns.

       . . . 

       4.4BSD (the select function first appeared in 4.2BSD).  Generally portable  to/from  non-BSD
       systems  supporting  clones of the BSD socket layer (including System V variants).  However,
       note that the System V variant typically sets the timeout variable before exit, but the  BSD
       variant does not.

       
       This keeps a copy of timeout value so select can't modify it.
  **/
  struct timeval timeout_tv_copy;
  int retval;
  if (timeout != 0)
    timeout_tv_copy = *timeout;
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_select)
    {
      rcs_print_error ("Cannot call select ().\n");
      return -1;
    }
  retval = (*ptr_to_select) (maxfd,(fd_set *) read_fds,(fd_set *) write_fds, (fd_set *)except_fds, timeout);
  if (timeout != 0)
    *timeout = timeout_tv_copy;
  return retval;
#endif
}

static SOCKET
dl_socket (int domain, int type, int protocol)
{
  sokintrf_socket_count++;
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
  return socket (domain, type, protocol);
#else
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_socket)
    {
      rcs_print_error ("Cannot call socket ().\n");
      return -1;
    }
  return (*ptr_to_socket) (domain, type, protocol);
#endif
}

int
dl_connect (SOCKET s, struct sockaddr *name, int namelen)
{
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
  return connect (s, name, namelen);
#else
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_connect)
    {
      rcs_print_error ("Cannot call connect ().\n");
      return -1;
    }
  return (*ptr_to_connect) (s, name, namelen);
#endif
}

#ifndef VXWORKS
int
dl_modified_gethostbyname (const char *hostname, struct hostent **hpp, int print_errors)
{
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
#ifdef VXWORKS
  if(print_errors)
    {
      rcs_print_error
	("VxWorks programs must use hostGetByName() rather than dl_gethostbyname().");
    }
  return NULL;
#else
  *hpp = (struct hostent *) gethostbyname (hostname);
  if(*hpp)
    {
      return 0;
    }
  else
    {
      if(print_errors)
	{
	  switch(h_errno)
	    {
#ifdef HOST_NOT_FOUND
	    case HOST_NOT_FOUND:
	      rcs_print_error("gethostbyname(%s) failed : h_errno = HOST_NOT_FOUND\n",hostname);
	      break;
#endif
#ifdef TRY_AGAIN
	    case TRY_AGAIN:
	      rcs_print_error("gethostbyname(%s) failed : h_errno = TRY_AGAIN\n",hostname);
	      break;
#endif
#ifdef NO_RECOVERY
	    case NO_RECOVERY:
	      rcs_print_error("gethostbyname(%s) failed : h_errno = NO_RECOVERY\n",hostname);
	      break;
#endif
#ifdef NO_ADDRESS
	    case NO_ADDRESS:
	      rcs_print_error("gethostbyname(%s) failed : h_errno = NO_ADDRESS\n",hostname);
	      break;
#endif
#if defined(NO_DATA) && (!defined(NO_ADDRESS) || NO_ADDRESS != NO_DATA)
	    case NO_DATA:
	      rcs_print_error("gethostbyname(%s) failed : h_errno = NO_DATA\n",hostname);
	      break;
#endif
#ifdef NETDB_INTERNAL
	    case NETDB_INTERNAL:
	      rcs_print_error("gethostbyname(%s) failed : h_errno = NETDB_INTERNAL\n",hostname);
	      break;
#endif
	    default:
	      rcs_print_error("gethostbyname(%s) failed : h_errno = %d\n",hostname,h_errno);
	      break;
	    }
	}
      return h_errno;
    }
#endif
#else
  if (socket_availability != SOCKETS_AVAILABLE
      || NULL == ptr_to_gethostbyname)
    {
      rcs_print_error ("Cannot call gethostbyname ().\n");
      return -1;
    }
  *hpp = (*ptr_to_gethostbyname) (hostname);
  if(*hpp)
    {
      return 0;
    }
  else 
    {
      return -1;
    }
#endif
}
#endif

int
dl_setsockopt (SOCKET s, int level, int opt, char * optval, int len)
{
#if !defined(USE_DYNAMICALLY_LOADED_POINTERS)
  return setsockopt (s, level, opt, optval, len);
#else
  if (socket_availability != SOCKETS_AVAILABLE || NULL == ptr_to_setsockopt)
    {
      rcs_print_error ("Cannot call setsockopt().\n");
      return -1;
    }
  return (*ptr_to_setsockopt) (s, level, opt, optval, len);
#endif
}


int
dl_get_last_socket_error_int(__unused_parameter__ int socket_fd)
{ 
#if MS_WINDOWS_API
  return dl_WSAGetLastError();
#else

#ifdef MSDOS
  return tk_geterrno(socket_fd);
#else
  return errno;
#endif
#endif

}

   
 const char *dl_get_last_socket_error_string( __unused_parameter__ int socket_fd, 
					      int error_int, 
					      __unused_parameter__ char *buf, 
					      __unused_parameter__ size_t maxbuflen)
{
#if defined(MS_WINDOWS_API) 
  switch(error_int)
    {
#ifdef WSAEINTR
    case WSAEINTR:
      return "WSAEINTR";
#endif

#ifdef WSAEBADF
    case WSAEBADF:
      return "WSAEBADF";
#endif

#ifdef WSAEACCES
    case WSAEACCES:
      return "WSAEACCES";
#endif

#ifdef WSAEFAULT
    case WSAEFAULT:
      return "WSAEFAULT";
#endif

#ifdef WSAEINVAL
    case WSAEINVAL:
      return "WSAEINVAL";
#endif

#ifdef WSAEMFILE
    case WSAEMFILE:
      return "WSAEMFILE";
#endif

#ifdef WSAEWOULDBLOCK
    case WSAEWOULDBLOCK:
      return "WSAEWOULDBLOCK";
#endif

#ifdef WSAEINPROGRESS
    case WSAEINPROGRESS:
      return "WSAEINPROGRESS";
#endif

#ifdef WSAEALREADY
    case WSAEALREADY:
      return "WSAEALREADY";
#endif

#ifdef WSAENOTSOCK
    case WSAENOTSOCK:
      return "WSAENOTSOCK";
#endif

#ifdef WSAEDESTADDRREQ
    case WSAEDESTADDRREQ:
      return "WSAEDESTADDRREQ";
#endif

#ifdef WSAEMSGSIZE
    case WSAEMSGSIZE:
      return "WSAEMSGSIZE";
#endif

#ifdef WSAEPROTOTYPE
    case WSAEPROTOTYPE:
      return "WSAEPROTOTYPE";
#endif

#ifdef WSAENOPROTOOPT
    case WSAENOPROTOOPT:
      return "WSAENOPROTOOPT";
#endif

#ifdef WSAEPROTONOSUPPORT
    case WSAEPROTONOSUPPORT:
      return "WSAEPROTONOSUPPORT";
#endif

#ifdef WSAESOCKTNOSUPPORT
    case WSAESOCKTNOSUPPORT:
      return "WSAESOCKTNOSUPPORT";
#endif

#ifdef WSAEOPNOTSUPP
    case WSAEOPNOTSUPP:
      return "WSAEOPNOTSUPP";
#endif

#ifdef WSAEPFNOSUPPORT
    case WSAEPFNOSUPPORT:
      return "WSAEPFNOSUPPORT";
#endif

#ifdef WSAEAFNOSUPPORT
    case WSAEAFNOSUPPORT:
      return "WSAEAFNOSUPPORT";
#endif

#ifdef WSAEADDRINUSE
    case WSAEADDRINUSE:
      return "WSAEADDRINUSE";
#endif

#ifdef WSAEADDRNOTAVAIL
    case WSAEADDRNOTAVAIL:
      return "WSAEADDRNOTAVAIL";
#endif

#ifdef WSAENETDOWN
    case WSAENETDOWN:
      return "WSAENETDOWN";
#endif

#ifdef WSAENETUNREACH
    case WSAENETUNREACH:
      return "WSAENETUNREACH";
#endif

#ifdef WSAENETRESET
    case WSAENETRESET:
      return "WSAENETRESET";
#endif

#ifdef WSAECONNABORTED
    case WSAECONNABORTED:
      return "WSAECONNABORTED";
#endif

#ifdef WSAECONNRESET
    case WSAECONNRESET:
      return "WSAECONNRESET";
#endif

#ifdef WSAENOBUFS
    case WSAENOBUFS:
      return "WSAENOBUFS";
#endif

#ifdef WSAEISCONN
    case WSAEISCONN:
      return "WSAEISCONN";
#endif

#ifdef WSAENOTCONN
    case WSAENOTCONN:
      return "WSAENOTCONN";
#endif

#ifdef WSAESHUTDOWN
    case WSAESHUTDOWN:
      return "WSAESHUTDOWN";
#endif

#ifdef WSAETOOMANYREFS
    case WSAETOOMANYREFS:
      return "WSAETOOMANYREFS";
#endif

#ifdef WSAETIMEDOUT
    case WSAETIMEDOUT:
      return "WSAETIMEDOUT";
#endif

#ifdef WSAECONNREFUSED
    case WSAECONNREFUSED:
      return "WSAECONNREFUSED";
#endif

#ifdef WSAELOOP
    case WSAELOOP:
      return "WSAELOOP";
#endif

#ifdef WSAENAMETOOLONG
    case WSAENAMETOOLONG:
      return "WSAENAMETOOLONG";
#endif

#ifdef WSAEHOSTDOWN
    case WSAEHOSTDOWN:
      return "WSAEHOSTDOWN";
#endif

#ifdef WSAEHOSTUNREACH
    case WSAEHOSTUNREACH:
      return "WSAEHOSTUNREACH";
#endif

#ifdef WSAENOTEMPTY
    case WSAENOTEMPTY:
      return "WSAENOTEMPTY";
#endif

#ifdef WSAEPROCLIM
    case WSAEPROCLIM:
      return "WSAEPROCLIM";
#endif

#ifdef WSAEUSERS
    case WSAEUSERS:
      return "WSAEUSERS";
#endif

#ifdef WSAEDQUOT
    case WSAEDQUOT:
      return "WSAEDQUOT";
#endif

#ifdef WSAESTALE
    case WSAESTALE:
      return "WSAESTALE";
#endif

#ifdef WSAEREMOTE
    case WSAEREMOTE:
      return "WSAEREMOTE";
#endif

#ifdef WSAEDISCON
    case WSAEDISCON:
      return "WSAEDISCON";
#endif

#ifdef WSASYSNOTREADY
    case WSASYSNOTREADY:
      return "WSASYSNOTREADY";
#endif

#ifdef WSAVERNOTSUPPORTED
    case WSAVERNOTSUPPORTED:
      return "WSAVERNOTSUPPORTED";
#endif

#ifdef WSANOTINITIALISED
    case WSANOTINITIALISED:
      return "WSANOTINITIALISED";
#endif

#ifdef WSAHOST_NOT_FOUND
    case WSAHOST_NOT_FOUND:
      return "WSAHOST_NOT_FOUND";
#endif

#ifdef WSATRY_AGAIN
    case WSATRY_AGAIN:
      return "WSATRY_AGAIN";
#endif

#ifdef WSANO_RECOVERY
    case WSANO_RECOVERY:
      return "WSANO_RECOVERY";
#endif

#ifdef WSANO_DATA
    case WSANO_DATA:
      return "WSANO_DATA";
#else 
#ifdef WSANO_ADDRESS
    case WSANO_ADDRESS:
      return "WSANO_ADDRESS";
#endif
#endif

    default:
      break;
    }
  return"";

  /* MS_WINDOWS_API */
#else

#if defined(MSDOS) 
  return tk_sperror();

#else
  return strerror(error_int);
  /* ! MS_DOS */
#endif

  /* ! MS_WINDOWS_API */
#endif
}

int
dl_socket_error_was_would_block ( 
				 __unused_parameter__ int socket_fd , 
				 int error_int)
{
#ifdef EINPROGRESS 
  if(EINPROGRESS == error_int)
    {
      return 1;
    }
#endif
#ifdef EAGAIN 
  if(EAGAIN == error_int)
    {
      return 1;
    }
#endif


// Rpatel added to fix error in am2 hosting large buffers
#if defined(EWOULDBLOCK) && defined(VXWORKS)
  if(EWOULDBLOCK == error_int)
    {
      return 1;
    }
#endif

#if defined(EWOULDBLOCK) && defined(MSDOS) && !defined(MS_WINDOWS_API)
  return (EWOULDBLOCK == error_int);
#else

#if MS_WINDOWS_API && defined(WSAEWOULDBLOCK)
  if (WSAEWOULDBLOCK == error_int )
    {
      return 1;
    }
#endif

#endif
  return 0;
}


int
dl_socket_error_was_connection_refused ( 
					__unused_parameter__ int socket_fd , 
					int error_int)
{
#ifdef ECONNREFUSED
  if(ECONNREFUSED == error_int)
    {
      return 1;
    }
#endif

#if MS_WINDOWS_API && defined(WSAEWOULDBLOCK)
  if (WSAECONNREFUSED == error_int )
    {
      return 1;
    }
#endif
  return 0;
}


int 
dl_select_ret_is_error(int select_ret)
{
#if MS_WINDOWS_APP
  return (select_ret == SOCKET_ERROR);
#else
  return (select_ret < 0);
#endif
}

int 
dl_select_connect_completed(int socket_fd, int print_errors, int *connect_error)
{
#if HAVE_GETSOCKOPT
  int optval =-15;
#if HAVE_SOCKLEN_T_TYPE
  socklen_t optvallen = sizeof(optval);
#else
  size_t optvallen = sizeof(optval);
#endif
  int getsockoptret;
  
  if(connect_error)
    {
      *connect_error = optval;
    }
#if HAVE_SOCKLEN_T_TYPE
  getsockoptret = getsockopt(socket_fd,SOL_SOCKET,SO_ERROR, 
			     (void *) &optval, 
			      &optvallen);
#else
  getsockoptret = getsockopt(socket_fd,SOL_SOCKET,SO_ERROR, 
			     (void *) &optval, 
			     (int *) &optvallen);
#endif
  rcs_print_debug(PRINT_ALL_SOCKET_REQUESTS,"dl_select_connect_completed(%d,%d) : getsockoptret=%d, optval=%d\n",
		  socket_fd,print_errors,getsockoptret,optval);
   if(connect_error)
    {
      *connect_error = optval;
    }
   if(getsockoptret == -1)
     {
      if(print_errors)
	{
	  rcs_print_error("getsockopt(%d,,SOL_SOCKET,SO_ERROR,...) failed. -- %d %s\n",
			  socket_fd,errno, strerror(errno));
	}
    }
  if(optval==0)
    {
      return 1;
    }
  if(print_errors)
    {
      rcs_print_error("postponed connect failed. -- %d=%s\n",
		      optval,strerror(optval));
    }
  return 0;
#else
  return 1;
#endif
}


int
dl_connect_in_with_timeout(int socket_fd, 
			   struct dl_sa *dl_sa_ptr,
			   double connect_timeout, int print_errors,
			   int continue_after_connection_refused,
			   int *error_code_ptr, int *timedout_ptr, 
			   int *interrupt_op_ptr,
			   char *sockerrbuf, 
			   size_t sockerrbuf_size)
{
  int connect_ret = -15;
  int connect_completed=0;
  int fatal_connect_error_occured=0;
  int sockerrno=0;
  struct timeval tm;
  int select_ret;
  double start_time, current_time;
  static char static_sockerrbuf[256];
  fd_set fds;
  const char *sockerrstr=0;
  int connect_waiting_error_given=0;
#ifndef MS_WINDOWS_API
  void (*old_handler) (int);
#endif

  if(interrupt_op_ptr && *interrupt_op_ptr)
    {
      return -1;
    }
#ifndef MS_WINDOWS_API
  old_handler=SIG_IGN;
  if(continue_after_connection_refused == 1)
    {
      old_handler = signal (SIGPIPE, SIG_IGN);
    }
#endif

  if(!sockerrbuf && print_errors)
    {
      sockerrbuf= static_sockerrbuf;
      sockerrbuf_size = sizeof(static_sockerrbuf);
    }
  if(!dl_sa_ptr)
    {
      if(print_errors)
	{
	  rcs_print_error("dl_sa_ptr is NULL\n");
	}
      if(timedout_ptr)
	{
	  *timedout_ptr = 0;
	}
#ifndef MS_WINDOWS_API
      if(continue_after_connection_refused == 1)
	{
	  signal (SIGPIPE, old_handler);
	}
#endif
      return -1;
    }

  if(!dl_sa_addr(dl_sa_ptr))
    {
      if(print_errors)
	{
	  rcs_print_error("dl_sa_addr(dl_sa_ptr) is NULL\n");
	}
      if(timedout_ptr)
	{
	  *timedout_ptr = 0;
	}
#ifndef MS_WINDOWS_API
      if(continue_after_connection_refused == 1)
	{
	  signal (SIGPIPE, old_handler);
	}
#endif
      return -1;
    }      

  if(socket_fd <= 0)
    {
      if(print_errors)
	{
	  rcs_print_error("socket_fd = %d\n",socket_fd);
	}
      if(timedout_ptr)
	{
	  *timedout_ptr = 0;
	}
#ifndef MS_WINDOWS_API
      if(continue_after_connection_refused == 1)
	{
	  signal (SIGPIPE, old_handler);
	}
#endif
      return -1;
    }
  if(print_errors)
    {
      rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS, 
		       "connect(socket_fd=%d,{IP address:%s, port=%d}\n",
		       socket_fd,
		       dl_sa_get_host(dl_sa_ptr),
		       dl_sa_get_port(dl_sa_ptr));
    }
  current_time = start_time = etime();
  connect_ret = dl_connect (socket_fd, 
			    dl_sa_addr(dl_sa_ptr),
			    dl_sa_len(dl_sa_ptr));
  if (connect_ret != 0 )
    {
      if(interrupt_op_ptr && *interrupt_op_ptr)
	{
#ifndef MS_WINDOWS_API
	  if(continue_after_connection_refused == 1)
	    {
	      signal (SIGPIPE, old_handler);
	    }
#endif
	  return -1;
	}
      sockerrno = dl_get_last_socket_error_int( socket_fd );
      if(print_errors)
	{
	  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS, 
			   "connect_ret=%d,sockerrno=%d\n",
			   connect_ret,sockerrno);
	}
      while(continue_after_connection_refused == 1 &&
	    (connect_timeout < 0 || current_time -start_time < connect_timeout) &&
	    dl_socket_error_was_connection_refused(socket_fd,sockerrno))
	{
	  if(interrupt_op_ptr && *interrupt_op_ptr)
	    {
#ifndef MS_WINDOWS_API
	      signal (SIGPIPE, old_handler);
#endif
	      return -1;
	    }
	  connect_ret = dl_connect (socket_fd, 
				    dl_sa_addr(dl_sa_ptr),
				    dl_sa_len(dl_sa_ptr));
	  sockerrno = dl_get_last_socket_error_int( socket_fd );
	  if(print_errors)
	    {
	      rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS, 
			       "connect_ret=%d,sockerrno=%d\n",
			       connect_ret,sockerrno);
	    }
	  if(0 == connect_ret)
	    {
#ifndef MS_WINDOWS_API
	      signal (SIGPIPE, old_handler);
#endif
	      return(0);
	    }
	  current_time = etime();
	  if(connect_timeout < 0 && !connect_waiting_error_given &&
	     current_time - start_time > 10.0 && print_errors)
	    {
	      rcs_print_warning("Connecting to %s:%d has taken longer than 10 seconds.\n I am configured to wait forever.\n Check that the server is running or will run.\n",
			     dl_sa_get_host(dl_sa_ptr),
			     dl_sa_get_port(dl_sa_ptr));
	      connect_waiting_error_given=1;
	    }
	  esleep(0.01);
	}

      if ( dl_socket_error_was_would_block( socket_fd ,sockerrno ))
	{
	  current_time = etime ();
	  while((connect_timeout < 0 || current_time - start_time < connect_timeout) &&
		!connect_completed &&
		!fatal_connect_error_occured)
	    {
	      if(interrupt_op_ptr && *interrupt_op_ptr)
		{
		  if(continue_after_connection_refused == 1)
		    {
#ifndef MS_WINDOWS_API
		      signal (SIGPIPE, old_handler);
#endif
		    }
		  return -1;
		}
	      if(connect_timeout < 0 && !connect_waiting_error_given &&
		 current_time - start_time > 10.0 && print_errors)
		{
		  rcs_print_warning("Connecting to %s:%d has taken longer than 10 seconds. I am configured to wait forever. Check that the server is running or willl run.\n",
				 dl_sa_get_host(dl_sa_ptr),
				 dl_sa_get_port(dl_sa_ptr));
		  connect_waiting_error_given=1;
		}
	      
	      /* Timeout after 10 seconds on connect even if we
	       * have no timeouts on other operations. */
	      if (connect_timeout < 0)
		{
		  tm.tv_sec = 10;
		  tm.tv_usec = 0;
		}
	      else
		{
		  tm.tv_sec = (long) connect_timeout;
		  tm.tv_usec = (long) (connect_timeout * 1000000.0);
		  if (tm.tv_usec >= 1000000)
		    {
		      tm.tv_usec = tm.tv_usec % 1000000;
		    }
		}
	      FD_ZERO (&fds);
	      RCS_FD_SET (socket_fd, &fds);
	      while (!
		     (select_ret =
		      dl_select (socket_fd + 1, NULL, &fds, NULL, &tm)))
		{
		  if(interrupt_op_ptr && *interrupt_op_ptr)
		    {
		      if(continue_after_connection_refused == 1)
			{
#ifndef MS_WINDOWS_API
			  signal (SIGPIPE, old_handler);
#endif
			}
		      return -1;
		    }
		  if(print_errors)
		    {
		      rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS, 
				       "select_ret=%d\n",
				       select_ret);
		    }
		  RCS_FD_SET (socket_fd, &fds);
		  if(interrupt_op_ptr && *interrupt_op_ptr)
		    {
		      if(continue_after_connection_refused == 1)
			{
#ifndef MS_WINDOWS_API
			  signal (SIGPIPE, old_handler);
#endif
			}
		      return -1;
		    }
		  current_time = etime ();
		  if (current_time - start_time > connect_timeout)
		    {
		      fatal_connect_error_occured=1;
		      if(print_errors)
			{
			  rcs_print_error
			    ("Timed out waiting for socket connection.\n");
			}
		      if(timedout_ptr)
			{
			  *timedout_ptr = 1;
			}
		      if(error_code_ptr)
			{
			  *error_code_ptr = sockerrno;
			}
		      if(continue_after_connection_refused == 1)
			{
#ifndef MS_WINDOWS_API
			  signal (SIGPIPE, old_handler);
#endif
			}
		      return -1;
		    }
		  esleep (0.01);
		}
	      if(print_errors)
		{		  
		  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS,
				   "select_ret=%d\n",
				   select_ret);
		}
	      if (dl_select_ret_is_error(select_ret))
		{
		  fatal_connect_error_occured=1;
		  sockerrno = dl_get_last_socket_error_int( socket_fd );
		  if(print_errors)
		    {
		      sockerrstr = dl_get_last_socket_error_string(socket_fd,
								   sockerrno,
								   sockerrbuf,
								   sockerrbuf_size);
		      rcs_print_error ("select error: %d -- %s\n", 
				       sockerrno,sockerrstr);
		      rcs_print_error ("Couldn't connect.\n");
		    }
		  if(error_code_ptr)
		    {
		      *error_code_ptr = sockerrno;
		    }
		  if(timedout_ptr)
		    {
		      *timedout_ptr = 0;
		    }		  
		  if(continue_after_connection_refused == 1)
		    {
#ifndef MS_WINDOWS_API
		      signal (SIGPIPE, old_handler);
#endif
		    }
		  return -1;
		}
	      if (dl_select_connect_completed(socket_fd,1,&sockerrno))
		{
		  connect_completed=1;
		  connect_ret=0;
		  if(continue_after_connection_refused == 1)
		    {
#ifndef MS_WINDOWS_API
		      signal (SIGPIPE, old_handler);
#endif
		    }
		  return 0;
		}
	      else
		{
		  current_time = etime ();
		  if (current_time - start_time > connect_timeout)
		    {
		      fatal_connect_error_occured=1;
		      if(print_errors)
			{
			  rcs_print_error
			    ("Timed out waiting for socket connection.\n");
			}
		      if(timedout_ptr)
			{
			  *timedout_ptr = 1;
			}
		      if(error_code_ptr)
			{
			  *error_code_ptr = sockerrno;
			}
		      if(continue_after_connection_refused == 1)
			{
#ifndef MS_WINDOWS_API
			  signal (SIGPIPE, old_handler);
#endif
			}
		      return -1;
		    }
		  if(interrupt_op_ptr && *interrupt_op_ptr)
		    {
		      if(continue_after_connection_refused == 1)
			{
#ifndef MS_WINDOWS_API
			  signal (SIGPIPE, old_handler);
#endif
			}
		      return -1;
		    }
		  if(continue_after_connection_refused == 1 &&
		     dl_socket_error_was_connection_refused(socket_fd,sockerrno))
		    {
		      if(interrupt_op_ptr && *interrupt_op_ptr)
			{
			  if(continue_after_connection_refused == 1)
			    {
#ifndef MS_WINDOWS_API
			      signal (SIGPIPE, old_handler);
#endif
			    }
			  return -1;
			}
		      esleep(0.1);
		      if(interrupt_op_ptr && *interrupt_op_ptr)
			{
			  if(continue_after_connection_refused == 1)
			    {
#ifndef MS_WINDOWS_API
			      signal (SIGPIPE, old_handler);
#endif
			    }
			  return -1;
			}
		      connect_ret = 
			dl_connect (socket_fd, 
				    dl_sa_addr(dl_sa_ptr),
				    dl_sa_len(dl_sa_ptr));
		      if(connect_ret == 0)
			{
			  connect_completed=1;
			}
		      else 
			{
			  if(interrupt_op_ptr && *interrupt_op_ptr)
			    {
			      if(continue_after_connection_refused == 1)
				{
#ifndef MS_WINDOWS_API
				  signal (SIGPIPE, old_handler);
#endif
				}
			      return -1;
			    }
			  sockerrno = dl_get_last_socket_error_int( socket_fd );
			 
			  if(print_errors)
			    {
			      rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS, 
					       "connect_ret=%d,sockerrno=%d\n",
					       connect_ret,sockerrno);
			    }
			  while(continue_after_connection_refused == 1 &&
				(connect_timeout < 0 || current_time -start_time < connect_timeout) &&
				dl_socket_error_was_connection_refused(socket_fd,sockerrno))
			    {
			      if(connect_timeout < 0 && !connect_waiting_error_given &&
				 current_time - start_time > 10.0 && print_errors)
				{
				  rcs_print_warning("Connecting to %s:%d has taken longer than 10 seconds. I am configured to wait forever. Check that the server is running or will run.\n",
						    dl_sa_get_host(dl_sa_ptr),
						    dl_sa_get_port(dl_sa_ptr));
				  connect_waiting_error_given=1;
				}
			      connect_ret = dl_connect (socket_fd, 
							dl_sa_addr(dl_sa_ptr),
							dl_sa_len(dl_sa_ptr));
			      sockerrno = dl_get_last_socket_error_int( socket_fd );
			      if(print_errors)
				{
				  rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS, 
						   "connect_ret=%d,sockerrno=%d\n",
						   connect_ret,sockerrno);
				}
			      if(0 == connect_ret)
				{
#ifndef MS_WINDOWS_API
				  signal (SIGPIPE, old_handler);
#endif
				  return(0);
				}
			      current_time = etime();
			      esleep(0.01);
			    }

			  if(!dl_socket_error_was_would_block( socket_fd ,sockerrno ))
			    {
			      fatal_connect_error_occured=1;
			      sockerrno = dl_get_last_socket_error_int( socket_fd );
			      if(print_errors)
				{
				  sockerrstr = 
				    dl_get_last_socket_error_string(socket_fd,
								    sockerrno,
								    sockerrbuf,
								    sockerrbuf_size);
				  rcs_print_error ("connect error: %d -- %s\n", 
						   sockerrno, sockerrstr);
				  
				  rcs_print_error
				    ("Error trying to connect to port %d of host %s.\n",
				     dl_sa_get_port(dl_sa_ptr),
				     dl_sa_get_host(dl_sa_ptr));
				}
			      if(error_code_ptr)
				{
				  *error_code_ptr = sockerrno;
				}
			      if(timedout_ptr)
				{
				  *timedout_ptr = 0;
				}
			      if(continue_after_connection_refused == 1)
				{
#ifndef MS_WINDOWS_API
				  signal (SIGPIPE, old_handler);
#endif
				}
			      return -1;
			    }			  
			}
		    }
		  else
		    {
		      fatal_connect_error_occured=1;
		      if(print_errors)
			{
			  sockerrstr = 
			    dl_get_last_socket_error_string(socket_fd,
							    sockerrno,
							    sockerrbuf,
							    sockerrbuf_size);
			  rcs_print_error ("select error: %d -- %s\n", 
					   sockerrno,sockerrstr);
			  rcs_print_error ("Couldn't connect.\n");
			}
		      if(error_code_ptr)
			{
			  *error_code_ptr = sockerrno;
			}
		      if(timedout_ptr)
			{
			  *timedout_ptr = 0;
			}
		      if(continue_after_connection_refused == 1)
			{
#ifndef MS_WINDOWS_API
			  signal (SIGPIPE, old_handler);
#endif
			}
		      return -1;
		    }
		}
	      current_time = etime();
	      if ((current_time+0.1) - start_time > connect_timeout)
		{
		  fatal_connect_error_occured=1;
		  if(print_errors)
		    {
		      rcs_print_error
			("Timed out waiting for connection.\n");
		    }
		  if(error_code_ptr)
		    {
		      *error_code_ptr = sockerrno;
		    }
		  if(timedout_ptr)
		    {
		      *timedout_ptr = 1;
		    }
		  if(continue_after_connection_refused == 1)
		    {
#ifndef MS_WINDOWS_API
		      signal (SIGPIPE, old_handler);
#endif
		    }
		  return -1;
		}
	      if(interrupt_op_ptr && *interrupt_op_ptr)
		{
		  if(continue_after_connection_refused == 1)
		    {
#ifndef MS_WINDOWS_API
		      signal (SIGPIPE, old_handler);
#endif
		    }
		  return -1;
		}
	      esleep(0.1);
	      if(interrupt_op_ptr && *interrupt_op_ptr)
		{
		  if(continue_after_connection_refused == 1)
		    {
#ifndef MS_WINDOWS_API
		      signal (SIGPIPE, old_handler);
#endif
		    }
		  return -1;
		}
	      current_time = etime();
	    }
	}
      else
	{	
	  fatal_connect_error_occured=1;
	  sockerrno = dl_get_last_socket_error_int( socket_fd );
	  if(print_errors)
	    {
	      sockerrstr = dl_get_last_socket_error_string(socket_fd,sockerrno,sockerrbuf,sockerrbuf_size);
	      rcs_print_error ("connect(socket_fd=%d,{host=%s,port=%d},%d) returned %d error: %d -- %s\n", 
			       socket_fd,
			       dl_sa_get_host(dl_sa_ptr),
			       dl_sa_get_port(dl_sa_ptr),
			       dl_sa_len(dl_sa_ptr),
			       connect_ret,
			       sockerrno, sockerrstr);
	      rcs_print_error
		("Error trying to connect to TCP port %d of host %s.\n",
				 dl_sa_get_port(dl_sa_ptr),
				 dl_sa_get_host(dl_sa_ptr));
	    }
	  if(error_code_ptr)
	    {
	      *error_code_ptr = sockerrno;
	    }
	  if(timedout_ptr)
	    {
	      *timedout_ptr = 0;
	    }
	  if(continue_after_connection_refused == 1)
	    {
#ifndef MS_WINDOWS_API
	      signal (SIGPIPE, old_handler);
#endif
	    }
	  return -1;
	}
    }
  if(print_errors)
    {
      rcs_print_debug (PRINT_ALL_SOCKET_REQUESTS, 
		       "connect_ret=%d,socket_fd=%d\n",
		       connect_ret,socket_fd);
    }
  if(continue_after_connection_refused == 1)
    {
#ifndef MS_WINDOWS_API
      signal (SIGPIPE, old_handler);
#endif
    }
  return connect_ret;
}


#if !defined(HAVE_SOCKETPAIR) || defined(MS_WINDOWS_API)
static struct sockaddr_in dl_socketpair_temp_addr[3];
static char dl_socketpair_sockerrbuf[256];
static struct sockaddr_in dl_socketpair_connect_to_addr;
#endif

int 
dl_socketpair(int d, int type, int protocol, int sv[2])
{
#if defined(HAVE_SOCKETPAIR) &&  !defined(MS_WINDOWS_API)
  return socketpair(d,type,protocol,sv);
#else
  int connect_socket=-1;
  int sizeof_addr=0;
  int sockerrno=-1;
  const char *sockerrstr = dl_socketpair_sockerrbuf;
  int connect_to_addr_len = sizeof(dl_socketpair_connect_to_addr);

  rcs_print_debug(PRINT_ALL_SOCKET_REQUESTS,"dl_socketpair(domain=%d,type=%d,protocol=%d,sv=%p);\n",d,type,protocol, (void*)sv);
  dl_socketpair_temp_addr[0].sin_family = AF_INET;
  dl_socketpair_temp_addr[0].sin_addr.s_addr = htonl (INADDR_LOOPBACK);
  dl_socketpair_temp_addr[0].sin_port = dl_htons (0);  
  dl_socketpair_temp_addr[1].sin_family = AF_INET;
  dl_socketpair_temp_addr[1].sin_addr.s_addr = INADDR_ANY;
  dl_socketpair_temp_addr[1].sin_port = dl_htons (0);  
  connect_socket= (int) dl_socket(AF_INET, SOCK_STREAM, 0);
  rcs_print_debug(PRINT_ALL_SOCKET_REQUESTS,"dl_socketpair: connect_socket=%d\n",connect_socket);
  if(connect_socket < 1)
    {
      sockerrno = dl_get_last_socket_error_int( connect_socket );
      sockerrstr = dl_get_last_socket_error_string(connect_socket,sockerrno,dl_socketpair_sockerrbuf,sizeof(dl_socketpair_sockerrbuf));
      rcs_print_error ("dl_socketpair: socket error %d = %s\n", 
		       sockerrno,sockerrstr);
      return -1;
    }
  sv[1] = (int) dl_socket(AF_INET, SOCK_STREAM, 0);
  rcs_print_debug(PRINT_ALL_SOCKET_REQUESTS,"dl_socketpair: sv[1]=%d\n",sv[1]);
  if(sv[1] < 1)
    {
      sockerrno = dl_get_last_socket_error_int( sv[1] );
      sockerrstr = dl_get_last_socket_error_string(sv[1],sockerrno,dl_socketpair_sockerrbuf,sizeof(dl_socketpair_sockerrbuf));
      rcs_print_error ("dl_socketpair: socket error %d = %s\n", 
		       sockerrno,sockerrstr);
      return -1;
    }
  if(dl_bind(connect_socket,
	     ((struct sockaddr*) &(dl_socketpair_temp_addr[0])),
	     sizeof(struct sockaddr_in)) < 0)
    {
      sockerrno = dl_get_last_socket_error_int( connect_socket );
      sockerrstr = dl_get_last_socket_error_string(connect_socket,sockerrno,dl_socketpair_sockerrbuf,sizeof(dl_socketpair_sockerrbuf));
      rcs_print_error ("dl_socketpair: bind error %d = %s\n", 
		       sockerrno,sockerrstr);
      return -1;
    }
  rcs_print_debug(PRINT_ALL_SOCKET_REQUESTS,"first bind passed\n");
  if(dl_bind(sv[1],
	     ((struct sockaddr *) &(dl_socketpair_temp_addr[1])),
	     sizeof(struct sockaddr_in)) < 0)
    {
      sockerrno = dl_get_last_socket_error_int( sv[1] );
      sockerrstr = dl_get_last_socket_error_string(sv[1],sockerrno,dl_socketpair_sockerrbuf,sizeof(dl_socketpair_sockerrbuf));
      rcs_print_error ("dl_socketpair: bind error %d = %s\n", 
		       sockerrno,sockerrstr);
      return -1;
    }
  rcs_print_debug(PRINT_ALL_SOCKET_REQUESTS,"second bind passed\n");
  if(dl_listen (connect_socket, 2) < 0)
    { 
      sockerrno = dl_get_last_socket_error_int( connect_socket );
      sockerrstr = dl_get_last_socket_error_string(connect_socket,sockerrno,dl_socketpair_sockerrbuf,sizeof(dl_socketpair_sockerrbuf));
      rcs_print_error ("dl_socketpair: listen error %d = %s\n", 
		       sockerrno,sockerrstr);
     return -1;
    }
  rcs_print_debug(PRINT_ALL_SOCKET_REQUESTS,"listen passed\n");
  if(getsockname(connect_socket,
		 (struct sockaddr *)&dl_socketpair_connect_to_addr,
		 &connect_to_addr_len) <0)
    {
      rcs_print_error("getsockname failed\n");
      return -1;
    }
  rcs_print_debug(PRINT_ALL_SOCKET_REQUESTS,"getsockname passed.\n");
  if(dl_connect(sv[1],
		((struct sockaddr *) &(dl_socketpair_connect_to_addr)),
		sizeof(struct sockaddr_in)) < 0)
    {
      sockerrno = dl_get_last_socket_error_int( sv[1] );
      sockerrstr = dl_get_last_socket_error_string(sv[1],sockerrno,dl_socketpair_sockerrbuf,sizeof(dl_socketpair_sockerrbuf));
      rcs_print_error ("dl_socketpair: connect(%d,%s:%d) error %d = %s\n", 
		       sv[1],
		       dl_inet_ptr_ntoa(&(dl_socketpair_connect_to_addr.sin_addr)),
		       dl_ntohs (dl_socketpair_connect_to_addr.sin_port),
		       sockerrno,sockerrstr);
      return -1;
    }
  rcs_print_debug(PRINT_ALL_SOCKET_REQUESTS,"connect passed.\n");
  sizeof_addr = sizeof(struct sockaddr_in);
  sv[0] = dl_accept(connect_socket,
		    ((struct sockaddr *) &(dl_socketpair_temp_addr[3])),
		    &sizeof_addr);
  rcs_print_debug(PRINT_ALL_SOCKET_REQUESTS,"dl_socketpair: sv[0]=%d\n",sv[0]);
  if(sv[0] < 1)
    {
      sockerrno = dl_get_last_socket_error_int( sv[0] );
      sockerrstr = dl_get_last_socket_error_string(sv[0],sockerrno,dl_socketpair_sockerrbuf,sizeof(dl_socketpair_sockerrbuf));
      rcs_print_error ("dl_socketpair: accept error %d = %s\n", 
		       sockerrno,sockerrstr);
      return -1;
    }
  dl_closesocket(connect_socket);
  rcs_print_debug(PRINT_ALL_SOCKET_REQUESTS,"dl_socketpair: returning 0\n");
  return 0;
#endif
}

static struct sockaddr *
dl_create_sockaddr_empty(
		   size_t *len_ptr, 
		   int use_ipv6)
{
  struct sockaddr *socket_address_ptr = 0;
  size_t len;

    /* Set up the socket address stucture. */

  len=sizeof(struct sockaddr_in);

#ifdef HAVE_STRUCT_SOCKADDR_STORAGE_TYPE
  if(use_ipv6)
    {
      len=(len > sizeof(struct sockaddr_storage))?len:sizeof(struct sockaddr_storage);
    }
#endif
  
#ifdef HAVE_STRUCT_SOCKADDR_IN6_TYPE
  if(use_ipv6)
    {
      len=(len > sizeof(struct sockaddr_in6))?len:sizeof(struct sockaddr_in6);
    }
#endif

  socket_address_ptr = (struct sockaddr *) malloc(len);
  if(!socket_address_ptr)
    {
      return 0;
    }
  memset (socket_address_ptr, 0, len);
  if(len_ptr)
    {
      *len_ptr = len;
    }

  return socket_address_ptr;
}

static struct sockaddr *
dl_create_sockaddr(const char *hostname, 
		   short port, 
		   size_t *len_ptr, 
		   int use_ipv6)
{
  struct sockaddr *socket_address_ptr = 0;
  int hostname_was_address = 0;
  char bufferhost_first_char = 0;
#ifdef HAVE_STRUCT_SOCKADDR_IN6_TYPE
  struct sockaddr_in6 *s6 = 0;
#else
#undef HAVE_GETADDRINFO

#endif
  struct sockaddr_in *s4 = 0;
#ifndef HAVE_INET_PTON
  long ls_addr=-1;
#endif


#ifndef HAVE_STRUCT_ADDRINFO_TYPE 
#undef HAVE_GETADDRINFO
#endif

#ifdef HAVE_GETADDRINFO
  int getaddrinfo_ret=0;
  struct addrinfo hints;
  struct addrinfo *ai=0;
#else
  struct hostent *host_entry=0;
#endif
  size_t len;

  socket_address_ptr = dl_create_sockaddr_empty(&len,use_ipv6);
  if(len_ptr)
    {
      *len_ptr = len;
    }

#ifdef HAVE_STRUCT_SOCKADDR_IN6_TYPE
  s6 = (struct sockaddr_in6 *) socket_address_ptr;
#endif

  s4 = (struct sockaddr_in *) socket_address_ptr;


  if(use_ipv6)
    {
#if defined(HAVE_STRUCT_SOCKADDR_IN6_TYPE) && defined(AF_INET6)
      s6->sin6_family = AF_INET6;
      s6->sin6_port = dl_htons (port);
      s6->sin6_addr = in6addr_any;
#else
      rcs_print_error("IPv6 not supported.\n");
      dl_free_sockaddr(socket_address_ptr);
      return(0);
#endif
    }
  else
    {
#ifdef VXWORKS
      s4->sin_len = len;
#endif
      s4->sin_family = AF_INET;
      s4->sin_port = dl_htons (port);
      s4->sin_addr.s_addr = INADDR_ANY;
    }


  if(!hostname)
    {
      return socket_address_ptr;
    }
 
  if(hostname)
    {
      bufferhost_first_char = *hostname;
      if ((bufferhost_first_char >= '0' && bufferhost_first_char <= '9' )
	  || 
	  (
	   use_ipv6 && 
	   (
	    (bufferhost_first_char >= 'A' && bufferhost_first_char <= 'F' ) ||
	    (bufferhost_first_char >= 'a' && bufferhost_first_char <= 'f' )
	    )
	   && strchr(hostname,':') != 0 
	   ) 
	  )
	{
#if HAVE_INET_PTON
	  if(strchr(hostname,'.') == 0 && strchr(hostname,':') != 0)
	    {
#if HAVE_STRUCT_SOCKADDR_IN6_TYPE && AF_INET6
	      if(inet_pton(AF_INET6,hostname,&s6->sin6_addr) <= 0)
		{
		  rcs_print_error("inet_pton(AF_INET6,%s) failed. -- %s\n",hostname,strerror(errno));
		  dl_free_sockaddr(socket_address_ptr);
		  return 0;
		}
#else
	      rcs_print_error("ipv6 not supported.\n");
	      dl_free_sockaddr(socket_address_ptr);
	      return 0;
#endif
	    }
	  else
	    {
	      if(inet_pton(AF_INET,hostname,&s4->sin_addr) <= 0)
		{
		  rcs_print_error("inet_pton(AF_INET,%s) failed. -- %s\n",hostname,strerror(errno));
		  dl_free_sockaddr(socket_address_ptr);
		  return 0;
		}
	    }
#if defined(HAVE_STRUCT_SOCKADDR_IN6_TYPE) && defined(AF_INET6) && defined(PF_INET6)
	  if(IN6_IS_ADDR_LINKLOCAL(&(s6->sin6_addr)))
	    {
#if HAVE_LINUX_SOCKIOS_H
	      SOCKET socket_fd = dl_socket (PF_INET6, SOCK_STREAM, 0);
#if HAVE_STRUCT_IFREQ_TYPE
	      struct ifreq ifr;
	      memset(&ifr, 0, sizeof(ifr));
	      strncpy(ifr.ifr_name, "eth0", IFNAMSIZ-1);
	      if(ioctl(socket_fd, SIOCGIFINDEX, &ifr) < 0)
		{
		  rcs_print_error("ioctl(socket_fd=%d, SIOCGIFINDEX, struct ifreq ifr.ifr_name=%s) failed.\n",
				  socket_fd, ifr.ifr_name);
		}
	      else
		{
		  s6->sin6_scope_id = ifr.ifr_ifindex;
		}
#endif
	      dl_closesocket(socket_fd);
#endif
	    }
#endif
	  return socket_address_ptr;
#else
	  s4->sin_addr.s_addr = dl_inet_addr (hostname);
	  ls_addr = (long) s4->sin_addr.s_addr;
	  if (ls_addr != 0 &&
	      ls_addr != -1)
	    {
	      return socket_address_ptr;
	    }
#endif
	}
    }

  if (!hostname_was_address)
    {
      if(!isalpha((int) bufferhost_first_char) && !use_ipv6)
	{
	  rcs_print_error("Bad host name %s\n",hostname);
	  dl_free_sockaddr(socket_address_ptr);
	  return 0;
	}
      /* Get the IP address of the server using it's hostname. */
#ifndef VXWORKS

#ifdef HAVE_GETADDRINFO
      memset(&hints,0,sizeof(hints));
      if(use_ipv6)
	{
	  hints.ai_family = AF_INET6;
	}
      else
	{
	  hints.ai_family = AF_INET;
	}
      if(!hostname && port > 0)
	{
	  hints.ai_flags = AI_PASSIVE;
	}
      getaddrinfo_ret = getaddrinfo(hostname,0,&hints,&ai);
      if(getaddrinfo_ret != 0 || !ai)
	{
	  rcs_print_error("getaddrinfo() failed for hostname=%s, error=%d:%s\n",
			  hostname,
			  getaddrinfo_ret,
#ifdef HAVE_GAI_STRERROR
			  gai_strerror(getaddrinfo_ret)
#else
			  ""
#endif
			  );
	  dl_free_sockaddr(socket_address_ptr);
	  return 0;
	}
      if(ai->ai_addrlen > len)
	{
	  socket_address_ptr = realloc(socket_address_ptr,ai->ai_addrlen);
	  s4 = (struct sockaddr_in *) socket_address_ptr;
	  s6 = (struct sockaddr_in6 *) socket_address_ptr;
	}
      memcpy(socket_address_ptr,ai->ai_addr,ai->ai_addrlen);
#if defined(HAVE_STRUCT_SOCKADDR_IN6_TYPE) && defined(AF_INET6) && defined(PF_INET6)
      if(ai->ai_family == AF_INET6)
#else
	if(0)
#endif
	{
#if defined(HAVE_STRUCT_SOCKADDR_IN6_TYPE) && defined(AF_INET6) && defined(PF_INET6)
	  s6->sin6_port = dl_htons (port);
#endif
	}
      else
	{
	  s4->sin_port = dl_htons (port);
	}
      if(len_ptr)
	{
	  *len_ptr = len = ai->ai_addrlen;
	}
      freeaddrinfo(ai);
#if defined(HAVE_STRUCT_SOCKADDR_IN6_TYPE) && defined(AF_INET6) && defined(PF_INET6)
      if(IN6_IS_ADDR_LINKLOCAL(&(s6->sin6_addr)))
	{
#if HAVE_LINUX_SOCKIOS_H
	  SOCKET socket_fd = dl_socket (PF_INET6, SOCK_STREAM, 0);
#if HAVE_STRUCT_IFREQ_TYPE
	  struct ifreq ifr;
	  memset(&ifr, 0, sizeof(ifr));
	  strncpy(ifr.ifr_name, "eth0", IFNAMSIZ-1);
	  if(ioctl(socket_fd, SIOCGIFINDEX, &ifr) < 0)
	    {
	      rcs_print_error("ioctl(socket_fd=%d, SIOCGIFINDEX, struct ifreq ifr.ifr_name=%s) failed.\n",
			      socket_fd, ifr.ifr_name);
	    }
	  else
	    {
	      s6->sin6_scope_id = ifr.ifr_ifindex;
	    }
#endif
	  dl_closesocket(socket_fd);
#endif
	}
#endif
      return(socket_address_ptr);

      /* #ifdef HAVE_GETADDRINFO */
#else

      dl_modified_gethostbyname (hostname, &host_entry,1);
      if (NULL == host_entry)
	{
#if MS_WINDOWS_API
	  rcs_print_sys_error (WSAGETLASTERROR_ERROR_SOURCE,
			       "gethostbyname error");
#endif
	  rcs_print_error ("Couldn't get host address for (%s).\n",
			   hostname);
	  dl_free_sockaddr(socket_address_ptr);
	  return(0);
	}
#ifdef __MSDOS__
      s4->sin_addr.s_addr =
	*((u_long *) host_entry->h_addr_list[0]);
#else
      s4->sin_addr.s_addr =
	*((int *) host_entry->h_addr_list[0]);
#endif
      s4->sin_family = host_entry->h_addrtype;
      /* #ifdef HAVE_GETADDRINFO */
#endif

      /* #ifndef VXWORKS */
#else
      ((struct sockaddr_in*)socket_address_ptr)->sin_addr.s_addr = hostGetByName (hostname);
      if (((struct sockaddr_in*)socket_address_ptr)->sin_addr.s_addr 
	  == ((unsigned)ERROR))
	{
	  rcs_print_error ("Couldn't get host address for (%s).\n",
			   hostname);
	  dl_free_sockaddr(socket_address_ptr);
	  return(0);
	}
      /* #ifndef VXWORKS */
#endif
    }
  if(len_ptr)
    {
      *len_ptr = len;
    }
  return(socket_address_ptr);
}



static struct sockaddr *
dl_create_broadcast_sockaddr(const char *hostname, short port, size_t *len_ptr, int use_ipv6)
{
  struct sockaddr *socket_address_ptr = 0;
  size_t len;
  int hostname_was_address = 0;
  char bufferhost_first_char = 0;
  long ls_addr=-1;
#ifdef HAVE_GETADDRINFO
  int getaddrinfo_ret=0;
  struct addrinfo *ai=0;
#else
  struct hostent *host_entry=0;
#endif
#ifdef HAVE_STRUCT_SOCKADDR_IN6_TYPE
  struct sockaddr_in6 *s6 = 0;
#endif
  struct sockaddr_in *s4 = 0;

  if(use_ipv6)
    {
      rcs_print_error("broadcast not supported for IPV6\n");
      
      return(0);
    }

  /* Set up the socket address stucture. */
#ifdef HAVE_STRUCT_SOCKADDR_STORAGE_TYPE
  len=sizeof(struct sockaddr_storage);
#else
  len=sizeof(struct sockaddr_in);
#ifdef HAVE_STRUCT_SOCKADDR_IN6_TYPE
  len=(len > sizeof(struct sockaddr_in6))?len:sizeof(struct sockaddr_in6);
#endif
#endif
  socket_address_ptr = (struct sockaddr *) malloc(len);
  if(!socket_address_ptr)
    {
      return 0;
    }
  memset (socket_address_ptr, 0, len);

#ifdef HAVE_STRUCT_SOCKADDR_IN6_TYPE
  s6 = (struct sockaddr_in6 *) socket_address_ptr;
#endif
  s4 = (struct sockaddr_in *) socket_address_ptr;

  if(use_ipv6)
    {
#if defined(HAVE_STRUCT_SOCKADDR_IN6_TYPE) && defined(AF_INET6)
      s6->sin6_family = AF_INET6;
      s6->sin6_port = dl_htons (port);
      s6->sin6_addr = in6addr_any;
#else
      rcs_print_error("IPv6 not supported.\n");
#endif
      dl_free_sockaddr(socket_address_ptr);
      return(0);
    }
  else
    {
      s4->sin_family = PF_INET;
      s4->sin_port = dl_htons (port);
      s4->sin_addr.s_addr = htonl(INADDR_BROADCAST);
    }

  if(!hostname)
    {
      return socket_address_ptr;
    }
      
  bufferhost_first_char = *hostname;
  if (bufferhost_first_char >= '0' && bufferhost_first_char <= '9' 
#if defined(HAVE_STRUCT_SOCKADDR_STORAGE) && defined(HAVE_INET_PTON) && defined(HAVE_STRUCT_SOCKADDR_INET6_TYPE)
      || (bufferhost_first_char >= 'A' && bufferhost_first_char <= 'F' && strchr(hostname,':') != 0 )
#endif
	  )
    {
#if HAVE_INET_PTON
      if(strchr(hostname,'.') == 0 && strchr(hostname,':') != 0)
	{
#if HAVE_STRUCT_SOCKADDR_IN6_TYPE && AF_INET6
	  if(inet_pton(AF_INET6,hostname,&s6->sin6_addr) <= 0)
	    {
	      rcs_print_error("inet_pton(AF_INET6,%s) failed. -- %s\n",hostname,strerror(errno));
	      return 0;
	    }
	  return(socket_address_ptr);
#else
	  rcs_print_error("ipv6 not supported.\n");
	  dl_free_sockaddr(socket_address_ptr);
	  return(0);
#endif
	}
      else
	{
	  if(inet_pton(AF_INET,hostname,&s4->sin_addr) <= 0)
	    {
	      rcs_print_error("inet_pton(AF_INET,%s) failed. -- %s\n",
			      hostname,strerror(errno));
	      return 0;
	    }
	  ls_addr = (long) s4->sin_addr.s_addr;
	}
#else
      s4->sin_addr.s_addr = dl_inet_addr (hostname);
      ls_addr = (long) s4->sin_addr.s_addr;
#endif
      if (ls_addr != 0 &&
	  ls_addr != -1)
	{
	  hostname_was_address = 1;
	}
    }

  if (!hostname_was_address)
    {
      if(!isalpha((int)bufferhost_first_char))
	{
	  rcs_print_error("Bad host name %s\n",hostname);
	  dl_free_sockaddr(socket_address_ptr);
	  return 0;
	}
      /* Get the IP address of the server using it's hostname. */
#ifndef VXWORKS

#ifdef HAVE_GETADDRINFO
      getaddrinfo_ret = getaddrinfo(hostname,0,0,&ai);
      if(getaddrinfo_ret != 0 || !ai)
	{
	  rcs_print_error("getaddrinfo() failed for hostname=%s, error=%d:%s\n",
			  hostname,
			  getaddrinfo_ret,
#ifdef HAVE_GAISTRERROR
			  gai_strerror(getaddrinfo_ret)
#else
			  ""
#endif
			  );
	  dl_free_sockaddr(socket_address_ptr);
	  return 0;
	}
      memcpy(socket_address_ptr,ai->ai_addr,ai->ai_addrlen);
#ifdef AF_INET6
      if(ai->ai_family == AF_INET6)
#else
	if(0)
#endif
	{
#ifdef HAVE_STRUCT_SOCKADDR_IN6_TYPE
	  ((struct sockaddr_in6 *)socket_address_ptr)->sin6_port = dl_htons (port);
#endif
	}
      else
	{
	  ((struct sockaddr_in *)socket_address_ptr)->sin_port = dl_htons (port);
	}
      if(len_ptr)
	{
	  *len_ptr = len;
	}
      freeaddrinfo(ai);
      return(socket_address_ptr);

      /* #ifdef HAVE_GETADDRINFO */
#else

      dl_modified_gethostbyname (hostname, &host_entry,1);
      if (NULL == host_entry)
	{
#if MS_WINDOWS_API
	  rcs_print_sys_error (WSAGETLASTERROR_ERROR_SOURCE,
			       "gethostbyname error");
#endif
	  rcs_print_error ("Couldn't get host address for (%s).\n",
			   hostname);
	  dl_free_sockaddr(socket_address_ptr);
	  return(0);
	}
#ifdef __MSDOS__
      s4->sin_addr.s_addr =
	*((u_long *) host_entry->h_addr_list[0]);
#else
      s4->sin_addr.s_addr =
	*((int *) host_entry->h_addr_list[0]);
#endif
      s4->sin_family = host_entry->h_addrtype;
      /* #ifdef HAVE_GETADDRINFO */
#endif

      /* #ifndef VXWORKS */
#else
      ((struct sockaddr_in*)socket_address_ptr)->sin_addr.s_addr = hostGetByName (hostname);
      if (((struct sockaddr_in*)socket_address_ptr)->sin_addr.s_addr 
	  == ((unsigned)ERROR))
	{
	  rcs_print_error ("Couldn't get host address for (%s).\n",
			   hostname);
	  dl_free_sockaddr(socket_address_ptr);
	  return(0);
	}
      /* #ifndef VXWORKS */
#endif
    }
  if(len_ptr)
    {
      *len_ptr = len;
    }
  return(socket_address_ptr);
}

void
dl_free_sockaddr(struct sockaddr *sp)
{
  if(sp)
    {
	  free(sp);
    }
}

#ifdef HAVE_INET_NTOP
char sockaddr_temp_string[256];
#endif

static const char *
dl_sockaddr_to_string(const struct sockaddr * addr, char *buf, int buflen, int use_ipv6)
{
  if(!addr)
    {
      return "..::NULL::..";
    }
  if(use_ipv6)
    {
#if defined(HAVE_INET_NTOP) && defined(AF_INET6) && defined(HAVE_STRUCT_SOCKADDR_IN6_TYPE)
      return inet_ntop(AF_INET6,
		       (const void *) &(((const struct sockaddr_in6 *)addr)->sin6_addr),
		       (buf?buf:sockaddr_temp_string), (buf?buflen:256));
#else
      return "ipv6.not.supported";
#endif

    }
  else
    {
      return dl_inet_ptr_ntoa(&((struct sockaddr_in*)addr)->sin_addr);
    }
}

static unsigned short
dl_sockaddr_get_port(const struct sockaddr * addr,int use_ipv6)
{
  if(!addr)
    {
      rcs_print_error("dl_sockaddr_get_port: addr == NULL\n");
      return -1;
    }

  if(use_ipv6)
    {
#ifdef HAVE_STRUCT_SOCKADDR_IN6_TYPE
      return ntohs (((struct sockaddr_in6*)addr)->sin6_port);
#else
      return -1;
#endif
    }
  else
    {
      return ntohs (((struct sockaddr_in*)addr)->sin_port);
    }
}


static void
dl_sockaddr_set_port(const struct sockaddr * addr,unsigned short port, int use_ipv6)
{
  if(use_ipv6)
    {
#ifdef HAVE_STRUCT_SOCKADDR_IN6_TYPE
      ((struct sockaddr_in6*)addr)->sin6_port = dl_htons(port);
#endif
    }
  else
    {
      ((struct sockaddr_in*)addr)->sin_port = dl_htons(port);
    }
}


struct dl_sa
{
  struct sockaddr *sockaddr_ptr;
  size_t len;
  char str_buf[256];
  int use_ipv6;
};

struct dl_sa *
dl_create_sa_empty(int use_ipv6)
{
  struct dl_sa *r= malloc(sizeof(struct dl_sa));

  r->use_ipv6 = use_ipv6;
  r->sockaddr_ptr =
    dl_create_sockaddr_empty(&r->len,use_ipv6);
  return r;
}

struct dl_sa *
dl_create_sa(const char *host, 
		   short port, 
		   int use_ipv6)
{
  struct dl_sa *r= malloc(sizeof(struct dl_sa));
  if(!r)
    {
      return 0;
    }
  r->use_ipv6 = use_ipv6;

  r->sockaddr_ptr =
    dl_create_sockaddr(host,port,&r->len,use_ipv6);
  return r;
}

struct sockaddr *
dl_sa_addr(struct dl_sa *d)
{
  if(d)
    {
      return d->sockaddr_ptr;
    }
  return(0);
}

int
dl_sa_len(struct dl_sa *d)
{
  if(d)
    {
      return (int) d->len;
    }
  return(0);
}

const char *
dl_sa_get_host(struct dl_sa * d)
{
  if(d)
    {
      const char *h = dl_sockaddr_to_string(d->sockaddr_ptr,
					    d->str_buf,
					    sizeof(d->str_buf),
					    d->use_ipv6);
      if(h)
	{
	  return h;
	}
      else
	{
	  return "..::!BAD_HOST_ADDRESS!::..";
	}
    }
  return("..::!BAD_HOST_ADDRESS!::..");
}

unsigned short
dl_sa_get_port(const struct dl_sa * d)
{
  if(d)
    {
      return dl_sockaddr_get_port(d->sockaddr_ptr,d->use_ipv6);
    }
  rcs_print_error("dl_sa_get_port: addr == NULL\n");
  return(-1);
}

void
dl_sa_set_port(struct dl_sa *d, unsigned short port)
{
    if(d)
    {
      dl_sockaddr_set_port(d->sockaddr_ptr,port,d->use_ipv6);
    }
}

void
dl_sa_set_host(struct dl_sa *d, const char *host)
{
  if(d)
    {
      short port = dl_sockaddr_get_port(d->sockaddr_ptr,d->use_ipv6);
      if(d->sockaddr_ptr)
	{
	  dl_free_sockaddr(d->sockaddr_ptr);
	}
      d->sockaddr_ptr = dl_create_sockaddr(host,port,&d->len,d->use_ipv6);
    }
}
					  
void dl_free_sa(struct dl_sa *d)
{
  if(d)
    {
      if(d->sockaddr_ptr)
	{
	  dl_free_sockaddr(d->sockaddr_ptr);
	  d->sockaddr_ptr=0;
	}
      free(d);
    }
}

void
dl_sa_copy(struct dl_sa **ptr_to_dest, struct dl_sa *src)
{
  struct dl_sa *dest;
  if(*ptr_to_dest)
    {
      dest = *ptr_to_dest;
    }
  else
    {
      dest=dl_create_sa_empty(src->use_ipv6);
      memcpy(dest->sockaddr_ptr,
	     src->sockaddr_ptr,
	     dest->len);
      *ptr_to_dest = dest;
      return;
    }

  if(dest->use_ipv6 != src->use_ipv6)
    {
      if(dest->sockaddr_ptr)
	{
	  dl_free_sockaddr(dest->sockaddr_ptr);
	  dest->sockaddr_ptr= 0;
	}
    }
  if(!dest->sockaddr_ptr)
    {
      dest->sockaddr_ptr = dl_create_sockaddr_empty(
					      &dest->len,
					      src->use_ipv6);
      memcpy(dest->sockaddr_ptr,
	     src->sockaddr_ptr,
	     dest->len);
      *ptr_to_dest = dest;
      return;
    } 
  memcpy(dest->sockaddr_ptr,
	 src->sockaddr_ptr,
	 dest->len);
}

struct dl_sa *dl_create_broadcast_sa(const char *host, 
				     short port, 
				     int use_ipv6)
{
  struct dl_sa *r= malloc(sizeof(struct dl_sa));
  if(!r)
    {
      return 0;
    }
  r->use_ipv6 = use_ipv6;

  r->sockaddr_ptr =
    dl_create_broadcast_sockaddr(host,port,&r->len,use_ipv6);
  return r;
}

int
dl_sa_compare(struct dl_sa *d1, struct dl_sa *d2)
{
  if(!d1 && !d2)
    {
      return 0;
    }
  if(!d1 || !d2)
    {
      return 1;
    }
  if(d1->use_ipv6 != d2->use_ipv6 ||
     d1->len != d2 ->len)
    {
      return 1;
    }
  if(!d1->sockaddr_ptr && !d2->sockaddr_ptr)
    {
      return 0;
    }
  if(!d1->sockaddr_ptr || !d2->sockaddr_ptr)
    {
      return 1;
    }
  if(dl_sockaddr_get_port(d1->sockaddr_ptr,d1->use_ipv6) != 
     dl_sockaddr_get_port(d2->sockaddr_ptr,d2->use_ipv6))
    {
      return 1;
    }
  if(!d1->use_ipv6)
    {
      if(((struct sockaddr_in *) d1->sockaddr_ptr)->sin_addr.s_addr != 
	 ((struct sockaddr_in *) d2->sockaddr_ptr)->sin_addr.s_addr )
	{
	  return 1;
	}
      return 0;
    }	
  return(memcmp(d1->sockaddr_ptr,d2->sockaddr_ptr,d2->len));
}


char this_host_buf[256];

#ifdef SIOCGIFADDR
#if HAVE_STRUCT_IFREQ_TYPE
const char *
dl_get_intf_address(const char *ifnamestr, 
			     struct ifreq *ifr_ptr,
			     int fd, int use_ipv6)
{
  strcpy(ifr_ptr->ifr_name,ifnamestr);
  if(0 == ioctl(fd, SIOCGIFADDR, ifr_ptr))
    {
      return dl_sockaddr_to_string(&ifr_ptr->ifr_addr,
				   this_host_buf,
				   sizeof(this_host_buf),use_ipv6);
    }
  return(0);
}
#endif
#endif

static int debug_address_is_local=0;

int debug_strcmp(const char *s1,const char *s2, const char *intrf)
{
  if(debug_address_is_local)
    {
      printf("comparing %s and %s  (from %s)\n",s1,s2,intrf);
    }
  return(strcmp(s1,s2));
}

int
dl_ip_address_on_this_host(const char *ip_string_to_compare, int use_ipv6)
{
#ifdef SIOCGIFADDR
#if HAVE_STRUCT_IFREQ_TYPE
  char nbuf[16];
  char ip_string_save_buf[256];
  const char *ip_string=0;
  int i=0;
  struct ifreq ifr;
  int fd = -1;
  if(use_ipv6)
    {
#ifdef PF_INET6
      fd = socket(PF_INET6,SOCK_DGRAM,0);
#else
      return 0;
#endif
    }
  else
    {
      fd = socket(PF_INET,SOCK_DGRAM,0);
    }
  if(fd < 1)
    {
      if(!use_ipv6)
	{
	  rcs_print_error("dl_get_this_host_address_string : socket() failed. %s\n",
			  strerror(errno));
	}
      return(0);
    }
  if(!ip_string_to_compare)
    {
      dl_closesocket(fd);
      return(0);
    }
  strncpy(ip_string_save_buf,ip_string_to_compare,sizeof(ip_string_save_buf));
  for(i = 0; i < 16 ; i++)
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(nbuf,sizeof(nbuf)),"eth%d",i);
      ip_string = dl_get_intf_address(nbuf,&ifr,fd,use_ipv6);
      if(ip_string)
	{
	  if(!debug_strcmp(ip_string_save_buf,ip_string,nbuf))
	    {
	      dl_closesocket(fd);
	      return 1;
	    }
	}
    }
  for(i = 0; i < 16 ; i++)
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(nbuf,sizeof(nbuf)),"wlan%d",i);
      ip_string = dl_get_intf_address(nbuf,&ifr,fd,use_ipv6);
      if(ip_string)
	{
	  if(!debug_strcmp(ip_string_save_buf,ip_string,nbuf))
	    {
	      dl_closesocket(fd);
	      return 1;
	    }
	}
    }
  for(i = 0; i < 16 ; i++)
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(nbuf,sizeof(nbuf)),"en%d",i);
      ip_string = dl_get_intf_address(nbuf,&ifr,fd,use_ipv6);
      if(ip_string)
	{
	  if(!debug_strcmp(ip_string_save_buf,ip_string,nbuf))
	    {
	      dl_closesocket(fd);
	      return 1;
	    }
	}
    }
  for(i = 0; i < 16 ; i++)
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(nbuf,sizeof(nbuf)),"ra%d",i);
      ip_string = dl_get_intf_address(nbuf,&ifr,fd,use_ipv6);
      if(ip_string)
	{
	  if(!debug_strcmp(ip_string_save_buf,ip_string,nbuf))
	    {
	      dl_closesocket(fd);
	      return 1;
	    }
	}
    }

  for(i = 0; i < 16 ; i++)
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(nbuf,sizeof(nbuf)),"ath%d",i);
      ip_string = dl_get_intf_address(nbuf,&ifr,fd,use_ipv6);
      if(ip_string)
	{
	  if(!debug_strcmp(ip_string_save_buf,ip_string,nbuf))
	    {
	      dl_closesocket(fd);
	      return 1;
	    }
	}
    }

  {
    SNPRINTF_FUNC ( SNPRINTF_ARGS(nbuf,sizeof(nbuf)),"lo");
    ip_string = dl_get_intf_address(nbuf,&ifr,fd,use_ipv6);
    if(ip_string)
      {
	if(!debug_strcmp(ip_string_save_buf,ip_string,nbuf))
	    {
	      dl_closesocket(fd);
	      return 1;
	    }
      }
  }
  dl_closesocket(fd);
#endif
#endif
  return 0;
}

const char *
dl_get_this_host_address_string(int use_ipv6)
{
#ifdef SIOCGIFADDR
#if HAVE_STRUCT_IFREQ_TYPE
  char nbuf[16];
  const char *ip_string=0;
  int i;
  struct ifreq ifr;
  int fd = -1;
  if(use_ipv6)
    {
#ifdef PF_INET6
      fd = socket(PF_INET6,SOCK_DGRAM,0);
#else
      return 0;
#endif
    }
  else
    {
      fd = socket(PF_INET,SOCK_DGRAM,0);
    }
  if(fd < 1)
    {
      if(!use_ipv6)
	{
	  rcs_print_error("dl_get_this_host_address_string : socket() failed. %s\n",
			  strerror(errno));
	}
      return(0);
    }
  for(i = 0; i < 16 ; i++)
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(nbuf,sizeof(nbuf)),"eth%d",i);
      ip_string = dl_get_intf_address(nbuf,&ifr,fd,use_ipv6);
      if(ip_string)
	{
	  dl_closesocket(fd);
	  return ip_string;
	}
    }
  for(i = 0; i < 16 ; i++)
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(nbuf,sizeof(nbuf)),"wlan%d",i);
      ip_string = dl_get_intf_address(nbuf,&ifr,fd,use_ipv6);
      if(ip_string)
	{
	  dl_closesocket(fd);
	  return ip_string;
	}
    }
  for(i = 0; i < 16 ; i++)
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(nbuf,sizeof(nbuf)),"en%d",i);
      ip_string = dl_get_intf_address(nbuf,&ifr,fd,use_ipv6);
      if(ip_string)
	{
	  dl_closesocket(fd);
	  return ip_string;
	}
    }
  for(i = 0; i < 16 ; i++)
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(nbuf,sizeof(nbuf)),"ra%d",i);
      ip_string = dl_get_intf_address(nbuf,&ifr,fd,use_ipv6);
      if(ip_string)
	{
	  dl_closesocket(fd);
	  return ip_string;
	}
    }
  dl_closesocket(fd);
#endif
#endif
  return 0;
}

static int last_dl_address_is_local=-1;
static char last_dl_address_is_local_hostname_to_check[256];

int 
dl_address_is_local(const char *hostname_to_check, 
		    int set_debug_address_is_local)
{
  char my_hostname[256];
#ifndef VXWORKS
  struct hostent *my_hostent_ptr = 0;
  struct hostent *hostname_to_check_hostent_ptr = 0;
  struct hostent *my_hostent_copy_ptr;
  char my_hostent_addresses[16][16];
  int num_my_hostent_addresses = 0;
  struct in_addr myaddress;
  int j, k;
#endif
  struct in_addr *ina_temp_ptr=0;
  const char *ip_temp_string=0;
  char ip_temp_string_save_buf[256];
  const char *ip_string = 0;


  debug_address_is_local = set_debug_address_is_local;
  if(last_dl_address_is_local >= 0 &&
     !strncmp(hostname_to_check,last_dl_address_is_local_hostname_to_check, sizeof(last_dl_address_is_local_hostname_to_check)))
  {
    return last_dl_address_is_local;
  }
  strncpy(last_dl_address_is_local_hostname_to_check, hostname_to_check,
	  sizeof(last_dl_address_is_local_hostname_to_check));


  if (!strncmp (hostname_to_check, "localhost", 9))
    {
      return  (last_dl_address_is_local=1);
    }

  if (!strncmp (hostname_to_check, "127.0.0.1", 9))
    {
      return  (last_dl_address_is_local=1);
    }
 	  
  ip_string = dl_get_this_host_address_string(0);
  if(ip_string && !strcmp (hostname_to_check,ip_string))
    {
      return  (last_dl_address_is_local=1);
    }

  ip_string = dl_get_this_host_address_string(1);
  if(ip_string && !strcmp (hostname_to_check,ip_string))
    {
      return  (last_dl_address_is_local=1);
    }

  dl_gethostname (my_hostname, 256);
  if (!debug_strcmp (hostname_to_check, my_hostname,"gethostname"))
    {
      return  (last_dl_address_is_local=1);
    }
  if(dl_ip_address_on_this_host(hostname_to_check, 0))
    {
      return  (last_dl_address_is_local=1);
    }

  if(dl_ip_address_on_this_host(hostname_to_check, 1))
    {
      return  (last_dl_address_is_local=1);
    }

#ifdef VXWORKS
  if (hostGetByName (my_hostname) ==(int) inet_addr(hostname_to_check))
    {
      return  (last_dl_address_is_local=1);
    }
#else
  dl_modified_gethostbyname (my_hostname, &my_hostent_ptr,0);
  if (0 == my_hostent_ptr)
    {
      return  (last_dl_address_is_local=0);
    }
  myaddress.s_addr = *((int *) my_hostent_ptr->h_addr_list[0]);
  if (!debug_strcmp (hostname_to_check, dl_inet_ptr_ntoa (&myaddress),"gethostbyname"))
    {
      return  (last_dl_address_is_local=1);
    }
  if (my_hostent_ptr->h_length < 1 || my_hostent_ptr->h_length > 16)
    {
      rcs_print_error ("Bad hostentry length.\n");
      return  (last_dl_address_is_local=0);
    }
  /* We need to make a copy of my_hostent and all its addresses in case they 
     are clobbered when we try to get the hostentry for hostname_to_check */
  my_hostent_copy_ptr = dl_create_new_hostent_struct();
  dl_copyhostent(my_hostent_copy_ptr, my_hostent_ptr);
  for (j = 0; j < 16 && 0 != dl_gethostent_h_addr_from_list(my_hostent_copy_ptr,j); j++)
    {
      memcpy (my_hostent_addresses[j], 
	      dl_gethostent_h_addr_from_list(my_hostent_copy_ptr,j),
	      dl_gethostent_h_length(my_hostent_copy_ptr));
    }
  num_my_hostent_addresses = j;
  if (num_my_hostent_addresses < 1)
    {
      if(my_hostent_copy_ptr)
	{
	  dl_free_created_hostent_struct(my_hostent_copy_ptr);
	  my_hostent_copy_ptr=0;
	}
      return  (last_dl_address_is_local=0);
    }
  dl_modified_gethostbyname (hostname_to_check, &hostname_to_check_hostent_ptr,0);
  if (0 == hostname_to_check_hostent_ptr)
    {
      if(my_hostent_copy_ptr)
	{
	  dl_free_created_hostent_struct(my_hostent_copy_ptr);
	  my_hostent_copy_ptr=0;
	}
      return  (last_dl_address_is_local=0);
    }
  j = 0;
  k = 0;
  if (dl_gethostent_h_length(hostname_to_check_hostent_ptr) 
      != dl_gethostent_h_length(my_hostent_copy_ptr))
    {
      rcs_print_error ("Mismatched hostentry lengths.\n");
      return  (last_dl_address_is_local=0);
    }
  while (j < num_my_hostent_addresses && j < 16)
    {
      k = 0;
      while (
	     (ina_temp_ptr = (struct in_addr *) dl_gethostent_h_addr_from_list(hostname_to_check_hostent_ptr,k)) != 0 && k < 16)
	{
	  if (!memcmp
	      (my_hostent_addresses[j],
	       ina_temp_ptr,
	       dl_gethostent_h_length(my_hostent_copy_ptr)))
	    {
	      return  (last_dl_address_is_local=1);
	    }
	  ip_temp_string = dl_inet_ptr_ntoa ( (struct in_addr *) ina_temp_ptr);
	  strncpy(ip_temp_string_save_buf,ip_temp_string,sizeof(ip_temp_string_save_buf));
	  if(dl_ip_address_on_this_host(ip_temp_string_save_buf, 1))
					
	    {
	      return  (last_dl_address_is_local=1);
	    }
	  if(dl_ip_address_on_this_host(ip_temp_string_save_buf,0))
	    {
	      return  (last_dl_address_is_local=1);
	    }
	  k++;
	}
      j++;
    }
#endif
  //#ifdef VXWORKS

  return  (last_dl_address_is_local=0);
}




SOCKET
dl_tcp_socket (int use_ipv6)
{
  SOCKET socket_fd;
  if(use_ipv6)
    {
#ifdef PF_INET6
      socket_fd = dl_socket (PF_INET6, SOCK_STREAM, 0);
#else
      return -1;
#endif
    }
  else
    {
      socket_fd = dl_socket (PF_INET, SOCK_STREAM, 0);
    }
  return socket_fd;
}

SOCKET
dl_udp_socket (int use_ipv6)
{
  SOCKET socket_fd;
  if(use_ipv6)
    {
#ifdef PF_INET6
      socket_fd = dl_socket (PF_INET6, SOCK_DGRAM, 0);
#else
      return -1;
#endif
    }
  else
    {
      socket_fd = dl_socket (PF_INET, SOCK_DGRAM, 0);
    }
  return socket_fd;
}


unsigned long 
dl_sa_addr_u_long(struct dl_sa *d)
{
  if(d && d->sockaddr_ptr)
    {
      if(d->use_ipv6)
	{
#ifdef HAVE_STRUCT_SOCKADDR_IN6_TYPE
	  return ntohl((long) ((struct sockaddr_in6 *)(d->sockaddr_ptr))->sin6_addr.s6_addr);
#endif
	}
      else
	{
	  return ntohl(((struct sockaddr_in *)(d->sockaddr_ptr))->sin_addr.s_addr);
	}	  
    }
  return(0);
}

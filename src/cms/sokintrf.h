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
 *  The purpose of this file is to clean up socket interface and
 *   not rely on system include files.
 */

#ifndef SOKINTRF_H
#define SOKINTRF_H

#include <stddef.h> 		/* size_t */

#ifdef __cplusplus
extern "C"
{
#endif

  struct hostent;
  struct in_addr;
  struct timeval;
  struct sockaddr;
  typedef void *fd_set_ptr;

#if !defined(SOCKET_DEFINED) && !defined(HAVE_SOCKET_T) && !defined(HAVE_WINSOCK2_H)
#define SOCKET_DEFINED
  typedef unsigned int SOCKET;
#endif

  extern void *socket_library_instance;


/* Declare Functions to load and unload the socket interface. */
  int load_socket_interface (void);
  void unload_socket_interface (void);


/* Declare Safe Dynamically Linked alternative socket functions. */
  int dl_accept (SOCKET s, struct sockaddr *name, int *plen);
  int dl_bind (SOCKET, struct sockaddr *, int);
  int dl_closesocket (SOCKET);
  int dl_connect (SOCKET, struct sockaddr *, int);
#if MS_WINDOWS_API
  int dl_ioctlsocket (SOCKET s, long, unsigned long *);
#endif
  int dl_ioctlsocket_fionread_ulp(int , unsigned long *);
  int dl_ioctlsocket_fionread(int , int *);

  unsigned short dl_htons (unsigned short hostshort);
  unsigned short dl_ntohs (unsigned short netshort);
  unsigned long dl_inet_addr (const char *);
  struct in_addr *dl_create_new_in_addr_struct(void);
  void dl_free_created_in_addr_struct(struct in_addr *);

  struct dl_sa;  

  int dl_address_is_local(const char *hostname_to_check, int dbg);
  int dl_ip_address_on_this_host(const char *ipstring_to_compare, int use_ipv6);
  const char *dl_get_this_host_address_string(int);
  struct dl_sa *dl_create_sa(const char *host, 
			     short port, 
			     int use_ipv6);
  struct dl_sa *dl_create_broadcast_sa(const char *host, 
				       short port, 
				       int use_ipv6);

  unsigned long dl_sa_addr_u_long(struct dl_sa *);
  struct sockaddr *dl_sa_addr(struct dl_sa *);
  int dl_sa_len(struct dl_sa *);
  const char *dl_sa_get_host(struct dl_sa * addr);
  unsigned short dl_sa_get_port(const struct dl_sa * addr);
  void dl_sa_set_port(struct dl_sa * addr, unsigned short port);
  void dl_sa_set_host(struct dl_sa * addr, const char *host);
  void dl_free_sa(struct dl_sa *);
  void dl_sa_copy(struct dl_sa **ptr_to_dest, struct dl_sa *src);
  int dl_sa_compare(struct dl_sa *, struct dl_sa *);

  int dl_select_connect_completed(int socketfd, int print_errors, int *connect_error);
  int dl_connect_in_with_timeout(int socket_fd, 
				 struct dl_sa *sinptr,
				 double connect_timeout, 
				 int print_errors, 
				 int continue_after_connecion_refused,
				 int *error_code_ptr, 
				 int *timedout_ptr,
				 int *interrupt_op_ptr,
				 char *sockerrbuf, 
				 size_t sockerrbuf_size);
       
  char *dl_inet_ptr_ntoa (struct in_addr *);

  /*  struct in_addr dl_inet_makeaddr (const int net, const int lna); */

  int dl_gethostname (char *hostname, int maxlen);
  int dl_listen (SOCKET, int);
  int dl_recv (SOCKET, char *, int, int);
  int dl_recvfrom (SOCKET, char *, int, int, struct sockaddr *,
			      int *);
  int dl_select (int, fd_set_ptr , fd_set_ptr , fd_set_ptr ,
			    struct timeval *);
  int dl_send (SOCKET, const void *, int, int);
  int dl_sendto (SOCKET, char *, int, int, struct sockaddr *, int);
  SOCKET dl_tcp_socket (int);
  SOCKET dl_udp_socket (int);
#if !defined(VXWORKS) 
  int dl_modified_gethostbyname(const char *, struct hostent**, int print_errors);
  struct hostent *dl_create_new_hostent_struct(void);
  void dl_free_created_hostent_struct(struct hostent *);
  int dl_copyhostent(struct hostent *dest, struct hostent *src);
  char *dl_gethostent_h_addr_from_list(struct hostent* , int i);
  int dl_gethostent_h_length(struct hostent *hptr);
#endif

  int dl_setsockopt (SOCKET s, int level, int opt,
				char * optval, int len);
  int dl_fd_isset (SOCKET s, fd_set_ptr  set);
  int dl_get_last_socket_error_int( int socket_fd);
  const char *dl_get_last_socket_error_string( int socket_fd, int error_int,
					       char *buf, size_t maxbuflen);
  int dl_socket_error_was_would_block ( int socket_fd , int error_int);
  int dl_socket_error_was_connection_refused ( int socket_fd , int error_int);
  int dl_select_ret_is_error(int select_ret);
  int dl_socketpair(int d, int type, int protocol, int sv[2]);

#ifdef __cplusplus
}
#endif

#if MS_WINDOWS_API
#define RCS_FD_SET(x,y) FD_SET(((SOCKET) x),y)
#define RCS_FD_CLR(x,y) FD_CLR(((SOCKET) x),y)
#else
#define RCS_FD_SET FD_SET
#define RCS_FD_CLR FD_CLR
#endif


#endif				/* SOKINTRF_H */

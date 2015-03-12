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
 * Select uses arrays of SOCKETs.  These macros manipulate such
 * arrays.  FD_SETSIZE may be defined by the user before including
 * this file, but the default here should be >= 64.
 *
 * CAVEAT IMPLEMENTOR and USER: THESE MACROS AND TYPES MUST BE
 * INCLUDED IN WINSOCK.H EXACTLY AS SHOWN HERE.
 */


#ifndef INADDR_ANY
#define INADDR_ANY ((unsigned long) 0)
#endif


#ifndef HOSTENT_DEFINED
#define HOSTENT_DEFINED
struct hostent
{
  char *h_name;			/* official name of host */
  char **h_aliases;		/* alias list */
  int h_addrtype;		/* host address type */
  int h_length;			/* length of address */
  char **h_addr_list;		/* list of addresses from name server */
#ifndef h_addr
#define h_addr  h_addr_list[0]	/* address, for backward compatiblity */
#endif
};
#endif

/*
 * Internet address (old style... should be updated)
 */
#ifndef IN_ADDR_DEFINED
#define IN_ADDR_DEFINED
struct in_addr
{
  union
  {
    struct
    {
      unsigned char s_b1, s_b2, s_b3, s_b4;
    }
    S_un_b;
    struct
    {
      unsigned short s_w1, s_w2;
    }
    S_un_w;
    unsigned long S_addr;
  }
  S_un;
#define s_addr  S_un.S_addr	/* can be used for most tcp & ip code */
#define s_host  S_un.S_un_b.s_b2	/* host on imp */
#define s_net   S_un.S_un_b.s_b1	/* network */
#define s_imp   S_un.S_un_w.s_w2	/* imp */
#define s_impno S_un.S_un_b.s_b4	/* imp # */
#define s_lh    S_un.S_un_b.s_b3	/* logical host */
};
#endif

/*
 * Structure used by kernel to store most
 * addresses.
 */
#ifndef SOCKADDR_DEFINED
#define SOCKADDR_DEFINED
struct sockaddr
{
  unsigned short sa_family;	/* address family */
  char sa_data[14];		/* up to 14 bytes of direct address */
};
#endif


/*
 * Socket address, internet style.
 */
#ifndef SOCKADDR_IN_DEFINED
#define SOCKADDR_IN_DEFINED
struct sockaddr_in
{
  short sin_family;
  unsigned short sin_port;
  struct in_addr sin_addr;
  char sin_zero[8];
};
#endif

/*
 * Structure used in select() call, taken from the BSD file sys/time.h.
 */
#ifndef TIMEVAL_DEFINED
#define TIMEVAL_DEFINED
struct timeval
{
  long tv_sec;			/* seconds */
  long tv_usec;			/* and microseconds */
};
#endif

#ifndef IOVEC_DEFINED
#define IOVEC_DEFINED
struct iovec
{
  char RCS_FAR *iov_base;
  int iov_len;
};
typedef struct iovec iovec_t;
#endif

#ifndef MSGHDR_DEFINED
#define MSGHDR_DEFINED
/*
 * Message header for recvmsg and sendmsg calls.
 */
struct msghdr
{
  char RCS_FAR *msg_name;	/* optional address */
  int msg_namelen;		/* size of address */
  struct iovec RCS_FAR *msg_iov;	/* scatter/gather array */
  int msg_iovlen;		/* # elements in msg_iov */
  char RCS_FAR *msg_accrights;	/* access rights sent/received */
  int msg_accrightslen;
};
#endif

#ifdef _WINDOWS


#ifndef HINSTANCE_ERROR
#define HINSTANCE_ERROR (32)
#endif

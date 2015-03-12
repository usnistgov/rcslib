/***
    Program for testing time synchronization between two computers.
    Run udptimesend on the other computer.

Compile with:
    gcc udptimesend.c -lrt -o udptimesend
OR 
    gcc udptimesend.c -DUSE_RCSLIB -I ~/rcslib/include -L ~/rcslib/b/ -lrcs -o udptimesend

This file is not needed for rcslib/nml etc.

***/


#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>

#ifdef USE_RCSLIB
#include "_timer.h"
#else

#ifdef WIN32
#include <windows.h>
#else
#include <time.h>
#endif

static int verbose = 0;

#ifdef WIN32

struct timespec {
	long tv_sec;
	long tv_nsec;
};
#define CLOCK_REALTIME 0

static LARGE_INTEGER
getFILETIMEoffset()
{
    SYSTEMTIME s;
    FILETIME f;
    LARGE_INTEGER t;

    s.wYear = 1970;
    s.wMonth = 1;
    s.wDay = 1;
    s.wHour = 0;
    s.wMinute = 0;
    s.wSecond = 0;
    s.wMilliseconds = 0;
    SystemTimeToFileTime(&s, &f);
    t.QuadPart = f.dwHighDateTime;
    t.QuadPart <<= 32;
    t.QuadPart |= f.dwLowDateTime;
    return (t);
}

static int
clock_gettime(int X, struct timespec *tv)
{
    LARGE_INTEGER           t;
    FILETIME            f;
    double                  microseconds;
    static LARGE_INTEGER    offset;
    static double           frequencyToMicroseconds;
    static int              initialized = 0;
    static BOOL             usePerformanceCounter = 0;
	static double microseconds_offset=0;

    if (!initialized) {
        LARGE_INTEGER performanceFrequency;
        LARGE_INTEGER epoch;
		LARGE_INTEGER qpc;
		initialized = 1;
        usePerformanceCounter = QueryPerformanceFrequency(&performanceFrequency);
		if(verbose) printf("usePeformanceCounter=%d\n",usePerformanceCounter);
        if (usePerformanceCounter) {
			GetSystemTimeAsFileTime(&f);
			t.QuadPart = f.dwHighDateTime;
			t.QuadPart <<= 32;
			t.QuadPart |= f.dwLowDateTime;
	        if(verbose) printf("t.QuadPard=%d\n",t.QuadPart);
			QueryPerformanceCounter(&qpc);
            if(verbose) printf("qpc.QuadPard=%d\n",qpc.QuadPart);
			frequencyToMicroseconds = (double)performanceFrequency.QuadPart / 1000000.;
			epoch = getFILETIMEoffset();
			if(verbose) printf("Jan 1, 1970 epoch.QuadPart=%d\n",epoch.QuadPart);
			microseconds_offset = qpc.QuadPart/frequencyToMicroseconds  -t.QuadPart/10.0 + epoch.QuadPart/10.0;
        } else {
            frequencyToMicroseconds = 10.;
			offset = getFILETIMEoffset();
		}
		if(verbose) printf("frequencytoMicrosecond=%f\n",frequencyToMicroseconds);
    }
    if (usePerformanceCounter) {
		QueryPerformanceCounter(&t);
		microseconds = t.QuadPart/frequencyToMicroseconds - microseconds_offset;
	}
    else {
        GetSystemTimeAsFileTime(&f);
        t.QuadPart = f.dwHighDateTime;
        t.QuadPart <<= 32;
        t.QuadPart |= f.dwLowDateTime;
		t.QuadPart -= offset.QuadPart;
		microseconds = (double)t.QuadPart / frequencyToMicroseconds;
    }

    t.QuadPart = (LONGLONG) microseconds;
    tv->tv_sec = (long) (t.QuadPart / 1000000L);
    tv->tv_nsec = (long) ((t.QuadPart % 1000000L)*1000);
    return (0);
}
#endif

/* number of seconds from some epoch (Jan 1 1970) , to clock tick resolution */
static double
etime ()
{
  double retval;
  struct timespec ts;
  clock_gettime (CLOCK_REALTIME, &ts);
  retval = ((double) ts.tv_sec) + ((double) ts.tv_nsec) * 1e-9;
  if(verbose) printf("etime() retval=%f\n",retval);
  return(retval);
}

#endif // end of ifdef USE_RCSLIB

#include <sys/types.h>

#ifdef WIN32
#include <WinSock.h>
typedef int socklen_t;
typedef unsigned long useconds_t;
#else 
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <netdb.h>
static int closesocket(int s) { 
	return close(s);
}
#endif

static int
set_addr_from_string(struct sockaddr_in *addr,
		     const char *str) {
  struct hostent *h;
  if(isdigit(str[0])) {
	  addr->sin_addr.s_addr = inet_addr(str);
      return 0;
  }
  h = gethostbyname(str);
  if(h) {
    memcpy(&(addr->sin_addr),h->h_addr_list[0],h->h_length);
    return 0;
  }
#ifndef WIN32
  fprintf(stderr,"h_errno=%d : %s\n",
	  h_errno,
	  hstrerror(h_errno));
#endif
  return -1;
}

static void 
htondouble(double d,unsigned char *ptr) {
  int i;
  if(ntohl(1) == 1) {
    memcpy(ptr,&d,sizeof(double));
  }
  for(i = 0; i < 8; i++) {
    *ptr = *(((unsigned char *)&d)+(7-i));
    ptr++;
  }
}

static int quit = 0;
static void
sigint_handler(int _sig) {
  quit=1;
}

int
main(int argc, const char **argv)
{
  static struct sockaddr_in socket_address;
  static struct sockaddr_in remote_address;
  static unsigned char data_buffer[512];
  short port_num = 5555;
  int socket_fd=-1;
  size_t sendto_ret=0;
  int i=0;
  int bind_ret=-1;
  useconds_t sleep_micros = 500000;
  int verbose = 0;
  socklen_t remote_address_len = sizeof(remote_address);
  int broadcast = 0;
  int setsockopt_ret=-1;

#ifdef WIN32
  WORD winsock_version = MAKEWORD (2, 0);
  WSADATA startup_data;
  int wsa_startup_retval = WSAStartup (winsock_version, &startup_data);
  if (wsa_startup_retval)
    {
      fprintf(stderr,"Error %d from WSAStartup.\n", wsa_startup_retval);
      exit(1);
    }
#endif
  if(argc <= 1) {
    fprintf(stderr,"%s usage : Destination_Host [--broadcast] [--port <port>] [--time_interval <time in microseconds>] [--verbose]\n",
	    argv[0]);
    exit(1);
  }
  signal(SIGINT,sigint_handler);

  for(i =2; i < argc; i++) {
    if(i < argc-1) {
      if(!strcmp(argv[i],"--port")) {
	++i;
	port_num = atoi(argv[i]);
	continue;
      }
      if(!strcmp(argv[i],"--time_interval")) {
	++i;
	sleep_micros = atoi(argv[i]);
	continue;
      }
    }
    if(!strcmp(argv[i],"--broadcast")) {
      broadcast=1;
      continue;
    }
    if(!strcmp(argv[i],"--verbose")) {
      verbose=1;
      continue;
    }
    fprintf(stderr," argument %d(%s) not recognized\n",
	    i,argv[i]);
    fprintf(stderr,"%s usage : Destination_Host [--broadcast] [--port <port>] [--time_interval <time in microseconds>] [--verbose] \n",
	    argv[0]);
    exit(1);
  }

  /* memsets  is just being paranoid unless we miss something we'll overwrite it later anyway. */
  memset(&socket_address,0,sizeof(socket_address));
  memset(&remote_address,0,sizeof(remote_address));
  memset(data_buffer,0,sizeof(data_buffer));

  socket_fd = socket(PF_INET,SOCK_DGRAM,0);
  if(socket_fd == -1)
    {
      fprintf(stderr,"socket returned %d -- %s\n",
	      socket_fd,strerror(errno));
      exit(1);
    }


  socket_address.sin_family = PF_INET;
  socket_address.sin_port = INADDR_ANY;
  socket_address.sin_addr.s_addr = INADDR_ANY;

  remote_address.sin_family = PF_INET;
  remote_address.sin_port = htons(port_num);
  remote_address.sin_addr.s_addr = INADDR_ANY;
  if(set_addr_from_string(&remote_address,argv[1])) {
    fprintf(stderr,"%s: %s not valid host or ip address\n",
	    argv[0],argv[1]);
    exit(1);
  }
  if(verbose) {
    printf("Remote IP = %s\n",inet_ntoa(remote_address.sin_addr));
    printf("calling bind()\n");
  }
  bind_ret = bind(socket_fd,
		  (struct sockaddr *) &socket_address,
		  sizeof(socket_address));
  if(bind_ret == -1) {
    perror("bind");
    closesocket(socket_fd);
    exit(1);
  }

  if(broadcast) {
    setsockopt_ret=setsockopt(socket_fd, 
			      SOL_SOCKET, SO_BROADCAST, 
			      &broadcast, 
			      sizeof(broadcast));
    if(verbose) {
      printf("calling setsockopt(..,SOL_SOCKET,SO_BROADCAST,..)\n");
    }
    if(setsockopt_ret<0) {
      fprintf(stderr,"setsockopt(..,SOL_SOCKET,SO_BROADCAST,..) failed. -- %s\n",
	      strerror(errno));
    }
  }
  while(!quit) {
    if(verbose) {
      printf("calling sendto(...)\n");
    }
    htondouble(etime(),data_buffer);
    sendto_ret = sendto(socket_fd,data_buffer,8,0,
			(struct sockaddr *)(&remote_address),
			remote_address_len);
    if(sendto_ret == ((size_t)-1)) {
      perror("sendto");
      closesocket(socket_fd);
      exit(1);
    }
    if(verbose) {
      printf("sendto_ret=%lu\n",sendto_ret);
    }
#ifdef WIN32
    Sleep(sleep_micros/1000);
#else
    usleep(sleep_micros);
#endif
  }
  closesocket(socket_fd);

  return 0;
}

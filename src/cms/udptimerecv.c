/***
    Program for testing time synchronization between two computers.
    Run udptimesend on the other computer.

Compile with:
    gcc udptimerecv.c -lrt -o udptimerecv
OR
    gcc udptimerecv.c -DUSE_RCSLIB -I ~/rcslib/include -L ~/rcslib/lib/ -lrcs -o udptimerecv

This file is not needed for rcslib/nml etc.

 ***/

#ifdef USE_RCSLIB
#include "_timer.h"
#else

#ifdef WIN32
#include <windows.h>
#endif
#include <time.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>

static int verbose = 0;
static int set = 0;

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

static void set_etime(double _new_time) {
	LARGE_INTEGER offset = getFILETIMEoffset();
	LARGE_INTEGER ut;
	FILETIME f;
	SYSTEMTIME st;
	ut.QuadPart = (LONGLONG)(_new_time*1e7);
	ut.QuadPart += offset.QuadPart;
	f.dwHighDateTime = ut.HighPart;
	f.dwLowDateTime = ut.LowPart;
	FileTimeToSystemTime(&f,&st);
	printf("st.wYear=%d\n",st.wYear);
	printf("st.wMonth=%d\n",st.wMonth);
	printf("st.wDay=%d\n",st.wDay);
	printf("st.wDayOfWeek=%d\n",st.wDayOfWeek);
	printf("st.wHour=%d\n",st.wHour);
	printf("st.wMinute=%d\n",st.wMinute);
	printf("st.wSecond=%d\n",st.wSecond);
	printf("st.wMilliseconds=%d\n",st.wMilliseconds);
	SetSystemTime(&st);
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

#ifndef WIN32
#include <sys/time.h>

static void set_etime(double _new_time) {
  struct timeval tv;
  tv.tv_sec = (time_t) _new_time;
  tv.tv_usec = (suseconds_t) ((_new_time - tv.tv_sec)*1e6);
  settimeofday(&tv,NULL);
}
 
#endif

#include <sys/types.h>

#ifdef WIN32
#include <WinSock.h>
typedef int socklen_t;
#else
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
static int closesocket(int s) { 
	return close(s);
}
#endif


static void
dumpHex(const unsigned char *buf,int buflen) {
  const unsigned char *endP = buf + buflen;
  const unsigned char *P = buf;
  printf("\n");
  while(P < endP) {
    printf("%2.2X",*P);
    P++;
    if(((int)(P-buf))%16 == 0) {
      printf("\n");
    }
    else if(((int)(P-buf))%4 == 0) {
      printf(" ");
    }
  }
  printf("\n");
  printf("\n");
  P = buf;
  printf("\n");
  while(P < endP) {
    if(isprint(*P)) {
      printf("%c",*P);
    }
    else {
      printf(".");
    }
    P++;
    if(((int)(P-buf))%16 == 0) {
      printf("\n");
    }
    else if(((int)(P-buf))%4 == 0) {
      printf(" ");
    }
  }
  printf("\n");
  printf("\n");
}

static double 
ntohdouble(const unsigned char *ptr) {
  double d;
  int i;
  if(ntohl(1) == 1) {
    return *ptr;
  }
  for(i = 0; i < 8; i++) {
    *(((unsigned char *)&d)+(7-i)) = *ptr;
    ptr++;
  }
  return d;
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
  static struct sockaddr_in filter_address;
  static unsigned char data_buffer[512];
  static struct tm local_time_tm;
  time_t local_time;
  static struct tm remote_time_tm;
  struct tm *tmP;
  time_t remote_time;
  double start_time;
  short port_num = 5555;
  int socket_fd=-1;
  size_t recvfrom_ret=0;
  int i=0;
  int bind_ret=-1;
  FILE *f=0;
  double remote_timestamp;
  double local_timestamp;
  const char *remote_ip_string;
  int filter_ip=0;
  socklen_t remote_address_len = sizeof(remote_address);
  int max_count = -1;
  int count = 0;
  
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
  verbose = 0;
  memset(&filter_address,0,sizeof(filter_address));
  filter_address.sin_family = PF_INET;
  filter_address.sin_port = htons(port_num);
  filter_address.sin_addr.s_addr = INADDR_ANY;
  

  for(i =1; i < argc; i++) {
    if(i < argc-1) {
      if(!strcmp(argv[i],"--port")) {
	++i;
	port_num = atoi(argv[i]);
	continue;
      }
      if(!strcmp(argv[i],"--filter_ip")) {
	++i;
	filter_ip=i;
	continue;
      }
      if(!strcmp(argv[i],"--log")) {
	++i;
	f = fopen(argv[i],"w");
	if(!f) {
	  fprintf(stderr,"%s can't open %s -- %s\n",
		  argv[0],
		  argv[i],
		  strerror(errno));
	  exit(1);
	}
	fprintf(f,"elapsed,local_secs,remote_secs,diff,ip1,ip2,ip3,ip4,lyear,lmon,lmday,lhour,lminute,lsec,ryear,rmon,rmday,rhour,rminute,rsec\n");
	continue;
      }
      if(!strcmp(argv[i],"--max_count")) {
	++i;
	max_count=atoi(argv[i]);
	continue;
      }
    }
    if(!strcmp(argv[i],"--set")) {
      set=1;
      continue;
    }
    if(!strcmp(argv[i],"--verbose")) {
      verbose=1;
      continue;
    }
    fprintf(stderr," argument %d(%s) not recognized\n",
	    i,argv[i]);
    fprintf(stderr,"%s usage : Destination_Host [--set] [--port <port>] [--filter_ip <filter_ip>] [--time_interval <time in microseconds>] [--max_count <max_count>] [--verbose] \n",
	    argv[0]);
    exit(1);
  }
  signal(SIGINT,sigint_handler);

  /* memsets  is just being paranoid unless we miss something we'll overwrite it later anyway. */
  memset(&socket_address,0,sizeof(socket_address));
  memset(&remote_address,0,sizeof(remote_address));
  memset(data_buffer,0,sizeof(data_buffer));

  socket_fd = socket(PF_INET,SOCK_DGRAM,0);
  if(socket_fd == -1)
    {
      fprintf(stderr,"socket returned %d -- %s\n",
	      socket_fd,strerror(errno));
      fclose(f);
      exit(1);
    }

  socket_address.sin_family = PF_INET;
  socket_address.sin_port = htons(port_num);
  socket_address.sin_addr.s_addr = INADDR_ANY;
  
  if(verbose) {
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
  start_time = etime();

  if(filter_ip) {
    filter_address.sin_addr.s_addr = inet_addr(argv[filter_ip]);
    printf("filter_ip=%d (%s)\n",
	   filter_address.sin_addr.s_addr,
	   inet_ntoa(filter_address.sin_addr));
  }

  while(!quit) {
    if(verbose) {
      printf("calling recvfrom(...)\n");
    }
    recvfrom_ret = recvfrom(socket_fd,
			    data_buffer,sizeof(data_buffer),0,
			    (struct sockaddr *)(&remote_address),
			    &remote_address_len);
    if(recvfrom_ret == ((size_t)-1)) {
      perror("recvfrom");
      closesocket(socket_fd);
      exit(1);
    }
    if(verbose) {
      //printf("Remote IP = %s\n",inet_ntoa(remote_address.sin_addr));
      printf("recvfrom_ret=%lu\n",recvfrom_ret);
      // Data is read into data buffer.
      dumpHex(data_buffer,recvfrom_ret);
    }
    if(filter_address.sin_addr.s_addr &&
       filter_address.sin_addr.s_addr != INADDR_ANY &&
       filter_address.sin_addr.s_addr != remote_address.sin_addr.s_addr) {
      if(verbose) {
	printf("filter_ip=%d (%s) != remote_address = %d (%s)\n",
	       filter_address.sin_addr.s_addr,
	       inet_ntoa(filter_address.sin_addr),
	       remote_address.sin_addr.s_addr,
	       inet_ntoa(remote_address.sin_addr));
      }
      continue;
    }
    remote_timestamp = ntohdouble(data_buffer);
    if(set) {
      set_etime(remote_timestamp);
      local_timestamp = etime();
      quit=1;
      remote_ip_string = inet_ntoa(remote_address.sin_addr);
      printf("Timestamp recieved from \"%s\".\n",
	     remote_ip_string);
      printf("New time set!!!\n");
      printf("offset = %f\n", 
	     (local_timestamp-remote_timestamp));
      break;
    }
    local_timestamp = etime();
    remote_ip_string = inet_ntoa(remote_address.sin_addr);
    local_time = (time_t) local_timestamp;
    remote_time = (time_t) remote_timestamp;
#ifdef WIN32
    tmP = localtime(&local_time);
    if(tmP) {
      local_time_tm = *tmP;
    }
    tmP = localtime(&remote_time);
    if(tmP) {
      remote_time_tm = *tmP;
    }
#else 
    localtime_r(&local_time,&local_time_tm);
    localtime_r(&remote_time,&remote_time_tm);
#endif
    printf("After %f seconds elapsed timestamp recieved from \n\t \"%s\" was offset %f seconds to \n\t %d-%d-%d %d:%02.2d:%02.2d.\n",
	   (local_timestamp-start_time),
	   remote_ip_string,
	   (local_timestamp-remote_timestamp),
	   1900+remote_time_tm.tm_year,
	   1+remote_time_tm.tm_mon,
	   remote_time_tm.tm_mday,
	   remote_time_tm.tm_hour,
	   remote_time_tm.tm_min,
	   remote_time_tm.tm_sec);
    if(f) {
      fprintf(f,"%8.6f,%17.6f,%17.6f,%8.6f,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\n",
	      (local_timestamp-start_time),
	      local_timestamp,
	      remote_timestamp,
	      (local_timestamp-remote_timestamp),
	      ((htonl(remote_address.sin_addr.s_addr)>>24)&0xFF),
	      ((htonl(remote_address.sin_addr.s_addr)>>16)&0xFF),
	      ((htonl(remote_address.sin_addr.s_addr)>>8)&0xFF),
	      ((htonl(remote_address.sin_addr.s_addr))&0xFF),
	      1900+local_time_tm.tm_year,
	      1+local_time_tm.tm_mon,
	      local_time_tm.tm_mday,
	      local_time_tm.tm_hour,
	      local_time_tm.tm_min,
	      local_time_tm.tm_sec,
	      1900+remote_time_tm.tm_year,
	      1+remote_time_tm.tm_mon,
	      remote_time_tm.tm_mday,
	      remote_time_tm.tm_hour,
	      remote_time_tm.tm_min,
	      remote_time_tm.tm_sec);
    }
    count++;
    if(verbose) {
      printf("count = %d/(max_count = %d)\n",count,max_count);
    }
    if(count > max_count && max_count > 0) {
      quit=1;
      break;
    }
  }
  if(f) {
    fclose(f);
  }
  return 0;
}


/*
 * timelogsvr.c
 *
 * This server performs a function similiar to rdate or NTP server but
 * far simpler. (Even simpler than SNTP.)
 * For any packet it receives on the selected port(default=5557) it replies
 * with the first 32 bits of the recieved packet plus the current time.
 * The current time will be a double converted to network-byte order aka big-endian.
 * The value of the double is the number of seconds since Jan 1,1970 12:00.
 *  aka unix time
 *
 * Compilation/Linking Notes:
 *   Linux: requires adding -lrt for the clock_gettime() function.
 *
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <sys/select.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <netdb.h>

#ifdef USE_RCSLIB
#include "_timer.h"
#else

#ifndef WIN32
#include <time.h>
#else
#endif

/* number of seconds from some epoch (Jan 1,1970), to clock tick resolution */
static double
etime ()
{
#ifndef WIN32
  double retval;
  struct timespec ts;
  if (clock_gettime (CLOCK_REALTIME, &ts))
    {
      fprintf(stderr,"clock_gettime: errno = %d %s\n", 
	      errno,
	      strerror(errno));
    }
  retval = ((double) ts.tv_sec) + ((double) ts.tv_nsec) * 1e-9;
  return retval;
#else //ndef WIN32
  SYSTEMTIME st;
  FILETIME ft;
  SYSTEMTIME unixEpochSysTime;
  FILETIME unixEpochFileTime;
  ULARGE_INTEGER epoch_ui;
  ULARGE_INTEGER ui;
  memset(&unixEpochSysTime,0,sizeof(unixEpochSysTime));
  unixEpochSysTime.wYear = 1970;
  unixEpochSysTime.wMonth = 1;
  unixEpochSysTime.wDay = 1;
  SystemTimeToFileTime(&unixEpochSysTime,&unixEpochFileTime);
  GetSystemTime(&st);
  SystemTimeToFileTime(&st,&ft);
  ui.LowPart = ft.dwLowDateTime;
  ui.HighPart = ft.dwHighDateTime;
  epoch_ui.LowPart = unixEpochFileTime.dwLowDateTime;
  epoch_ui.HighPart = unixEpochFileTime.dwHighDateTime;
  return (1.0e-7 * (ui.QuadPart - epoch_ui.QuadPart));
#endif // ndef WIN32
}
#endif //ndef USE_RCSLIB


static double
ntohdouble(const unsigned char *ptr) {
  double d;
  int i;
  long l1=1;
  if(((char *)&l1) == 0) {
    return *ptr;
  }
  for(i = 0; i < 8; i++) {
    *(((unsigned char *)&d)+(7-i)) = *ptr;
    ptr++;
  }
  return d;
}

static void
htondouble(double d,unsigned char *ptr) {
  int i;
  long l1=1;
  if(((char *)&l1) == 0) {
    for(i = 0; i < 8; i++) {
      *ptr = *(((unsigned char *)&d)+i);
      ptr++;
    }
  } else {
    for(i = 0; i < 8; i++) {
      *ptr = *(((unsigned char *)&d)+(7-i));
      ptr++;
    }
  }
}

static void
set_address_from_string(struct sockaddr_in *addr, const char *str) {
  struct hostent *h=0;
  if(str[0] >= '0' && str[0] <= '9') {
    inet_aton(str,&(addr->sin_addr));
  }
  else {
    h = gethostbyname(str);
    if(h && h->h_length>0) {
      memcpy(&(addr->sin_addr),h->h_addr,h->h_length);
    }
  }
}

int
main(int argc, const char **argv)
{
  static struct sockaddr_in local_socket_address;
  static struct sockaddr_in send_socket_address;
  static struct sockaddr_in recv_socket_address;
  static unsigned char recv_buffer[512];
  static char recv_ip_name[512];
  static unsigned char send_buffer[12];
  static struct timeval timeout_timeval;
  int send_count = 0;
  int sent_count = 0;
  int n_send_count=0;
  short port_num = 5557;
  int socket_fd=-1;
  size_t recvfrom_ret=0;
  size_t sendto_ret=0;
  int bind_ret=-1;
  double dtime;
  socklen_t recv_address_len=sizeof(recv_socket_address);
  struct hostent *h=0;
  fd_set select_fdset;
  int select_ret = -1;
  double recvd_time=-1.0;
  double send_time=0.0;
  double offset = 0.0;
  double round_trip = 0.0;

  /* memsets  is just being paranoid unless we miss something 
     we'll overwrite it later anyway. */
  memset(&recv_socket_address,0,sizeof(recv_socket_address));
  memset(&send_socket_address,0,sizeof(send_socket_address));
  memset(&local_socket_address,0,sizeof(local_socket_address));
  memset(recv_buffer,0,sizeof(recv_buffer));
  memset(send_buffer,0,sizeof(send_buffer));

  if(argc < 2) {
    fprintf(stderr,"%s usage: host_name_or_ip\n",
	    argv[0]);
    exit(1);
  }

  if(argc > 2) {
    port_num = atoi(argv[2]);
  }

  socket_fd = socket(PF_INET,SOCK_DGRAM,0);
  if(socket_fd == -1)
    {
      fprintf(stderr,"socket returned %d -- %s\n",
	      socket_fd,strerror(errno));
      exit(1);
    }

  send_socket_address.sin_family = PF_INET;
  send_socket_address.sin_port = htons(port_num);
  send_socket_address.sin_addr.s_addr = INADDR_ANY;
  set_address_from_string(&send_socket_address,argv[1]);

  local_socket_address.sin_family = PF_INET;
  local_socket_address.sin_port = htons(INADDR_ANY);
  local_socket_address.sin_addr.s_addr = INADDR_ANY;

  recv_socket_address.sin_family = PF_INET;
  recv_socket_address.sin_port = htons(INADDR_ANY);
  recv_socket_address.sin_addr.s_addr = INADDR_ANY;
   
  bind_ret = bind(socket_fd,
		      (struct sockaddr *) &local_socket_address,
		      sizeof(local_socket_address));
  if(bind_ret == -1) {
    perror("bind");
    close(socket_fd);
    exit(1);
  }

  while(1) {
    FD_ZERO(&select_fdset);
    FD_SET(socket_fd,&select_fdset);
    timeout_timeval.tv_sec = 0;
    timeout_timeval.tv_usec = 50000;
    select_ret = select(1,
			&select_fdset, /*read fds */
			NULL, /* write fds */
			NULL, /* except fds */
			&timeout_timeval);
    if(select_ret == 0) {
	send_count++;
	n_send_count = htonl(send_count);
	dtime=etime();
	memcpy(send_buffer,&n_send_count,4);
	htondouble(dtime,send_buffer+4);
	sendto_ret = sendto(socket_fd,
			    send_buffer,
			    12,
			    0, /* flags */
			    (struct sockaddr *) &send_socket_address,
			    sizeof(send_socket_address));
      }

    memset(&recv_socket_address,0,sizeof(recv_socket_address));
    recv_address_len=sizeof(recv_socket_address);
    memset(recv_buffer,0,sizeof(recv_buffer));
    memset(recv_ip_name,0,sizeof(recv_ip_name));
    recvfrom_ret = recvfrom(socket_fd,
			    recv_buffer, 
			    sizeof(recv_buffer),
			    0, /* flags  */
			    (struct sockaddr *) &recv_socket_address,
			    &recv_address_len);
    if(recvfrom_ret != ((size_t)-1)) {
      dtime=etime();
      sent_count = ntohl(*((int*)recv_buffer));
      send_time =  ntohdouble(recv_buffer+4);
      recvd_time = ntohdouble(recv_buffer+12);
      offset = (dtime+send_time)/2.0 - recvd_time;
      round_trip = dtime-send_time;
      printf("%06d,%.6f,%.6f,%.6f,%+.6f,%+.6f,\"%s\"\n",
	     sent_count,
	     send_time,
	     recvd_time,
	     dtime,
	     offset,
	     round_trip,
	     inet_ntoa(recv_socket_address.sin_addr));
    }
  }
  close(socket_fd);
  return 0;
}


/*
 * timelogsvr.c
 *
 * This server performs a function similiar to rdate or NTP server but
 * far simpler. (Even simpler than SNTP.)
 * For any packet it receives on the selected port(default=5557) it replies
 * by echoing the recieved packet plus the current time.
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

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

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

int
main(int argc, const char **argv)
{
  static struct sockaddr_in socket_address;
  static struct sockaddr_in client_socket_address;
  static unsigned char recv_buffer[512];
  static unsigned char send_buffer[512];
  short port_num = 5557;
  int socket_fd=-1;
  size_t recvfrom_ret=0;
  size_t sendto_ret=0;
  int bind_ret=-1;
  double dtime;
  socklen_t client_address_len=sizeof(client_socket_address);

  if(argc > 1) {
    port_num = atoi(argv[1]);
  }

  /* memsets  is just being paranoid unless we miss something 
     we'll overwrite it later anyway. */
  memset(&socket_address,0,sizeof(socket_address));
  memset(&client_socket_address,0,sizeof(client_socket_address));
  memset(recv_buffer,0,sizeof(recv_buffer));
  memset(send_buffer,0,sizeof(send_buffer));

  socket_fd = socket(PF_INET,SOCK_DGRAM,0);
  if(socket_fd == -1)
    {
      fprintf(stderr,"socket returned %d -- %s\n",
	      socket_fd,strerror(errno));
      exit(1);
    }

  socket_address.sin_family = PF_INET;
  socket_address.sin_port = htons(port_num);
  socket_address.sin_addr.s_addr = INADDR_ANY;
   
  bind_ret = bind(socket_fd,
		      (struct sockaddr *) &socket_address,
		      sizeof(socket_address));
  if(bind_ret == -1) {
    perror("bind");
    close(socket_fd);
    exit(1);
  }

  while(1) {
    client_address_len=sizeof(client_socket_address);
    recvfrom_ret = recvfrom(socket_fd,
			    recv_buffer, 
			    sizeof(recv_buffer)-8,
			    0, // flags 
			   (struct sockaddr *) &client_socket_address,
			   &client_address_len);
    if(((long)recvfrom_ret) <= 0 
       || recvfrom_ret > (sizeof(recv_buffer)-8)) {
      continue;
    }
    dtime=etime();
    memcpy(send_buffer,recv_buffer,recvfrom_ret);
    htondouble(dtime,send_buffer+recvfrom_ret);
    sendto_ret = sendto(socket_fd,
			send_buffer,
			recvfrom_ret+8,
			0, // flags
			(struct sockaddr *) &client_socket_address,
			client_address_len);
  }
  close(socket_fd);
  return 0;
}

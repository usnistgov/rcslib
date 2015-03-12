
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <unistd.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

struct my_msg_packed_data
{
  long type;
  size_t size;
  int int_in_mymsg;
} __attribute__((packed));

int
main(int argc, const char **argv)
{
  static struct sockaddr_in server_socket_address;
  static struct msghdr request_message_header;
  static struct iovec request_iov2[2];
  static unsigned char request_header_buffer[28];
  static unsigned char request_data_buffer[28];
  struct my_msg_packed_data *usr_app_msg_data = (struct my_msg_packed_data *) &(request_data_buffer[1]);

#ifdef CONFIRM_WRITE
  static struct msghdr reply_message_header;
  static struct iovec reply_iov2[2];
  static unsigned char reply_header_buffer[20];
  static unsigned char reply_data_buffer[256];
#endif

  int socket_fd=-1;
  long serial_number=200;
  int sub_division_number=0;
  size_t sendmsg_ret=0;
  size_t message_size_on_socket=0;

#ifdef CONFIRM_WRITE
  long serial_number_returned=0;
  size_t recvmsg_ret=0;
  int status=-1;
  int was_read = 0;
#endif

  if(argc > 1)
    {
      serial_number=atoi(argv[1]);
    }

  /* memsets  is just being paranoid unless we miss something we'll overwrite it later anyway. */
  memset(&server_socket_address,0,sizeof(server_socket_address));
  memset(&request_message_header,0,sizeof(request_message_header));
  memset(&request_iov2,0,sizeof(request_iov2));
  memset(request_header_buffer,0,sizeof(request_header_buffer));
  memset(request_data_buffer,0,sizeof(request_data_buffer));

#ifdef CONFIRM_WRITE
  memset(&reply_message_header,0,sizeof(reply_message_header));
  memset(&reply_iov2,0,sizeof(reply_iov2));
  memset(reply_header_buffer,0,sizeof(reply_header_buffer));
  memset(reply_data_buffer,0,sizeof(reply_data_buffer));
#endif

  socket_fd = socket(PF_INET,SOCK_DGRAM,0);
  if(socket_fd == -1)
    {
      fprintf(stderr,"socket returned %d -- %s\n",
	      socket_fd,strerror(errno));
      exit(1);
    }

  server_socket_address.sin_family = PF_INET;
  server_socket_address.sin_port = htons(5055); /* must match value in udp_direct.nml UDP= parameter on the bufferline for udp_buf. */
  server_socket_address.sin_addr.s_addr = inet_addr("127.0.0.1");
  request_message_header.msg_name = (caddr_t) 
    &server_socket_address;
  request_message_header.msg_namelen = sizeof(struct sockaddr_in);
  request_message_header.msg_iov = request_iov2;
  request_message_header.msg_iovlen = 2;
  request_iov2[0].iov_base = request_header_buffer;
  request_iov2[0].iov_len = 28;  

  /*
   * the size that we will need depends on the encoding type as well as the application message.
   * This assumes packed encoding type.
   */
  message_size_on_socket = sizeof(struct my_msg_packed_data)+1;

  request_iov2[1].iov_base = request_data_buffer;
  request_iov2[1].iov_len = message_size_on_socket;  
  
#define REMOTE_CMS_WRITE_REQUEST_TYPE 2
  /* see enum REMOTE_CMS_REQUEST_TYPE in src/cms/rem_msg.hh for other possible values. */
  ((long *)request_header_buffer)[0] = htonl(REMOTE_CMS_WRITE_REQUEST_TYPE);

  /* buffer number of 11 must match buf# field of udp_direct.nml on the buffer line for udp_buf */
  ((long *)request_header_buffer)[1] = htonl(11); 

  /* the serial number should be incremented each time */
  ((long *)request_header_buffer)[2] = htonl(serial_number); 

#define CMS_WRITE_ACCESS 4
  /* see enum CMS_INTERNAL_ACCESS_TYPE in src/cms/cms_types.hh for other possible values. */
  ((long *)request_header_buffer)[3] = htonl(CMS_WRITE_ACCESS);

  /* the number of bytes we will send after the header */
  ((long *)request_header_buffer)[4] = htonl(message_size_on_socket);

  /* the subdivision number is only non zero if subdivisions are being used, in which case it is passed  */
  /* as an argument to read_subdivision(), or write_subdivision() */
  ((long *)request_header_buffer)[6] = htonl(sub_division_number);

  /* Tell the server that we are sending in little-endian with 32 bit longs. */
  /* endian type may be 
     'L' -- 32 bit longs little-endian, 
     'B' -- 32 bit longs  
     'l' -- 64 bit longs little-endian, 
     'b' -- 64 bit longs big-endian, 
  */
  request_data_buffer[0] = 'L';
  usr_app_msg_data = (struct my_msg_packed_data *) &(request_data_buffer[1]);
  usr_app_msg_data->type = 2002; // This must mach MYMSG_TYPE defined in mymsg.hh
  usr_app_msg_data->size = sizeof(usr_app_msg_data);
  usr_app_msg_data->int_in_mymsg = 4044; // just some random data to show the write is working.

  sendmsg_ret = sendmsg(socket_fd,&request_message_header,0);
  if(sendmsg_ret != 28 + message_size_on_socket)
    {
      fprintf(stderr,
	      "sendmsg returned %u -- %s\n",
	      sendmsg_ret,strerror(errno));
    }

  // Normall we do not confirm writes so we are done.
  // This means this process doesn't much about how things went on the server.
#ifdef CONFIRM_WRITE
  
  reply_message_header.msg_iov = reply_iov2;
  reply_message_header.msg_iovlen = 2;
  reply_iov2[0].iov_base = reply_header_buffer;
  reply_iov2[0].iov_len = 20;  
  reply_iov2[1].iov_base = reply_data_buffer;
  reply_iov2[1].iov_len = sizeof(reply_data_buffer);  

  recvmsg_ret = recvmsg(socket_fd,&reply_message_header,0);
  printf("recvmsg_ret=%u\n",recvmsg_ret);

  serial_number_returned  = htonl(((long *) reply_header_buffer)[0]);
  if(serial_number_returned != serial_number)
    {
      printf("serial number returned does not match sent serial number perhaps we need to ignore this packet and hope the next one matches our request.\n");
      exit(1);
    }

  /* These values are taken from the enum CMS_STATUS in src/cms/cms_types.hh there are additional possible values */
#define CMS_WRITE_OK 3
  status  = htonl(((long *) reply_header_buffer)[1]);
  switch(status)
    {
    case CMS_READ_OK:
      /* this is what we were hoping for */
      break;
      
    default:
      /* something bad happened */
      fprintf(stderr,"status=%d\n",status);
      exit(1);
    }

  /* the buffers current was_read status  */
  was_read =  htonl(((long *) reply_header_buffer)[4]);
  printf("was_read=%d\n",was_read);
 
#endif       
  serial_number++;
  
  close(socket_fd);
  return 0;
}

  
  
  

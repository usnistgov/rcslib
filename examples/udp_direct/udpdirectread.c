
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

  static struct msghdr reply_message_header;
  static struct iovec reply_iov2[2];
  static unsigned char reply_header_buffer[20];
  static unsigned char reply_data_buffer[256];
  struct my_msg_packed_data *usr_app_msg_data = (struct my_msg_packed_data *) &(reply_data_buffer[1]);

  int socket_fd=-1;
  long serial_number=0;
  long serial_number_returned=0;
  int in_buffer_id=0;
  int sub_division_number=0;
  size_t sendmsg_ret=0;
  size_t recvmsg_ret=0;
  size_t message_size_on_socket=0;
  int status=-1;
  int was_read = 0;

  /* memsets  is just being paranoid unless we miss something we'll overwrite it later anyway. */
  memset(&server_socket_address,0,sizeof(server_socket_address));
  memset(&request_message_header,0,sizeof(request_message_header));
  memset(&request_iov2,0,sizeof(request_iov2));
  memset(request_header_buffer,0,sizeof(request_header_buffer));
  memset(&reply_message_header,0,sizeof(reply_message_header));
  memset(&reply_iov2,0,sizeof(reply_iov2));
  memset(reply_header_buffer,0,sizeof(reply_header_buffer));
  memset(reply_data_buffer,0,sizeof(reply_data_buffer));

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
  request_message_header.msg_iovlen = 1;
  request_iov2[0].iov_base = request_header_buffer;
  request_iov2[0].iov_len = 28;  
  
#define REMOTE_CMS_READ_REQUEST_TYPE 1
  /* see enum REMOTE_CMS_REQUEST_TYPE in src/cms/rem_msg.hh for other possible values. */
  ((long *)request_header_buffer)[0] = htonl(REMOTE_CMS_READ_REQUEST_TYPE);

  /* buffer number of 11 must match buf# field of udp_direct.nml on the buffer line for udp_buf */
  ((long *)request_header_buffer)[1] = htonl(11); 

  /* the serial number should be incremented each time */
  ((long *)request_header_buffer)[2] = htonl(serial_number); 

#define CMS_READ_ACCESS 1
  /* see enum CMS_INTERNAL_ACCESS_TYPE in src/cms/cms_types.hh for other possible values. */
  ((long *)request_header_buffer)[3] = htonl(CMS_READ_ACCESS);


  /* the in buffer id for the next request is sent with the reply, use 0 for the first request */
  ((long *)request_header_buffer)[4] = htonl(in_buffer_id);

  /* the subdivision number is only non zero if subdivisions are being used, in which case it is passed  */
  /* as an argument to read_subdivision(), or write_subdivision() */
  ((long *)request_header_buffer)[6] = htonl(sub_division_number);

  sendmsg_ret = sendmsg(socket_fd,&request_message_header,0);
  if(sendmsg_ret != 28)
    {
      fprintf(stderr,"sendmsg returned %u -- %s\n",
	      sendmsg_ret,strerror(errno));
    }

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
#define CMS_READ_OLD 1
#define CMS_READ_OK 2

  status  = htonl(((long *) reply_header_buffer)[1]);
  switch(status)
    {
    case CMS_READ_OLD:
      printf("old data");
      /* The comms are ok but the buffer has never been written to or is the same as the last read we did. */
	return 0;
    
    case CMS_READ_OK:
      /* we should have data; */
      break;
      
    default:
      /* something bad happened */
      fprintf(stderr,"status=%d\n",status);
      exit(1);
    }

  message_size_on_socket = (size_t) htonl(((long *) reply_header_buffer)[2]);
  /* this should match the number of bytes in the packet after the header */
  printf("message_size_on_socket=%u\n",message_size_on_socket);

  /* an ID that we would add to subsequent read requests. */
  in_buffer_id =  htonl(((long *) reply_header_buffer)[3]);
  printf("in_buffer_id=%d\n",in_buffer_id);

  /* the buffers current was_read status  */
  was_read =  htonl(((long *) reply_header_buffer)[4]);
  printf("was_read=%d\n",was_read);
 
  /* The format of the data in the buffer depends on the encoding type ie disp, xml, xdr, or packed. */
  /* This code assumes packed is on the buffer line in udp_direct.nml */
  char endian_type =  (char) reply_data_buffer[0];
  printf("endian_type=%c\n",endian_type);
  /* endian type may be 
     'L' -- 32 bit longs little-endian, 
     'B' -- 32 bit longs  
     'l' -- 64 bit longs little-endian, 
     'b' -- 64 bit longs big-endian, 

  */
  
  /* This code assumes the endian_type matches our system */
  /* Otherwise we would have to do a bunch of byte swapping and or moves for diffent sizes. */
  /* It also only works if there are no dynamic or unbounded length arrays or we would have to 
     move any bytes after those arrays based on the current length */
  usr_app_msg_data = (struct my_msg_packed_data *) &(reply_data_buffer[1]);

  printf("nml message type =%ld\n",usr_app_msg_data->type); 
  printf("nml message size =%u\n",usr_app_msg_data->size); 
  printf("nml message int_in_mymsg =%d\n",usr_app_msg_data->int_in_mymsg); 
      
  serial_number++;
  
  close(socket_fd);
  return 0;
}

  
  
  

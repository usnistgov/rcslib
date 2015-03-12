
This example serves as a brief documentation of the
UDP protocol as used by NML.

 It consists of two normal C++ NML applications that
read and write (mynmlread/mynmlwrite)  a buffer and two C applications that read and write to the same buffer without linking in or including any of the 
RCS or NML header files or libraries.

The C applications have only been tested on x86 Linux.

Messages from the client to the server are called requests. Messages from the 
server to the client are called replys.

Each message contains a header. (28 bytes for requests and 20 bytes for replies.)
Some requests and some replys contain a variable amount of data after the header. The header contains the size of the data section.

The data portion of the message depends on the encoding type ( xdr,packed, xml, disp, etc) while the header is always a list of 32 bit integers in network byte order.

The request header is  28 bytes or 7 32 bit integers.

request_type
buffer_number
serial_number
internal_access_type
in_buffer_id
data_size
subdivion_number

The reply_header is 20 bytes or 5 32 bit integers

serial_number
status
data_size
in_buffer_id
was_read


The makefile needs to know the location of the rcslib, which can be set from 
the command line with something like

make RCSLIB_DIR=/usr/local/rcslib/


To run :

start mynmlsvr in the background with

export LD_LIBRARY_PATH=${RCSLIB_DIR)/lib:${LD_LIBRARY_PATH}

./mynmlsvr &

Then in any particular order run  ./mynmlwrite ./mynmlread, ./udpdirectread, ./udpdirectwrite 

The reads should print information stored in the buffer by the writes.







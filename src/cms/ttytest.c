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



// Fun with command line arguments.
#define __GNU_SOURCE
#include <getopt.h>		// getopt_long()


static char __attribute__ ((unused)) ident[] =
  "$Id: ttytest.c 1944 2012-09-17 18:19:34Z shackle $";


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <termios.h>
#include <sys/ioctl.h>
#include "ttyintf.h"
#include <ctype.h>
#include <signal.h>
#include <errno.h>
#include <sched.h>

#include "_timer.h"

#ifdef ENABLE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
char *readline_ret=0;

#endif

int convert_rcvd_cr_to_nl = 0;
int cycles_to_skip = 0;
struct rcs_serial_port_setting settings;
char *ttyDevName;
int verbosity = 0;
char *endline_str = 0;
int endline_str_len = 0;
int local_echo = 0;
int canonical_mode = 0;
int readline_mode = 0;
int priority=0;

static int sigint_recieved = 0;

static void
sigint_handler (int sig)
{
  if(sig) {
    sigint_recieved = 1;
  }
}

static void
printCmdLineHelp (const char *progname)
{
  fprintf (stderr, "%s: \n",progname);
  fprintf (stderr, "Usage: [Options] ttydevice \n");
  fprintf (stderr, "Options:\n");
  fprintf (stderr,
	   "\t-h             \t--help                  \tPrint this help message.\n");
  fprintf (stderr,
	   "\t-V             \t--version               \tPrint version info.\n");
  fprintf (stderr,
	   "\t-v             \t--verbose               \tMore verbose output.\n");
  fprintf (stderr,
	   "\t-c             \t--canonical             \tRun in canonical mode.\n");
  fprintf (stderr,
	   "\t-R             \t--cr_to_nl              \tConvert carraige returns recieved on the serial port to new lines.\n");
  fprintf (stderr,
	   "\t-E             \t--echo                  \tEcho characters locally.\n");
  fprintf (stderr,
	   "\t-b [baudrate]  \t--baudrate [baudrate]   \tSet baudrate(default=9600)\n");
  fprintf (stderr,
	   "\t-p (e/o/n)     \t--parity  (e/o/n)       \tSet parity to even odd or none(default=n)\n");
  fprintf (stderr,
	   "\t-P #     \t--priority #       \tSet priority of this task, (requires root permission)\n");
  fprintf (stderr,
	   "\t-d [dbits]     \t--data   [dbits]        \tSet the number of data bits.\n");
  fprintf (stderr,
	   "\t-s [sbits]     \t--stop   [sbits]        \tSet the number of stop bits.\n");
  fprintf (stderr,
	   "\t-e (r/n/rn/nr) \t--endline (s/r/n/rn/nr) \tSet the string new-line character is replaced with on output. r = \\r n= \\n s=<space> \n");
  fprintf (stderr, "\nReport bugs to shackle@nist.gov\n");
}

static void
parseCmdLineArgs (int argc, char **argv)
{
  int opt = 0;
  int i = 0;
  int option_index;
  static struct option long_options[] = {
    {"verbose", 0, 0, 'v'},
    {"version", 0, 0, 'V'},
    {"baudrate", 1, 0, 'b'},
    {"data", 1, 0, 'd'},
    {"stop", 1, 0, 's'},
    {"endlline", 1, 0, 'e'},
    {"parity", 1, 0, 'p'},
    {"priority", 1, 0, 'P'},
    {"echo", 1, 0, 'E'},
    {"canonical", 1, 0, 'c'},
    {"help", 0, 0, 'h'},
    {"cr_to_nl", 0, 0, 'R'},
    {0, 0, 0, 0}
  };
  while (!sigint_recieved)
    {

      opt = getopt_long (argc, argv, "vVb:d:s:e:p:hcEP:",
			 long_options, &option_index);
      if (opt == -1)
	break;

      switch (opt)
	{
	default:
	  fprintf (stderr, "Invalid option !!!\n");
	  printCmdLineHelp (argv[0]);
	  exit (-1);
	  break;

	case 'h':
	  printCmdLineHelp (argv[0]);
	  exit (0);
	  break;

	case 'v':
	  verbosity++;
	  break;


	case 'R':
	  convert_rcvd_cr_to_nl=1;
	  break;


	case 'V':
	  fprintf (stderr, "ttytest %s (" __DATE__ ") \n", ident);
	  exit (0);
	  break;

	case 'E':
	  local_echo = 1;
	  break;

	case 'c':
	  canonical_mode = 1;
#ifdef ENABLE_READLINE
	  readline_mode=1;
#endif
	  break;

	case 'b':
	  if (optarg)
	    {
	      settings.baud_rate = strtol (optarg, 0, 0);
	      settings.baud_rate_set = 1;
	    }
	  else
	    {
	      fprintf (stderr, "Invalid arguments!!");
	      printCmdLineHelp (argv[0]);
	      exit (-1);
	    }
	  break;


	case 's':
	  if (optarg)
	    {
	      settings.stop_bits = strtol (optarg, 0, 0);
	      settings.stop_bits_set = 1;
	    }
	  else
	    {
	      fprintf (stderr, "Invalid arguments!!");
	      printCmdLineHelp (argv[0]);
	      exit (-1);
	    }
	  break;

	case 'd':
	  if (optarg)
	    {
	      settings.data_bits = strtol (optarg, 0, 0);
	      settings.data_bits_set = 1;
	    }
	  else
	    {
	      fprintf (stderr, "Invalid arguments!!");
	      printCmdLineHelp (argv[0]);
	      exit (-1);
	    }
	  break;

	case 'e':
	  if (optarg)
	    {
	      if (!strcmp ("n", optarg))
		{
		  endline_str = "\n";
		}
	      else if (!strcmp ("r", optarg))
		{
		  endline_str = "\r";
		}
	      else if (!strcmp ("rn", optarg))
		{
		  endline_str = "\r\n";
		}
	      else if (!strcmp ("nr", optarg))
		{
		  endline_str = "\n\r";
		}
	      else if (!strcmp ("s", optarg))
		{
		  endline_str = " ";
		}
	      else
		{
		  fprintf (stderr, "Invalid arguments!!");
		  printCmdLineHelp (argv[0]);
		  exit (-1);
		}
	      endline_str_len = strlen(endline_str);
	    }
	  else
	    {
	      fprintf (stderr, "Invalid arguments!!");
	      printCmdLineHelp (argv[0]);
	      exit (-1);
	    }
	  break;

	case 'p':
	  if (optarg)
	    {
	      settings.use_parity_set = 1;
	      settings.even_parity_set = 1;
	      switch (optarg[0])
		{
		case 'e':
		case 'E':
		  settings.use_parity = 1;
		  settings.even_parity = 1;
		  break;

		case 'o':
		case '0':
		  settings.use_parity = 1;
		  settings.even_parity = 0;
		  break;

		case 'n':
		case 'N':
		  settings.use_parity = 0;
		  settings.even_parity = 0;
		  break;

		default:
		  settings.use_parity_set = 0;
		  settings.even_parity_set = 0;
		  fprintf (stderr, "Invalid arguments!!");
		  printCmdLineHelp (argv[0]);
		  exit (-1);
		}
	    }
	  else
	    {
	      fprintf (stderr, "Invalid arguments!!");
	      printCmdLineHelp (argv[0]);
	      exit (-1);
	    }
	  break;

	case 'P':
	  if (optarg)
	    {
	      priority=strtol(optarg,0,0);
	    }
	  else
	    {
	      fprintf (stderr, "Invalid arguments!!");
	      printCmdLineHelp (argv[0]);
	      exit (-1);
	    }
	  break;
	}
    }


  if (optind != argc - 1)
    {
      fprintf (stderr, "Invalid option !!! ");
      for (i = optind > 0 ? optind : 0; i < argc; i++)
	{
	  fprintf (stderr, "%s ", argv[i]);
	}
      fprintf (stderr, "\n");
      printCmdLineHelp (argv[0]);
      exit (-1);
    }
  ttyDevName = strdup (argv[optind]);
}

static const char *
convertToGraph (const char *in, char *out, int max)
{
  const char *iptr = in;
  char *optr = out;
  const char *const i_end = in + max;
  memset (out, 0, max);
  if (0 == in || 0 == out)
    {
      return 0;
    }
  while (iptr < i_end && !sigint_recieved)
    {
      char ci = *iptr;
      if (isgraph (ci) || ci == ' ')
	{
	  *optr = ci;
	  optr++;
	}
      else
	{
	  optr += sprintf (optr, "<0x%X>", ci);
	}
      iptr++;
    }
  *optr = 0;
  return out;

}

int
main (int argc, char **argv)
{
  struct termios stdin_termios;
  struct termios stdout_termios;
  struct termios handle_termios;
  struct termios orig_stdin_termios;
  struct termios orig_stdout_termios;
  struct termios orig_handle_termios;
  RCS_SERIAL_PORT_HANDLE handle;
  char buf[4096];
  char buf2[16384];
  fd_set readfds;
  fd_set exceptfds;
  int bytes_read_from_stdin = 0;
  int max_fileno;
  int bytes_read = 0;
  int i = 0;
  int bytes_sent = 0;
  int write_ret = 0;
  char *cr_ptr;
  int bytes_ready;
  int total_bytes_read_from_stdin = 0;
  int total_bytes_written_to_stdout = 0;
  int total_bytes_read_from_serial_port = 0;
  int total_bytes_written_to_serial_port = 0;
  int bytes_written;
  int t;
  double start_time=0;
  double cur_time=0;
  double last_time=0;
  double min=0;
  double max=0;
  double diff=0;
  int count=0;

  signal (SIGINT, sigint_handler);

  settings.baud_rate = 9600;
  settings.data_bits = 8;
  settings.stop_bits = 1;
  settings.use_parity = 0;
  settings.even_parity = 0;
  settings.baud_rate_set = 0;
  settings.data_bits_set = 0;
  settings.stop_bits_set = 0;
  settings.use_parity_set = 0;
  settings.even_parity_set = 0;

  parseCmdLineArgs (argc, argv);

  if (argc < 2)
    {
      fprintf (stderr,
	       "ttytest usage: DeviceName [BaudRate] [DataBits] [StopBits] [Parity(e/o/n)] [Debug(y/n)]\n");
      fprintf (stderr, "Example: ttytest /dev/ttyb 9600 8 1 n n\n");
      exit (-1);
    }
  handle = open_serial_communications_port (ttyDevName);
  if (handle <= 0)
    {
      exit (-1);
    }
  if (verbosity >= 1)
    {
      fprintf (stderr, "\nOriginal settings for serial port:\n");
      print_serial_port_configuration (handle);
      fprintf (stderr, "\n");
      fprintf (stderr, "\nOriginal settings for STDIN:\n");
      print_serial_port_configuration (STDIN_FILENO);
      fprintf (stderr, "\n");
      fprintf (stderr, "\nOriginal settings for STDOUT:\n");
      print_serial_port_configuration (STDOUT_FILENO);
      fprintf (stderr, "\n");
    }

  tcgetattr (handle, &handle_termios);
  orig_handle_termios = handle_termios;
  handle_termios.c_iflag &=
    ~(INLCR | IUCLC | IMAXBEL | ISTRIP | IGNCR | ICRNL | IXON);
  handle_termios.c_oflag &= ~(ONOCR | OLCUC | ONLRET | ONLCR | OPOST);
  handle_termios.c_lflag &=
    ~(ICANON | ISIG | ECHO | ECHOE | ECHOKE | ECHOCTL | ECHOK | IEXTEN);
  handle_termios.c_cflag &= ~(CLOCAL);

  tcsetattr (handle, TCSANOW, &handle_termios);
  if (settings.baud_rate_set ||
      settings.data_bits_set ||
      settings.stop_bits_set ||
      settings.use_parity_set || settings.even_parity_set)
    {
      set_serial_port_configuration (handle, &settings);
    }
  if (verbosity >= 1)
    {
      fprintf (stderr, "\nNew settings for serial port:\n");
      print_serial_port_configuration (handle);
      fprintf (stderr, "\n");
    }
#ifdef ENABLE_READLINE
  rl_catch_signals=0;
#endif

  if(!readline_mode)
    {
      tcgetattr (STDIN_FILENO, &stdin_termios);
      orig_stdin_termios = stdin_termios;
      tcgetattr (STDOUT_FILENO, &stdout_termios);
      orig_stdout_termios = stdout_termios;
      stdin_termios.c_iflag &=
	~(INLCR | IUCLC | IMAXBEL | ISTRIP | IGNCR | ICRNL);
      stdin_termios.c_oflag &= ~(ONOCR | OLCUC | ONLRET);
      stdin_termios.c_cflag &= ~CSIZE;
      stdin_termios.c_cflag |= CS8;
      stdin_termios.c_lflag &=
	~(ICANON | ECHO | ECHOE | ECHOKE | ECHOCTL | ECHOK | IEXTEN);
      if (local_echo)
	{
	  stdin_termios.c_iflag |= ICRNL;
	  stdin_termios.c_lflag |= ECHO | ECHOE;
	}
      if (canonical_mode)
	{
	  stdin_termios.c_lflag |= ICANON;
	}
      tcsetattr (STDIN_FILENO, TCSANOW, &stdin_termios);
      if (verbosity >= 1)
	{
	  fprintf (stderr, "\nNew settings for stdin:\n");
	  print_serial_port_configuration (STDIN_FILENO);
	  fprintf (stderr, "\n");
	}
      stdout_termios.c_iflag &=
	~(INLCR | IUCLC | IMAXBEL | ISTRIP | IGNCR | ICRNL);
      stdout_termios.c_oflag &= ~(ONOCR | OLCUC | ONLRET);
      stdout_termios.c_cflag &= ~CSIZE;
      stdout_termios.c_cflag |= CS8;
      stdout_termios.c_lflag &=
	~(ICANON | ECHO | ECHOE | ECHOKE | ECHOCTL | ECHOK | IEXTEN);
      if (local_echo)
	{
	  stdout_termios.c_iflag |= ICRNL;
	  stdout_termios.c_lflag |= ECHO | ECHOE;
	}
      if (canonical_mode)
	{
	  stdout_termios.c_lflag |= ICANON;
	}
      tcsetattr (STDOUT_FILENO, TCSANOW, &stdout_termios);
      if (verbosity >= 1)
	{
	  fprintf (stderr, "\nNew settings for stdout:\n");
	  print_serial_port_configuration (STDOUT_FILENO);
	  fprintf (stderr, "\n");
	}
    }
  memset (buf, 0, sizeof (buf));
  max_fileno = STDIN_FILENO > handle ? STDIN_FILENO : handle;
         FD_ZERO (&readfds);
	 max_fileno++;
  FD_ZERO (&readfds);

  start_time = etime();
  min=1e99;
  max=-1e99;

  if(priority > 0)
    {
      struct sched_param sparm;
      sparm.sched_priority = priority;
      if(-1 == sched_setscheduler(0,SCHED_FIFO,&sparm))
	{
	  fprintf(stderr,"sched_setscheduler(0,SCHED_FIFO,&sparm) failed: %s\n",
		  strerror(errno));
	}
    }

  while (!sigint_recieved)
    {
      int select_ret;
      if(cycles_to_skip <= 0)
	{
	  FD_ZERO (&readfds);
	  FD_SET (STDIN_FILENO, &readfds);
	  FD_SET (handle, &readfds);
	  FD_ZERO (&exceptfds);
	  FD_SET (STDIN_FILENO, &exceptfds);
	  FD_SET (handle, &exceptfds);
	  count++;
	  cur_time=etime();
	  if(count > 1)
	    {
	      diff = cur_time-last_time;
	      if(min > diff)
		{
		  min=diff;
		}
	      if(max < diff)
		{
		  max=diff;
		}
	    }
	  last_time=cur_time;
	  if(verbosity >0)
	    {
	      fprintf(stderr,"\ntime=%f,count=%d,min=%f,max=%f,avg=%f\n",
		      (cur_time-start_time),
		      count, min,max,(cur_time-start_time)/count);
	    }
	  select_ret = select (max_fileno, &readfds, 0, &exceptfds, 0);
	  if (sigint_recieved)
	    {
	      break;
	    }
	  if (select_ret < 0)
	    {
	      perror ("select");
	      sigint_recieved = 1;
	      break;
	    }
	}
      if(cycles_to_skip > 0)
	{
	  if(verbosity > 0)
	    {
	      printf("cycles_to_skip=%d\n",cycles_to_skip);
	    }
	  //usleep(10000);
	  cycles_to_skip--;
	}
      memset (buf, 0, sizeof (buf));
      memset (buf2, 0, sizeof (buf2));
      if (FD_ISSET (handle, &exceptfds))
	{
	  fprintf (stderr, "Exception in select from serial port handle.\n");
	}
      if (FD_ISSET (STDIN_FILENO, &exceptfds))
	{
	  fprintf (stderr, "Exception in select from stdin.\n");
	}
      if (cycles_to_skip > 0  || FD_ISSET (handle, &readfds))
	{
	  ioctl (handle, FIONREAD, &bytes_ready);
	  if (bytes_ready > ((int) sizeof (buf)))
	    {
	      bytes_ready = sizeof (buf);
	    }
	  if (verbosity > 0)
	    {
	      fprintf (stderr, "time=%f, bytes_ready=%d\n", etime(), bytes_ready);
	    }
	  bytes_read =
	    read_serial_communications_port (handle, buf, bytes_ready);
	  if (bytes_read < 0 && errno != EAGAIN)
	    {
	      perror ("read_serial_communications_port");
	      sigint_recieved = 1;
	      break;
	    }
	  if (bytes_read > 0)
	    {
	      total_bytes_read_from_serial_port += bytes_read;
	    }
	  if (bytes_read != 0)
	    {
	      if (verbosity >= 1)
		{
		  printf ("\ntime=%f, bytes_read = %d\n", etime(),bytes_read);
		}
	      if (verbosity >= 1)
		{
		  fprintf (stderr, "\n<recvd>%s</recvd>\n",
			   convertToGraph (buf, buf2, bytes_read));
		}
	      if(convert_rcvd_cr_to_nl)
		{
		  cr_ptr = strchr (buf, '\r');
		  while (cr_ptr >= buf && cr_ptr < buf + sizeof (buf))
		    {
		      int string_left;
		      if (cr_ptr > buf)
			{
			  if (*(cr_ptr - 1) == '\n')
			    {
			      string_left = strlen (cr_ptr);
			      memmove (cr_ptr - 1, cr_ptr, string_left);
			      *cr_ptr = '\n';
			      *(cr_ptr + string_left - 1) = 0;
			      cr_ptr = strchr (cr_ptr, '\r');
			      continue;
			    }
			}
		      if (*(cr_ptr + 1) == '\n')
			{
			  string_left = strlen (cr_ptr + 1);
			  memmove (cr_ptr, cr_ptr + 1, string_left);
			  *cr_ptr = '\n';
			  *(cr_ptr + string_left - 1) = 0;
			  cr_ptr = strchr (cr_ptr, '\r');
			  continue;
			}
		      *cr_ptr = '\n';
		      cr_ptr = strchr (cr_ptr, '\r');
		    }
		}
	      bytes_written = 0;
	      while (bytes_written < bytes_read && !sigint_recieved)
		{
		  write_ret = write (STDOUT_FILENO, buf + bytes_written,
				     bytes_read - bytes_written);
		  if (write_ret < 0 && errno != EAGAIN)
		    {
		      perror ("write");
		      sigint_recieved = 1;
		      break;
		    }
		  else if (write_ret > 0)
		    {
		      total_bytes_written_to_stdout += write_ret;
		      bytes_written += write_ret;
		    }
		}
	    }
	}
      if (cycles_to_skip <= 0 && (readline_mode || FD_ISSET (STDIN_FILENO, &readfds)))
	{
#ifdef ENABLE_READLINE
	  if(readline_mode)
	    {
	      printf("\n");
	      readline_ret = readline(0);
	      if(verbosity > 0)
		{
		  fprintf(stderr,"readline returned %s\n",readline_ret);
		}
	      if(readline_ret)
		{
		  strncpy(buf,readline_ret,sizeof(buf));
		  if(endline_str)
		    {
		      strcat(buf,endline_str);
		    }
		  bytes_read_from_stdin = strlen(buf);
		  if(readline_ret[0] != 0)
		    {
		      add_history(readline_ret);
		    }
		  readline_ret[0]=0;
		  free(readline_ret);
		}
	      else
		{
		  bytes_read_from_stdin = 0;
		}
	    }
#endif
	  if(!readline_mode)
	    {
	      ioctl (STDIN_FILENO, FIONREAD, &bytes_ready);
	      if (bytes_ready > ((int) (sizeof (buf) - 2)))
		{
		  bytes_ready = sizeof (buf) - 2;
		}
	      if (verbosity > 0)
		{
		  fprintf (stderr, "time=%f,bytes_ready=%d\n",etime(), bytes_ready);
		}
	      bytes_read_from_stdin = read (STDIN_FILENO, buf, bytes_ready);
	    }
	  if (bytes_read_from_stdin < 0 && errno != EAGAIN)
	    {
	      perror ("read");
	      sigint_recieved = 1;
	      break;
	    }
	  else if (bytes_read_from_stdin > 0)
	    {
	      total_bytes_read_from_stdin += bytes_read_from_stdin;
	    }
	  if (bytes_read_from_stdin == 0)
	    {
	      break;
	    }
	  if(endline_str)
	    {
	      for (i = 0; i < bytes_read_from_stdin; i++)
		{
		  if (buf[i] == '\n' || buf[i] == '\r')
		    {
		      if(endline_str_len > 1 )
			{
			  memmove(buf+i+endline_str_len,buf+i,(bytes_read_from_stdin -i));
			  bytes_read_from_stdin += (endline_str_len-1);
			}
		      strcpy(buf+i,endline_str);
		      if(endline_str_len > 1 )
			{
			  i+= (endline_str_len-1);
			}
		    }
		}
	    }
	  bytes_sent = 0;
	  while (bytes_sent < bytes_read_from_stdin && !sigint_recieved)
	    {
	      write_ret = write_serial_communications_port (handle,
							    buf + bytes_sent,
							    bytes_read_from_stdin
							    - bytes_sent);
	      if (write_ret < 0 && errno != EAGAIN)
		{
		  perror ("write_serial_communications_port");
		  sigint_recieved = 1;
		  break;
		}
	      else if (write_ret > 0)
		{
		  total_bytes_written_to_serial_port += write_ret;
		  bytes_sent += write_ret;
		}
	      if (verbosity >= 1)
		{
		  fprintf (stderr, "\time=%f\n", etime());
		  fprintf (stderr, "\nbytes_sent=%d\n", bytes_sent);
		  fprintf (stderr, "\nwrite_ret=%d\n", write_ret);
		  fprintf (stderr, "\n<sent>%s</sent>\n",
			   convertToGraph (buf + bytes_sent - write_ret, buf2,
					   write_ret));
		}
	    }
	  memset (buf, 0, sizeof (buf));
	  if(readline_mode && endline_str)
	    {
	      //usleep(10000);
	      write_ret = write_serial_communications_port (handle,
							    endline_str,
							    endline_str_len);
	      if (write_ret < 0 && errno != EAGAIN)
		{
		  perror ("write_serial_communications_port");
		  sigint_recieved = 1;
		  break;
		}
	      else if (write_ret > 0)
		{
		  total_bytes_written_to_serial_port += write_ret;
		  bytes_sent += write_ret;
		}
	      if (verbosity >= 1)
		{
		  fprintf (stderr, "\time=%f\n", etime());
		  fprintf (stderr, "\nbytes_sent=%d\n", bytes_sent);
		  fprintf (stderr, "\nwrite_ret=%d\n", write_ret);
		  fprintf (stderr, "\n<sent>%s</sent>\n",
			   convertToGraph (endline_str, buf2,
					   write_ret));
		}
	      cycles_to_skip=0;//10;
	    }
	}
    }

  t = 0;
  while (!sigint_recieved && t < 100)
    {
      t++;
      ioctl (handle, FIONREAD, &bytes_ready);
      while (bytes_ready > 0)
	{
	  if (bytes_ready > ((int)sizeof (buf)))
	    {
	      bytes_ready = sizeof (buf);
	    }
	  if (verbosity > 0)
	    {
	      fprintf (stderr, "time=%f, bytes_ready=%d\n", etime(),bytes_ready);
	    }
	  bytes_read =
	    read_serial_communications_port (handle, buf, bytes_ready);
	  if (bytes_read < 0 && errno != EAGAIN)
	    {
	      perror ("read_serial_communications_port");
	      sigint_recieved = 1;
	      break;
	    }
	  if (bytes_read > 0)
	    {
	      total_bytes_read_from_serial_port += bytes_read;
	    }
	  if (bytes_read != 0)
	    {
	      if (verbosity >= 1)
		{
		  printf ("\ntime=%f, bytes_read = %d\n", etime(), bytes_read);
		}
	      for (i = 0; i < bytes_read; i++)
		{
		  if (buf[i] == '\r')
		    {
		      buf[i] = '\n';
		    }
		}

	      if (verbosity >= 1)
		{
		  fprintf (stderr, "time=%f\n", etime());
		  fprintf (stderr, "\n<recvd>%s</recvd>\n",
			   convertToGraph (buf, buf2, bytes_read));
		}
	    }
	  ioctl (handle, FIONREAD, &bytes_ready);
	}
      //usleep (10000);
    }
  tcsetattr (handle, TCSANOW, &orig_handle_termios);
  if (verbosity >= 1)
    {
      fprintf (stderr, "\nFinal serial port settings:\n");
      print_serial_port_configuration (handle);
      fprintf (stderr, "\n");
    }
  close_serial_communications_port (handle);
  if(!readline_mode)
    {
      tcsetattr (STDIN_FILENO, TCSANOW, &orig_stdin_termios);
      if (verbosity >= 1)
	{
	  fprintf (stderr, "\nFinal stdin settings:\n");
	  print_serial_port_configuration (STDIN_FILENO);
	  fprintf (stderr, "\n");
	}
      tcsetattr (STDOUT_FILENO, TCSANOW, &orig_stdout_termios);
    }
  if (verbosity >= 1)
    {
      fprintf (stderr, "\nFinal stdout settings:\n");
      print_serial_port_configuration (STDOUT_FILENO);
      fprintf (stderr, "\n");
      
      fprintf (stderr, "total_bytes_read_from_stdin=%d\n",
	       total_bytes_read_from_stdin);
      fprintf (stderr, "total_bytes_written_to_stdout=%d\n",
	       total_bytes_written_to_stdout);
      fprintf (stderr, "total_bytes_read_from_serial_port=%d\n",
	       total_bytes_read_from_serial_port);
      fprintf (stderr, "total_bytes_written_to_serial_port=%d\n",
	       total_bytes_written_to_serial_port);
    }
  return 0;
}








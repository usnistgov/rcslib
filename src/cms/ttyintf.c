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


/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if defined(ENABLE_RCS_TTY)

#if HAVE_CONFIG_H
#include "rcs_config_include.h"

#if !defined(MS_WINDOWS_API) && !defined(UNIX)
#define UNIX 1
#endif
#else
#include "ttyintf_no_config.h"
#endif

#include "ttyintf.h"
#include "rcs_prnt.hh"


struct HANDLE_INFO
{
  RCS_SERIAL_PORT_HANDLE handle;
  char name[256];
  int refcount;
};

#ifdef __cplusplus
RCS_LINKED_LIST *handles_list;

HANDLE_INFO *
findHandle (const char *name)
{
  HANDLE_INFO *hi;
  if (NULL == handles_list)
    {
      return NULL;
    }
  hi = (HANDLE_INFO *) handles_list->get_head ();
  while (NULL != hi)
    {
      if (!strncmp (hi->name, name, 256))
	{
	  return hi;
	}
      // bug reported by xshr_001@163.com on  Aug, 12 2005 fixed here. 
      hi = (HANDLE_INFO *) handles_list->get_next ();
    }
  return NULL;
}

void
addHandle (const char *name, RCS_SERIAL_PORT_HANDLE _handle)
{
  if (NULL == handles_list)
    {
      handles_list = new RCS_LINKED_LIST ();
    }
  HANDLE_INFO hi;
  strncpy (hi.name, name, 256);
  hi.refcount = 1;
  hi.handle = _handle;
  handles_list->store_at_tail (&hi, sizeof (HANDLE_INFO), 1);
}


int
removeHandle (RCS_SERIAL_PORT_HANDLE _handle)
{
  int refcount = 0;
  HANDLE_INFO *hi;
  if (NULL == handles_list)
    {
      return -1;
    }
  hi = (HANDLE_INFO *) handles_list->get_head ();
  while (NULL != hi)
    {
      if (hi->handle == _handle)
	{
	  hi->refcount--;
	  if (hi->refcount <= 0)
	    {
	      handles_list->delete_current_node ();
	    }
	  refcount = hi->refcount;
	  break;
	}
      // bug reported by xshr_001@163.com on  Aug, 12 2005 fixed here. 
      hi = (HANDLE_INFO *) handles_list->get_next (); 
    }
  if (handles_list->list_size <= 0)
    {
      delete handles_list;
      handles_list = NULL;
    }
  return refcount;
}

#endif

static char clean_string_buf[256];

char *
clean_string (char *string, int len)
{
  int i = 0;
  char *cptr = string;
  char c = *cptr;
  char *dptr;
  if (len > 256)
    {
      len = 256;
    }
  while (i < len
	 && (c == ' ' || c == '\t' || c == '=' || c == '\r' || c == '\n')
	 && c != 0)
    {
      cptr++;
      c = *cptr;
      i++;
    }
  memset (clean_string_buf, 0, 256);
  if (c == 0 || i > len)
    {
      return clean_string_buf;
    }
  c = *cptr;
  dptr = clean_string_buf;
  while (i < len && c != 0 && c != ' ' && c != '\t' && c != '\r' && c != '\n')
    {
      *dptr = *cptr;
      cptr++;
      dptr++;
      i++;
      c = *cptr;
    }
  return clean_string_buf;
}


int
print_serial_port_configuration (RCS_SERIAL_PORT_HANDLE _handle)
{
#ifdef UNIX
  int ttyFileFd = _handle;
  const char *crdly_string = 0;
  const char *tabdly_string = 0;
  struct termios ttysettings;
  int ispeed, ospeed;
  fprintf (stderr, "tcgetattr returned %d\n",
	   tcgetattr (ttyFileFd, &ttysettings));
  fprintf (stderr, "c_iflag: 0x%4.4X \n", ttysettings.c_iflag);
  if (ttysettings.c_iflag & BRKINT)
    fprintf (stderr,
	     "BRKINT  0x%4.4X -- If IGNBRK is not set, generate SIGINT on BREAK condition, else read BREAK as character.\n",
	     BRKINT);
  if (ttysettings.c_iflag & ICRNL)
    fprintf (stderr,
	     "ICRNL   0x%4.4X --  translate  carriage  return  to  newline  on  input (unless IGNCR is set)\n",
	     ICRNL);
  if (ttysettings.c_iflag & IGNBRK)
    fprintf (stderr, "IGNBRK  0x%4.4X -- ignore BREAK condition on input\n",
	     IGNBRK);
  if (ttysettings.c_iflag & IGNCR)
    fprintf (stderr, "IGNCR   0x%4.4X -- ignore carriage return on input\n",
	     IGNCR);
  if (ttysettings.c_iflag & IMAXBEL)
    fprintf (stderr,
	     "IMAXBEL 0x%4.4X -- ring bell when input queue is full\n",
	     IMAXBEL);
  if (ttysettings.c_iflag & INLCR)
    fprintf (stderr, "INLCR   0x%4.4X -- translate NL to CR on input\n",
	     INLCR);
  if (ttysettings.c_iflag & INPCK)
    fprintf (stderr, "INPCK   0x%4.4X -- enable input parity checking\n",
	     INPCK);
  if (ttysettings.c_iflag & IUCLC)
    fprintf (stderr,
	     "IUCLC   0x%4.4X -- map uppercase characters to lowercase on input\n",
	     IUCLC);
  if (ttysettings.c_iflag & IXANY)
    fprintf (stderr,
	     "IXANY   0x%4.4X -- enable any character to restart output\n",
	     IXANY);
  if (ttysettings.c_iflag & IXOFF)
    fprintf (stderr,
	     "IXOFF   0x%4.4X -- enable XON/XOFF flow control on input\n",
	     IXOFF);
  if (ttysettings.c_iflag & IXON)
    fprintf (stderr,
	     "IXON    0x%4.4X -- enable XON/XOFF flow control on output\n",
	     IXON);
  if (ttysettings.c_iflag & PARMRK)
    fprintf (stderr,
	     "PARMRK  0x%4.4X -- if  IGNPAR  is  not  set, prefix a character with a parity error or framing error.\n   If neither  IGNPAR nor PARMRK is set, read a character with a parity error or framing error as.\n\n",
	     PARMRK);
  fprintf (stderr, "\n");
  fprintf (stderr, "c_oflag: 0x%4.4X\n", ttysettings.c_oflag);

  if (ttysettings.c_oflag & BSDLY)
    fprintf (stderr,
	     "BSDLY  0x%4.4X = %s  -- backspace delay mask.  Values are BS0 or BS1.\n",
	     BSDLY, (((ttysettings.c_oflag & BSDLY) == BS0) ? "BS0" : "BS1"));

  if (ttysettings.c_oflag & CRDLY)
    {
      switch (ttysettings.c_oflag & CRDLY)
	{
	case CR0:
	  crdly_string = "CR0";
	  break;
	case CR1:
	  crdly_string = "CR1";
	  break;
	case CR2:
	  crdly_string = "CR2";
	  break;
	case CR3:
	  crdly_string = "CR3";
	  break;
	}
      fprintf (stderr,
	       "CRDLY  0x%4.4X = %s -- carriage  return  delay mask.  Values are CR0, CR1,CR2, or CR3\n",
	       CRDLY, crdly_string);
    }
  if (ttysettings.c_oflag & FFDLY)
    fprintf (stderr,
	     "FFDLY  0x%4.4X = %s -- form feed delay mask.  Values are FF0 or FF1.",
	     FFDLY, (((ttysettings.c_oflag & FFDLY) == FF0) ? "FF0" : "FF1"));

  if (ttysettings.c_oflag & NLDLY)
    fprintf (stderr,
	     "NLDLY  0x%4.4X = %s -- new line delay mask.  Values are NL0 or NL1.",
	     NLDLY, (((ttysettings.c_oflag & NLDLY) == NL0) ? "NL0" : "NL1"));
  if (ttysettings.c_oflag & OCRNL)
    fprintf (stderr, "OCRNL  0x%4.4X -- map CR to NL on output", OCRNL);
  if (ttysettings.c_oflag & OFDEL)
    fprintf (stderr,
	     "OFDEL  0x%4.4X -- fill character is ASCII DEL.  If unset, fill  character is ASCII NUL\n",
	     OFDEL);
  if (ttysettings.c_oflag & OFILL)
    fprintf (stderr,
	     "OFILL  0x%4.4X -- send fill characters for a delay, rather than using a timed delay\n",
	     OFILL);
  if (ttysettings.c_oflag & OLCUC)
    fprintf (stderr,
	     "OLCUC  0x%4.4X -- map lowercase characters to uppercase on output\n",
	     OLCUC);
  if (ttysettings.c_oflag & ONLCR)
    fprintf (stderr, "ONLCR  0x%4.4X -- map NL to CR-NL on output\n", ONLCR);
  if (ttysettings.c_oflag & ONLRET)
    fprintf (stderr, "ONLRET 0x%4.4X -- don't output CR\n", ONLRET);
  if (ttysettings.c_oflag & ONOCR)
    fprintf (stderr, "ONOCR 0x%4.4X  -- don't output CR at column 0\n",
	     ONOCR);
#ifdef ONEOT
  if (ttysettings.c_oflag & ONOEOT)
    fprintf (stderr, "ONOEOT 0x%4.4X  -- \n", ONOEOT);
#endif
  if (ttysettings.c_oflag & OPOST)
    fprintf (stderr,
	     "OPOST  0x%4.4X --  enable implementation-defined output processing\n",
	     OPOST);
#ifdef OXTABS
  if (ttysettings.c_oflag & OXTABS)
    fprintf (stderr, "OXTABS 0x%4.4X -- ", OXTABS);
#endif

  if (ttysettings.c_oflag & TABDLY)
    {
      switch (ttysettings.c_oflag & TABDLY)
	{
	case TAB0:
	  tabdly_string = "TAB0";
	  break;
	case TAB1:
	  tabdly_string = "TAB1";
	  break;
	case TAB2:
	  tabdly_string = "TAB2";
	  break;
#ifdef XTABS
#if TAB3 != XTABS
	case TAB3:
	  tabdly_string = "TAB3";
	  break;
#endif
	case XTABS:
	  tabdly_string = "XTABS";
	  break;
#else
#ifdef TAB3
	case TAB3:
	  tabdly_string = "TAB3";
	  break;
#endif
#endif

	}
      fprintf (stderr,
	       "TABDLY 0x%4.4X = %s -- horizontal tab delay mask.  Values are TAB0,  TAB1,TAB2,  TAB3,  or  XTABS.   A value of XTABS expands tabs  to  spaces  (with  tab  stops   every   eight columns).\n",
	       TABDLY, tabdly_string);
    }
  fprintf (stderr, "\n");
  fprintf (stderr, "c_cflag: 0x%4.4X\n", ttysettings.c_cflag);
#ifdef CCTS_OFLOW
  if (ttysettings.c_cflag & CCTS_OFLOW)
    fprintf (stderr, "CCTS_OFLOW \n");
#endif

#ifdef CIGNORE
  if (ttysettings.c_cflag & CIGNORE)
    fprintf (stderr, "CIGNORE \n");
#endif

  if (ttysettings.c_cflag & CLOCAL)
    fprintf (stderr, "CLOCAL 0x%4.4X -- ignore modem control lines\n",
	     CLOCAL);
  if (ttysettings.c_cflag & CREAD)
    fprintf (stderr, "CREAD  0x%4.4X -- enable receiver.\n", CREAD);
#ifdef CRTS_IFLOW
  if (ttysettings.c_cflag & CRTS_IFLOW)
    fprintf (stderr, "CRTS_IFLOW \n");
#endif
  fprintf (stderr,
	   "CSIZE  character  size mask.  Values are CS5, CS6, CS7, or CS8\n");
  if ((ttysettings.c_cflag & CSIZE) == CS5)
    fprintf (stderr, "CS5 \n");
  if ((ttysettings.c_cflag & CSIZE) == CS6)
    fprintf (stderr, "CS6 \n");
  if ((ttysettings.c_cflag & CSIZE) == CS7)
    fprintf (stderr, "CS7 \n");
  if ((ttysettings.c_cflag & CSIZE) == CS8)
    fprintf (stderr, "CS8 \n");
  if (ttysettings.c_cflag & CSTOPB)
    fprintf (stderr,
	     "CSTOPB 0x%4.4X -- set two stop bits, rather than one.\n",
	     CSTOPB);
  if (ttysettings.c_cflag & HUPCL)
    fprintf (stderr,
	     "HUPCL  0x%4.4X -- lower modem control lines after last process closes the device (hang up).\n",
	     HUPCL);
#ifdef MDMBUF
  if (ttysettings.c_cflag & MDMBUF)
    fprintf (stderr, "MDMBUF \n");
#endif
  if (ttysettings.c_cflag & PARENB)
    fprintf (stderr,
	     "PARENB 0x%4.4X -- enable  parity  generation  on  output  and  parity checking for input.\n",
	     PARENB);
  if (ttysettings.c_cflag & PARODD)
    fprintf (stderr,
	     "PARODD 0x%4.4X -- parity for input and output is odd.\n",
	     PARODD);
#ifdef CRTSCTS
  if (ttysettings.c_cflag & CRTSCTS)
    fprintf (stderr, "CRTSCTS 0x%4.4X -- flow control.\n", CRTSCTS);
#endif
  fprintf (stderr, "\n");
  fprintf (stderr, "c_lflag: 0x%4.4X\n", ttysettings.c_lflag);
#ifdef ALTWERASE
  if (ttysettings.c_lflag & ALTWERASE)
    fprintf (stderr, "ALTWERASE \n");
#endif
  if (ttysettings.c_lflag & ECHO)
    fprintf (stderr, "ECHO     0x%4.4X -- echo input characters.\n", ECHO);
  if (ttysettings.c_lflag & ECHOCTL)
    fprintf (stderr,
	     "ECHOCTL  0x%4.4X -- if ECHO is also set, ASCII  control  signals  other than  TAB,  NL,  START,  and STOP are echoed as ^X, where X is  the  character  with  ASCII  code  0x40 greater  than  the  control  signal.   For example, character 0x08 (BS) is echoed as ^H.\n",
	     ECHOCTL);
  if (ttysettings.c_lflag & ECHOE)
    fprintf (stderr,
	     "ECHOE     0x%4.4X -- if ICANON is also set, the ERASE  character  erases the  preceding  input  character, and WERASE erases the preceding word.\n",
	     ECHOE);
  if (ttysettings.c_lflag & ECHOK)
    fprintf (stderr,
	     "ECHOK     0x%4.4X -- if ICANON is also set, the  KILL  character  erases the current line.\n",
	     ECHOK);
  if (ttysettings.c_lflag & ECHOKE)
    fprintf (stderr,
	     "ECHOKE    0x%4.4X -- if  ICANON  is  also set, KILL is echoed by erasing each character on the line, as specified  by  ECHOE and ECHOPRT\n",
	     ECHOKE);
  if (ttysettings.c_lflag & ECHONL)
    fprintf (stderr,
	     "ECHONL    0x%4.4X -- if  ICANON  is also set, echo the NL character even if ECHO is not set.\n",
	     ECHONL);
#ifdef ECHOPRT
  if (ttysettings.c_lflag & ECHOPRT)
    fprintf (stderr,
	     "ECHOPRT   0x%4.4X -- if ICANON and IECHO are also  set,  characters  are printed as they are being erased.\n",
	     ECHOPRT);
#endif
#ifdef FLUSHO
  if (ttysettings.c_lflag & FLUSHO)
    fprintf (stderr,
	     "FLUSHO     0x%4.4X -- output  is  being flushed.  This flag is toggled by typing the DISCARD character.\n",
	     FLUSHO);
#endif
  if (ttysettings.c_lflag & ICANON)
    fprintf (stderr,
	     "ICANON     0x%4.4X -- enable canonical mode.  This  enables  the  special characters  EOF,  EOL,  EOL2, ERASE, KILL, REPRINT,STATUS, and WERASE, and buffers by lines.\n",
	     ICANON);
  if (ttysettings.c_lflag & IEXTEN)
    fprintf (stderr,
	     "IEXTEN     0x%4.4X -- enable implementation-defined input processing.\n",
	     IEXTEN);
  if (ttysettings.c_lflag & ISIG)
    fprintf (stderr,
	     "ISIG      0x%4.4X --  when any of the characters  INTR,  QUIT,  SUSP,  or DSUSP are received, generate the corresponding signal.",
	     ISIG);
  if (ttysettings.c_lflag & NOFLSH)
    fprintf (stderr,
	     "NOFLSH    0x%4.4X -- disable flushing the input and output  queues  when generating  the  SIGINT  and  SIGQUIT  signals, and flushing the input queue when generating  the  SIGSUSP signal.\n",
	     NOFLSH);
#ifdef NOKERNINFO
  if (ttysettings.c_lflag & NOKERNINFO)
    fprintf (stderr, "NOKERNINFO \n");
#endif
#ifdef PENDIN
  if (ttysettings.c_lflag & PENDIN)
    fprintf (stderr,
	     "PENDIN   0x%4.4X -- all  characters  in  the  input queue are reprinted when the next character  is  read.   (bash  handles typeahead this way.)\n",
	     PENDIN);
#endif
  if (ttysettings.c_lflag & TOSTOP)
    fprintf (stderr,
	     "TOSTOP   0x%4.4X -- send  the  SIGTTOU signal to the process group of a background process which tries to write to its controlling terminal.\n",
	     TOSTOP);
  if (ttysettings.c_lflag & XCASE)
    fprintf (stderr,
	     "XCASE    0x%4.4X -- if ICANON is also set, terminal is uppercase  only. Input is converted to lowercase, except for characters preceded by \\.  On output,  uppercase  characters are preceded by \\ and lowercase characters are converted to uppercase.\n",
	     XCASE);

  fprintf (stderr, "\n");
  ispeed = cfgetispeed (&ttysettings);
  fprintf (stderr, "cfgetispeed = %d (0x%4.4X)\n", ispeed, ispeed);
  switch (ispeed)
    {
#ifdef B50
    case B50:
      fprintf (stderr, "B50\n");
      break;
#endif

#ifdef B75
    case B75:
      fprintf (stderr, "B75\n");
      break;
#endif

#ifdef B110
    case B110:
      fprintf (stderr, "B110\n");
      break;
#endif

#ifdef B150
    case B150:
      fprintf (stderr, "B150\n");
      break;
#endif

#ifdef B200
    case B200:
      fprintf (stderr, "B200\n");
      break;
#endif

#ifdef B300
    case B300:
      fprintf (stderr, "B300\n");
      break;
#endif

#ifdef B600
    case B600:
      fprintf (stderr, "B600\n");
      break;
#endif

#ifdef B1200
    case B1200:
      fprintf (stderr, "B1200\n");
      break;
#endif

#ifdef B1800
    case B1800:
      fprintf (stderr, "B1800\n");
      break;
#endif

#ifdef B2400
    case B2400:
      fprintf (stderr, "B2400\n");
      break;
#endif

#ifdef B4800
    case B4800:
      fprintf (stderr, "B4800\n");
      break;
#endif

#ifdef B9600
    case B9600:
      fprintf (stderr, "B9600\n");
      break;
#endif


#ifdef B19200
    case B19200:
      fprintf (stderr, "B19200\n");
      break;
#endif


#ifdef B38400
    case B38400:
      fprintf (stderr, "B38400\n");
      break;
#endif


#ifdef B57600
    case B57600:
      fprintf (stderr, "B57600\n");
      break;
#endif


#ifdef B76800
    case B76800:
      fprintf (stderr, "B76800\n");
      break;
#endif


#ifdef B115200
    case B115200:
      fprintf (stderr, "B115200\n");
      break;
#endif


#ifdef B153600
    case B153600:
      fprintf (stderr, "B153600\n");
      break;
#endif


#ifdef B230400
    case B230400:
      fprintf (stderr, "B230400\n");
      break;
#endif


#ifdef B307200
    case B307200:
      fprintf (stderr, "B307200\n");
      break;
#endif

#ifdef B460800
    case B460800:
      fprintf (stderr, "B460800\n");
      break;
#endif

    default:
      fprintf (stderr, "Invalid baud rate for serial port. (%d)\n", ispeed);
    }
  ospeed = cfgetospeed (&ttysettings);
  fprintf (stderr, "cfgetospeed = %d (0x%4.4X)\n", ospeed, ospeed);
  switch (ospeed)
    {
#ifdef B50
    case B50:
      fprintf (stderr, "B50\n");
      break;
#endif

#ifdef B75
    case B75:
      fprintf (stderr, "B75\n");
      break;
#endif

#ifdef B110
    case B110:
      fprintf (stderr, "B110\n");
      break;
#endif

#ifdef B150
    case B150:
      fprintf (stderr, "B150\n");
      break;
#endif

#ifdef B200
    case B200:
      fprintf (stderr, "B200\n");
      break;
#endif

#ifdef B300
    case B300:
      fprintf (stderr, "B300\n");
      break;
#endif

#ifdef B600
    case B600:
      fprintf (stderr, "B600\n");
      break;
#endif

#ifdef B1200
    case B1200:
      fprintf (stderr, "B1200\n");
      break;
#endif

#ifdef B1800
    case B1800:
      fprintf (stderr, "B1800\n");
      break;
#endif

#ifdef B2400
    case B2400:
      fprintf (stderr, "B2400\n");
      break;
#endif

#ifdef B4800
    case B4800:
      fprintf (stderr, "B4800\n");
      break;
#endif

#ifdef B9600
    case B9600:
      fprintf (stderr, "B9600\n");
      break;
#endif


#ifdef B19200
    case B19200:
      fprintf (stderr, "B19200\n");
      break;
#endif


#ifdef B38400
    case B38400:
      fprintf (stderr, "B38400\n");
      break;
#endif


#ifdef B57600
    case B57600:
      fprintf (stderr, "B57600\n");
      break;
#endif


#ifdef B76800
    case B76800:
      fprintf (stderr, "B76800\n");
      break;
#endif


#ifdef B115200
    case B115200:
      fprintf (stderr, "B115200\n");
      break;
#endif


#ifdef B153600
    case B153600:
      fprintf (stderr, "B153600\n");
      break;
#endif


#ifdef B230400
    case B230400:
      fprintf (stderr, "B230400\n");
      break;
#endif


#ifdef B307200
    case B307200:
      fprintf (stderr, "B307200\n");
      break;
#endif

#ifdef B460800
    case B460800:
      fprintf (stderr, "B460800\n");
      break;
#endif

    default:
      fprintf (stderr, "Invalid baud rate for serial port. (%d)\n", ospeed);
    }


#endif

#if MS_WINDOWS_API
  DCB dcb;
  if (!GetCommState (_handle, &dcb))
    {
      return -1;
    }
  fprintf (stderr, "DCBlength=%d\n", dcb.DCBlength);	/* sizeof(DCB) */
  fprintf (stderr, "BaudRate=%d\n", dcb.BaudRate);	/* current baud rate */
  fprintf (stderr, "fBinary=%d\n", dcb.fBinary);	/* binary mode, no EOF check */
  fprintf (stderr, "fParity=%d\n", dcb.fParity);	/* enable parity checking */
  fprintf (stderr, "fOutxCtsFlow=%d\n", dcb.fOutxCtsFlow);	/* CTS output flow control */
  fprintf (stderr, "fOutxDsrFlow=%d\n", dcb.fOutxDsrFlow);	/* DSR output flow control */
  fprintf (stderr, "fDtrControl=%d\n", dcb.fDsrSensitivity);	/* DTR flow control type */
  fprintf (stderr, "fDsrSensitivity=%d\n", dcb.fDsrSensitivity);	/* DSR sensitivity */
  fprintf (stderr, "fTXContinueOnXoff=%d\n", dcb.fTXContinueOnXoff);	/* XOFF continues Tx */
  fprintf (stderr, "fOutX=%d\n", dcb.fOutX);	/* XON/XOFF out flow control */
  fprintf (stderr, "fInX=%d\n", dcb.fInX);	/* XON/XOFF in flow control */
  fprintf (stderr, "fErrorChar=%d\n", dcb.fErrorChar);	/* enable error replacement */
  fprintf (stderr, "fNull=%d\n", dcb.fNull);	/* enable null stripping */
  fprintf (stderr, "fRtsControl=%d\n", dcb.fRtsControl);	/* RTS flow control */
  fprintf (stderr, "fAbortOnError=%d\n", dcb.fAbortOnError);	/* abort reads/writes on error */
  fprintf (stderr, "XonLim=%d\n", dcb.XonLim);	/* transmit XON threshold */
  fprintf (stderr, "XoffLim=%d\n", dcb.XoffLim);	/* transmit XOFF threshold */
  fprintf (stderr, "ByteSize=%d\n", dcb.ByteSize);	/* number of bits/byte, 4-8 */
  fprintf (stderr, "Parity=%d\n", dcb.Parity);	/* 0-4=no,odd,even,mark,space */
  fprintf (stderr, "StopBits=%d\n", dcb.StopBits);	/* 0,1,2 = 1, 1.5, 2 */
  fprintf (stderr, "XonChar=%x %c\n", dcb.XoffChar, dcb.XoffChar);	/* Tx and Rx XON character */
  fprintf (stderr, "XoffChar=%x %c\n", dcb.XoffChar, dcb.XoffChar);	/* Tx and Rx XOFF character */
  fprintf (stderr, "ErrorChar=%x %c\n", dcb.ErrorChar, dcb.ErrorChar);	/* error replacement character */
  fprintf (stderr, "EofChar=%x %c\n", dcb.EofChar, dcb.EofChar);	/* end of input character */
  fprintf (stderr, "EvtChar=%x %c\n", dcb.EvtChar, dcb.EvtChar);	/* received event character */
#endif
  return 0;
}

int
set_serial_port_configuration (RCS_SERIAL_PORT_HANDLE _handle,
			       struct rcs_serial_port_setting *_settings)
{
#ifdef UNIX
  struct termios ttysettings;
  int cfsetting;
  int ttyFileFd = _handle;
  if (tcgetattr (ttyFileFd, &ttysettings) < 0)
    {
      return -1;
    }
  ttysettings.c_cflag |= CLOCAL | CREAD;
  if (NULL != _settings)
    {
      if (_settings->data_bits_set)
	{
	  ttysettings.c_cflag &= ~CSIZE;
	  if (_settings->data_bits == 8)
	    {
	      ttysettings.c_cflag |= CS8;
	    }
	  else if (_settings->data_bits == 7)
	    {
	      ttysettings.c_cflag |= CS7;
	    }
	}
      if (_settings->use_parity_set)
	{
	  ttysettings.c_cflag &= ~(PARENB | PARODD);
	  if (_settings->use_parity)
	    {
	      ttysettings.c_cflag |= PARENB;
	      if (_settings->even_parity_set)
		{
		  if (!_settings->even_parity)
		    {
		      ttysettings.c_cflag |= PARODD;
		    }
		}
	    }
	}
      if (_settings->stop_bits_set)
	{
	  if (_settings->stop_bits == 2)
	    {
	      ttysettings.c_cflag |= CSTOPB;
	    }
	  else
	    {
	      ttysettings.c_cflag &= ~CSTOPB;
	    }
	}
    }
  ttysettings.c_lflag = 0;
  ttysettings.c_cc[VMIN] = 0;
  ttysettings.c_cc[VTIME] = 0;
  cfsetting = B9600;
  if (NULL != _settings)
    {
      if (_settings->baud_rate_set)
	{
	  switch (_settings->baud_rate)
	    {
#ifdef B50
	    case 50:
	      cfsetting = B50;
	      break;
#endif

#ifdef B75
	    case 75:
	      cfsetting = B75;
	      break;
#endif

#ifdef B110
	    case 110:
	      cfsetting = B110;
	      break;
#endif

#ifdef B150
	    case 150:
	      cfsetting = B150;
	      break;
#endif

#ifdef B200
	    case 200:
	      cfsetting = B200;
	      break;
#endif

#ifdef B300
	    case 300:
	      cfsetting = B300;
	      break;
#endif

#ifdef B600
	    case 600:
	      cfsetting = B600;
	      break;
#endif

#ifdef B1200
	    case 1200:
	      cfsetting = B1200;
	      break;
#endif

#ifdef B1800
	    case 1800:
	      cfsetting = B1800;
	      break;
#endif

#ifdef B2400
	    case 2400:
	      cfsetting = B2400;
	      break;
#endif

#ifdef B4800
	    case 4800:
	      cfsetting = B4800;
	      break;
#endif

#ifdef B9600
	    case 9600:
	      cfsetting = B9600;
	      break;
#endif


#ifdef B19200
	    case 19200:
	      cfsetting = B19200;
	      break;
#endif


#ifdef B38400
	    case 38400:
	      cfsetting = B38400;
	      break;
#endif


#ifdef B57600
	    case 57600:
	      cfsetting = B57600;
	      break;
#endif


#ifdef B76800
	    case 76800:
	      cfsetting = B76800;
	      break;
#endif


#ifdef B115200
	    case 115200:
	      cfsetting = B115200;
	      break;
#endif


#ifdef B153600
	    case 153600:
	      cfsetting = B153600;
	      break;
#endif


#ifdef B230400
	    case 230400:
	      cfsetting = B230400;
	      break;
#endif


#ifdef B307200
	    case 307200:
	      cfsetting = B307200;
	      break;
#endif

#ifdef B460800
	    case 460800:
	      cfsetting = B460800;
	      break;
#endif

	    default:
	      fprintf (stderr, "Invalid baud rate for serial port. (%d)\n",
		       _settings->baud_rate);
	    }
	  if (cfsetospeed (&ttysettings, cfsetting) < 0)
	    {
	      fprintf (stderr,
		       "Can't set baud rate. cfsetospeed(*,%d): errno = %d %s\n",
		       cfsetting, errno, strerror (errno));
	      return -1;
	    }
	  if (cfsetispeed (&ttysettings, cfsetting) < 0)
	    {
	      fprintf (stderr,
		       "Can't set baud rate. cfsetospeed(*,%d): errno = %d %s\n",
		       cfsetting, errno, strerror (errno));
	      return -1;
	    }
	}
    }
  if (tcsetattr (ttyFileFd, TCSANOW, &ttysettings) < 0)
    {
      fprintf (stderr,
	       "Can't set serial port attributes. tcsetattr: errno = %d %s\n",
	       errno, strerror (errno));
      return -1;
    }
#endif
#if MS_WINDOWS_API
  DCB dcb;
  if (!GetCommState (_handle, &dcb))
    {
      rcs_print_sys_error (GETLASTERROR_ERROR_SOURCE,
			   "Can't GetCommState for a serial communications port.\n");
      return -1;
    }
  /* initialize TTY info structure */
  dcb.BaudRate = CBR_9600;
  if (NULL != _settings)
    {
      dcb.BaudRate = _settings->baud_rate;
    }
  dcb.fBinary = FALSE;
  dcb.fParity = NOPARITY;
  dcb.fOutxCtsFlow = FALSE;
  dcb.fOutxDsrFlow = FALSE;
  dcb.fDtrControl = DTR_CONTROL_DISABLE;
  dcb.fDsrSensitivity = FALSE;
  dcb.fTXContinueOnXoff = TRUE;
  dcb.fOutX = FALSE;
  dcb.fInX = FALSE;
  dcb.fErrorChar = FALSE;
  dcb.fNull = FALSE;
  dcb.fRtsControl = RTS_CONTROL_DISABLE;
  dcb.ByteSize = 8;
  dcb.Parity = NOPARITY;
  dcb.StopBits = ONESTOPBIT;
  if (NULL != _settings)
    {
      if (_settings->data_bits_set)
	{
	  dcb.ByteSize = _settings->data_bits;
	}
      if (_settings->use_parity_set)
	{
	  if (!_settings->use_parity)
	    {
	      dcb.Parity = NOPARITY;
	    }
	  else
	    {
	      if (_settings->even_parity_set)
		{
		  if (!_settings->even_parity)
		    {
		      dcb.Parity = ODDPARITY;
		    }
		  else
		    {
		      dcb.Parity = EVENPARITY;
		    }
		}
	      dcb.fParity = TRUE;
	    }
	}
      if (_settings->stop_bits_set)
	{
	  if (_settings->stop_bits == 2)
	    {
	      dcb.StopBits = TWOSTOPBITS;
	    }
	}
    }
  if (!SetCommState (_handle, &dcb))
    {
      rcs_print_sys_error (GETLASTERROR_ERROR_SOURCE,
			   "Can't SetCommState for a serial communications port.\n");
      return -1;
    }
#endif
  return 0;
}


RCS_SERIAL_PORT_HANDLE
open_serial_communications_port (const char *name)
{
  RCS_SERIAL_PORT_HANDLE rcs_handle = (RCS_SERIAL_PORT_HANDLE) -1;
#ifdef __cplusplus
  HANDLE_INFO *hi = findHandle (name);
  if (NULL != hi)
    {
      hi->refcount++;
      return hi->handle;
    }
#endif

#ifdef UNIX
  int ttyFileFd = open (name, O_RDWR | O_NDELAY);
  if (ttyFileFd < 0)
    {
      fprintf (stderr,
	       "Can't open %s as a serial communications port. : %d %s\n",
	       name, errno, strerror (errno));
      return ((RCS_SERIAL_PORT_HANDLE) - 1);
    }
  rcs_handle = ttyFileFd;
#endif
#if MS_WINDOWS_API
  HANDLE handle =
    CreateFile (name, GENERIC_READ | GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
		FILE_FLAG_NO_BUFFERING, NULL);
  if (handle == INVALID_HANDLE_VALUE)
    {
      rcs_print_sys_error (GETLASTERROR_ERROR_SOURCE,
			   "Can't open %s as a serial communications port.\n",
			   name);
      return ((RCS_SERIAL_PORT_HANDLE) - 1);
    }
  rcs_handle = handle;
#endif
#ifdef __cplusplus
  if (((int) rcs_handle) > 0)
    {
      addHandle (name, rcs_handle);
    }
#endif
  return rcs_handle;
}


int
close_serial_communications_port (RCS_SERIAL_PORT_HANDLE _handle)
{
#ifdef __cplusplus
  if (removeHandle (_handle))
    {
      return 0;
    }
#endif
#ifdef UNIX
  return close (_handle);
#endif
#if MS_WINDOWS_API
  return CloseHandle (_handle);
#endif
  return 0;
}


int
read_serial_communications_port (RCS_SERIAL_PORT_HANDLE _handle, char *buf,
				 long maxlen)
{
#ifdef UNIX
  int ttyFileFd = _handle;
  int bytes_read = read (ttyFileFd, buf, maxlen);
  return bytes_read;
#endif
#if MS_WINDOWS_API
  unsigned long bytes_read = 0;
  if (!ReadFile (_handle, buf, maxlen, &bytes_read, NULL))
    {
      rcs_print_sys_error (GETLASTERROR_ERROR_SOURCE,
			   "Can't read from a serial communications port.\n");
      return -1;
    }
  return bytes_read;
#endif
  return 0;
}


int
readn_serial_communications_port (RCS_SERIAL_PORT_HANDLE _handle, char *buf,
				  long len)
{
  int bytes_read = 0;
  while (bytes_read < len)
    {
      int rval = read_serial_communications_port (_handle, buf + bytes_read,
						  len - bytes_read);
      if (rval < 0)
	{
	  return rval;
	}
      bytes_read += rval;
    }
  return bytes_read;
}

int
write_serial_communications_port (RCS_SERIAL_PORT_HANDLE _handle, char *buf,
				  long maxlen)
{
#ifdef UNIX
  int ttyFileFd = _handle;
  return write (ttyFileFd, buf, maxlen);
#endif
#if MS_WINDOWS_API
  unsigned long bytes_written = 0;
  if (!WriteFile (_handle, buf, maxlen, &bytes_written, NULL))
    {
      rcs_print_sys_error (GETLASTERROR_ERROR_SOURCE,
			   "Can't write to a serial communications port.\n");
      return -1;
    }
  return bytes_written;
#endif
  return -1;
}

/*  defined(ENABLE_RCS_TTY) */
#else
#include "rcs_empty_source"
#endif

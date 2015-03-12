
/* Program Name to print at startup. */
static char program_id[] = "nmlperf compiled on " __DATE__;

#include "rcs_defs.hh"		/*  EXTERN_C_STD_HEADERS  */

#ifdef SUN
#define USE_TIMES_FUNCTION
#else
#define USE_CLOCK_FUNCTION
#endif

#ifdef  EXTERN_C_STD_HEADERS 
extern "C" {
#endif
#include <stdlib.h>		/* exit() */
#include <string.h>		/* strcmp() */
#include <signal.h>		/* SIGINT, signal */
#include <time.h>		// clock()
#ifdef USE_TIMES_FUNCTION
#include <sys/times.h>		// times()
#endif
#ifndef VXWORKS
#include <limits.h>
#endif


#ifdef  EXTERN_C_STD_HEADERS 
}
#endif

#include "rcs.hh"		/* NML,  */
#include "perftype.hh"		/* perf_type_format, */
				/* NML_PERFORMANCE_TEST_MSG */


enum DISPLAY_MODE {
 INVALID_DISPLAY_MODE,
 BYTES_PER_SEC_MODE,
 MSG_PER_SEC_MODE
 };


enum CONNECT_STATUS {
  CS_NOT_ATTEMPTED = 0,
  CS_FAILED,
  CS_SUCCEEDED
};

char CS_ARRAY[3][3] = { "NA", "F", "S"};
static unsigned char nmlperf_stop_flag = 0;

void nmlperf_stop()
{
	nmlperf_stop_flag = 1;
}


int nml_perf_read_test(NML *);
int nml_perf_write_test(NML *, int message_size);
int nml_perf_combined_test(NML *, int message_size);

int nmlperf_control_c_count = 0;
void control_c_handler(int sig)
{
  nmlperf_control_c_count++;
}

long nmlperf_max_message_count = -1;
double nmlperf_min_cycle_time = -1.0;


#if  defined(VXWORKS) || defined(WINDOWS_GUI)
extern "C" {
int nmlperf();
int nmlperf_go(int priority);
}

#ifdef VXWORKS
#include "taskLib.h"
int nmlperf_def_iterations =1000;
char  *nmlperf_def_timeout ="0.1";

int nmlperf_go(int priority)
{
  return taskSpawn("tNmlperf", priority, VX_FP_TASK, 0x4000, (FUNCPTR) nmlperf,
	   0, 0, 0, 0, 0, 0, 0, 0,0,0);
}

#endif

char *nmlperf_config_file="test.nml";
char *nmlperf_buffer_name="testbuf";
char *nmlperf_proc_name="nmlperf";
char *nmlperf_test_type="C";
long nmlperf_message_size=100;

int nmlperf()
#else
int main(int argc, char **argv)
#endif
{
  NML *nml = NULL;
#if  !defined(VXWORKS) && !defined(WINDOWS_GUI)
  char config_file[CMS_CONFIG_LINELEN];
  char buffer_name[CMS_CONFIG_LINELEN];
  char proc_name[CMS_CONFIG_LINELEN];
  char test_type[20];
  char message_size_string[20];
  unsigned long message_size;
#else
  char *config_file=nmlperf_config_file;
  char *buffer_name=nmlperf_buffer_name;
  char *proc_name=nmlperf_proc_name;
  char *test_type=nmlperf_test_type;
  long message_size=nmlperf_message_size;

#endif
  char display_mode_string[20];
  RCS_LINKED_LIST *output_list;
  char master_string[20];
  char check_match_string[20];
  char iterations_string[20];
  char increments_string[20];
  char detailed_output_string[20];
  char connection_number_string[20];
  char count_string[20];
  output_list = new RCS_LINKED_LIST;
  int buffers_tested = 0;


  set_rcs_print_destination(RCS_PRINT_TO_STDOUT);
  rcs_print("\nNML Performance Testing Program\n");
#if  !defined(VXWORKS) && !defined(WINDOWS_GUI)
  rcs_print("Configuration File? (test.nml) ");
  gets(config_file);
  if(strlen(config_file) < 2)
    {
      strcpy(config_file, "test.nml");
    }
  rcs_print("Buffer Name? (testbuf) ");
  gets(buffer_name);
  if(strlen(buffer_name) < 2)
    {
      strcpy(buffer_name, "testbuf");
    }
  rcs_print("Process Name? (nmlperf) ");
  gets(proc_name);
  if(strlen(proc_name) < 2)
    {
      strcpy(proc_name, "nmlperf");
    }
  rcs_print("Maximum Message Count (Infinite) ?");
  gets(count_string);
  if(strlen(count_string) > 2)
    {
      errno = 0;
      nmlperf_max_message_count = strtol(count_string, NULL,0);
      if(errno != 0)
	{
	  nmlperf_max_message_count = -1;
	}
    }
#endif

  nml = new NML(perf_types_format, buffer_name, proc_name, config_file);
  if(NULL == nml)
    {
      exit(-1);
    }
  if(!nml->valid())
    {
      delete nml;
      exit(-1);
    }


#if  !defined(VXWORKS) && !defined(WINDOWS_GUI)
  rcs_print("\n \tThere are 3 different tests that can be performed.\n");
  rcs_print("The Read test only reads from the buffer, unless the write test is run is also run there will be no new data available for the read.\n");
  rcs_print("The Write test only writes to the buffer.\n");
  rcs_print("The Combined test alternates reads and writes.\n");
  rcs_print("Test Type ( R = Read, W = Write, C = Combined)?\n");
  gets(test_type);
#endif
  switch(test_type[0])
  {
  case 'w':
  case 'W':
#if  !defined(VXWORKS) && !defined(WINDOWS_GUI)
    rcs_print("Message Size?");
    gets(message_size_string);
    message_size = strtol(message_size_string,NULL, 0);
#endif
    nml_perf_write_test(nml, message_size);
    break;


  case 'c':
  case 'C':
#if  !defined(VXWORKS) && !defined(WINDOWS_GUI)
    rcs_print("Message Size?");
    gets(message_size_string);
    message_size = strtol(message_size_string,NULL, 0);
#endif
    nml_perf_combined_test(nml, message_size);
    break;
    

  case 'r':
  case 'R':
    nml_perf_read_test(nml);
    break;

  default:
    rcs_print_error("Invalid test type.\n");
  }

  delete nml;
  exit(-1);
}

int nml_perf_write_test(NML *nml, int message_size)
{
#ifdef USE_TIMES_FUNCTION
  struct tms tms_buffer;
#endif

  double start_time;
  double end_time;
  int data_type = 0;

  clock_t cpu_used_start_time;
  clock_t cpu_used_end_time;
  double total_cpu_time_used;
  double total_time_used;
  double last_time;
  double max_time = 0;
  double min_time = 1E6;
  double cur_time = 0;
  double time_dif = 0;
  NML_PERFORMANCE_TEST_MSG *test_msg = NULL;
  rcs_print("NML Write Only Performance Test\n");
  set_real_test_msg_size(message_size);
  test_msg = new NML_PERFORMANCE_TEST_MSG();
  test_msg->size = message_size;
  nmlperf_control_c_count = 0;
  signal(SIGINT, control_c_handler);
  rcs_print("Press ^C to end the test.\n");
  unsigned long messages_written = 0;

#ifdef USE_CLOCK_FUNCTION
  cpu_used_start_time = clock();
#endif

#ifdef USE_TIMES_FUNCTION
  times(&tms_buffer);
  cpu_used_start_time = tms_buffer.tms_utime + tms_buffer.tms_stime ;
#endif

  cur_time = last_time = start_time = etime();
  RCS_TIMER *timer = NULL;
  if(nmlperf_min_cycle_time > 1e-6)
    {
      timer = new RCS_TIMER(nmlperf_min_cycle_time);
    }
  int errors = 0;

  while(nmlperf_control_c_count == 0)
    {
      data_type++;
      data_type = data_type%6;
      test_msg->test_type = data_type;
      test_msg->compute_array_length();
      if(nml->write(test_msg) < 0)
	{
	  errors++;
	}
      messages_written++;
      cur_time = etime();
      time_dif = cur_time - last_time;
      if(time_dif > max_time)
	{
	  max_time = time_dif;
	}
      if(time_dif < min_time)
	{
	  min_time = time_dif;
	}
      last_time = cur_time;
      if(nmlperf_max_message_count > 0)
	{
	  if(nmlperf_max_message_count < messages_written)
	    {
	      break;
	    }
	}
      if(nmlperf_min_cycle_time > 1e-6)
	{
	  timer->wait();
	}

    }
  
  end_time = etime();

#ifdef USE_CLOCK_FUNCTION
  cpu_used_end_time = clock();
#endif

#ifdef USE_TIMES_FUNCTION
  times(&tms_buffer);
  cpu_used_end_time = tms_buffer.tms_utime + tms_buffer.tms_stime ;
#endif

  total_cpu_time_used = (cpu_used_end_time - cpu_used_start_time) *clk_tck();
  total_time_used = end_time - start_time;

  rcs_print("The test took %f seconds in real time  \n", total_time_used);
  if(total_cpu_time_used > 0)
    {
      rcs_print(" and used %f seconds of CPU time.\n", total_cpu_time_used);
    }

  rcs_print("%d messages were written.\n", messages_written);
  rcs_print("Message Size:%d\n", message_size);
  if(total_time_used > 0)
    {
      rcs_print("Bytes written per second : %f\n", message_size*messages_written/total_time_used);
    }
  if(errors > 0)
    {
      rcs_print("%d errors occurred.\n", errors);
    }
  if(total_time_used > 0.0)
    {
      rcs_print("Messages written per second: %f\n", messages_written/total_time_used);
    }
  rcs_print("Minimum time to write a message = %f\n", min_time);
  rcs_print("Maximum time to write a message = %f\n", max_time);
  if(messages_written > 0)
    {
      rcs_print("Average time to write a message = %f\n", total_time_used/messages_written);
      if(total_cpu_time_used > 0)
	{
	  rcs_print("Average CPU time used to write a message = %f\n", total_cpu_time_used/messages_written);
	}
    }
  rcs_print("BufferLine: %s", nml->cms->BufferLine);
  rcs_print("ProcessLine: %s\n", nml->cms->ProcessLine);
  return 0;
}
      
      
  
  
int nml_perf_combined_test(NML *nml, int message_size)
{
  NML *nml2 = new NML(nml,-1,-1); /* Make a clone of the original nml channel but with
				   separate socket sessions, local buffers, and status
				   variables. */
 
#ifdef USE_TIMES_FUNCTION
  struct tms tms_buffer;
#endif
 
  double start_time;
  double end_time;
  int data_type = 0;
  clock_t cpu_used_start_time;
  clock_t cpu_used_end_time;
  double total_cpu_time_used;
  double total_time_used;
  double last_time;
  double max_time = 0;
  int latency_count=0;
  double min_time = 1E6;
  double cur_time = 0;
  double time_dif = 0;
  NMLTYPE read_return_value; 
  unsigned long messages_received = 0;
  unsigned long read_returned_no_new_data_count = 0;
  rcs_print("NML Combined Read/Write Performance Test\n");
  NML_PERFORMANCE_TEST_MSG *test_msg = NULL;
  set_real_test_msg_size(message_size);
  test_msg = new NML_PERFORMANCE_TEST_MSG();
  test_msg->size = message_size;
  nmlperf_control_c_count = 0;
  signal(SIGINT, control_c_handler);
  rcs_print("Press ^C to end the test.\n");
  unsigned long messages_written = 0;

#ifdef USE_CLOCK_FUNCTION
  cpu_used_start_time = clock();
#endif

#ifdef USE_TIMES_FUNCTION
  times(&tms_buffer);
  cpu_used_start_time = tms_buffer.tms_utime + tms_buffer.tms_stime ;
#endif

  cur_time = last_time = start_time = etime();
  NML_PERFORMANCE_TEST_MSG *recvd_msg = (NML_PERFORMANCE_TEST_MSG *)nml2->get_address();
  RCS_TIMER *timer = NULL;
  if(nmlperf_min_cycle_time > 1e-6)
    {
      timer = new RCS_TIMER(nmlperf_min_cycle_time);
    }
   int errors = 0;

  while(nmlperf_control_c_count == 0)
    {
      data_type++;
      data_type = data_type%6;
      test_msg->test_type = data_type;
      test_msg->serial_number = messages_written;
      test_msg->compute_array_length();
      last_time = etime();
      if(nml->write(test_msg) < 0)
	{
	  errors++;
	}
      cur_time = etime();
      time_dif = cur_time - last_time;
      if(time_dif > max_time)
	{
	  max_time = time_dif;
	}
      if(time_dif < min_time)
	{
	  min_time = time_dif;
	}
      messages_written++;
      if(nmlperf_min_cycle_time > 1e-6)
	{
	  timer->wait();
	}
      last_time = etime();
      read_return_value  = nml2->read();
      if(read_return_value < 0)
	{
	  errors++;
	}
      if(read_return_value > 0)
	{
	  int serial_difference = (messages_written - recvd_msg->serial_number);
	  if(serial_difference > 1)
	    {
	      latency_count += (serial_difference-1);
	    }
	  messages_received++;
	}
      else
	{
	  read_returned_no_new_data_count++;
	}
      cur_time = etime();
      time_dif = cur_time - last_time;
      if(time_dif > max_time)
	{
	  max_time = time_dif;
	}
      if(time_dif < min_time)
	{
	  min_time = time_dif;
	}
      last_time = cur_time;
      if(nmlperf_max_message_count > 0)
	{
	  if(nmlperf_max_message_count < messages_written +messages_received+
	     read_returned_no_new_data_count )
	    {
	      break;
	    }
	}
      if(nmlperf_min_cycle_time > 1e-6)
	{
	  timer->wait();
	}
     
    }
  
  end_time = etime();

  
#ifdef USE_CLOCK_FUNCTION
  cpu_used_end_time = clock();
#endif

#ifdef USE_TIMES_FUNCTION
  times(&tms_buffer);
  cpu_used_end_time = tms_buffer.tms_utime + tms_buffer.tms_stime ;
#endif

  total_cpu_time_used = (cpu_used_end_time - cpu_used_start_time) *clk_tck();

  total_time_used = end_time - start_time;

  rcs_print("The test took %f seconds in real time  \n", total_time_used);
  if(total_cpu_time_used > 0)
    {
      rcs_print(" and used %f seconds of CPU time.\n", total_cpu_time_used);
    }

  if(errors > 0)
    {
      rcs_print("%d errors occurred.\n",errors);
    }

  rcs_print("%d messages were written.\n", messages_written);
  rcs_print("%d messages were received.\n", messages_received);
  if(total_time_used > 0)
    {
      rcs_print("Throughput (Messages per Second): %f\n", messages_received/total_time_used);
    }
  rcs_print("Message Size:%d\n", message_size);
  if(total_time_used > 0)
    {
      rcs_print("Bytes read per second : %f\n", message_size*messages_received/total_time_used);
    }
  rcs_print("Read returned No New data %d times.\n", read_returned_no_new_data_count);
  rcs_print("Minimum time to read or write a message = %f\n", min_time);
  rcs_print("Maximum time to read or write a message = %f\n", max_time);
  double avg_time = total_time_used/messages_written/2.0;
  rcs_print("Average time to read or write a message = %f\n",avg_time);
  if(messages_received > 0)
    {
      rcs_print("Latency: %f\n", latency_count/messages_received *avg_time);
    }
  else
    {
      rcs_print("Latency: Unknown\n");
    }

  if(total_cpu_time_used > 0 && messages_written > 0)
    {
      rcs_print("Average CPU time used to read/write a message = %f\n", total_cpu_time_used/messages_written/2);
    }
  rcs_print("BufferLine: %s", nml->cms->BufferLine);
  rcs_print("ProcessLine: %s\n", nml->cms->ProcessLine);
  delete nml2;


  return 0;
}
      


int nml_perf_read_test(NML *nml)
{
#ifdef USE_TIMES_FUNCTION
  struct tms tms_buffer;
#endif

  clock_t cpu_used_start_time;
  clock_t cpu_used_end_time;
  double total_cpu_time_used;

  double start_time;
  double end_time;
  double total_time_used;
  double last_time;
  double max_time = 0;
  double min_time = 1E6;
  double cur_time = 0;
  double time_dif = 0;
  NMLTYPE read_return_value; 
  unsigned long messages_received = 0;
  unsigned long read_returned_no_new_data_count = 0;
  nmlperf_control_c_count = 0;
  rcs_print("NML Read Only Performance Test\n");
  signal(SIGINT, control_c_handler);
  rcs_print("Press ^C to end the test.\n");

#ifdef USE_TIMES_FUNCTION
  times(&tms_buffer);
  cpu_used_start_time = tms_buffer.tms_utime + tms_buffer.tms_stime ;
#endif

#ifdef USE_CLOCK_FUNCTION
  cpu_used_start_time = clock();
#endif

  RCS_TIMER *timer = NULL;
  if(nmlperf_min_cycle_time > 1e-6)
    {
      timer = new RCS_TIMER(nmlperf_min_cycle_time);
    }
  int bytes_received = 0;

  NMLmsg *recvdmsg = nml->get_address();

  // Throw the first read away for more acuracy.
  read_return_value  = nml->read();
      
  cur_time = last_time = start_time = etime();
  
  int errors = 0;

  while(nmlperf_control_c_count == 0)
    {
      read_return_value  = nml->read();
      if(read_return_value < 0)
	{
	  errors++;
	}
      if(nmlperf_control_c_count != 0)
	{
	  // throw the last read away for more accuracy.
	  break;
	}
      if(read_return_value > 0)
	{
	  bytes_received += recvdmsg->size;
	  messages_received++;
	}
      else
	{
	  read_returned_no_new_data_count++;
	}
      cur_time = etime();
      time_dif = cur_time - last_time;
      if(time_dif > max_time)
	{
	  max_time = time_dif;
	}
      if(time_dif < min_time)
	{
	  min_time = time_dif;
	}
      last_time = cur_time;
      if(nmlperf_max_message_count > 0)
	{
	  if(nmlperf_max_message_count < messages_received+
	     read_returned_no_new_data_count )
	    {
	      break;
	    }
	}
      if(nmlperf_min_cycle_time > 1e-6)
	{
	  timer->wait();
	}
    }
  
  end_time = etime();

#ifdef USE_CLOCK_FUNCTION
  cpu_used_end_time = clock();
#endif

#ifdef USE_TIMES_FUNCTION
  times(&tms_buffer);
  cpu_used_end_time = tms_buffer.tms_utime + tms_buffer.tms_stime ;
#endif

  total_cpu_time_used = (cpu_used_end_time - cpu_used_start_time) *clk_tck();

  total_time_used = end_time - start_time;
  rcs_print("The test took %f seconds in real time  \n", total_time_used);
  if(total_cpu_time_used > 0)
    {
      rcs_print(" and used %f seconds of CPU time.\n", total_cpu_time_used);
    }

  if(errors > 0)
    {
      rcs_print("%d errors occurred.\n", errors);
    }

  rcs_print("%d messages were received.\n", messages_received);
  if(total_time_used > 0.0)
    {
      rcs_print("Throughput: %f\n", messages_received/total_time_used);
    }
  rcs_print("Bytes received:%d\n", bytes_received);
  if(total_time_used > 0)
    {
      rcs_print("Bytes read per second : %f\n", bytes_received/total_time_used);
    }
  rcs_print("Read returned No New data %d times.\n", read_returned_no_new_data_count);
  rcs_print("Minimum time to call read() = %f\n", min_time);
  rcs_print("Maximum time to call read()  = %f\n", max_time);
  rcs_print("Average time to call_read = %f\n", total_time_used/(messages_received +  read_returned_no_new_data_count));
  rcs_print("Average time between new data received= %f\n", total_time_used/messages_received);

  if(total_cpu_time_used > 0 && (messages_received +  read_returned_no_new_data_count) > 0)
    {
      rcs_print("Average CPU time used to call read() = %f\n",  total_cpu_time_used/(messages_received +  read_returned_no_new_data_count));
    }
  rcs_print("BufferLine: %s", nml->cms->BufferLine);
  rcs_print("ProcessLine: %s\n", nml->cms->ProcessLine);

  return 0;
}

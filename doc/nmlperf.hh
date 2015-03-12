

#ifndef NMLPERF_HH
#define NMLPERF_HH




 extern "C" {
 void nmlperf(char *config_file,
	      char *host_name,
	      int master,
	      int iterations, int increments,
	      int detailed_output, DISPLAY_MODE display_mode,
	      int connection_number, int check_match, int timeout_micros);

 void nmlperf_stop();
 }

 #endif


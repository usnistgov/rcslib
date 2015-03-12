
#include "rcs.hh"
#include <time.h>

#include <cstdio>
#include <iostream>

int main(int argc,const char **argv) {

  if(argc > 1) {
    for(int i = 1; i < argc ; i++) {
      if(strcmp(argv[i],"--") == 0) {
	while(std::cin.good()) {
	  double dt = 0;
	  std::cin >> dt;
	  time_t dt_t = (time_t) dt;
	  if(dt <= 0) {
	    return 0;
	  }
	  printf("%s\n",ctime(&dt_t));
	}
	return 0;
      } else {
	time_t t = (time_t) (atof(argv[i])); 
	printf("%s\n",ctime(&t));  
      } 
    }
  }
  else {
    double cur_time = etime();
    printf("current_time = %f\n",cur_time);
    time_t ct = (time_t) cur_time;
    printf(" Or %s\n",ctime(&ct));
  }
  return 0;
}

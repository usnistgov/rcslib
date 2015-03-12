#ifndef TIMETRACKER_HH
#define TIMETRACKER_HH

#define TIMETRACKER_IN_RCS 1

struct time_tracker
{
  int count;
  double last;
  double now;
  double start;
  double elapsed;
  double min;
  double max;
  double avg;
};

extern "C" {

extern void reset_time_tracker(struct time_tracker *tt);
extern void cycle_time_tracker(struct time_tracker *tt);

};


#endif

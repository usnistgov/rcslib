
#include "rcs.hh"
#include "timetracker.hh"

void reset_time_tracker(struct time_tracker *tt)
{
  tt->now = tt->start = etime();
  tt->max = -1E9;
  tt->min = 1e9;
  tt->avg = -1E9;
  tt->elapsed = 0.0;
  tt->count =0;
  tt->last=0;
}

void cycle_time_tracker(struct time_tracker *tt)
{
  if(tt->count <= 0)
    {
      reset_time_tracker(tt);
      tt->count = 1;
    }
  else
    {
      double new_time = etime();
      tt->last  = new_time - tt->now;
      if(tt->last > tt->max)
	{
	  tt->max = tt->last;
	}
      if(tt->last < tt->min)
	{
	  tt->min = tt->last;
	}
      tt->now = new_time;
      tt->elapsed = tt->now - tt->start;
      tt->avg = tt->elapsed/tt->count;
      tt->count++;
    }
}


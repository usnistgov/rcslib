/* nml_ex10.cc */
#include "rcs.hh"
#include "nml_ex1.hh"

int
main(int , const char **)
{
  /* NML( format function, buffer name, process name, configuration file ) */
  NML *nml1 = new NML(ex_format, "ex_buf1","ex10_svr","ex_cfg.nml");
  EXAMPLE_MSG example_msg;

  RCS_TIMER timer(5.);

  /* Spawn server and continue. */
  nml_start(); 
  rcs_print( "Server started..\n" );
  timer.wait();

  /* set values to something */
  rcs_print( "Sending message 1 at %f\n", etime() );
  example_msg.f = 123.456;
  example_msg.c = 'a';
  example_msg.i = 99;
  nml1->write(example_msg);
  timer.wait();
 
  rcs_print( "Sending message 2 at %f\n", etime() );
  example_msg.f = 456.456;
  example_msg.c = 'd';
  example_msg.i = 100;
  nml1->write(example_msg);
  timer.wait();
 
  rcs_print( "Sending message 3 at %f\n", etime() );
  example_msg.f = 789.012;
  example_msg.c = 'b';
  example_msg.i = 101;
  nml1->write(example_msg);
  timer.wait();
  
  rcs_print( "Sending message 4 at %f\n", etime() );
  example_msg.f = 789.012;
  example_msg.c = 'b';
  example_msg.i = 101;
  nml1->write(example_msg);
  timer.wait();
  
  rcs_print( "Sending message 5 at %f\n", etime() );
  example_msg.f = 789.012;
  example_msg.c = 'b';
  example_msg.i = 101;
  nml1->write(example_msg);
  timer.wait();
  delete nml1;

  /* Kill the 2 servers and close NML channels. */
  nml_cleanup();
}

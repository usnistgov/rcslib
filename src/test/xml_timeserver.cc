#include "clock.hh"
#include "clock_n_codegen_protos.hh"

#include <signal.h>

static int quit_clock=0;

static void clock_control_c_handler(int sig)
{
  quit_clock=1;
}

int main(int argc, const char *argv)
{

  cms_force_http_port=8090;
  cms_http_show_files=1;

  signal(SIGINT,clock_control_c_handler);

  NML nml(MY_CLOCK_format,"clock_buf","clock_proc","clock.nml");
  MY_CLOCK clk;


  nml_start();
  get_current_cms_time(&clk.now);
  nml.xmlMsgSaveAs(&clk,"clock.xml");
  while(!quit_clock)
    {
      get_current_cms_time(&clk.now);
      nml.write(clk);
      esleep(1.0);
    }
  
  nml_cleanup();
}

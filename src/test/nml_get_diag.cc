
#include "rcs.hh"
#include "nmldiag.hh"

#include <stdio.h>

int null_format(NMLTYPE type, void *buf, CMS *cms) {};

int 
main(int argc, const char **argv)
{

  NML *nml =new NML(null_format,argv[1],argv[2],argv[3]);

  
  class NML_DIAGNOSTICS_INFO *ndi = nml->get_diagnostics_info();
  

  if(ndi)
    {
      printf("ndi->last_writer=%d\n",ndi->last_writer);
      printf("ndi->last_reader=%d\n",ndi->last_reader);
    }

  if(ndi && ndi->last_writer_dpi)
    {
      printf("\n\nlast_writer:\n");
      printf("name=%s\n", ndi->last_writer_dpi->name);
      printf("host_sysinfo=%s\n", ndi->last_writer_dpi->host_sysinfo);
      printf("pid=%d\n", ndi->last_writer_dpi->pid);
      printf("access_type=%d\n", ndi->last_writer_dpi->access_type);
      printf("msg_id=%d\n", ndi->last_writer_dpi->msg_id);
      printf("msg_size=%ld\n",ndi->last_writer_dpi->msg_size);
      printf("msg_type=%ld\n",ndi->last_writer_dpi->msg_type);
      printf("number_of_accesses=%ld\n", ndi->last_writer_dpi->number_of_accesses);
      printf("bytes_moved=%f\n", ndi->last_writer_dpi->bytes_moved);
      printf("bytes_moved_accross_socket=%f\n", ndi->last_writer_dpi->bytes_moved_across_socket);
      printf("last_access_time=%f\n", ndi->last_writer_dpi->last_access_time);
      printf("first_access_time=%f\n", ndi->last_writer_dpi->first_access_time);
      printf("max_difference=%f\n", ndi->last_writer_dpi->max_difference);
      printf("min_difference=%f\n", ndi->last_writer_dpi->min_difference);
    }
  if(ndi && ndi->last_reader_dpi)
    {
      printf("\n\nlast_reader:\n");
      printf("name=%s\n", ndi->last_reader_dpi->name);
      printf("host_sysinfo=%s\n", ndi->last_reader_dpi->host_sysinfo);
      printf("pid=%d\n", ndi->last_reader_dpi->pid);
      printf("access_type=%d\n", ndi->last_reader_dpi->access_type);
      printf("msg_id=%d\n", ndi->last_reader_dpi->msg_id);
      printf("msg_size=%ld\n",ndi->last_reader_dpi->msg_size);
      printf("msg_type=%ld\n",ndi->last_reader_dpi->msg_type);
      printf("number_of_accesses=%ld\n", ndi->last_reader_dpi->number_of_accesses);
      printf("bytes_moved=%f\n", ndi->last_reader_dpi->bytes_moved);
      printf("bytes_moved_accross_socket=%f\n", ndi->last_reader_dpi->bytes_moved_across_socket);
      printf("last_access_time=%f\n", ndi->last_reader_dpi->last_access_time);
      printf("first_access_time=%f\n", ndi->last_reader_dpi->first_access_time);
      printf("max_difference=%f\n", ndi->last_reader_dpi->max_difference);
      printf("min_difference=%f\n", ndi->last_reader_dpi->min_difference);
    }
};

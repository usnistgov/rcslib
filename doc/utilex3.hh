/* utilex3.hh */
#ifndef UTILEX3_HH
#define UTILEX3_HH

#ifdef __cplusplus
extern "C" {
#endif
#include <sys/stat.h>		/* defines the S_IXXXX permision flags */
#ifdef __cplusplus
} /* END of extern "C" */
#endif

/* ID that processes connecting to this semaphore must agree on. */
#define MY_SEM_ID 101

/* Permissions Mode for rw_rw_r__ */
#define MY_SEM_MODE (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH) 

#endif	/* end of UTILEX3_HH */

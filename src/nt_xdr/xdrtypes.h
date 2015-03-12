/*********************************************************************
 * RPC for the Windows NT Operating System
 * 1993 by Martin F. Gergeleit
 * Users may use, copy or modify Sun RPC for the Windows NT Operating
 * System according to the Sun copyright below.
 *
 * RPC for the Windows NT Operating System COMES WITH ABSOLUTELY NO
 * WARRANTY, NOR WILL I BE LIABLE FOR ANY DAMAGES INCURRED FROM THE
 * USE OF. USE ENTIRELY AT YOUR OWN RISK!!!
 *********************************************************************/

/* @(#)types.h  2.3 88/08/15 4.0 RPCSRC */
/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */
/*      @(#)types.h 1.18 87/07/24 SMI      */

/* NIST Modifications:
10-Dec-1998 the name of this file was changed from types.h to xdr_types.h
although originally this file was part of RPC it will be used here only
to compile XDR for Windows NT to be used internally within NML. Other platforms
will continue to use the system provided headers for XDR and RPC.

*/


/*
 * Rpc additions to <sys/types.h>
 */
#ifndef __TYPES_RPC_HEADER__
#define __TYPES_RPC_HEADER__

#define bool_t  int
#define enum_t  int
#ifndef FALSE
#define FALSE   (0)
#endif
#ifndef TRUE
#define TRUE    (1)
#endif
#define __dontcare__    -1
#ifndef NULL
#       define NULL 0
#endif

#define mem_alloc       malloc
#define mem_free(ptr, bsize)    free(ptr)

#ifndef makedev /* ie, we haven't already included it */
#include <sys/types.h>
#endif

#ifndef INADDR_LOOPBACK
#define       INADDR_LOOPBACK         (u_long)0x7F000001
#endif
#ifndef MAXHOSTNAMELEN
#define        MAXHOSTNAMELEN  64
#endif

#if !defined(HAVE_CADDR_T_TYPE) && !defined(CADDR_T_DEFINED) && !defined(HAVE__CADDR_T)
typedef char *caddr_t;
#define CADDR_T_DEFINED
#endif
#ifndef mingw32
#ifndef HAVE_U_INT
typedef unsigned int u_int;
#endif
#ifndef HAVE_U_LONG
typedef unsigned long u_long;
#endif
#ifndef HAVE_U_SHORT
typedef unsigned short u_short;
#endif
#else
#include <winsock2.h>
#endif

#endif /* ndef __TYPES_RPC_HEADER__ */

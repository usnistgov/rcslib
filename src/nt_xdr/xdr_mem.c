/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if (!defined(HAVE_RPC_XDR_H) && !defined(HAVE_RPC_RPC_H) && defined(ENABLE_RCS_XDR) ) || !defined(HAVE_CONFIG_H)


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

/* @(#)xdr_mem.c        2.1 88/07/29 4.0 RPCSRC */
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
#if !defined(lint) && defined(SCCSIDS)
static char sccsid[] = "@(#)xdr_mem.c 1.19 87/08/11 Copyr 1984 Sun Micro";
#endif

/*
 * xdr_mem.h, XDR implementation using memory buffers.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 *
 * If you have some data to be interpreted as external data representation
 * or to be converted to external data representation in a memory buffer,
 * then this is the package for you.
 *
 */


#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "xdr_mem_no_config.h"
#endif

#include "xdr.h"

static bool_t   xdrmem_getlong();
static bool_t   xdrmem_putlong();
static bool_t   xdrmem_getbytes();
static bool_t   xdrmem_putbytes();
static u_int    xdrmem_getpos();
static bool_t   xdrmem_setpos();
static long *   xdrmem_inline();
static void     xdrmem_destroy();

static struct   xdr_ops xdrmem_ops = {
        xdrmem_getlong,
        xdrmem_putlong,
        xdrmem_getbytes,
        xdrmem_putbytes,
        xdrmem_getpos,
        xdrmem_setpos,
        xdrmem_inline,
        xdrmem_destroy
};

/*
 * The procedure xdrmem_create initializes a stream descriptor for a
 * memory buffer.
 */
void
xdrmem_create(xdrs, addr, size, op)
        register XDR *xdrs;
        caddr_t addr;
        u_int size;
        enum xdr_op op;
{

        xdrs->x_op = op;
        xdrs->x_ops = &xdrmem_ops;
        xdrs->x_private = xdrs->x_base = addr;
        xdrs->x_handy = size;
}

static void
xdrmem_destroy(/*xdrs*/)
        /*XDR *xdrs;*/
{
}

static bool_t
xdrmem_getlong(xdrs, lp)
        register XDR *xdrs;
        long *lp;
{

        if ((xdrs->x_handy -= sizeof(long)) < 0)
                return (FALSE);
        *lp = (long)ntohl((u_long)(*((long *)(xdrs->x_private))));
        xdrs->x_private += sizeof(long);
        return (TRUE);
}

static bool_t
xdrmem_putlong(xdrs, lp)
        register XDR *xdrs;
        long *lp;
{

        if ((xdrs->x_handy -= sizeof(long)) < 0)
                return (FALSE);
        *(long *)xdrs->x_private = (long)htonl((u_long)(*lp));
        xdrs->x_private += sizeof(long);
        return (TRUE);
}

static bool_t
xdrmem_getbytes(xdrs, addr, len)
        register XDR *xdrs;
        caddr_t addr;
        register u_int len;
{

        if ((xdrs->x_handy -= len) < 0)
                return (FALSE);
        memcpy(addr, xdrs->x_private, len);
        xdrs->x_private += len;
        return (TRUE);
}

static bool_t
xdrmem_putbytes(xdrs, addr, len)
        register XDR *xdrs;
        caddr_t addr;
        register u_int len;
{

        if ((xdrs->x_handy -= len) < 0)
                return (FALSE);
        memcpy(xdrs->x_private, addr, len);
        xdrs->x_private += len;
        return (TRUE);
}

static u_int
xdrmem_getpos(xdrs)
        register XDR *xdrs;
{

        return ((u_int)xdrs->x_private - (u_int)xdrs->x_base);
}

static bool_t
xdrmem_setpos(xdrs, pos)
        register XDR *xdrs;
        u_int pos;
{
        register caddr_t newaddr = xdrs->x_base + pos;
        register caddr_t lastaddr = xdrs->x_private + xdrs->x_handy;

        if ((long)newaddr > (long)lastaddr)
                return (FALSE);
        xdrs->x_private = newaddr;
        xdrs->x_handy = (int)lastaddr - (int)newaddr;
        return (TRUE);
}

static long *
xdrmem_inline(xdrs, len)
        register XDR *xdrs;
        int len;
{
        long *buf = 0;

        if (xdrs->x_handy >= len) {
                xdrs->x_handy -= len;
                buf = (long *) xdrs->x_private;
                xdrs->x_private += len;
        }
        return (buf);
}

/* matches #if !defined(HAVE_RPC_XDR_H) || !defined(HAVE_XDR_H) */
#else
#include "rcs_empty_source"
#endif

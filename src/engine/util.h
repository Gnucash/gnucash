/********************************************************************\
 * util.h -- utility functions that are used everywhere else for    *
 *           xacc (X-Accountant)                                    *
 * Copyright (C) 1997 Robin D. Clark                                *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#ifndef __XACC_UTIL_H__
#define __XACC_UTIL_H__

#include <stdlib.h>
#include "config.h"

#define BUFSIZE   1024

/** DEBUGGING MACROS ************************************************/
/* The debuging macros enable the setting of trace messages */
#include <stdio.h>
#include <nana.h>

/* override standard system assert with nana I assertion */
#ifdef assert 
#undef assert
#endif 
#define assert I

#define MOD_ENGINE     1
#define MOD_IO         2
#define MOD_REGISTER   3
#define MOD_LEDGER     4
#define MOD_GUI        5
#define MOD_SCRUB      6
#define MODULE_MAX     7

extern int loglevel[MODULE_MAX];

#define LERR    (1 <= loglevel[module])
#define LWARN   (2 <= loglevel[module])
#define LINFO   (3 <= loglevel[module])
#define LDEBUG  (4 <= loglevel[module])
#define LDETAIL (5 <= loglevel[module])


/* utility macros  */
#define PERR(x...)     LG(LERR,    "Error: ");   LG(LERR,    ##x);
#define PWARN(x...)    LG(LWARN,   "Waring: ");  LG(LWARN,   ##x);
#define PINFO(x...)    LG(LINFO,   "Info: ");    LG(LINFO,   ##x);
#define DEBUG(x...)    LG(LDEBUG,  "Debug: ");   LG(LDEBUG,  ##x);
#define ENTER(x...)    LG(LDEBUG,  "Enter: ");   LG(LDEBUG,  ##x);
#define LEAVE(x...)    LG(LDEBUG,  "Leave: ");   LG(LDEBUG,  ##x);
#define DETAIL(x...)   LG(LDETAIL, "Detail: ");  LG(LDETAIL, ##x);

#define DEBUGCMD(x) { if (LINFO) { x; }}

#include <errno.h>
#define ERROR()     fprintf(stderr,"%s: Line %d, error = %s\n", \
			    __FILE__, __LINE__, strerror(errno));

#ifdef DEBUGMEMORY
void   *dmalloc( size_t size );
void   dfree( void *ptr );
size_t dcoresize();
#  define _malloc(x)   dmalloc(x)
#  define _free(x)     dfree(x)
#  define _coresize()  dcoresize()
#else
#  define _malloc(x)    malloc(x)
#  define _free(x)      free(x)
#  define _coresize()   0
#endif

/** COOL MACROS *****************************************************/
#ifndef ABS
#define ABS(x)   ((x)>=0) ? (x) : (-1*(x))
#endif
#define DABS(x)  ((x)>=0.0) ? (x) : (-1.0*(x))
#define DMAX(x,y) ((x)>(y)) ? (x) : (y)
#define isNum(x) (((x)-0x30) < 0) ? 0 : (((x)-0x30) > 9) ? 0 : 1

#define EPS  (1.0e-4)
#define DEQ(x,y) (((((x)+EPS)>(y)) ? 1 : 0) && ((((x)-EPS)<(y)) ? 1 : 0))


#define SAFE_STRCMP(da,db) {		\
  if ((da) && (db)) {			\
    int retval = strcmp ((da), (db));	\
    /* if strings differ, return */	\
    if (retval) return retval;		\
  } else 				\
  if ((!(da)) && (db)) {		\
    return -1;				\
  } else 				\
  if ((da) && (!(db))) {		\
    return +1;				\
  }					\
}

int safe_strcmp (const char * da, const char * db);

/********************************************************/
/* the ultostr() subroutihne is the inverse of strtoul().
 *    It accepts a number and prints it in the indicated base.
 *    The returned string should be freed when done.
 */

char * ultostr (unsigned long val, int base);

/** PROTOTYPES ******************************************************/

#define PRTSYM 0x1
#define PRTSHR 0x2
#define PRTSEP 0x4
/*
 * The xaccPrintAmount() subroutine converts a double amount
 * to a printable string, depending on the argument shrs:
 * 0 -- print two decimal places
 * 1 -- print currency symbol & two decimal places
 * 2 -- print three decimal places
 * 3 -- prints three decimal places followed by string "shrs"
 * 4-7 as above, but comma separated
 *
 * shrs must be bitwise-OR of PRTSYM, PRTSHR and PRTSEP
 */
char * xaccPrintAmount (double val, short shrs);

/********************************************************************\
 * xaccParseUSAmount                                                * 
 *   parses U.S. style monetary strings                             *
 *   (strings of the form DDD,DDD,DDD.CC                            *
 *                                                                  * 
\********************************************************************/
double xaccParseUSAmount (const char * str);


/** TEMPLATES ******************************************************/

#define FIND_IN_LIST(Type,list,id,member,found) 	\
{							\
  int i;						\
  Type *elt;						\
							\
  /* lets see if we have an */				\
  /* open window for this account */			\
  found = NULL;						\
  if (id && list) {					\
    i = 0;						\
    elt = list[i];					\
    while (elt) {					\
      if (id == elt->member) found = elt;		\
      i++;						\
      elt = list[i];					\
    } 							\
  } 							\
}

#define FETCH_FROM_LIST(Type,list,id,member,elt) 	\
{							\
  int i;						\
  Type **newlist;					\
  if (!(id)) return 0x0;				\
							\
  /* lets see if we already have an */			\
  /* open window for this account */			\
  i = 0;						\
  if (list) {						\
    elt = list[i];					\
    while (elt) {					\
      if ((id) == elt->member) return elt;		\
      i++;						\
      elt = list[i];					\
    }    						\
  } 							\
							\
  /* if we are here, we didn't find it */		\
  newlist = (Type **) malloc ((i+2) * sizeof (Type *));	\
  i = 0;						\
  if (list) {						\
    elt = list[i];					\
    while (elt) {					\
      newlist[i] = elt;					\
      i++;						\
      elt = list[i];					\
    }    						\
    free (list);					\
  }							\
  list = newlist;					\
							\
  /* malloc a new one, add it to the list */		\
  elt = (Type *) malloc (sizeof (Type));		\
  elt->member = (id);					\
  list [i] = elt;					\
  list [i+1] = NULL;					\
}

#define REMOVE_FROM_LIST(Type,list,id,member)	 	\
{							\
  int i,j;						\
  Type *elt;						\
							\
  /* find the item in the list, and remove it */	\
  if (list && id) {					\
    i = j = 0;						\
    elt = list[i];					\
    while (elt) {					\
      list[j] = elt;					\
      if (id == elt->member) j--; 			\
      i++; j++;						\
      elt = list[i];					\
    }    						\
    list[j] = NULL;					\
  }							\
}


#endif /* __XACC_UTIL_H__ */

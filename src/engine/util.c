/********************************************************************\
 * util.c -- utility functions that are used everywhere else for    *
 *           xacc (X-Accountant)                                    *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998 Linas Vepstas                           *
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

#include <math.h>
#include <malloc.h>
#include <string.h>

#include "config.h"
#include "messages.h"
#include "util.h"

/* hack alert -- stpcpy prototype is missing, use -DGNU */
char * stpcpy (char *dest, const char *src);

/** GLOBALS *********************************************************/
/* 
   0 == disable all messages
   1 == enble only error messages
   2 == print warnings
   3 == print info messages
   4 == print debugging messages
 */
int loglevel[MODULE_MAX] =
{0,      /* DUMMY */
 2,      /* ENGINE */
 2,      /* IO */
 4,      /* REGISTER */
 2,      /* LEDGER */
 4,      /* HTML */
 2,      /* GUI */
 4,      /* SCRUB */
 4,      /* GTK_REG */
};

/********************************************************************\
 * DEBUGGING MEMORY ALLOCATION STUFF                                * 
\********************************************************************/
#if DEBUG_MEMORY

// #if defined (__NetBSD__) || defined(__FreeBSD__)

#ifndef HAVE_MALLOC_USABLE_SIZE
#define malloc_usable_size(ptr) 0
#endif

size_t core=0;

void
dfree( void *ptr )
  {
  core -= malloc_usable_size(ptr);
  free(ptr);
  }

void*
dmalloc( size_t size )
  {
  int i;
  char *ptr;
  ptr = (char *)malloc(size);
  for( i=0; i<size; i++ )
    ptr[i] = '.';
  
  core +=  malloc_usable_size(ptr);
  return (void *)ptr;
  }

size_t
dcoresize(void)
  {
  return core;
  }
#endif

/********************************************************************\
\********************************************************************/

int 
safe_strcmp (const char * da, const char * db) {
   SAFE_STRCMP (da, db);
   return 0;
}

/********************************************************************\
\********************************************************************/
/* inverse of strtoul */

#define MAX_DIGITS 50

char *
ultostr (unsigned long val, int base)
{
  char buf[MAX_DIGITS];
  unsigned long broke[MAX_DIGITS];
  int i;
  unsigned long places=0, reval;
  
  if ((2>base) || (36<base)) return NULL;

  /* count digits */
  places = 0;
  for (i=0; i<MAX_DIGITS; i++) {
     broke[i] = val;
     places ++;
     val /= base;
     if (0 == val) break;
  }

  /* normalize */
  reval = 0;
  for (i=places-2; i>=0; i--) {
    reval += broke[i+1];
    reval *= base;
    broke[i] -= reval;
  }

  /* print */
  for (i=0; i<places; i++) {
    if (10>broke[i]) {
       buf[places-1-i] = 0x30+broke[i];  /* ascii digit zero */
    } else {
       buf[places-1-i] = 0x41-10+broke[i];  /* ascii capaital A */
    }
  }
  buf[places] = 0x0;

  return strdup (buf);
}

/********************************************************************\
 * stpcpy for those platforms that don't have it.
\********************************************************************/

#if !HAVE_STPCPY
char *
stpcpy (char *dest, const char *src) 
{
   strcpy(dest, src);
   return(dest + strlen(src));
}
#endif

/********************************************************************\
 * currency & locale related stuff.
 * first attempt at internationalization i18n of currency amounts
 * In the long run, amounts should be printed with punctuation
 * returned from the localconv() subroutine
\********************************************************************/

/* The PrtAmtComma() routine prints a comma-separated currency value */

/* THOU_SEP is a comma in U.S. but a period in some parts of Europe */
/* CENT_SEP is a period in U.S. but a comma in some parts of Europe */
#define THOU_SEP  ','
#define CENT_SEP '.'

static int
PrtAmtComma (char * buf, double val, int prec)
{
   int i, ival, ncommas = 0;
   double tmp, amt=0.0;
   char *start = buf;

   /* check if we're printing infinity */
   if (!finite(val)) {
      strcpy (buf, "inf");
      return 3;
   }

   /* Round to 100'ths or 1000'nths now.  Must do this before we start printing. */
   if (2 == prec) val += 0.005;
   if (3 == prec) val += 0.0005;

   /* count number of commas */
   tmp = val;
   while (tmp > 1000.0) {
      tmp *= 0.001;
      ncommas ++;
   }

   /* print digits in groups of three, separated by commas */
   for (i=ncommas; i>=0; i--) {
      int j;

      amt *= 1000.0;
      tmp = val;
      for (j=i; j>0; j--) tmp *= 0.001;
      tmp -= amt;
      ival = tmp;  
      if (i !=ncommas) {
        buf += sprintf (buf, "%03d", ival);   
      } else {
        buf += sprintf (buf, "%d", ival);   
      }
      *buf = THOU_SEP; buf++;
      amt += ival;
   }

   /* place decimal point */
   buf --; *buf = CENT_SEP; buf++;

   /* print two or three decimal places */
   if (3 == prec) {
      ival = 1000.0 * (val-amt);
      buf += sprintf (buf, "%03d", ival); 
   } else {
      ival = 100.0 * (val-amt);
      buf += sprintf (buf, "%02d", ival); 
   }

   return (buf-start);
}

int
xaccSPrintAmount (char * bufp, double val, short shrs) 
{
   char * orig_bufp = bufp;

   if (!bufp) return 0;

   if (DEQ(val, 0.0))
     val = 0.0;

   if (0.0 > val) {
      bufp[0] = '-';
      bufp ++;
      val = -val;
   }

   if (shrs & PRTSHR) {
      if (shrs & PRTSEP) {
         bufp += PrtAmtComma (bufp, val, 3);
      } else {
         bufp += sprintf( bufp, "%.3f", val );
      }
      if (shrs & PRTSYM) {
         /* stpcpy returns pointer to end of string, not like strcpy */
         bufp = stpcpy (bufp, " shrs");
      }
   } else {

      if (shrs & PRTSYM) {
         bufp += sprintf( bufp, "%s ", CURRENCY_SYMBOL);
      }
      if (shrs & PRTSEP) {
         bufp += PrtAmtComma (bufp, val, 2);
      } else {
         bufp += sprintf( bufp, "%.2f", val );
      }
   }

   /* return length of printed string */
   return (bufp-orig_bufp);
}

char * 
xaccPrintAmount (double val, short shrs) 
{
   /* hack alert -- this is not thread safe ... */
   static char buf[BUFSIZE];

   xaccSPrintAmount (buf, val, shrs);

   /* its OK to return buf, since we declared it static */
   return buf;
}


/********************************************************************\
 * xaccParseUSAmount                                                * 
 *   parses U.S. style monetary strings                             *
 *   (strings of the form DDD,DDD,DDD.CC                            *
 *                                                                  * 
 * Args:   str -- pointer to string rep of sum                      * 
 * Return: double -- the parsed amount                              * 
 *
 * Note: be careful changing this algorithm.  The Quicken-file-format
 * parser depends a lot on the ability of this routine to do what it's
 * doing.  Don't break it!
\********************************************************************/

/* The following tokens are used to define the US-style monetary
 * strings.  With a bit of cleverness, it should be possible to modify
 * these to handle various international styles ... maybe ... */

#define MINUS_SIGN '-'
#define K_SEP      ','      /* thousands separator */
#define DEC_SEP    '.'      /* decimal point */

double xaccParseUSAmount (const char * instr) 
{
   char *mstr, *str, *tok;
   double dollars = 0.0;
   int len;
   int isneg = 0;

   if (!instr) return 0.0;
   mstr = strdup (instr);
   str = mstr;

   /* strip off garbage at end of the line */
   tok = strchr (str, '\r');
   if (tok) *tok = 0x0;
   tok = strchr (str, '\n');
   if (tok) *tok = 0x0;

   /* search for a minus sign */
   tok = strchr (str, MINUS_SIGN);
   if (tok) {
      isneg = 1;
      str = tok+sizeof(char);
   }

   /* remove comma's */
   tok = strchr (str, K_SEP);
   while (tok) {
      *tok = 0x0;
      dollars *= 1000.0;
      dollars += ((double) (1000 * atoi (str)));
      str = tok+sizeof(char);
      tok = strchr (str, K_SEP);
   }

   /* search for a decimal point */
   tok = strchr (str, DEC_SEP);
   if (tok) {
      *tok = 0x0;
      dollars += ((double) (atoi (str)));
      str = tok+sizeof(char);

      /* if there is anything trailing the decimal 
       * point, convert it  */
      if (str[0]) {

         /* strip off garbage at end of the line */
         tok = strchr (str, ' ');
         if (tok) *tok = 0x0;
   
         /* adjust for number of decimal places */
         len = strlen(str);
         if (6 == len) {
            dollars += 0.000001 * ((double) atoi (str));
         } else
         if (5 == len) {
            dollars += 0.00001 * ((double) atoi (str));
         } else
         if (4 == len) {
            dollars += 0.0001 * ((double) atoi (str));
         } else
         if (3 == len) {
            dollars += 0.001 * ((double) atoi (str));
         } else
         if (2 == len) {
            dollars += 0.01 * ((double) atoi (str));
         } else 
         if (1 == len) {
            dollars += 0.1 * ((double) atoi (str));
         } 
      }

   } else {
      dollars += ((double) (atoi (str)));
   }

   if (isneg) dollars = -dollars;

   free (mstr);
   return dollars;
}

/************************* END OF FILE ******************************\
\********************************************************************/

/* Copyright (C) 1995,1996,1997,1998, 1999, 2000 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  */

/* Software engineering face-lift by Greg J. Badros, 11-Dec-1999,
   gjb@cs.washington.edu, http://www.cs.washington.edu/homes/gjb */



#include <stdio.h>
#include "libguile/__scm.h"
#include "libguile.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif

# ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
# else
#  ifdef HAVE_SYS_TIME_H
#   include <sys/time.h>
#  else
#   ifdef HAVE_TIME_H
#    include <time.h>
#   endif
#  endif
# endif

#ifdef HAVE_SYS_TIMES_H
# include <sys/times.h>
#endif

#ifdef HAVE_SYS_TIMEB_H
# include <sys/timeb.h>
#endif

#ifndef tzname /* For SGI.  */
extern char *tzname[]; /* RS6000 and others reject char **tzname.  */
#endif

#ifdef MISSING_STRPTIME_DECL
extern char *strptime ();
#endif

/* This should be figured out by autoconf.  */
#if ! defined(CLKTCK) && defined(CLK_TCK)
#  define CLKTCK CLK_TCK
#endif
#if ! defined(CLKTCK) && defined(CLOCKS_PER_SEC)
#  define CLKTCK CLOCKS_PER_SEC
#endif
#if ! defined(CLKTCK)
#  define CLKTCK 60
#endif

#include "strptime.h"

#ifdef __STDC__
# define timet time_t
#else
# define timet long
#endif
static SCM
filltime (struct tm *bd_time, int zoff, char *zname);

static SCM
filltime (struct tm *bd_time, int zoff, char *zname)
{
    SCM result = scm_make_vector (SCM_MAKINUM(11), SCM_UNDEFINED);

    SCM_VELTS (result)[0] = SCM_MAKINUM (bd_time->tm_sec);
    SCM_VELTS (result)[1] = SCM_MAKINUM (bd_time->tm_min);
    SCM_VELTS (result)[2] = SCM_MAKINUM (bd_time->tm_hour);
    SCM_VELTS (result)[3] = SCM_MAKINUM (bd_time->tm_mday);
    SCM_VELTS (result)[4] = SCM_MAKINUM (bd_time->tm_mon);
    SCM_VELTS (result)[5] = SCM_MAKINUM (bd_time->tm_year);
    SCM_VELTS (result)[6] = SCM_MAKINUM (bd_time->tm_wday);
    SCM_VELTS (result)[7] = SCM_MAKINUM (bd_time->tm_yday);
    SCM_VELTS (result)[8] = SCM_MAKINUM (bd_time->tm_isdst);
    SCM_VELTS (result)[9] = SCM_MAKINUM (zoff);
    SCM_VELTS (result)[10] = zname ? scm_makfrom0str (zname) : SCM_BOOL_F;
    return result;
}

#ifndef HAVE_STRPTIME
SCM_DEFINE (scm_strptime, "strptime", 2, 0, 0,
            (SCM format, SCM string),
            "Performs the reverse action to @code{strftime}, parsing @var{string}\n"
            "according to the specification supplied in @var{template}.  The\n"
            "interpretation of month and day names is dependent on the current\n"
            "locale.  The\n"
            "value returned is a pair.  The CAR has an object with time components \n"
            "in the form returned by @code{localtime} or @code{gmtime},\n"
            "but the time zone components\n"
            "are not usefully set.\n"
            "The CDR reports the number of characters from @var{string} which\n"
            "vwere used for the conversion.")
#define FUNC_NAME s_scm_strptime
{
    struct tm t;
    char *fmt, *str, *rest;

    SCM_VALIDATE_ROSTRING (1, format);
    SCM_VALIDATE_ROSTRING (2, string);

    SCM_COERCE_SUBSTR (format);
    SCM_COERCE_SUBSTR (string);
    fmt = SCM_ROCHARS (format);
    str = SCM_ROCHARS (string);

    /* initialize the struct tm */
#define tm_init(field) t.field = 0
    tm_init (tm_sec);
    tm_init (tm_min);
    tm_init (tm_hour);
    tm_init (tm_mday);
    tm_init (tm_mon);
    tm_init (tm_year);
    tm_init (tm_wday);
    tm_init (tm_yday);
#undef tm_init

    t.tm_isdst = -1;
    SCM_DEFER_INTS;
    if ((rest = strptime (str, fmt, &t)) == NULL)
        SCM_SYSERROR;

    SCM_ALLOW_INTS;
    return scm_cons (filltime (&t, 0, NULL),  SCM_MAKINUM (rest - str));
}
#undef FUNC_NAME
#endif /* HAVE_STRPTIME */

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/


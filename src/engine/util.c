/********************************************************************\
 * util.c -- utility functions that are used everywhere else for    *
 *           xacc (X-Accountant)                                    *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2000 Linas Vepstas <linas@linas.org>          *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
 *   Author: Rob Clark (rclark@cs.hmc.edu)                          *
 *   Author: Linas Vepstas (linas@linas.org)                        *
\********************************************************************/

#define _GNU_SOURCE
#include <string.h>

#include "config.h"

#ifdef HAVE_IEEEFP_H
#  include <ieeefp.h>    /* for finite in Solaris 8 */
#endif

#include <ctype.h>
#include <errno.h>
#include <glib.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <stdarg.h>
#include <stdlib.h>

#include "messages.h"
#include "gnc-engine.h"
#include "gnc-common.h"
#include "gnc-commodity.h"
#include "util.h"

/** GLOBALS *********************************************************/
gncLogLevel loglevel[MOD_LAST + 1] =
{
  GNC_LOG_FATAL,        /* DUMMY */
  GNC_LOG_WARNING,      /* ENGINE */
  GNC_LOG_WARNING,      /* IO */
  GNC_LOG_WARNING,      /* REGISTER */
  GNC_LOG_WARNING,      /* LEDGER */
  GNC_LOG_WARNING,      /* HTML */
  GNC_LOG_WARNING,      /* GUI */
  GNC_LOG_WARNING,      /* SCRUB */
  GNC_LOG_WARNING,      /* GTK_REG */
  GNC_LOG_WARNING,      /* GUILE */
  GNC_LOG_DEBUG,        /* BACKEND */
  GNC_LOG_WARNING,      /* QUERY */
};

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_ENGINE;


/* Set the logging level of the given module. */
void
gnc_set_log_level(gncModuleType module, gncLogLevel level)
{
  if ((module < 0) || (module > MOD_LAST))
    return;

  loglevel[module] = level;
}

/* Set the logging level for all modules. */
void
gnc_set_log_level_global(gncLogLevel level)
{
  gncModuleType module;

  for (module = 0; module <= MOD_LAST; module++)
    loglevel[module] = level;
}

/* prettify() cleans up subroutine names. AIX/xlC has the habit of
 * printing signatures not names; clean this up. On other operating
 * systems, truncate name to 30 chars. Note this routine is not thread
 * safe. Note we wouldn't need this routine if AIX did something more
 * reasonable. Hope thread safety doesn't poke us in eye. */
static const char *
prettify (const char *name)
{
  static char bf[128];
  char *p;

  strncpy (bf, name, 29); bf[28] = 0;
  p = strchr (bf, '(');

  if (p)
  {
    *(p+1) = ')';
    *(p+2) = 0x0;
  }
  else
    strcpy (&bf[26], "...()");

  return bf;
}

void
gnc_log (gncModuleType module, gncLogLevel log_level, const char *prefix,
         const char *function_name, const char *format, ...)
{
  va_list ap;

  if (module < 0 || module > MOD_LAST)
  {
    PERR ("Bad module: %d", module);
    return;
  }

  if (log_level > loglevel[module])
    return;

  fprintf (stderr, "%s: %s: ", prefix, prettify (function_name));

  va_start (ap, format);

  vfprintf (stderr, format, ap);

  va_end (ap);

  fprintf (stderr, "\n");
}

/* DxaccParseAmount configuration */
static gboolean auto_decimal_enabled = FALSE;
static int auto_decimal_places = 2;    /* default, can be changed */

/* enable/disable the auto_decimal_enabled option */
void
gnc_set_auto_decimal_enabled(gboolean enabled)
{
  auto_decimal_enabled = enabled;
}

/* set the number of auto decimal places to use */
void
gnc_set_auto_decimal_places( int places )
{
  auto_decimal_places = places;
}


/********************************************************************\
\********************************************************************/

/* Search for str2 in first nchar chars of str1, ignore case.. 
 * Return pointer to first match, or null.
 */

char *
strncasestr(const char *str1, const char *str2, size_t len) 
{
  while (*str1 && len--) 
  {
    if (toupper(*str1) == toupper(*str2)) 
    {
      if (strncasecmp(str1,str2,strlen(str2)) == 0) 
      {
        return (char *) str1;
      }
    }
    str1++;
  }
  return NULL;
}

/* Search for str2 in str1, ignore case. 
 * Return pointer to first match, or null. 
 */

char *
strcasestr(const char *str1, const char *str2) 
{
   size_t len = strlen (str1);
   char * retval = strncasestr (str1, str2, len);
   return retval;
}

/* Reversed strstr -- search for a needle in the haystack,
 * from the far end 
 */

char *
rstrstr (const char *haystack, const char * needle)
{
    int haylen = strlen (haystack);
    int neelen = strlen (needle);

    const char * hp = haystack + haylen - 1;
    const char * np = needle + neelen - 1;

    if ((0 == neelen) || (0 == haylen)) return NULL;

    while (hp >= haystack+neelen) {
        if (*hp == *np) {
            --np;
            if (np < needle) return (char *) hp;
        } else {
            np = needle + neelen - 1;
        }
        --hp;
    }

    return NULL;
}


/* The strpskip() function locates the first occurrence in the
 * string s that does not match any of the characters in "reject".
 * This is the opposite of strpbrk()
 */

char * 
strpskip (const char * s, const char *reject)
{
   size_t i, rlen;
   char * retval;

   if (!s) return NULL;
   if (!reject) return (char *) s;

   rlen = sizeof (reject);
   retval = (char *) s;

   while (*retval) {
      int match = 0;
      for (i=0; i<rlen; i++) {
         if (reject[i] == *retval) {match=1; break; }
      }
      if (!match) return retval;
      retval ++;
   }
   return NULL;
}


/********************************************************************\
\********************************************************************/

int 
safe_strcmp (const char * da, const char * db)
{
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
       buf[places-1-i] = 0x41-10+broke[i];  /* ascii capital A */
    }
  }
  buf[places] = 0x0;

  return g_strdup (buf);
}

/********************************************************************\
 * utility function to convert floating point value to a string
\********************************************************************/

static int
util_fptostr(char *buf, double val, int prec)
{
  int  i;
  char formatString[10];
  char prefix[]  = "%0.";
  char postfix[] = "f";

  /* This routine can only handle precision between 0 and 9, so
   * clamp precision to that range */
  if (prec > 9) prec = 9;
  if (prec < 0) prec = 0;

  /* Make sure that the output does not resemble "-0.00" by forcing
   * val to 0.0 when we have a very small negative number */
  if ((val <= 0.0) && (val > -pow(0.1, prec+1) * 5.0))
    val = 0.0;

  /* Create a format string to pass into sprintf.  By doing this,
   * we can get sprintf to convert the number to a string, rather
   * than maintaining conversion code ourselves.  */
  i = 0;
  strcpy(&formatString[i], prefix);
  i += strlen(prefix);
  formatString[i] = '0' + prec;  /* add prec to ASCII code for '0' */
  i += 1;
  strcpy(&formatString[i], postfix);
  i += strlen(postfix);

  sprintf(buf, formatString, val);

  return strlen(buf);
}

/********************************************************************\
 * returns TRUE if the string is a number, possibly with whitespace
\********************************************************************/

gboolean
gnc_strisnum(const char *s)
{
  if (s == NULL) return FALSE;
  if (*s == 0) return FALSE;

  while (*s && isspace(*s))
    s++;

  if (*s == 0) return FALSE;
  if (!isdigit(*s)) return FALSE;

  while (*s && isdigit(*s))
    s++;

  if (*s == 0) return TRUE;

  while (*s && isspace(*s))
    s++;

  if (*s == 0) return TRUE;

  return FALSE;
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
\********************************************************************/

static void
gnc_lconv_set(char **p_value, char *default_value)
{
  char *value = *p_value;

  if ((value == NULL) || (value[0] == 0))
    *p_value = default_value;
}

static void
gnc_lconv_set_char(char *p_value, char default_value)
{
  if ((p_value != NULL) && (*p_value == CHAR_MAX))
    *p_value = default_value;
}

struct lconv *
gnc_localeconv(void)
{
  static struct lconv lc;
  static gboolean lc_set = FALSE;

  if (lc_set)
    return &lc;

  lc = *localeconv();

  gnc_lconv_set(&lc.decimal_point, ".");
  gnc_lconv_set(&lc.thousands_sep, ",");
  gnc_lconv_set(&lc.grouping, "\003");
  gnc_lconv_set(&lc.int_curr_symbol, "USD ");
  gnc_lconv_set(&lc.currency_symbol, "$");
  gnc_lconv_set(&lc.mon_decimal_point, ".");
  gnc_lconv_set(&lc.mon_thousands_sep, ",");
  gnc_lconv_set(&lc.mon_grouping, "\003");
  gnc_lconv_set(&lc.negative_sign, "-");

  gnc_lconv_set_char(&lc.frac_digits, 2);
  gnc_lconv_set_char(&lc.int_frac_digits, 2);
  gnc_lconv_set_char(&lc.p_cs_precedes, 1);
  gnc_lconv_set_char(&lc.p_sep_by_space, 0);
  gnc_lconv_set_char(&lc.n_cs_precedes, 1);
  gnc_lconv_set_char(&lc.n_sep_by_space, 0);
  gnc_lconv_set_char(&lc.p_sign_posn, 1);
  gnc_lconv_set_char(&lc.n_sign_posn, 1);

  lc_set = TRUE;

  return &lc;
}

const gnc_commodity *
gnc_locale_default_currency(void)
{
  static gnc_commodity * currency;
  struct lconv         * lc;
  static gboolean      got_it = FALSE;

  if (got_it == FALSE)
  {
    char *symbol;

    lc = gnc_localeconv();

    symbol = g_strdup (lc->int_curr_symbol);

    /* The int_curr_symbol includes a space at the end! Note: you
     * can't just change "USD " to "USD" in gnc_localeconv, because
     * that is only used if int_curr_symbol was not defined in the
     * current locale. If it was, it will have the space! */
    g_strstrip (symbol);

    currency = gnc_commodity_table_lookup (gnc_engine_commodities(),
                                           GNC_COMMODITY_NS_ISO,
                                           symbol);

    g_free (symbol);
    got_it = TRUE;
  }

  return currency;
}


/* Return the number of decimal places for this locale. */
int 
gnc_locale_decimal_places( void )
{
  static gboolean got_it = FALSE;
  static int places;
  struct lconv *lc;

  if( got_it )
    return( places );

  lc = gnc_localeconv();
  places = lc->frac_digits;

  /* frac_digits is already initialized by gnc_localeconv,
   * hopefully to a reasonable default.                    */

  got_it = TRUE;

  return( places );
}


/* Utility function for printing non-negative amounts */
static int
PrintAmt(char *buf, double val, int prec,
         gboolean use_separators,
         gboolean monetary,
         int min_trailing_zeros)
{
  int i, string_length, num_whole_digits;
  struct lconv *lc = gnc_localeconv();
  char decimal_point;
  char temp_buf[50];

  /* check if we're printing infinity */
  if (!finite(val)) {
    strcpy (buf, "inf");
    return 3;
  }

  /* print the absolute value */
  if (val < 0.0)
    val = ABS(val);

  /* print the value without separators */
  util_fptostr(temp_buf, val, prec);

  if (monetary)
    decimal_point = lc->mon_decimal_point[0];
  else
    decimal_point = lc->decimal_point[0];

  /* fix up the decimal place, if there is one */
  string_length = strlen(temp_buf);
  num_whole_digits = -1;

  for (i = string_length - 1; i >= 0; i--)
    if ((temp_buf[i] == '.') ||
        (temp_buf[i] == lc->mon_decimal_point[0]) ||
        (temp_buf[i] == lc->decimal_point[0]))
    {
      temp_buf[i] = decimal_point;
      num_whole_digits = i;
      break;
    }

  if (num_whole_digits < 0)
    num_whole_digits = string_length;  /* Can't find decimal place, it's
                                        * a whole number */

  /* just a quick check */
  assert (num_whole_digits > 0);

  /* Here we strip off trailing decimal zeros per the argument. */
  if (prec > 0)
  {
    int max_delete;
    char *p;

    max_delete = prec - min_trailing_zeros;

    p = temp_buf + strlen(temp_buf) - 1;

    while ((*p == '0') && (max_delete > 0))
    {
      *p-- = '\0';
      max_delete--;
    }

    if (*p == decimal_point)
      *p = '\0';
  }

  if (!use_separators)
  {
    strcpy(buf, temp_buf);
  }
  else
  {
    int group_count;
    char separator;
    char *temp_ptr;
    char *buf_ptr;
    char *group;

    if (monetary)
    {
      separator = lc->mon_thousands_sep[0];
      group = lc->mon_grouping;
    }
    else
    {
      separator = lc->thousands_sep[0];
      group = lc->grouping;
    }

    buf_ptr = buf;
    temp_ptr = &temp_buf[num_whole_digits - 1];
    group_count = 0;

    while (temp_ptr != temp_buf)
    {
      *buf_ptr++ = *temp_ptr--;

      if (*group != CHAR_MAX)
      {
        group_count++;

        if (group_count == *group)
        {
          *buf_ptr++ = separator;
          group_count = 0;

          /* Peek ahead at the next group code */
          switch (group[1])
          {
            /* A null char means repeat the last group indefinitely */
            case '\0':
              break;
            /* CHAR_MAX means no more grouping allowed */
            case CHAR_MAX:
              /* fall through */
            /* Anything else means another group size */
            default:
              group++;
              break;
          }
        }
      }
    }

    /* We built the string backwards, now reverse */
    *buf_ptr++ = *temp_ptr;
    *buf_ptr = '\0';
    g_strreverse(buf);

    strcpy(buf_ptr, &temp_buf[num_whole_digits]);
  } /* endif */

  return strlen(buf);
}

int
DxaccSPrintAmountGeneral (char * bufp, double val,
                         GNCPrintAmountFlags flags,
                         int precision,
                         int min_trailing_zeros,
                         const char *curr_sym)
{
   struct lconv *lc;

   char *orig_bufp = bufp;
   const char *currency_symbol;
   const char *sign;

   char cs_precedes;
   char sep_by_space;
   char sign_posn;

   gboolean print_sign = TRUE;

   if (!bufp) return 0;

   lc = gnc_localeconv();

   if (DEQ(val, 0.0))
     val = 0.0;

   if (flags & PRTSHR)
   {
     currency_symbol = "shrs";
     cs_precedes = 0;  /* currency symbol follows amount */
     sep_by_space = 1; /* they are separated by a space  */
   }
   else
   {
     if (flags & PRTEUR)
     {
       currency_symbol = "EUR ";
     }
     else if (curr_sym == NULL)
     {
       currency_symbol = lc->currency_symbol;
     }
     else
     {
       currency_symbol = curr_sym;
     }

     if (val < 0.0)
     {
       cs_precedes  = lc->n_cs_precedes;
       sep_by_space = lc->n_sep_by_space;
     }
     else
     {
       cs_precedes  = lc->p_cs_precedes;
       sep_by_space = lc->p_sep_by_space;
     }
   }

   if (val < 0.0)
   {
     sign = lc->negative_sign;
     sign_posn = lc->n_sign_posn;
   }
   else
   {
     sign = lc->positive_sign;
     sign_posn = lc->p_sign_posn;
   }

   if ((val == 0.0) || (sign == NULL) || (sign[0] == 0))
     print_sign = FALSE;

   /* See if we print sign now */
   if (print_sign && (sign_posn == 1))
     bufp = stpcpy(bufp, sign);

   /* Now see if we print currency */
   if (cs_precedes)
   {
     /* See if we print sign now */
     if (print_sign && (sign_posn == 3))
       bufp = stpcpy(bufp, sign);

     if (flags & PRTSYM)
     {
       bufp = stpcpy(bufp, currency_symbol);
       if (sep_by_space)
         bufp = stpcpy(bufp, " ");
     }

     /* See if we print sign now */
     if (print_sign && (sign_posn == 4))
       bufp = stpcpy(bufp, sign);
   }

   /* Now see if we print parentheses */
   if (print_sign && (sign_posn == 0))
     bufp = stpcpy(bufp, "(");

   /* Now print the value */
   bufp += PrintAmt(bufp, ABS(val), precision, flags & PRTSEP,
                    !(flags & PRTNMN), min_trailing_zeros);

   /* Now see if we print parentheses */
   if (print_sign && (sign_posn == 0))
     bufp = stpcpy(bufp, ")");

   /* Now see if we print currency */
   if (!cs_precedes)
   {
     /* See if we print sign now */
     if (print_sign && (sign_posn == 3))
       bufp = stpcpy(bufp, sign);

     if (flags & PRTSYM)
     {
       if (sep_by_space)
         bufp = stpcpy(bufp, " ");
       bufp = stpcpy(bufp, currency_symbol);
     }

     /* See if we print sign now */
     if (print_sign && (sign_posn == 4))
       bufp = stpcpy(bufp, sign);
   }

   /* See if we print sign now */
   if (print_sign && (sign_posn == 2))
     bufp = stpcpy(bufp, sign);

   /* return length of printed string */
   return (bufp - orig_bufp);
}

int
DxaccSPrintAmount (char * bufp, double val, GNCPrintAmountFlags flags,
                  const char * curr_code) 
{
   struct lconv *lc;
   int precision;
   int min_trailing_zeros;
   char curr_sym[5];

   lc = gnc_localeconv();

   if (curr_code && (strncmp(curr_code, lc->int_curr_symbol, 3) == 0))
     curr_code = NULL;
   else if (curr_code)
   {
     strncpy(curr_sym, curr_code, 3);
     curr_sym[3] = '\0';
     strcat(curr_sym, " ");
     curr_code = curr_sym;
   }

   if (curr_code && (strncmp(curr_code, "EUR", 3) == 0))
     flags |= PRTEUR;

   if (flags & PRTCUR)
   {
     precision = 5;
     min_trailing_zeros = 0;
   }
   else if (flags & PRTSHR)
   {
     precision = 4;
     min_trailing_zeros = 0;
   }
   else if (flags & PRTEUR)
   {
     precision = 2;
     min_trailing_zeros = 2;
   }
   else
   {
     precision = lc->frac_digits;
     min_trailing_zeros = lc->frac_digits;
   }

   return DxaccSPrintAmountGeneral(bufp, val, flags, precision,
                                  min_trailing_zeros, curr_code);
}

const char *
DxaccPrintAmount (double val, GNCPrintAmountFlags flags, const char *curr_code) 
{
   /* hack alert -- this is not thread safe ... */
   static char buf[BUFSIZE];

   DxaccSPrintAmount (buf, val, flags, curr_code);

   /* its OK to return buf, since we declared it static */
   return buf;
}

const char *
DxaccPrintAmountArgs (double val, gboolean print_currency_symbol,
                     gboolean print_separators, gboolean is_shares_value,
                     const char *curr_code)
{
  GNCPrintAmountFlags flags = 0;

  if (print_currency_symbol) flags |= PRTSYM;
  if (print_separators)      flags |= PRTSEP;
  if (is_shares_value)       flags |= PRTSHR;

  return DxaccPrintAmount(val, flags, curr_code);
}


/********************************************************************\
 * DxaccParseAmount                                                  *
 *   parses amount strings using locale data                        *
 *                                                                  *
 * Args: in_str   -- pointer to string rep of num                   *
 *       monetary -- boolean indicating whether value is monetary   *
 *       result   -- pointer to result location, may be NULL        *
 *       endstr   -- used to store first digit not used in parsing  *
 * Return: gboolean -- TRUE if a number found and parsed            *
 *                     If FALSE, result is not changed              *
\********************************************************************/

/* Parsing state machine states */
typedef enum
{
  START_ST,       /* Parsing initial whitespace */
  NEG_ST,         /* Parsed a negative sign or a left paren */
  PRE_GROUP_ST,   /* Parsing digits before grouping and decimal characters */
  START_GROUP_ST, /* Start of a digit group encountered (possibly) */
  IN_GROUP_ST,    /* Within a digit group */
  FRAC_ST,        /* Parsing the fractional portion of a number */
  DONE_ST,        /* Finished, number is correct module grouping constraints */
  NO_NUM_ST       /* Finished, number was malformed */
} ParseState;

#define done_state(state) (((state) == DONE_ST) || ((state) == NO_NUM_ST))

G_INLINE_FUNC long long multiplier (int num_decimals);

G_INLINE_FUNC long long
multiplier (int num_decimals)
{
  switch (num_decimals)
  {
    case 8:
      return 100000000;
    case 7:
      return 10000000;
    case 6:
      return 1000000;
    case 5:
      return 100000;
    case 4:
      return 10000;
    case 3:
      return 1000;
    case 2:
      return 100;
    case 1:
      return 10;
    default:
      PERR("bad fraction length");
      g_assert_not_reached();
      break;
  }

  return 1;
}

gboolean
DxaccParseAmount (const char * in_str, gboolean monetary, double *result,
                  char **endstr)
{
  gnc_numeric answer;

  if (!xaccParseAmount (in_str, monetary, &answer, endstr))
    return FALSE;

  if (result)
    *result = gnc_numeric_to_double (answer);

  return TRUE;
}

gboolean
xaccParseAmount (const char * in_str, gboolean monetary, gnc_numeric *result,
                 char **endstr)
{
  struct lconv *lc = gnc_localeconv();
  gboolean is_negative;
  gboolean got_decimal;
  gboolean need_paren;
  GList  * group_data;
  int      group_count;
  long long numer;
  long long denom;

  ParseState state;

  char negative_sign;
  char decimal_point;
  char group_separator;
  const char *in;
  char *out_str;
  char *out;

  /* Initialize *endstr to in_str */
  if (endstr != NULL)
    *endstr = (char *) in_str;

  if (in_str == NULL)
    return FALSE;

  negative_sign = lc->negative_sign[0];
  if (monetary)
  {
    group_separator = lc->mon_thousands_sep[0];
    decimal_point = lc->mon_decimal_point[0];
  }
  else
  {
    group_separator = lc->thousands_sep[0];
    decimal_point = lc->decimal_point[0];
  }

  /* 'out_str' will be used to store digits for numeric conversion.
   * 'out' will be used to traverse out_str. */
  out = out_str = g_new(char, strlen(in_str) + 1);

  /* 'in' is used to traverse 'in_str'. */
  in = in_str;

  is_negative = FALSE;
  got_decimal = FALSE;
  need_paren = FALSE;
  group_data = NULL;
  group_count = 0;
  numer = 0;
  denom = 1;

  /* Initialize the state machine */
  state = START_ST;

  /* This while loop implements a state machine for parsing numbers. */
  while (TRUE)
  {
    ParseState next_state = state;

    /* Note we never need to check for then end of 'in_str' explicitly.
     * The 'else' clauses on all the state transitions will handle that. */
    switch (state)
    {
      /* START_ST means we have parsed 0 or more whitespace characters */
      case START_ST:
        if (isdigit(*in))
        {
          *out++ = *in; /* we record the digits themselves in out_str
                         * for later conversion by libc routines */
          next_state = PRE_GROUP_ST;
        }
        else if (*in == decimal_point)
        {
          next_state = FRAC_ST;
        }
        else if (isspace(*in))
        {
        }
        else if (*in == negative_sign)
        {
          is_negative = TRUE;
          next_state = NEG_ST;
        }
        else if (*in == '(')
        {
          is_negative = TRUE;
          need_paren = TRUE;
          next_state = NEG_ST;
        }
        else
        {
          next_state = NO_NUM_ST;
        }

        break;

      /* NEG_ST means we have just parsed a negative sign. For now,
       * we only recognize formats where the negative sign comes first. */
      case NEG_ST:
        if (isdigit(*in))
        {
          *out++ = *in;
          next_state = PRE_GROUP_ST;
        }
        else if (*in == decimal_point)
        {
          next_state = FRAC_ST;
        }
        else if (isspace(*in))
        {
        }
        else
        {
          next_state = NO_NUM_ST;
        }

        break;

      /* PRE_GROUP_ST means we have started parsing the number, but
       * have not encountered a decimal point or a grouping character. */
      case PRE_GROUP_ST:
        if (isdigit(*in))
        {
          *out++ = *in;
        }
        else if (*in == decimal_point)
        {
          next_state = FRAC_ST;
        }
        else if (*in == group_separator)
        {
          next_state = START_GROUP_ST;
        }
        else if (*in == ')' && need_paren)
        {
          next_state = DONE_ST;
          need_paren = FALSE;
        }
        else
        {
          next_state = DONE_ST;
        }

        break;

      /* START_GROUP_ST means we have just parsed a group character.
       * Note that group characters might be whitespace!!! In general,
       * if a decimal point or a group character is whitespace, we
       * try to interpret it in the fashion that will allow parsing
       * of the current number to continue. */
      case START_GROUP_ST:
        if (isdigit(*in))
        {
          *out++ = *in;
          group_count++; /* We record the number of digits
                          * in the group for later checking. */
          next_state = IN_GROUP_ST;
        }
        else if (*in == decimal_point)
        {
          /* If we now get a decimal point, and both the decimal
           * and the group separator are also whitespace, assume
           * the last group separator was actually whitespace and
           * stop parsing. Otherwise, there's a problem. */
          if (isspace(group_separator) && isspace(decimal_point))
            next_state = DONE_ST;
          else
            next_state = NO_NUM_ST;
        }
        else if (*in == ')' && need_paren)
        {
          if (isspace(group_separator))
          {
            next_state = DONE_ST;
            need_paren = FALSE;
          }
          else
            next_state = NO_NUM_ST;
        }
        else
        {
          /* If the last group separator is also whitespace,
           * assume it was intended as such and stop parsing.
           * Otherwise, there is a problem. */
          if (isspace(group_separator))
            next_state = DONE_ST;
          else
            next_state = NO_NUM_ST;
        }
        break;

      /* IN_GROUP_ST means we are in the middle of parsing
       * a group of digits. */
      case IN_GROUP_ST:
        if (isdigit(*in))
        {
          *out++ = *in;
          group_count++; /* We record the number of digits
                          * in the group for later checking. */
        }
        else if (*in == decimal_point)
        {
          next_state = FRAC_ST;
        }
        else if (*in == group_separator)
        {
          next_state = START_GROUP_ST;
        }
        else if (*in == ')' && need_paren)
        {
          next_state = DONE_ST;
          need_paren = FALSE;
        }
        else
        {
          next_state = DONE_ST;
        }

        break;

      /* FRAC_ST means we are now parsing fractional digits. */
      case FRAC_ST:
        if (isdigit(*in))
        {
          *out++ = *in;
        }
        else if (*in == decimal_point)
        {
          /* If a subsequent decimal point is also whitespace,
           * assume it was intended as such and stop parsing.
           * Otherwise, there is a problem. */
          if (isspace(decimal_point))
            next_state = DONE_ST;
          else
            next_state = NO_NUM_ST;
        }
        else if (*in == group_separator)
        {
          /* If a subsequent group separator is also whitespace,
           * assume it was intended as such and stop parsing.
           * Otherwise, there is a problem. */
          if (isspace(group_separator))
            next_state = DONE_ST;
          else
            next_state = NO_NUM_ST;
        }
        else if (*in == ')' && need_paren)
        {
          next_state = DONE_ST;
          need_paren = FALSE;
        }
        else
        {
          next_state = DONE_ST;
        }

        break;

      default:
        PERR("bad state");
        g_assert_not_reached();
        break;
    }

    /* If we're moving out of the IN_GROUP_ST, record data for the group */
    if ((state == IN_GROUP_ST) && (next_state != IN_GROUP_ST))
    {
      group_data = g_list_prepend(group_data, GINT_TO_POINTER(group_count));
      group_count = 0;
    }

    /* If we're moving into the FRAC_ST or out of the machine
     * without going through FRAC_ST, record the integral value. */
    if (((next_state == FRAC_ST) && (state != FRAC_ST)) ||
        ((next_state == DONE_ST) && !got_decimal))
    {
      *out = '\0';

      if (sscanf(out_str, "%lld", &numer) < 1)
      {
        next_state = NO_NUM_ST;
      }
      else if (next_state == FRAC_ST)
      {
        /* reset the out pointer to record the fraction */
        out = out_str;
        *out = '\0';

        got_decimal = TRUE;
      }
    }

    state = next_state;
    if (done_state (state))
      break;

    in++;
  }

  /* If there was an error, just quit */
  if (need_paren || (state == NO_NUM_ST))
  {
    g_free(out_str);
    g_list_free(group_data);
    return FALSE;
  }

  /* If there were groups, validate them */
  if (group_data != NULL)
  {
    gboolean good_grouping = TRUE;
    GList *node;
    char *group;

    group = monetary ? lc->mon_grouping : lc->grouping;

    /* The groups were built in reverse order. This
     * is the easiest order to verify them in. */
    for (node = group_data; node; node = node->next)
    {
      /* Verify group size */
      if (*group != GPOINTER_TO_INT(node->data))
      {
        good_grouping = FALSE;
        break;
      }

      /* Peek ahead at the next group code */
      switch (group[1])
      {
        /* A null char means repeat the last group indefinitely */
        case '\0':
          break;
        /* CHAR_MAX means no more grouping allowed */
        case CHAR_MAX:
          if (node->next != NULL)
            good_grouping = FALSE;
          break;
        /* Anything else means another group size */
        default:
          group++;
          break;
      }

      if (!good_grouping)
        break;
    }

    g_list_free(group_data);

    if (!good_grouping)
    {
      g_free(out_str);
      return FALSE;
    }
  }

  /* Cap the end of the fraction string, if any */
  *out = '\0';

  /* Add in fractional value */
  if (got_decimal && (*out_str != '\0'))
  {
    size_t len;
    long long fraction;

    len = strlen(out_str);

    if (len > 8)
    {
      out_str[8] = '\0';
      len = 8;
    }

    if (sscanf (out_str, "%lld", &fraction) < 1)
    {
      g_free(out_str);
      return FALSE;
    }

    denom = multiplier(len);
    numer *= denom;
    numer += fraction;
  }
  else if (auto_decimal_enabled && !got_decimal)
  {
    /* No decimal point and auto decimal point enabled, so assume
     * that the value is an integer number of cents or a cent-type
     * unit. For each auto decimal place requested, move the final
     * decimal point one place to the left. */
    if ((auto_decimal_places > 0) && (auto_decimal_places < 9))
    {
      denom = multiplier(auto_decimal_places);
      numer *= denom;
    }
  }

  if (result != NULL)
  {
    *result = gnc_numeric_create (numer, denom);
    if (is_negative)
      *result = gnc_numeric_neg (*result);
  }

  if (endstr != NULL)
    *endstr = (char *) in;

  g_free (out_str);

  return TRUE;
}


/************************* END OF FILE ******************************\
\********************************************************************/

/********************************************************************\
 * guile-mappings.h - Guile version compatability mappings          *
 * Copyright (C) 2003, David Hampton                                *
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
\********************************************************************/

/*
 * This include should go away when guile 1.4 is no longer supported
 * and all the gh_xxx mappings have been removed.
 */
#include <guile/gh.h>

/*
 * Configure insists on a minimum version of guile of 1.3.4.
 */
#if ((GNC_GUILE_MAJOR_VERSION == 1) && (GNC_GUILE_MINOR_VERSION < 6))
  #define scm_int2num		gh_int2scm
  #define scm_mem2string(a,b)	gh_str2scm((char *)a,b)
  #define scm_c_eval_string	gh_eval_str
  #define scm_num2int(a,b,c)	gh_scm2int(a)
  #define scm_apply(a,b,c)	gh_apply(a,b)
  #define scm_call_0(fn)	gh_call0(fn)
  #define scm_call_1(fn,a)	gh_call1(fn,a)
  #define scm_call_2(fn,a,b)	gh_call2(fn,a,b)
  #define scm_call_3(fn,a,b,c)	gh_call3(fn,a,b,c)
  #define scm_c_define_gsubr(nm,req,opt,rst,fn) \
  	gh_new_procedure(nm,fn,req,opt,rst)
  #define scm_num2long(a,b,c)	scm_num2long(a, (char *)b, c)
  #define scm_num2ulong(a,b,c)	scm_num2ulong(a, (char *)b, c)

  #if ((GNC_GUILE_MAJOR_VERSION == 1) && (GNC_GUILE_MINOR_VERSION < 4))
    #define scm_make_real	gh_double2scm
    #define SCM_BOOL		gh_bool2scm
    #define SCM_BOOLP		gh_boolean_p
    #define SCM_EQ_P		gh_eq_p
    #ifndef SCM_MAKE_CHAR
       #define SCM_MAKE_CHAR	gh_char2scm
    #endif
    #define scm_str2symbol(a)	gh_symbol2scm((char *)a)

    /* Workaround bugs in guile 1.3.4. */
    #undef  SCM_CONSP
    #define SCM_CONSP(x)	SCM_NFALSEP (scm_pair_p (x))
    #undef  SCM_STRINGP
    #define SCM_STRINGP(x)	gh_string_p(x)
  #else
    #define scm_str2symbol(a)	gh_symbol2scm(a)
  #endif /* Guile < 1.4 */
#endif /* Guile < 1.6 */


/* Convenience macros */

#define SCM_EQUALP(obj1,obj2)	SCM_NFALSEP(scm_equal_p(obj1,obj2))
#define SCM_EXACTP(obj)		SCM_NFALSEP(scm_exact_p(obj))
#define SCM_LISTP(obj)		SCM_NFALSEP(scm_list_p(obj))
#define SCM_PROCEDUREP(obj)	SCM_NFALSEP(scm_procedure_p(obj))

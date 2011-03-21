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

#include <libguile/version.h> /* for SCM_MAJOR_VERSION etc */

/* Give Guile 1.8 a 2.0-like interface */
#if (SCM_MAJOR_VERSION == 1) && (SCM_MINOR_VERSION <= 8)
# define scm_c_string_length scm_i_string_length
#endif

/* Convenience macros */

#define scm_is_equal(obj1,obj2)	scm_is_true(scm_equal_p(obj1,obj2))
#define scm_is_exact(obj)	scm_is_true(scm_exact_p(obj))
#define scm_is_list(obj)	scm_is_true(scm_list_p(obj))
#define scm_is_procedure(obj)	scm_is_true(scm_procedure_p(obj))

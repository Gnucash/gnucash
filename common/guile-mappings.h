/********************************************************************\
 * guile-mappings.h - Guile version compatibility mappings          *
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

#include <libguile.h> /* for SCM_MAJOR_VERSION etc */

/* Convenience macros */

#if defined(scm_to_utf8_string) && SCM_MAJOR_VERSION >= 2
#undef scm_to_utf8_string
#undef scm_from_utf8_string
#undef SWIG_scm2str
#define SWIG_scm2str(s) scm_to_utf8_string(s)
#undef SWIG_str02scm
#define SWIG_str02scm(str) \
  str ? scm_from_utf8_string(str) : SCM_BOOL_F
#endif
#define scm_is_equal(obj1,obj2)	scm_is_true(scm_equal_p(obj1,obj2))
#define scm_is_exact(obj)	scm_is_true(scm_exact_p(obj))
#define scm_is_list(obj)	scm_is_true(scm_list_p(obj))
#define scm_is_procedure(obj)	scm_is_true(scm_procedure_p(obj))

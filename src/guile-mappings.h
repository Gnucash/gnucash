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

/* Convenience macros */

#define SCM_EQUALP(obj1,obj2)	SCM_NFALSEP(scm_equal_p(obj1,obj2))
#define SCM_EXACTP(obj)		SCM_NFALSEP(scm_exact_p(obj))
#define SCM_LISTP(obj)		SCM_NFALSEP(scm_list_p(obj))
#define SCM_PROCEDUREP(obj)	SCM_NFALSEP(scm_procedure_p(obj))

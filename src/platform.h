/********************************************************************\
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef GNC_PLATFORM_H
#define GNC_PLATFORM_H

#include "config.h"

/* PLATFORM handles OS, operating environment, graphics API, and CPU */
#define PLATFORM(GNC_FEATURE) (defined( GNC_PLATFORM_##GNC_FEATURE ) && GNC_PLATFORM_##GNC_FEATURE)
#define COMPILER(GNC_FEATURE) (defined( GNC_COMPILER_##GNC_FEATURE ) && GNC_COMPILER_##GNC_FEATURE)
#define HAVE(GNC_FEATURE) (defined( HAVE_##GNC_FEATURE ) && HAVE_##GNC_FEATURE)
#define USE(GNC_FEATURE) (defined( GNC_USE_##GNC_FEATURE ) && GNC_USE_##GNC_FEATURE)
#define ENABLE(GNC_FEATURE) (defined( ENABLE_##GNC_FEATURE ) && ENABLE_##GNC_FEATURE)

/* Operating systems - low-level dependencies */

/* Operating environments */

/* Graphics engines */

/* CPU */

/* Compiler */

/* COMPILER(MSVC) */
#if defined(_MSC_VER)
#define GNC_COMPILER_MSVC 1
#if _MSC_VER < 1400
#define GNC_COMPILER_MSVC7 1
#endif
#endif

/* COMPILER(RVCT) */
#if defined(__CC_ARM) || defined(__ARMCC__)
#define GNC_COMPILER_RVCT 1
#endif

/* COMPILER(GCC) */
/* --gnu option of the RVCT compiler also defines __GNUC__ */
#if defined(__GNUC__) && !COMPILER(RVCT)
#define GNC_COMPILER_GCC 1
#endif

/* COMPILER(MINGW) */
#if defined(MINGW) || defined(__MINGW32__)
#define GNC_COMPILER_MINGW 1
#endif

/* ENABLE macro defaults */

#endif /* GNC_PLATFORM_H */

/********************************************************************\
 * acconfig.h -- configuration defines for gnucash                  *
 * Copyright (C) 1997 Robin D. Clark (rclark@cs.hmc.edu)            *
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
\********************************************************************/


#ifndef GNC_CONFIG_H
#define GNC_CONFIG_H

/* Package name and version number */
#undef PACKAGE
#undef VERSION

#undef GNUCASH_MAJOR_VERSION
#undef GNUCASH_MINOR_VERSION
#undef GNUCASH_MICRO_VERSION

/* Are we compiling for GNOME?  The answer right now is always yes... */
#undef GNOME

/* Do some memory debugging stuff */
#define DEBUG_MEMORY        

/* limits.h header present */
#undef HAVE_LIMITS_H

/* ieeefp.h header present */
#undef HAVE_IEEEFP_H

/* memcpy present */
#undef HAVE_MEMCPY

/* check for stpcpy for Solaris */
#undef HAVE_STPCPY

/* If configure found libXpm, then use it */
#undef HAVE_XPM            

/* is guile available */
#undef HAVE_GUILE

/* New or old Guile Smob for G-wrap */
#undef GWRAP_OLD_GUILE_SMOB

/* The db1 database library */
#undef PREFER_DB1

/* Should we add guppi support? */
#undef USE_GUPPI

/* Should we have efence linked in */
#undef USE_EFENCE

/* Use the new gtkhtml widget instead of the old xmhtml widget */
#undef HAVE_LIBGTKHTML

/* SSL support for ghttp is still not in the main tree, so test for it
 * separately */
#undef HAVE_LIBGHTTP
#undef HAVE_OPENSSL

/* misc image and compression libs needed by html widget */
#undef HAVE_ZLIB               
#undef HAVE_PNG             
#undef HAVE_JPEG            

/* Configure found the function malloc_usable_size */
#undef HAVE_MALLOC_USABLE_SIZE  


/*** Begin i18n ***/

/* internationalization with gettext */ 
#undef HAVE_GETTEXT

/* internationalization with catgets */ 
#undef HAVE_CATGETS

/* locale.h contains LC_MESSAGES */ 
#undef HAVE_LC_MESSAGES

/* specific locale directory */ 
#undef HAVE_LOCALE_DIR

/* defined if NLS is available */
#undef ENABLE_NLS

/*** End i18n ***/

#undef HAVE_XML_VERSION_HEADER

#endif

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

/* ieeefp.h header present */
#undef HAVE_IEEEFP_H

/* If configure found libXpm, then use it */
#undef HAVE_XPM            

/* is guile available */
#undef HAVE_GUILE

/* Guile version number. Needed because guile < 1.6 doesn't provide this. */
#undef GNC_GUILE_MAJOR_VERSION
#undef GNC_GUILE_MINOR_VERSION
#undef GNC_GUILE_MICRO_VERSION

/* New or old Guile Smob for G-wrap */
#undef GWRAP_OLD_GUILE_SMOB

/* misc image and compression libs needed by html widget */
#undef HAVE_ZLIB               
#undef HAVE_PNG             
#undef HAVE_JPEG            

/* Configure found the function malloc_usable_size */
#undef HAVE_MALLOC_USABLE_SIZE  


/*** Begin i18n ***/

/* internationalization with catgets */ 
#undef HAVE_CATGETS

/* specific locale directory */ 
#undef HAVE_LOCALE_DIR

/*** End i18n ***/

#undef HAVE_XML_VERSION_HEADER

#endif

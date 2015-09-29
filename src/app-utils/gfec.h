/*  Authors: Eric M. Ludlam <zappo@ultranet.com>
 *           Russ McManus <russell.mcmanus@gs.com>
 *           Dave Peticolas <dave@krondo.com>
 *
 *  gfec stands for 'guile fancy error catching'.
 *  This code is in the public domain.
 */
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


#ifndef GFEC_H
#define GFEC_H

#include <libguile.h>
#include <glib.h>
#include "guile-mappings.h"

typedef void (*gfec_error_handler)(const char *error_message);

SCM gfec_eval_file(const char *file, gfec_error_handler error_handler);
SCM gfec_eval_string(const char *str, gfec_error_handler error_handler);
SCM gfec_apply(SCM proc, SCM arglist, gfec_error_handler error_handler);
gboolean gfec_try_load(gchar *fn);

#endif

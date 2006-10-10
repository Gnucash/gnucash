/*  Authors: Eric M. Ludlam <zappo@ultranet.com>
 *           Russ McManus <russell.mcmanus@gs.com>
 *           Dave Peticolas <dave@krondo.com>
 *
 *  gfec stands for 'guile fancy error catching'.
 *  This code is in the public domain.
 */

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

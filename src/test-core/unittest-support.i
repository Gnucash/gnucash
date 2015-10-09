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

%module unittest_support
%{
#include "config.h"
#include "unittest-support.h"
%}

#if defined(SWIGGUILE)
%{
#include "guile-mappings.h"

SCM scm_init_unittest_support_module (void);
%}
#endif

#if defined(SWIGPYTHON)
%{
/* avoid no previous prototype warning/error */
#if PY_VERSION_HEX >= 0x03000000
PyObject*
#else
void
#endif
SWIG_init (void);
%}
#endif

%import "base-typemaps.i"

typedef struct
{
    GLogLevelFlags log_level;
    gchar *log_domain;
    gchar *msg;
} TestErrorStruct;

typedef enum {
  /* log flags */
  G_LOG_FLAG_RECURSION          = 1 << 0,
  G_LOG_FLAG_FATAL              = 1 << 1,

  /* GLib log levels */
  G_LOG_LEVEL_ERROR             = 1 << 2,       /* always fatal */
  G_LOG_LEVEL_CRITICAL          = 1 << 3,
  G_LOG_LEVEL_WARNING           = 1 << 4,
  G_LOG_LEVEL_MESSAGE           = 1 << 5,
  G_LOG_LEVEL_INFO              = 1 << 6,
  G_LOG_LEVEL_DEBUG             = 1 << 7,

  G_LOG_LEVEL_MASK              = ~(G_LOG_FLAG_RECURSION | G_LOG_FLAG_FATAL)
} GLogLevelFlags;

typedef gboolean (*GLogFunc) (const gchar *log_domain,
				       GLogLevelFlags log_level,
				       const gchar *message,
				       gpointer user_data);

void test_add_error (TestErrorStruct *error);
void test_clear_error_list (void);
guint test_set_checked_handler (const char *domain, GLogLevelFlags level,
				gpointer data);
guint test_set_list_handler (const char *domain, GLogLevelFlags level,
				gpointer data);
guint test_set_null_handler (const char *domain, GLogLevelFlags level,
				gpointer data);

%{

static guint
test_set_checked_handler (const char *domain, GLogLevelFlags level,
			  gpointer data)
{
    return g_log_set_handler (domain, level,
			      (GLogFunc)test_checked_handler, data);
}
static guint
test_set_list_handler (const char *domain, GLogLevelFlags level,
			  gpointer data)
{
    return g_log_set_handler (domain, level,
			      (GLogFunc)test_list_handler, data);
}
static guint
test_set_null_handler (const char *domain, GLogLevelFlags level,
			  gpointer data)
{
    return g_log_set_handler (domain, level,
			      (GLogFunc)test_null_handler, data);
}
%}
void g_log_remove_handler (const char *log_domain, guint handler);


/* *****************************************************************\
 * deprecated.c -- QOF deprecated function replacements            *
 * Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>          *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "qof.h"

/* Don't be fooled: gnc_trace_num_spaces has external linkage and
   static storage, but can't be defined with 'extern' because it has
   an initializer, and can't be declared with 'static' because that
   would give it internal linkage. (this is why it is deprecated) */
gint __attribute__ ((unused)) gnc_trace_num_spaces = 0;
void  gnc_log_init (void) { qof_log_init(); }
void gnc_set_log_level(QofLogModule log_module, gncLogLevel level)
{
	qof_log_set_level(log_module, (QofLogLevel)level);
}
void gnc_set_log_level_global(gncLogLevel level)
{
	qof_log_set_level_registered((QofLogLevel)level);
}
void qof_log_set_level_global(QofLogLevel level)
{
	qof_log_set_level_registered((QofLogLevel)level);
}
void gnc_set_logfile (FILE *outfile)
{
	qof_log_set_file(outfile);
}
const char * gnc_log_prettify (const char *name)
{
	return qof_log_prettify(name);
}
void gnc_start_clock (int a, QofLogModule b, gncLogLevel c,  const char *d, const char *e, ...) { }
void gnc_report_clock (int a, QofLogModule b, gncLogLevel c, const char *d, const char *e, ...) { }
void gnc_report_clock_total (int a, QofLogModule b, gncLogLevel c, const char *d, const char *e, ...) { }
gboolean gnc_should_log(QofLogModule log_module, gncLogLevel log_level)
{
	return qof_log_check(log_module, log_level);
}

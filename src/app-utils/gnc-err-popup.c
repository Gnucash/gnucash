/********************************************************************\
 * gnc-err-popup.c -- GnuCash error GUI popups                      *
 * Copyright (c) 2001 Linux Developers Group, Inc.                  *
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
\********************************************************************/

#include "config.h"

#include <glib.h>
#include <stdarg.h>
#include <string.h>

#include "gnc-err-popup.h"


/********************************************************************\
  Callbacks so that app can display gui messages.
\********************************************************************/

static GNCGuiMessage gnc_gui_warning_func = NULL;
static GNCGuiMessage gnc_gui_error_func = NULL;

void 
gnc_set_warning_message (GNCGuiMessage func)
{
  gnc_gui_warning_func = func;
}

void 
gnc_set_error_message (GNCGuiMessage func)
{
  gnc_gui_error_func = func;
}

gboolean
gnc_send_gui_warning(const gchar *format, ...)
{
  va_list args;

  if (!gnc_gui_warning_func) return FALSE;

  va_start (args, format);
  gnc_gui_warning_func(format, args);
  va_end(args);
  return(TRUE);
}

gboolean
gnc_send_gui_error(const gchar *format, ...)
{
  va_list args;

  if (!gnc_gui_error_func) return(FALSE);

  va_start (args, format);
  gnc_gui_error_func(format, args);
  va_end(args);
  return(TRUE);
}


/************************* END OF FILE ******************************\
\********************************************************************/

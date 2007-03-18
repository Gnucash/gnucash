/********************************************************************\
 * print-session.c -- simple printing manager for gnucash           *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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

#include <gnome.h>
#include <glib/gi18n.h>
#include <libgnomeprint/gnome-font.h>
#include <libgnomeprintui/gnome-print-job-preview.h>

#include "print-session.h"


PrintSession * 
gnc_print_session_create(gboolean hand_built_pages)
{
  PrintSession * ps = g_new0(PrintSession, 1);
  GnomePrintConfig *config;
  GtkWidget *dialog;
  gint response;

  /* Ask the user what to do with the output */
  config = gnome_print_config_default();
  ps->job = gnome_print_job_new(config);
  g_object_unref(config);
  dialog = gnome_print_dialog_new(ps->job, (guchar *) _("Print GnuCash Document"), 0);
  response = gtk_dialog_run(GTK_DIALOG(dialog));
  switch (response) {
    case GNOME_PRINT_DIALOG_RESPONSE_PRINT: 
    case GNOME_PRINT_DIALOG_RESPONSE_PREVIEW:
      gtk_widget_destroy(dialog);
      ps->context = gnome_print_job_get_context(ps->job);
      break;
    
    default:
      gtk_widget_destroy(dialog);
      g_object_unref(ps->job);
      g_free(ps);
      return NULL;
  }

  ps->hand_built_pages = hand_built_pages;
  ps->print_type = response;

  ps->default_font = gnome_font_find_closest((guchar *)"Sans Regular", 12);

  if (hand_built_pages) {
    gnome_print_beginpage(ps->context, (guchar *)"");
    gnome_print_setrgbcolor(ps->context, 0.0, 0.0, 0.0);
    gnome_print_setfont(ps->context, ps->default_font);
  }
  return ps;
}

void 
gnc_print_session_destroy(PrintSession * ps)
{
  g_object_unref(ps->job);
  g_object_unref(ps->default_font);

  g_free(ps);
}


void
gnc_print_session_done(PrintSession * ps)
{
  GtkWidget *widget;

  if (ps->hand_built_pages) {
    gnome_print_showpage(ps->context);
  }
  gnome_print_job_close (ps->job);

  switch (ps->print_type) {
    case GNOME_PRINT_DIALOG_RESPONSE_PRINT: 
      gnome_print_job_print(ps->job);
      break;

    case GNOME_PRINT_DIALOG_RESPONSE_PREVIEW:
      widget = gnome_print_job_preview_new(ps->job, (guchar*)"Print Preview");
      gtk_widget_show(widget);
      break;
    
    default:
      break;
  }
}

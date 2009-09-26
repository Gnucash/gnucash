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

#ifdef HAVE_GTK_2_10
#    include <gtk/gtkprintoperation.h>
#endif

#if !WANT_WEBKIT && !defined(GTKHTML_USES_GTKPRINT)
#    include <gnome.h>
#    include <glib/gi18n.h>
#    include <libgnomeprint/gnome-font.h>
#    include <libgnomeprintui/gnome-print-job-preview.h>
#endif

#include "print-session.h"
#include "gnc-gconf-utils.h" /* for gnc_gconf_set_string() */

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.printing"

#ifdef HAVE_GTK_2_10
/* Do not treat -Wstrict-aliasing warnings as errors because of problems of the
 * G_LOCK* macros as declared by glib.  See
 * http://bugzilla.gnome.org/show_bug.cgi?id=316221 for additional information.
 */
#    if (__GNUC__ >= 4 && __GNUC_MINOR__ >= 2)
#        pragma GCC diagnostic warning "-Wstrict-aliasing"
#    endif

static GtkPrintSettings *print_settings = NULL;
static GtkPageSetup *page_setup = NULL;
G_LOCK_DEFINE_STATIC(print_settings);
G_LOCK_DEFINE_STATIC(page_setup);
#endif


#ifdef HAVE_GTK_2_10
void
gnc_print_operation_save_print_settings(GtkPrintOperation *op)
{
  g_return_if_fail(op);

  G_LOCK(print_settings);
  if (print_settings)
    g_object_unref(print_settings);
  print_settings = g_object_ref(gtk_print_operation_get_print_settings(op));
  G_UNLOCK(print_settings);
}

void
gnc_print_operation_init(GtkPrintOperation *op)
{
  g_return_if_fail(op);

  /* Restore print settings */
  G_LOCK(print_settings);
  if (print_settings)
    gtk_print_operation_set_print_settings(op, print_settings);
  G_UNLOCK(print_settings);

  /* Restore page setup */
  G_LOCK(page_setup);
  if (page_setup)
    gtk_print_operation_set_default_page_setup(op, page_setup);
  G_UNLOCK(page_setup);
}

void
gnc_ui_page_setup(GtkWindow *parent)
{
  GtkPrintSettings *settings = NULL;
  GtkPageSetup *old_page_setup, *new_page_setup;

  /* Get a reference to the current print settings */
  G_LOCK(print_settings);
  settings = print_settings;
  if (settings)
    g_object_ref(settings);
  G_UNLOCK(print_settings);

  /* Get a reference to the current page setup */
  G_LOCK(page_setup);
  old_page_setup = page_setup;
  if (old_page_setup)
    g_object_ref(old_page_setup);
  G_UNLOCK(page_setup);

  /* Run dialog */
  new_page_setup = gtk_print_run_page_setup_dialog(parent, old_page_setup,
                                                   settings);

  /* Save new page setup */
  G_LOCK(page_setup);
  if (page_setup)
    g_object_unref(page_setup);
  page_setup = new_page_setup;
  G_UNLOCK(page_setup);

  /* Release references */
  if (settings)
    g_object_unref(settings);
  if (old_page_setup)
    g_object_unref(old_page_setup);
}
#endif  /* HAVE_GTK_2_10 */


#if !WANT_WEBKIT && !defined(GTKHTML_USES_GTKPRINT)
static void gnc_print_session_fontsel_cb(GtkButton *widget, gpointer user_data)
{
  PrintSession *ps = (PrintSession *)user_data;
  GtkWidget *dialog;
  gint response;

  dialog = gtk_font_selection_dialog_new("GnuCash Print Font");
  if (ps->pango_font_string == NULL) {
    GtkStyle *style = gtk_style_new();
    ps->pango_font_string = pango_font_description_to_string(style->font_desc);
    g_object_unref(style);
  }
  if (ps->pango_font_string != NULL)
      gtk_font_selection_dialog_set_font_name((GtkFontSelectionDialog *)dialog, ps->pango_font_string);

  response = gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);

  switch (response) {
    case GTK_RESPONSE_OK:
      g_free(ps->pango_font_string);
      ps->pango_font_string = gtk_font_selection_dialog_get_font_name((GtkFontSelectionDialog *)dialog);
      gnc_gconf_set_string(NULL, "pango_font_string", ps->pango_font_string, NULL);
      break;

    default:
      break;
  }
}

PrintSession * 
gnc_print_session_create(gboolean hand_built_pages)
{
  PrintSession * ps = g_new0(PrintSession, 1);
  GnomePrintConfig *config;
  GtkWidget *dialog;
  GtkWidget *button;
  gint response;

  ps->default_font = NULL;
  ps->pango_font_string = gnc_gconf_get_string (NULL, "pango_font_string", NULL);

  /* Ask the user what to do with the output */
  config = gnome_print_config_default();
  ps->job = gnome_print_job_new(config);
  g_object_unref(config);
  dialog = gnome_print_dialog_new(ps->job, (guchar *) _("Print GnuCash Document"), 0);

  button = gtk_button_new_from_stock(GTK_STOCK_SELECT_FONT);
  g_signal_connect(button, "clicked", G_CALLBACK(gnc_print_session_fontsel_cb), ps);
  gtk_container_add (GTK_CONTAINER (GTK_DIALOG(dialog)->action_area), button);
  gtk_button_box_set_child_secondary((GtkButtonBox *)GTK_DIALOG(dialog)->action_area, button, TRUE);
  gtk_widget_show(button); /* shouldn't be needed but is */

  response = gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);

  switch (response) {
    case GNOME_PRINT_DIALOG_RESPONSE_PRINT: 
    case GNOME_PRINT_DIALOG_RESPONSE_PREVIEW:
      break;

    default:
      gnc_print_session_destroy(ps);
      return NULL;
  }

  ps->hand_built_pages = hand_built_pages;
  ps->print_type = response;
  ps->context = gnome_print_job_get_context(ps->job);

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
  if (ps->default_font != NULL)
    g_object_unref(ps->default_font);
  g_free(ps->pango_font_string);

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
  gnc_print_session_destroy(ps);
}
#endif  /* !GTKHTML_USES_GTKPRINT */

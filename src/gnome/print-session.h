/********************************************************************\
 * print-session.h -- data structures for printing via gnome print  *  
 *                       (GnuCash)                                  *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#ifndef __PRINT_SESSION_H_
#define __PRINT_SESSION_H_

#include "config.h"

#ifdef HAVE_LIBGNOMEPRINT

#include <gnome.h>
#include <libgnomeprint/gnome-printer.h>
#include <libgnomeprint/gnome-print.h>
#include <libgnomeprint/gnome-print-meta.h>
#include <libgnomeprint/gnome-print-preview.h>
#include <libgnomeprint/gnome-printer-dialog.h>
#include <libgnomeprint/gnome-printer-profile.h>
#include <libgnomeprint/gnome-font.h>

#include "glade-gnc-dialogs.h"

typedef struct {
  GnomePrintMeta     * meta;
  GnomePrinter       * printer;
  GnomeFont          * default_font;
} PrintSession;

typedef struct {
  GtkWidget         * toplevel;
  GtkWidget         * canvas;
  GnomePrintContext * pc;
  PrintSession      * session;
} PrintPreviewDialog;

typedef struct {
  GtkWidget         * toplevel;
  PrintSession      * session;
} PrintDialog;

#else 

/* type stubs for g-wrap */
typedef int PrintSession;
typedef int PrintPreviewDialog;
typedef int PrintDialog;

#endif

/* print preview dialog stuff */
PrintPreviewDialog * gnc_ui_print_preview_create(PrintSession * ps);
void gnc_ui_print_preview_OK_cb(GtkWidget * widget, gpointer user_data);
void gnc_ui_print_preview_destroy(PrintPreviewDialog * ppd);

/* print check dialog stuff */
PrintDialog * gnc_ui_print_dialog_create(PrintSession * ps);
void gnc_ui_print_dialog_destroy(PrintDialog * pcd);

void gnc_ui_print_dialog_select_printer_cb(GtkWidget * widget, 
                                           gpointer user_data);
void gnc_ui_print_dialog_preview_cb(GtkWidget * widget, gpointer user_data);
void gnc_ui_print_dialog_ok_cb(GtkWidget * widget, gpointer user_data);
void gnc_ui_print_dialog_cancel_cb(GtkWidget * widget, gpointer user_data);


/* printsession stuff */
PrintSession * gnc_print_session_create();
void gnc_print_session_destroy(PrintSession * ps);

void gnc_print_session_moveto(PrintSession * ps, double x, double y);
void gnc_print_session_text(PrintSession * ps, char * text);
void gnc_print_session_done(PrintSession * ps);

void gnc_print_session_preview(PrintSession * ps);
void gnc_print_session_print(PrintSession * ps);

#endif

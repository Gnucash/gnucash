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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef PRINT_SESSION_H
#define PRINT_SESSION_H

#include "config.h"

#include <gnome.h>
#include <libgnomeprint/gnome-printer.h>
#include <libgnomeprint/gnome-print.h>
#include <libgnomeprint/gnome-print-meta.h>
#include <libgnomeprint/gnome-print-preview.h>
#include <libgnomeprint/gnome-printer-dialog.h>
#include <libgnomeprint/gnome-print-dialog.h>
#include <libgnomeprint/gnome-print-master.h>
#include <libgnomeprint/gnome-print-master-preview.h>
/* #include <libgnomeprint/gnome-printer-profile.h> */
#include <libgnomeprint/gnome-font.h>

typedef struct {
  GnomePrintMaster   * master;
  GnomePrintMeta     * meta;
  GnomeFont          * default_font;
  char               * paper;
} PrintSession;

typedef struct {
  GtkWidget         * toplevel;
  GtkWidget         * canvas;
  GnomePrintContext * pc;
  PrintSession      * session;
} PrintPreviewDialog;

typedef struct {
  GtkWidget         * toplevel;
  GtkWidget         * printer_entry;
  GtkWidget         * paper_entry;
  PrintSession      * session;
} PrintDialog;

typedef struct {
  GtkWidget         * toplevel;
  GtkWidget         * papersel;
  GtkWidget         * entry;
  PrintSession      * session;
} PaperDialog;


/* paper selector dialog */
PaperDialog * gnc_ui_paper_dialog_create(PrintSession * ps, GtkWidget * entry);
void gnc_ui_paper_dialog_destroy(PaperDialog * psd);

/* print preview dialog stuff */
PrintPreviewDialog * gnc_ui_print_preview_create(PrintSession * ps);
void gnc_ui_print_preview_destroy(PrintPreviewDialog * ppd);

/* print check dialog stuff */
PrintDialog * gnc_ui_print_dialog_create(PrintSession * ps);
void gnc_ui_print_dialog_destroy(PrintDialog * pcd);

/* printsession stuff */
PrintSession * gnc_print_session_create(gboolean);
void gnc_print_session_destroy(PrintSession * ps);

void gnc_print_session_moveto(PrintSession * ps, double x, double y);
void gnc_print_session_text(PrintSession * ps, const char * text);
void gnc_print_session_done(PrintSession * ps, gboolean);

void gnc_print_session_preview(PrintSession * ps);
void gnc_print_session_print(PrintSession * ps);
void gnc_print_session_render(PrintSession * ps);

#endif

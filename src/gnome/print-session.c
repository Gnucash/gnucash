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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#define _GNU_SOURCE

#include "config.h"

#include <stdio.h>
#include <gnome.h>

#include "window-help.h"
#include "messages.h"
#include "print-session.h"
#include "gnc-ui.h"
#include "glade-cb-gnc-dialogs.h"


PrintSession * 
gnc_print_session_create() {
  PrintSession * ps = g_new0(PrintSession, 1);

  /* this is about the most basic we can get */
  ps->master       = gnome_print_master_new();
  ps->meta         = gnome_print_meta_new();
  ps->default_font = gnome_font_new("Courier", 12);

  gnome_print_setrgbcolor(GNOME_PRINT_CONTEXT(ps->meta),
                          0.0, 0.0, 0.0);
  gnome_print_setfont(GNOME_PRINT_CONTEXT(ps->meta), 
                      GNOME_FONT(ps->default_font));

  return ps;
}

void 
gnc_print_session_destroy(PrintSession * ps) {
  gtk_object_unref(GTK_OBJECT(ps->meta));
  gtk_object_unref(GTK_OBJECT(ps->master));
  gtk_object_unref(GTK_OBJECT(ps->default_font));

  g_free(ps);
}

void 
gnc_print_session_moveto(PrintSession * ps, double x, double y) {
  gnome_print_moveto(GNOME_PRINT_CONTEXT(ps->meta), x, y);
}


void 
gnc_print_session_text(PrintSession * ps, const char * text) {
  gnome_print_show(GNOME_PRINT_CONTEXT(ps->meta), text);  
}


void
gnc_print_session_done(PrintSession * ps) {
  gnome_print_showpage(GNOME_PRINT_CONTEXT(ps->meta));
  gnome_print_context_close(GNOME_PRINT_CONTEXT(ps->meta));

}


void
gnc_print_session_print(PrintSession * ps) {
  GtkWidget * dialog    = gnome_print_dialog_new("Print Gnucash Document", 0);
  int button            = gnome_dialog_run(GNOME_DIALOG(dialog));
  
  switch(button) {
  case 0: 
    /* print button */
    if(ps->master) {
      gtk_object_unref(GTK_OBJECT(ps->master));
      ps->master = NULL;
    }
    ps->master = 
      gnome_print_master_new_from_dialog(GNOME_PRINT_DIALOG(dialog));
    gnome_dialog_close(GNOME_DIALOG(dialog));
    gnc_print_session_render(ps);
    break;
    
  case 1:
    if(ps->master) {
      gtk_object_unref(GTK_OBJECT(ps->master));
      ps->master = NULL;
    }
    ps->master = 
      gnome_print_master_new_from_dialog(GNOME_PRINT_DIALOG(dialog));
    gnome_dialog_close(GNOME_DIALOG(dialog));
    gnc_print_session_preview(ps);    
    break;

  case 2:
    gnome_dialog_close(GNOME_DIALOG(dialog));
    break;
  }
}


void 
gnc_print_session_render(PrintSession * ps) {
  GnomePrintContext * pc = gnome_print_master_get_context(ps->master);
  
  gnome_print_meta_render_from_object(GNOME_PRINT_CONTEXT(pc),
                                      GNOME_PRINT_META(ps->meta)); 
  gnome_print_context_close(pc);
  gnome_print_master_print(ps->master);
}

void 
gnc_print_session_preview(PrintSession * ps) {
  GnomePrintContext * pc = gnome_print_master_get_context(ps->master);
  GnomePrintMasterPreview * preview;

  gnome_print_meta_render_from_object(GNOME_PRINT_CONTEXT(pc),
                                      GNOME_PRINT_META(ps->meta)); 
  gnome_print_context_close(pc);
  
  preview = gnome_print_master_preview_new(ps->master, "Print Preview");
  gtk_widget_show_all(GTK_WIDGET(preview));
}

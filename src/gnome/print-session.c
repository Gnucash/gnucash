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

#include "top-level.h"
#include "window-help.h"
#include "messages_i18n.h"
#include "print-session.h"
#include "ui-callbacks.h"
#include "glade-cb-gnc-dialogs.h"


PrintPreviewDialog *
gnc_ui_print_preview_create(PrintSession * ps) {
  PrintPreviewDialog * ppd = g_new0(PrintPreviewDialog, 1);
  ppd->toplevel = create_Print_Preview_Dialog();
  ppd->canvas   = gtk_object_get_data(GTK_OBJECT(ppd->toplevel),
                                      "preview_canvas");
  ppd->session  = ps;
  ppd->pc = 
    gnome_print_preview_new (GNOME_CANVAS(ppd->canvas), 
                             ps->paper);
  
  gtk_object_set_data(GTK_OBJECT(ppd->toplevel), "print_preview_struct",
                      ppd);

  gnome_print_meta_render_from_object(GNOME_PRINT_CONTEXT(ppd->pc),
                                      GNOME_PRINT_META(ppd->session->meta)); 
  
  gnome_print_context_close(GNOME_PRINT_CONTEXT(ppd->pc));
  gtk_widget_show_all(ppd->toplevel);

  return ppd;
}

void
gnc_ui_print_preview_OK_cb(GtkButton * button, gpointer user_data) {
  PrintPreviewDialog * ppd;

  if(user_data) {
    ppd = gtk_object_get_data(GTK_OBJECT(user_data), "print_preview_struct");
    gnc_ui_print_preview_destroy(ppd);
  }
}


void
gnc_ui_print_preview_destroy(PrintPreviewDialog * ppd) {
  gnome_dialog_close(GNOME_DIALOG(ppd->toplevel));

  /* gtk_object_unref(GTK_OBJECT(ppd->toplevel)); */
  gtk_object_unref(GTK_OBJECT(ppd->pc)); 
  
  ppd->session = NULL;
  g_free(ppd);
}


PrintDialog *
gnc_ui_print_dialog_create(PrintSession * ps) {
  PrintDialog * pcd = g_new0(PrintDialog, 1);
  char        * printer_string;

  pcd->toplevel = create_Print_Dialog();
  pcd->session  = ps;
  
  pcd->paper_entry = gtk_object_get_data(GTK_OBJECT(pcd->toplevel),
                                         "paper_entry");

  pcd->printer_entry = gtk_object_get_data(GTK_OBJECT(pcd->toplevel),
                                           "printer_entry");
  
  gtk_object_set_data(GTK_OBJECT(pcd->toplevel), "print_struct",
                      pcd);
  
  if(ps->printer->driver) {
    if(ps->printer->filename) {
      asprintf(&printer_string, "(%s) %s",
               ps->printer->driver, ps->printer->filename);
    }
    else {
      printer_string = ps->printer->driver;
    }
  }
  else {
    printer_string = _("(none)");
  }

  gtk_entry_set_text(GTK_ENTRY(pcd->paper_entry), ps->paper);
  gtk_entry_set_text(GTK_ENTRY(pcd->printer_entry), printer_string);
  gtk_widget_show_all(pcd->toplevel);

  return pcd;
}

void
gnc_ui_paper_dialog_cancel_cb(GtkButton * button, gpointer user_data) {
  PaperDialog * psd  = gtk_object_get_data(GTK_OBJECT(user_data),
                                           "paper_struct");
  gnc_ui_paper_dialog_destroy(psd);
}

void
gnc_ui_paper_dialog_ok_cb(GtkButton * button, gpointer user_data) {
  PaperDialog * psd  = gtk_object_get_data(GTK_OBJECT(user_data),
                                           "paper_struct");
  char * newpaper = g_strdup(gnome_paper_selector_get_name
                             (GNOME_PAPER_SELECTOR(psd->papersel)));
  gtk_entry_set_text(GTK_ENTRY(psd->entry), newpaper);
  psd->session->paper = newpaper;
  gnc_ui_paper_dialog_destroy(psd);
  
}


PaperDialog *
gnc_ui_paper_dialog_create(PrintSession * session, GtkWidget * entry) {
  PaperDialog * psd = g_new0(PaperDialog, 1);

  psd->toplevel = create_Paper_Size_Selector_Dialog();
  psd->papersel = gtk_object_get_data(GTK_OBJECT(psd->toplevel),
                                      "paperselector1");
  psd->session  = session;
  psd->entry    = entry;

  gtk_widget_ref(psd->entry);
  gtk_object_set_data(GTK_OBJECT(psd->toplevel),
                      "paper_struct", psd);
  gtk_widget_show_all(psd->toplevel);
  return psd;
}


void
gnc_ui_paper_dialog_destroy(PaperDialog * psd) {
  gnome_dialog_close(GNOME_DIALOG(psd->toplevel));
  gtk_widget_unref(psd->entry);
  g_free(psd);
}


void
gnc_ui_print_dialog_destroy(PrintDialog * pcd) {
  gnome_dialog_close(GNOME_DIALOG(pcd->toplevel));
  gnc_print_session_destroy(pcd->session);
  g_free(pcd);
}


void
gnc_ui_print_dialog_select_printer_cb(GtkButton * button, gpointer user_data) {
  PrintDialog  * pcd;
  GnomePrinter * printer;
  char         * printer_string;

  if(user_data) {
    pcd = gtk_object_get_data(GTK_OBJECT(user_data), "print_struct");    
    printer = 
      gnome_printer_dialog_new_modal();

    if(printer) {
      pcd->session->printer = printer;

      if(pcd->session->printer->driver) {
        if(pcd->session->printer->filename) {
          asprintf(&printer_string, "(%s) %s",
                   pcd->session->printer->driver, 
                   pcd->session->printer->filename);
        }
        else {
          printer_string = pcd->session->printer->driver;
        }
      }
      else {
        printer_string = _("(none)");
      }

      gtk_entry_set_text(GTK_ENTRY(pcd->printer_entry), printer_string);
    }
  }
}

void
gnc_ui_print_dialog_select_paper_cb(GtkButton * button, gpointer user_data) {
  PrintDialog * pcd;

  if(user_data) {
    pcd = gtk_object_get_data(GTK_OBJECT(user_data), "print_struct");    
    gnc_ui_paper_dialog_create(pcd->session, pcd->paper_entry);
  }
}

void
gnc_ui_print_dialog_preview_cb(GtkButton * button, gpointer user_data) {
  PrintDialog * pcd;
  if(user_data) {
    pcd = gtk_object_get_data(GTK_OBJECT(user_data), "print_struct");    
    gnc_print_session_preview(pcd->session);
  }
}

void
gnc_ui_print_dialog_ok_cb(GtkButton * button, gpointer user_data) {
  PrintDialog * pcd;
  if(user_data) {
    pcd = gtk_object_get_data(GTK_OBJECT(user_data), "print_struct");    
    if(!pcd->session->printer) {
      gnc_error_dialog(_("You must select a printer first."));
    }
    else {
      gnc_print_session_print(pcd->session);
      gnc_print_session_done(pcd->session);
      gnc_ui_print_dialog_destroy(pcd);
    }
  }  
}

void
gnc_ui_print_dialog_cancel_cb(GtkButton * button, gpointer user_data) {
  PrintDialog * pcd;
  if(user_data) {
    pcd = gtk_object_get_data(GTK_OBJECT(user_data), "print_struct");    
    gnc_ui_print_dialog_destroy(pcd);
  }
}

void
gnc_ui_print_dialog_help_cb(GtkButton * button, gpointer user_data) {
  helpWindow(NULL, HELP_STR, HH_PRINT);
}


PrintSession * 
gnc_print_session_create() {
  PrintSession * ps = g_new0(PrintSession, 1);

  /* this is about the most basic we can get */
  ps->meta         = gnome_print_meta_new();
  ps->default_font = gnome_font_new("Courier", 12);
  ps->printer      = gnome_printer_new_generic_ps("gnucash-output.ps");
  ps->paper        = g_strdup(gnome_paper_name_default());

  gnome_print_setrgbcolor(GNOME_PRINT_CONTEXT(ps->meta),
                          0.0, 0.0, 0.0);
  gnome_print_setfont(GNOME_PRINT_CONTEXT(ps->meta), 
                      GNOME_FONT(ps->default_font));

  return ps;
}

void 
gnc_print_session_destroy(PrintSession * ps) {
  gtk_object_unref(GTK_OBJECT(ps->meta));
  if(ps->printer) 
    gtk_object_unref(GTK_OBJECT(ps->printer));

  gtk_object_unref(GTK_OBJECT(ps->default_font));

  g_free(ps->paper);
  g_free(ps);
}

void 
gnc_print_session_moveto(PrintSession * ps, double x, double y) {
  gnome_print_moveto(GNOME_PRINT_CONTEXT(ps->meta), x, y);
}


void 
gnc_print_session_text(PrintSession * ps, char * text) {
  gnome_print_show(GNOME_PRINT_CONTEXT(ps->meta), text);  
}


void
gnc_print_session_done(PrintSession * ps) {
  gnome_print_showpage(GNOME_PRINT_CONTEXT(ps->meta));
  gnome_print_context_close(GNOME_PRINT_CONTEXT(ps->meta));

}

void 
gnc_print_session_preview(PrintSession * ps) {

  gnc_ui_print_preview_create(ps);

}

void 
gnc_print_session_print(PrintSession * ps) {
  GnomePrintContext * pc;

  pc = gnome_print_context_new(ps->printer);
  gnome_print_meta_render_from_object(GNOME_PRINT_CONTEXT(pc),
                                      GNOME_PRINT_META(ps->meta)); 
  gnome_print_context_close(pc);
  gtk_object_unref(GTK_OBJECT(pc));
}

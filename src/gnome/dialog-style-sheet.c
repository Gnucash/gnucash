/********************************************************************
 * dialog-style-sheet.c -- window for configuring HTML style        *
 *                         sheets in GnuCash                        *
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
 ********************************************************************/

#include "config.h"

#include <gnome.h>

#include "dialog-style-sheet.h"
#include "dialog-options.h"
#include "glade-gnc-dialogs.h"
#include "messages.h"

StyleSheetDialog * gnc_style_sheet_dialog = NULL;

struct _stylesheetdialog {
  GtkWidget  * toplevel;
  GtkWidget  * list;
  GtkWidget  * options_frame;
  struct ss_info * selection;
};

struct ss_info {
  GNCOptionWin  * odialog;
  GNCOptionDB   * odb;
  SCM           stylesheet;
};


static void
gnc_style_sheet_options_apply_cb(GNCOptionWin * propertybox,
                                 gpointer user_data) {
  struct ss_info * ssi = (struct ss_info *)user_data;
  SCM    apply_changes = gh_eval_str("gnc:html-style-sheet-apply-changes");
  gnc_option_db_commit(ssi->odb);
  gh_call1(apply_changes, ssi->stylesheet);
}


static void
gnc_style_sheet_options_close_cb(GNCOptionWin * propertybox,
                                 gpointer user_data) {
  StyleSheetDialog * ss = user_data;
  gtk_widget_hide(GTK_WIDGET(ss->toplevel));
}


static void
gnc_style_sheet_dialog_select_cb(GtkCList * list, gint row, gint col,
                                 GdkEvent * ev, gpointer user_data) {
  StyleSheetDialog * ssd = (StyleSheetDialog *)user_data;
  struct ss_info   * ssinfo = gtk_clist_get_row_data(list, row);
  GList * children = gtk_container_children(GTK_CONTAINER(ssd->options_frame));
  
  if(children) {
    gtk_container_remove(GTK_CONTAINER(ssd->options_frame), 
                         GTK_WIDGET(children->data));
  }
  gtk_container_add(GTK_CONTAINER(ssd->options_frame), 
                    gnc_options_dialog_widget(ssinfo->odialog));
  gtk_widget_show(gnc_options_dialog_widget(ssinfo->odialog));
  gtk_widget_show(ssd->options_frame);
  ssd->selection = ssinfo;
}

static void
row_data_destroy_cb(gpointer data) {
  struct ss_info * ss = data;
  gnc_option_db_destroy(ss->odb);
  gnc_options_dialog_destroy(ss->odialog);
  scm_unprotect_object(ss->stylesheet);
  g_free(ss);
}


static void
gnc_style_sheet_dialog_fill(StyleSheetDialog * ss, SCM selected) {
  SCM stylesheets = gh_eval_str("(gnc:get-html-style-sheets)");
  SCM get_options = gh_eval_str("gnc:html-style-sheet-options");
  SCM get_name    = gh_eval_str("gnc:html-style-sheet-name");
  int sel_row = 0;

  /* pack it full of content */
  for(; !gh_null_p(stylesheets); stylesheets=gh_cdr(stylesheets)) {
    SCM            scm_name = gh_call1(get_name, gh_car(stylesheets));
    SCM            scm_options = gh_call1(get_options, gh_car(stylesheets));
    struct ss_info * ssinfo = g_new0(struct ss_info, 1);
    int            this_row;
    char           * c_names[1];
    char           * c_name;

    /* make the options DB and dialog, but don't parent it yet */ 
    ssinfo->odialog = gnc_options_dialog_new(FALSE, NULL);
    ssinfo->odb     = gnc_option_db_new(scm_options);
    ssinfo->stylesheet = gh_car(stylesheets);

    scm_protect_object(ssinfo->stylesheet);
    gtk_widget_ref(gnc_options_dialog_widget(ssinfo->odialog));

    gnc_build_options_dialog_contents(ssinfo->odialog, 
                                      ssinfo->odb);
    gnc_options_dialog_set_apply_cb(ssinfo->odialog, 
                                    gnc_style_sheet_options_apply_cb,
                                    ssinfo);    
    gnc_options_dialog_set_close_cb(ssinfo->odialog, 
                                    gnc_style_sheet_options_close_cb,
                                    ss);

    /* add the column name */
    c_name = gh_scm2newstr(scm_name, NULL);
    if (!c_name)
      continue;

    c_names[0] = _(c_name);
    this_row   = gtk_clist_append(GTK_CLIST(ss->list), c_names);
    free (c_name);

    gtk_clist_set_row_data_full(GTK_CLIST(ss->list), this_row, 
                                (gpointer)ssinfo, 
                                row_data_destroy_cb);
    if(gh_eq_p(selected, gh_car(stylesheets))) {
      sel_row = this_row;
    }
  }

  gtk_clist_select_row(GTK_CLIST(ss->list), sel_row, 0);
}


static void
gnc_style_sheet_new_cb(GtkWidget * w, gpointer user_data) {
  StyleSheetDialog * ssd = user_data;
  SCM              make_ss   = gh_eval_str("gnc:make-html-style-sheet");
  SCM              templates = gh_eval_str("(gnc:get-html-templates)");
  SCM              t_name = gh_eval_str("gnc:html-style-sheet-template-name");
  GtkWidget        * template_entry;
  GtkWidget        * name_entry;
  GtkWidget        * template_combo;
  GList            * strings=NULL;
  GList            * lptr;
  gint             dialog_retval;
  char             * template_str = NULL;
  char             * name_str = NULL;

  /* get the new name for the style sheet */
  GtkWidget * dlg = create_New_Style_Sheet_Dialog();
  GtkObject * tlo = GTK_OBJECT(dlg);
  template_entry = gtk_object_get_data(tlo, "template_entry");
  template_combo = gtk_object_get_data(tlo, "template_combo");
  name_entry     = gtk_object_get_data(tlo, "name_entry");

  /* put in the list of style sheet type names */
  for(; !gh_null_p(templates); templates=gh_cdr(templates)) {
    SCM t = gh_car(templates);
    strings = g_list_append(strings, gh_scm2newstr(gh_call1(t_name, t), 
                                                   NULL));
  }
  
  gtk_combo_set_popdown_strings(GTK_COMBO(template_combo), strings);
  
  /* free the strings list */
  for(lptr = strings; lptr; lptr = lptr->next) {
    free(lptr->data);
  }
  g_list_free(strings);

  /* get the name */
  dialog_retval = gnome_dialog_run(GNOME_DIALOG(dlg));

  if(dialog_retval == 0) {
    template_str = gtk_entry_get_text(GTK_ENTRY(template_entry));
    name_str     = gtk_entry_get_text(GTK_ENTRY(name_entry));
    if(template_str && name_str) {
      SCM new_ss = gh_call2(make_ss, 
                            gh_str02scm(template_str),
                            gh_str02scm(name_str));
      gtk_clist_clear(GTK_CLIST(ssd->list));
      gnc_style_sheet_dialog_fill(ssd, new_ss);
    }
  }  
  gnome_dialog_close(GNOME_DIALOG(dlg));
}


static void
gnc_style_sheet_delete_cb(GtkWidget * w, gpointer user_data) {
  StyleSheetDialog * ssd = user_data;
  struct ss_info   * ssi = ssd->selection;
  SCM remover = gh_eval_str("gnc:html-style-sheet-remove");
  gh_call1(remover, ssi->stylesheet);
  gtk_clist_clear(GTK_CLIST(ssd->list));
  gnc_style_sheet_dialog_fill(ssd, SCM_BOOL_F);
}


static int
gnc_style_sheet_dialog_close_cb(GtkWidget * w, GdkEventAny * ev,
                                gpointer user_data) {
  StyleSheetDialog * ss = user_data;
  gtk_widget_hide(GTK_WIDGET(ss->toplevel));
  return TRUE;
}

static StyleSheetDialog *
gnc_style_sheet_dialog_create() {
  StyleSheetDialog  * ss = g_new0(StyleSheetDialog, 1);
  GtkObject         * tlo;
  GtkWidget         * new_button=NULL;
  GtkWidget         * delete_button=NULL;
  ss->toplevel   = create_HTML_Style_Sheet_Dialog();

  tlo = GTK_OBJECT(ss->toplevel);
  ss->list          = gtk_object_get_data(tlo, "style_sheet_list");
  ss->options_frame = gtk_object_get_data(tlo, "style_sheet_options");
  new_button        = gtk_object_get_data(tlo, "new_button");
  delete_button     = gtk_object_get_data(tlo, "delete_button");

  gtk_signal_connect(GTK_OBJECT(ss->list), "select_row",
                     GTK_SIGNAL_FUNC(gnc_style_sheet_dialog_select_cb), ss);
  gtk_signal_connect(GTK_OBJECT(new_button), "clicked",
                     GTK_SIGNAL_FUNC(gnc_style_sheet_new_cb), ss);
  gtk_signal_connect(GTK_OBJECT(delete_button), "clicked",
                     GTK_SIGNAL_FUNC(gnc_style_sheet_delete_cb), ss); 
  gtk_signal_connect(GTK_OBJECT(ss->toplevel), "delete_event",
                     GTK_SIGNAL_FUNC(gnc_style_sheet_dialog_close_cb), ss); 
  
  gnc_style_sheet_dialog_fill(ss, SCM_BOOL_F);

  gtk_window_set_policy(GTK_WINDOW(ss->toplevel), TRUE, TRUE, FALSE);

  gtk_clist_columns_autosize (GTK_CLIST (ss->list));

  gtk_widget_show(ss->toplevel);

  return ss;
}


void
gnc_style_sheet_dialog_open(void) {
  if(gnc_style_sheet_dialog) {
    gtk_widget_show(gnc_style_sheet_dialog->toplevel);
  }
  else {
    gnc_style_sheet_dialog = 
      gnc_style_sheet_dialog_create();
  }
}


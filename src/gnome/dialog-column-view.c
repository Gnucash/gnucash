/********************************************************************
 * dialog-column-view.c -- editor for simple column view of reports *
 * Copyright (C) 2001 Bill Gribble <grib@billgribble.com>           *
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
#include <guile/gh.h>

#include "dialog-column-view.h"
#include "dialog-options.h"
#include "glade-cb-gnc-dialogs.h"
#include "glade-gnc-dialogs.h"
#include "messages.h"
#include "option-util.h"


struct gncp_column_view_edit {
  GNCOptionWin * optwin;
  GtkCList  * available;
  GtkCList  * contents; 

  SCM          options; 
  SCM          view;
  GNCOptionDB  * odb;

  SCM       available_list;
  int       available_selected;

  SCM       contents_list;
  int       contents_selected;
};

static void
update_display_lists(gnc_column_view_edit * view) {
  SCM   get_names = gh_eval_str("gnc:all-report-template-names");
  SCM   find_report = gh_eval_str("gnc:find-report");
  SCM   get_options = gh_eval_str("gnc:report-options");
  SCM   get_option  = gh_eval_str("gnc:lookup-option");
  SCM   get_value  = gh_eval_str("gnc:option-value");
  SCM   names = gh_call0(get_names);
  SCM   contents = 
    gnc_option_db_lookup_option(view->odb, "__general", "report-list",
                                SCM_BOOL_F);
  SCM   this_report, this_options, this_name;
  char  * name[3];

  scm_unprotect_object(view->available_list);
  view->available_list = names;
  scm_protect_object(view->available_list);

  gtk_clist_clear(view->available);
  if(gh_list_p(names)) {
    for(; !gh_null_p(names); names = gh_cdr(names)) {
      name[0] = gh_scm2newstr(gh_car(names), NULL);      
      gtk_clist_append(view->available, name);
      free(name[0]);
    }
  }
  gtk_clist_select_row(view->available, view->available_selected, 0);
  
  scm_unprotect_object(view->contents_list);
  view->contents_list = contents;
  scm_protect_object(view->contents_list);
  
  gtk_clist_clear(view->contents);
  if(gh_list_p(contents)) {
    for(; !gh_null_p(contents); contents = gh_cdr(contents)) {
      this_report = gh_call1(find_report, gh_caar(contents));
      this_options = gh_call1(get_options, this_report);
      this_name = gh_call1(get_value, 
                           gh_call3(get_option, this_options, 
                                    gh_str02scm("General"), 
                                    gh_str02scm("Report name")));
      name[0] = gh_scm2newstr(this_name, NULL);
      name[1] = g_strdup_printf("%d", gh_scm2int(gh_cadr(gh_car(contents))));
      name[2] = g_strdup_printf("%d", gh_scm2int(gh_caddr(gh_car(contents))));
      gtk_clist_append(view->contents, name);
      free(name[0]);
      g_free(name[1]);
      g_free(name[2]);
    }
  }
  gtk_clist_select_row(view->contents, view->contents_selected, 0);
}

static void
gnc_column_view_select_avail_cb(GtkCList * clist, gint row, gint col,
                                GdkEvent * ev, gpointer data) {
  gnc_column_view_edit * r = data;
  r->available_selected = row;  
}

static void
gnc_column_view_select_contents_cb(GtkCList * clist, gint row, gint col,
                                   GdkEvent * ev, gpointer data) {
  gnc_column_view_edit * r = data;
  r->contents_selected = row;  
}

static void
gnc_column_view_edit_apply_cb(GNCOptionWin * w, gpointer user_data) {
  SCM  dirty_report = gh_eval_str("gnc:report-set-dirty?!");
  gnc_column_view_edit * win = user_data;
  
  if(!win) return;
  gnc_option_db_commit(win->odb);
  gh_call2(dirty_report, win->view, SCM_BOOL_T);
}

static void
gnc_column_view_edit_close_cb(GNCOptionWin * win, gpointer user_data) {
  gnc_column_view_edit * r = user_data;
  gnc_column_view_edit_destroy(r);
}


/********************************************************************
 * gnc_column_view_edit_new
 * create the editor. 
 ********************************************************************/

gnc_column_view_edit * 
gnc_column_view_edit_new(SCM options, SCM view) {
  gnc_column_view_edit * r = g_new0(gnc_column_view_edit, 1);
  GtkObject * tlo;
  GtkWidget * editor;

  r->optwin = gnc_options_dialog_new(TRUE);

  tlo = GTK_OBJECT(create_Edit_Column_View_Page());
  
  editor       = gtk_object_get_data(tlo, "view_contents_hbox");
  r->available = gtk_object_get_data(tlo, "available_list");
  r->contents  = gtk_object_get_data(tlo, "contents_list");  
  r->options   = options;
  r->view      = view;
  r->available_selected = 0;
  r->available_list = SCM_EOL;
  r->contents_selected = 0;
  r->contents_list = SCM_EOL;
  r->odb       = gnc_option_db_new(r->options);

  gnc_build_options_dialog_contents(r->optwin, r->odb);

  gtk_widget_ref(editor);
  gtk_container_remove(GTK_CONTAINER(tlo), editor);
  gtk_notebook_append_page(GTK_NOTEBOOK(gnc_options_dialog_notebook
                                        (r->optwin)),
                           editor, 
                           gtk_label_new(_("Contents")));
  
  scm_protect_object(r->options);
  scm_protect_object(r->view);
  scm_protect_object(r->available_list);
  scm_protect_object(r->contents_list);
  
  gtk_object_set_data(tlo, "view_edit_struct", (gpointer)r);

  gtk_signal_connect(GTK_OBJECT(r->available), "select_row", 
                     gnc_column_view_select_avail_cb, (gpointer)r);
  gtk_signal_connect(GTK_OBJECT(r->contents), "select_row", 
                     gnc_column_view_select_contents_cb, (gpointer)r);
  
  update_display_lists(r);

  gnc_options_dialog_set_apply_cb(r->optwin, 
                                  gnc_column_view_edit_apply_cb, r);
  gnc_options_dialog_set_close_cb(r->optwin, 
                                  gnc_column_view_edit_close_cb, r);

  gtk_widget_show_all(gnc_options_dialog_widget(r->optwin));
  return r;
}


void 
gnc_column_view_edit_destroy(gnc_column_view_edit * view) {
  gnc_options_dialog_destroy(view->optwin);
  scm_unprotect_object(view->options);
  scm_unprotect_object(view->view);
  gnc_option_db_destroy(view->odb);
  g_free(view);
}

void
gnc_column_view_edit_add_cb(GtkButton * button, gpointer user_data) {
  gnc_column_view_edit * r = 
    gtk_object_get_data(GTK_OBJECT(user_data), "view_edit_struct");
  SCM make_report = gh_eval_str("gnc:make-report");
  SCM find_report = gh_eval_str("gnc:find-report");
  SCM add_child = gh_eval_str("gnc:report-add-child-by-id!");
  SCM add_parent = gh_eval_str("gnc:report-add-parent!");
  SCM template_name;
  SCM set_value = gh_eval_str("gnc:option-set-value");
  SCM new_report;
  SCM newlist = SCM_EOL;
  SCM oldlist = r->contents_list;
  int count;
  int oldlength; 

  if(gh_list_p(r->available_list) && 
     (gh_length(r->available_list) > r->available_selected)) {
    template_name = gh_list_ref(r->available_list, 
                                gh_int2scm(r->available_selected));
    new_report = gh_call1(make_report, template_name);
    gh_call2(add_child, r->view, new_report);
    gh_call2(add_parent, gh_call1(find_report, new_report), r->view);
    
    oldlength = gh_length(r->contents_list);
    
    if(oldlength > r->contents_selected) {
      for(count = 0; count < r->contents_selected; count++) {
        newlist = gh_cons(gh_car(oldlist), newlist);
        oldlist = gh_cdr(oldlist);
      }
      newlist = gh_append2(gh_reverse(gh_cons(SCM_LIST3(new_report,
                                                        gh_int2scm(1),
                                                        gh_int2scm(1)), 
                                              newlist)),
                           oldlist);
    }
    else {
      newlist = gh_append2(oldlist, 
                           SCM_LIST1(SCM_LIST3(new_report,
                                               gh_int2scm(1),
                                               gh_int2scm(1))));
      r->contents_selected = oldlength;
    }
    
    scm_unprotect_object(r->contents_list);
    r->contents_list = newlist;
    scm_protect_object(r->contents_list);
    
    gnc_option_db_set_option(r->odb, "__general", "report-list",
                             r->contents_list);

    gnc_options_dialog_changed (r->optwin);
  }

  update_display_lists(r);
}

void
gnc_column_view_edit_remove_cb(GtkButton * button, gpointer user_data) {
  gnc_column_view_edit * r = 
    gtk_object_get_data(GTK_OBJECT(user_data), "view_edit_struct");
  SCM newlist = SCM_EOL;
  SCM oldlist = r->contents_list;
  int count;
  int oldlength;
  
  printf("remove cb\n");

  if(gh_list_p(r->contents_list)) {
    oldlength = gh_length(r->contents_list);
    if(oldlength > r->contents_selected) {
      for(count=0; count < r->contents_selected; count++) {
        newlist = gh_cons(gh_car(oldlist), newlist);
        oldlist = gh_cdr(oldlist);
      }
      if(count <= oldlength) {
        newlist = gh_append2(gh_reverse(newlist), gh_cdr(oldlist));
      }
    }
    
    if(oldlength == r->contents_selected + 1) {
      r->contents_selected --;
    }

    scm_unprotect_object(r->contents_list);
    r->contents_list = newlist;
    scm_protect_object(r->contents_list);

    gnc_option_db_set_option(r->odb, "__general", "report-list",
                             r->contents_list);

    gnc_options_dialog_changed (r->optwin);
  }

  update_display_lists(r);
}

void
gnc_edit_column_view_move_up_cb(GtkButton * button, gpointer user_data) {
  gnc_column_view_edit * r = 
    gtk_object_get_data(GTK_OBJECT(user_data), "view_edit_struct");
  SCM oldlist = r->contents_list;
  SCM newlist = SCM_EOL;
  SCM temp;
  int oldlength;
  int count;

  oldlength = gh_length(r->contents_list);
  if((r->contents_selected > 0) && (oldlength > r->contents_selected)) {
    for(count = 1; count < r->contents_selected; count++) {
      newlist = gh_cons(gh_car(oldlist), newlist);
      oldlist = gh_cdr(oldlist);
    }
    temp = gh_car(oldlist);
    oldlist = gh_cdr(oldlist);
    newlist = gh_cons(temp, gh_cons(gh_car(oldlist), newlist));
    newlist = gh_append2(gh_reverse(newlist), gh_cdr(oldlist));

    scm_unprotect_object(r->contents_list);
    r->contents_list = newlist;
    scm_protect_object(r->contents_list);

    r->contents_selected = r->contents_selected - 1;

    gnc_option_db_set_option(r->odb, "__general", "report-list",
                             r->contents_list);

    gnc_options_dialog_changed (r->optwin);

    update_display_lists(r);
  }
}

void
gnc_edit_column_view_move_down_cb(GtkButton * button, gpointer user_data) {
  gnc_column_view_edit * r = 
    gtk_object_get_data(GTK_OBJECT(user_data), "view_edit_struct");
  SCM oldlist = r->contents_list;
  SCM newlist = SCM_EOL;
  SCM temp;
  int oldlength;
  int count;

  oldlength = gh_length(r->contents_list);
  if(oldlength > (r->contents_selected + 1)) {
    for(count = 0; count < r->contents_selected; count++) {
      newlist = gh_cons(gh_car(oldlist), newlist);
      oldlist = gh_cdr(oldlist);
    }
    temp = gh_car(oldlist);
    oldlist = gh_cdr(oldlist);
    newlist = gh_cons(temp, gh_cons(gh_car(oldlist), newlist));
    newlist = gh_append2(gh_reverse(newlist), gh_cdr(oldlist));

    scm_unprotect_object(r->contents_list);
    r->contents_list = newlist;
    scm_protect_object(r->contents_list);

    r->contents_selected = r->contents_selected + 1;

    gnc_option_db_set_option(r->odb, "__general", "report-list",
                             r->contents_list);    

    gnc_options_dialog_changed (r->optwin);

    update_display_lists(r);
  }
}

void
gnc_column_view_edit_size_cb(GtkButton * button, gpointer user_data) {
  gnc_column_view_edit * r = 
    gtk_object_get_data(GTK_OBJECT(user_data), "view_edit_struct");
  
  GtkWidget * dlg = create_Edit_Report_Size();
  SCM current;
  int length;
  int dlg_ret;

  /* get the spinner widgets */ 
  GtkWidget * rowspin = gtk_object_get_data(GTK_OBJECT(dlg),
                                            "row_spin");
  GtkWidget * colspin = gtk_object_get_data(GTK_OBJECT(dlg),
                                            "col_spin");
  
  length = gh_length(r->contents_list);
  if(length > r->contents_selected) {
    current = gh_list_ref(r->contents_list, 
                          gh_int2scm(r->contents_selected));
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(colspin),
                              (float)gh_scm2int(gh_cadr(current)));
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(rowspin),
                              (float)gh_scm2int(gh_caddr(current)));
  
    dlg_ret = gnome_dialog_run_and_close(GNOME_DIALOG(dlg));

    if(dlg_ret == 0) {
      current = SCM_LIST3(gh_car(current),
                          gh_int2scm(gtk_spin_button_get_value_as_int
                                     (GTK_SPIN_BUTTON(colspin))),
                          gh_int2scm(gtk_spin_button_get_value_as_int
                                     (GTK_SPIN_BUTTON(rowspin))));
      scm_unprotect_object(r->contents_list);
      r->contents_list = scm_list_set_x(r->contents_list, 
                                        gh_int2scm(r->contents_selected),
                                        current);
      scm_protect_object(r->contents_list);
      gnc_options_dialog_changed (r->optwin);
      update_display_lists(r);
    }
  }
}

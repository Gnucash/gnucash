/********************************************************************
 * dialog-column-view.c -- editor for simple column view of reports *
 * Copyright (C) 2001 Bill Gribble <grib@billgribble.com>           *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
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
 ********************************************************************/

#include "config.h"

#include <glib/gi18n.h>
#include <libguile.h>
#include "swig-runtime.h"

#include "dialog-column-view.h"
#include "dialog-options.h"
#include "dialog-utils.h"
#include "option-util.h"
#include "window-report.h"
#include "guile-mappings.h"
#include "gnc-report.h"

enum available_cols {
  AVAILABLE_COL_NAME = 0,
  AVAILABLE_COL_ROW,
  NUM_AVAILABLE_COLS
};

enum contents_cols {
  CONTENTS_COL_NAME = 0,
  CONTENTS_COL_ROW,
  CONTENTS_COL_REPORT_ROWS,
  CONTENTS_COL_REPORT_COLS,
  NUM_CONTENTS_COLS
};

struct gncp_column_view_edit {
  GNCOptionWin * optwin;
  GtkTreeView  * available;
  GtkTreeView  * contents; 

  SCM          options; 
  SCM          view;
  GNCOptionDB  * odb;

  SCM       available_list;
  int       available_selected;

  SCM       contents_list;
  int       contents_selected;
};

static void gnc_column_view_edit_add_cb(GtkButton * button,
                                        gpointer user_data);
static void gnc_column_view_edit_remove_cb(GtkButton * button,
                                           gpointer user_data);
static void gnc_edit_column_view_move_up_cb(GtkButton * button,
                                            gpointer user_data);
static void gnc_edit_column_view_move_down_cb(GtkButton * button,
                                              gpointer user_data);
static void gnc_column_view_edit_size_cb(GtkButton * button,
                                         gpointer user_data);


static void
gnc_column_view_set_option(GNCOptionDB * odb, char * section, char * name,
                           SCM new_value)
{
  GNCOption *  option = 
    gnc_option_db_get_option_by_name(odb, section, name);
  
  if(option)
  {
    gnc_option_db_set_option(odb, section, name, new_value);

    /* set_option doesn't do this */
    gnc_option_set_changed (option, TRUE);
  }
}

static void 
gnc_column_view_edit_destroy(gnc_column_view_edit * view)
{
  gnc_options_dialog_destroy(view->optwin);
  scm_gc_unprotect_object(view->options);
  scm_gc_unprotect_object(view->view);
  gnc_option_db_destroy(view->odb);
  g_free(view);
}

static void
update_display_lists(gnc_column_view_edit * view)
{
  SCM   get_names = scm_c_eval_string("gnc:all-report-template-names");
  SCM   template_menu_name = scm_c_eval_string("gnc:report-template-menu-name/report-guid");
  SCM   report_menu_name = scm_c_eval_string("gnc:report-menu-name");
  SCM   names = scm_call_0(get_names);
  SCM   contents = 
    gnc_option_db_lookup_option(view->odb, "__general", "report-list",
                                SCM_BOOL_F);
  SCM   this_report;
  SCM   selection;
  const gchar *name;
  int   row, i, id;
  GtkListStore *store;
  GtkTreeIter iter;
  GtkTreePath *path;
  GtkTreeSelection *tree_selection;


  /* Update the list of available reports (left selection box). */
  row = view->available_selected;

  if(SCM_LISTP(view->available_list) && !SCM_NULLP (view->available_list)) {
    row = MIN (row, scm_ilength (view->available_list) - 1);
    selection = scm_list_ref (view->available_list, scm_int2num (row));
  }
  else {
    selection = SCM_UNDEFINED;
  }

  scm_gc_unprotect_object(view->available_list);
  view->available_list = names;
  scm_gc_protect_object(view->available_list);

  store = GTK_LIST_STORE(gtk_tree_view_get_model(view->available));
  gtk_list_store_clear(store);

  if(SCM_LISTP(names)) {
    for(i = 0; !SCM_NULLP(names); names = SCM_CDR(names), i++) {
      if (SCM_EQUALP (SCM_CAR(names), selection))
        row = i;
      name = _(SCM_STRING_CHARS(scm_call_2(template_menu_name, SCM_CAR(names),
    		  SCM_BOOL_F)));
      gtk_list_store_append(store, &iter);
      gtk_list_store_set(store, &iter,
			 AVAILABLE_COL_NAME, name,
			 AVAILABLE_COL_ROW, i,
			 -1);
    }

  }
  
  tree_selection = gtk_tree_view_get_selection(view->available);
  path = gtk_tree_path_new_from_indices(row, -1);
  gtk_tree_selection_select_path(tree_selection, path);
  gtk_tree_path_free(path);


  /* Update the list of selected reports (right selection box). */
  row = view->contents_selected;

  if(SCM_LISTP(view->contents_list) && !SCM_NULLP (view->contents_list)) {
    row = MIN (row, scm_ilength (view->contents_list) - 1);
    selection = scm_list_ref (view->contents_list, scm_int2num (row));
  }
  else {
    selection = SCM_UNDEFINED;
  }

  scm_gc_unprotect_object(view->contents_list);
  view->contents_list = contents;
  scm_gc_protect_object(view->contents_list);

  store = GTK_LIST_STORE(gtk_tree_view_get_model(view->contents));
  gtk_list_store_clear(store);
  if(SCM_LISTP(contents)) {
    for(i = 0; !SCM_NULLP(contents); contents = SCM_CDR(contents), i++) {
      if (SCM_EQUALP (SCM_CAR(contents), selection))
        row = i;

      id = scm_num2int(SCM_CAAR(contents), SCM_ARG1, G_STRFUNC);
      this_report = gnc_report_find(id);
      name = _(SCM_STRING_CHARS(scm_call_1(report_menu_name, this_report)));

      gtk_list_store_append(store, &iter);
      gtk_list_store_set
	(store, &iter,
	 CONTENTS_COL_NAME, name,
	 CONTENTS_COL_ROW, i,
	 CONTENTS_COL_REPORT_COLS, scm_num2int(SCM_CADR(SCM_CAR(contents)),
					       SCM_ARG1, G_STRFUNC),
	 CONTENTS_COL_REPORT_ROWS, scm_num2int(SCM_CADDR(SCM_CAR(contents)),
					       SCM_ARG1, G_STRFUNC),
	 -1);
    }
  }
  
  tree_selection = gtk_tree_view_get_selection(view->contents);
  path = gtk_tree_path_new_from_indices(row, -1);
  gtk_tree_selection_select_path(tree_selection, path);
  //  gtk_tree_view_scroll_to_cell(view->contents, path, NULL, TRUE, 0.5, 0.0);
  gtk_tree_path_free(path);
}

static void
gnc_column_view_select_avail_cb(GtkTreeSelection *selection,
				gnc_column_view_edit *r)
{
  GtkTreeModel *model;
  GtkTreeIter iter;

  if (gtk_tree_selection_get_selected(selection, &model, &iter))
    gtk_tree_model_get(model, &iter,
		       AVAILABLE_COL_ROW, &r->available_selected,
		       -1);
}

static void
gnc_column_view_select_contents_cb(GtkTreeSelection *selection,
				   gnc_column_view_edit *r)
{
  GtkTreeModel *model;
  GtkTreeIter iter;

  if (gtk_tree_selection_get_selected(selection, &model, &iter))
    gtk_tree_model_get(model, &iter,
		       AVAILABLE_COL_ROW, &r->contents_selected,
		       -1);
}

static void
gnc_column_view_edit_apply_cb(GNCOptionWin * w, gpointer user_data)
{
  SCM  dirty_report = scm_c_eval_string("gnc:report-set-dirty?!");
  gnc_column_view_edit * win = user_data;
  
  if(!win) return;
  gnc_option_db_commit(win->odb);
  scm_call_2(dirty_report, win->view, SCM_BOOL_T);
}

static void
gnc_column_view_edit_close_cb(GNCOptionWin * win, gpointer user_data)
{
  gnc_column_view_edit * r = user_data;
  SCM set_editor = scm_c_eval_string("gnc:report-set-editor-widget!");
  
  scm_call_2(set_editor, r->view, SCM_BOOL_F);
  gnc_column_view_edit_destroy(r);
}


/********************************************************************
 * gnc_column_view_edit_options
 * create the editor. 
 ********************************************************************/

GtkWidget * 
gnc_column_view_edit_options(SCM options, SCM view)
{
  SCM get_editor = scm_c_eval_string("gnc:report-editor-widget");
  SCM ptr;
  GtkWidget * editor;
  GtkListStore *store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkTreeSelection *selection;

  ptr = scm_call_1(get_editor, view);
  if(ptr != SCM_BOOL_F) {
#define FUNC_NAME "gtk_window_present"
    GtkWindow * w = SWIG_MustGetPtr(ptr, SWIG_TypeQuery("_p_GtkWidget"), 1, 0);
    gtk_window_present(w);
#undef FUNC_NAME
    return NULL;
  }
  else {
    gnc_column_view_edit * r = g_new0(gnc_column_view_edit, 1);
    GladeXML *xml;

    r->optwin = gnc_options_dialog_new(NULL);

    /* Hide the generic dialog page list. */
    {
      GtkWidget *dialog, *page_list;

      dialog = gnc_options_dialog_widget(r->optwin);
      page_list = gnc_glade_lookup_widget (dialog, "page_list");
      gtk_widget_hide(page_list);
    }

    xml = gnc_glade_xml_new ("report.glade", "view_contents_table");

    glade_xml_signal_connect_data
      (xml, "gnc_column_view_edit_add_cb",
       G_CALLBACK (gnc_column_view_edit_add_cb), r);

    glade_xml_signal_connect_data
      (xml, "gnc_column_view_edit_remove_cb",
       G_CALLBACK (gnc_column_view_edit_remove_cb), r);

    glade_xml_signal_connect_data
      (xml, "gnc_edit_column_view_move_up_cb",
       G_CALLBACK (gnc_edit_column_view_move_up_cb), r);

    glade_xml_signal_connect_data
      (xml, "gnc_edit_column_view_move_down_cb",
       G_CALLBACK (gnc_edit_column_view_move_down_cb), r);

    glade_xml_signal_connect_data
      (xml, "gnc_column_view_edit_size_cb",
       G_CALLBACK (gnc_column_view_edit_size_cb), r);

    editor       = glade_xml_get_widget (xml, "view_contents_table");
    r->available = GTK_TREE_VIEW (glade_xml_get_widget (xml, "available_view"));
    r->contents  = GTK_TREE_VIEW (glade_xml_get_widget (xml, "contents_view"));
    r->options   = options;
    r->view      = view;
    r->available_selected = 0;
    r->available_list = SCM_EOL;
    r->contents_selected = 0;
    r->contents_list = SCM_EOL;
    r->odb       = gnc_option_db_new(r->options);

    gnc_options_dialog_build_contents(r->optwin, r->odb);

    gtk_notebook_append_page(GTK_NOTEBOOK(gnc_options_dialog_notebook
                                          (r->optwin)),
                             editor, 
                             gtk_label_new(_("Contents")));

    scm_gc_protect_object(r->options);
    scm_gc_protect_object(r->view);
    scm_gc_protect_object(r->available_list);
    scm_gc_protect_object(r->contents_list);

    /* Build the 'available' view */
    store = gtk_list_store_new (NUM_AVAILABLE_COLS, G_TYPE_STRING, G_TYPE_INT);
    gtk_tree_view_set_model(r->available, GTK_TREE_MODEL(store));
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(store),AVAILABLE_COL_NAME,GTK_SORT_ASCENDING);
    g_object_unref(store);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes("", renderer,
						      "text", AVAILABLE_COL_NAME,
						      NULL);
    gtk_tree_view_append_column(r->available, column);

    selection = gtk_tree_view_get_selection(r->available);
    g_signal_connect(selection, "changed",
		     G_CALLBACK(gnc_column_view_select_avail_cb), r);

    /* Build the 'contents' view */
    store = gtk_list_store_new (NUM_CONTENTS_COLS, G_TYPE_STRING, G_TYPE_INT,
				G_TYPE_INT, G_TYPE_INT);
    gtk_tree_view_set_model(r->contents, GTK_TREE_MODEL(store));
    g_object_unref(store);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Report"), renderer,
						      "text", CONTENTS_COL_NAME,
						      NULL);
    gtk_tree_view_append_column(r->contents, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Rows"), renderer,
						      "text", CONTENTS_COL_REPORT_ROWS,
						      NULL);
    gtk_tree_view_append_column(r->contents, column);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes(_("Cols"), renderer,
                  "text", CONTENTS_COL_REPORT_COLS,
                  NULL);
    gtk_tree_view_append_column(r->contents, column);

    selection = gtk_tree_view_get_selection(r->contents);
    g_signal_connect(selection, "changed",
		     G_CALLBACK(gnc_column_view_select_contents_cb), r);

    update_display_lists(r);

    gnc_options_dialog_set_apply_cb(r->optwin, 
                                    gnc_column_view_edit_apply_cb, r);
    gnc_options_dialog_set_close_cb(r->optwin, 
                                    gnc_column_view_edit_close_cb, r);

    gtk_widget_show(gnc_options_dialog_widget(r->optwin));
    return gnc_options_dialog_widget(r->optwin);
  }
}

static void
gnc_column_view_edit_add_cb(GtkButton * button, gpointer user_data)
{
  gnc_column_view_edit * r = user_data;
  SCM make_report = scm_c_eval_string("gnc:make-report");
  SCM mark_report = scm_c_eval_string("gnc:report-set-needs-save?!");
  SCM template_name;
  SCM new_report;
  SCM newlist = SCM_EOL;
  SCM oldlist = r->contents_list;
  int count;
  int oldlength, id;
  
  if(SCM_LISTP(r->available_list) && 
     (scm_ilength(r->available_list) > r->available_selected)) {
    template_name = scm_list_ref(r->available_list, 
                                scm_int2num(r->available_selected));
    new_report = scm_call_1(make_report, template_name);
    id = scm_num2int(new_report, SCM_ARG1, G_STRFUNC);
    scm_call_2(mark_report, gnc_report_find(id), SCM_BOOL_T);
    oldlength = scm_ilength(r->contents_list);
    
    if(oldlength > r->contents_selected) {
      for(count = 0; count < r->contents_selected; count++) {
        newlist = scm_cons(SCM_CAR(oldlist), newlist);
        oldlist = SCM_CDR(oldlist);
      }
      newlist = scm_append
	(scm_listify(scm_reverse(scm_cons(SCM_LIST4(new_report,
						    scm_int2num(1),
						    scm_int2num(1),
						    SCM_BOOL_F), 
					  newlist)),
		     oldlist,
		     SCM_UNDEFINED));
    }
    else {
      newlist = scm_append
	(scm_listify(oldlist, 
		     SCM_LIST1(SCM_LIST4(new_report,
					 scm_int2num(1),
					 scm_int2num(1),
					 SCM_BOOL_F)),
		     SCM_UNDEFINED));
      r->contents_selected = oldlength;
    }
    
    scm_gc_unprotect_object(r->contents_list);
    r->contents_list = newlist;
    scm_gc_protect_object(r->contents_list);
    
    gnc_column_view_set_option(r->odb, "__general", "report-list",
                               r->contents_list);
    gnc_options_dialog_changed (r->optwin);
  }

  update_display_lists(r);
}

static void
gnc_column_view_edit_remove_cb(GtkButton * button, gpointer user_data)
{
  gnc_column_view_edit * r = user_data;
  SCM newlist = SCM_EOL;
  SCM oldlist = r->contents_list;
  int count;
  int oldlength;
  
  if(SCM_LISTP(r->contents_list)) {
    oldlength = scm_ilength(r->contents_list);
    if(oldlength > r->contents_selected) {
      for(count=0; count < r->contents_selected; count++) {
        newlist = scm_cons(SCM_CAR(oldlist), newlist);
        oldlist = SCM_CDR(oldlist);
      }
      if(count <= oldlength) {
        newlist = scm_append(scm_listify(scm_reverse(newlist), SCM_CDR(oldlist), SCM_UNDEFINED));
      }
    }
    
    if(r->contents_selected > 0 && oldlength == r->contents_selected + 1) {
      r->contents_selected --;
    }

    scm_gc_unprotect_object(r->contents_list);
    r->contents_list = newlist;
    scm_gc_protect_object(r->contents_list);

    gnc_column_view_set_option(r->odb, "__general", "report-list",
                               r->contents_list);

    gnc_options_dialog_changed (r->optwin);
  }

  update_display_lists(r);
}

static void
gnc_edit_column_view_move_up_cb(GtkButton * button, gpointer user_data)
{
  gnc_column_view_edit * r = user_data;
  SCM oldlist = r->contents_list;
  SCM newlist = SCM_EOL;
  SCM temp;
  int oldlength;
  int count;

  oldlength = scm_ilength(r->contents_list);
  if((r->contents_selected > 0) && (oldlength > r->contents_selected)) {
    for(count = 1; count < r->contents_selected; count++) {
      newlist = scm_cons(SCM_CAR(oldlist), newlist);
      oldlist = SCM_CDR(oldlist);
    }
    temp = SCM_CAR(oldlist);
    oldlist = SCM_CDR(oldlist);
    newlist = scm_cons(temp, scm_cons(SCM_CAR(oldlist), newlist));
    newlist = scm_append(scm_listify(scm_reverse(newlist), SCM_CDR(oldlist), SCM_UNDEFINED));

    scm_gc_unprotect_object(r->contents_list);
    r->contents_list = newlist;
    scm_gc_protect_object(r->contents_list);

    r->contents_selected = r->contents_selected - 1;

    gnc_column_view_set_option(r->odb, "__general", "report-list",
                               r->contents_list);
    
    gnc_options_dialog_changed (r->optwin);

    update_display_lists(r);
  }
}

static void
gnc_edit_column_view_move_down_cb(GtkButton * button, gpointer user_data)
{
  gnc_column_view_edit * r = user_data;
  SCM oldlist = r->contents_list;
  SCM newlist = SCM_EOL;
  SCM temp;
  int oldlength;
  int count;

  oldlength = scm_ilength(r->contents_list);
  if(oldlength > (r->contents_selected + 1)) {
    for(count = 0; count < r->contents_selected; count++) {
      newlist = scm_cons(SCM_CAR(oldlist), newlist);
      oldlist = SCM_CDR(oldlist);
    }
    temp = SCM_CAR(oldlist);
    oldlist = SCM_CDR(oldlist);
    newlist = scm_cons(temp, scm_cons(SCM_CAR(oldlist), newlist));
    newlist = scm_append(scm_listify(scm_reverse(newlist), SCM_CDR(oldlist), SCM_UNDEFINED));

    scm_gc_unprotect_object(r->contents_list);
    r->contents_list = newlist;
    scm_gc_protect_object(r->contents_list);

    r->contents_selected = r->contents_selected + 1;

    gnc_column_view_set_option(r->odb, "__general", "report-list",
                               r->contents_list);    

    gnc_options_dialog_changed (r->optwin);

    update_display_lists(r);
  }
}

static void
gnc_column_view_edit_size_cb(GtkButton * button, gpointer user_data)
{
  gnc_column_view_edit * r = user_data;
  GtkWidget * rowspin;
  GtkWidget * colspin;
  GtkWidget * dlg;
  GladeXML *xml;
  SCM current;
  int length;
  int dlg_ret;

  xml = gnc_glade_xml_new ("report.glade", "Edit Report Size");
  dlg = glade_xml_get_widget (xml, "Edit Report Size");

  /* get the spinner widgets */ 
  rowspin = glade_xml_get_widget (xml, "row_spin");
  colspin = glade_xml_get_widget (xml, "col_spin");

  length = scm_ilength(r->contents_list);
  if(length > r->contents_selected) {
    current = scm_list_ref(r->contents_list, 
                          scm_int2num(r->contents_selected));
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(colspin),
                              (float)scm_num2int(SCM_CADR(current),
						 SCM_ARG1, G_STRFUNC));
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(rowspin),
                              (float)scm_num2int(SCM_CADDR(current),
						 SCM_ARG1, G_STRFUNC));
  
    dlg_ret = gtk_dialog_run(GTK_DIALOG(dlg));
    gtk_widget_hide(dlg);

    if(dlg_ret == GTK_RESPONSE_OK) {
      current = SCM_LIST4(SCM_CAR(current),
                          scm_int2num(gtk_spin_button_get_value_as_int
                                     (GTK_SPIN_BUTTON(colspin))),
                          scm_int2num(gtk_spin_button_get_value_as_int
                                     (GTK_SPIN_BUTTON(rowspin))),
                          SCM_BOOL_F);
      scm_gc_unprotect_object(r->contents_list);
      r->contents_list = scm_list_set_x(r->contents_list, 
                                        scm_int2num(r->contents_selected),
                                        current);
      scm_gc_protect_object(r->contents_list);
      gnc_options_dialog_changed (r->optwin);
      update_display_lists(r);
    }
    gtk_widget_destroy(dlg);
  }
}

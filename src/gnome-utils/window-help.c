/********************************************************************\
 * window-help.c -- a help window for hypertext help.               *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
 * Copyright (C) 1999 Jeremy Collins ( gtk-xmhtml port )            *
 * Copyright (C) 2000 Dave Peticolas                                *
 * Copyright (C) 2000 Bill Gribble                                  *
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
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <errno.h>
#include <fcntl.h>
#include <gnome.h>
#include <guile/gh.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

/* needed for db.h with 'gcc -ansi -pedantic' */
#ifndef _BSD_SOURCE
#  define _BSD_SOURCE 1
#endif

#ifdef PREFER_DB1
#ifdef HAVE_DB1_DB_H
# include <db1/db.h>
#else
# ifdef HAVE_DB_185_H
#  include <db_185.h>
# else
#  include <db.h>
# endif
#endif
#else
#ifdef HAVE_DB_185_H
# include <db_185.h>
#else
# ifdef HAVE_DB_H
#  include <db.h>
# else
#  include <db1/db.h>
# endif
#endif
#endif

#include "file-utils.h"
#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-engine-util.h"
#include "gnc-html-history.h"
#include "gnc-html.h"
#include "window-help.h"

#define WINDOW_HELP_CM_CLASS "window-help"

struct _gnc_help_window {
  GtkWidget   * toplevel;

  GtkWidget   * toolbar;
  GtkWidget   * statusbar;  
  GtkWidget   * statusbar_hbox;
  GtkWidget   * html_vbox;
  GtkWidget   * topics_tree;
  GtkWidget   * paned;

  GtkWidget   * search_entry;
  GtkWidget   * search_results;
  GtkWidget   * type_pixmap;

  DB          * index_db;
  gnc_html    * html;
};

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

static gint last_width = 0;
static gint last_height = 0;


/********************************************************************
 * gnc_help_window_check_urltype
 * is it OK to show a certain URLType in this window?
 ********************************************************************/

static int
gnc_help_window_check_urltype(URLType t) {
  if (!safe_strcmp (t, URL_TYPE_FILE) ||
      !safe_strcmp (t, URL_TYPE_HELP) ||
      !safe_strcmp (t, URL_TYPE_HTTP) ||
      !safe_strcmp (t, URL_TYPE_SECURE)) {
    return TRUE;

  } else {
    return FALSE;
  }
}


/********************************************************************
 * gnc_help_window_check_urltype
 * is it OK to show a certain URLType in this window?
 ********************************************************************/

static void
gnc_help_window_url_flyover(gnc_html * html, const gchar * url,
                            gpointer data) {
  gnc_help_window * help = (gnc_help_window *)data;
  gtk_statusbar_pop(GTK_STATUSBAR(help->statusbar), 1);
  if(url) {
    gtk_statusbar_push(GTK_STATUSBAR(help->statusbar),
                       1, url);
  }
}


/********************************************************************
 * after-load cb : enable/disable history buttons
 ********************************************************************/

static void
gnc_help_window_set_back_button(gnc_help_window * win, int enabled) {
  GtkToolbar * tb = GTK_TOOLBAR(win->toolbar);
  gtk_widget_set_sensitive
    (GTK_WIDGET(((GtkToolbarChild *)g_list_nth_data(tb->children, 0))->widget),
     enabled);
}


static void
gnc_help_window_set_fwd_button(gnc_help_window * win, int enabled) {
  GtkToolbar * tb = GTK_TOOLBAR(win->toolbar);
  gtk_widget_set_sensitive
    (GTK_WIDGET(((GtkToolbarChild *)g_list_nth_data(tb->children, 1))->widget),
     enabled);
}


static gint
compare_locations (gconstpointer a, gconstpointer b)
{
  return safe_strcmp (a, b);
}

static void
gnc_help_show_topic (gnc_help_window *help, const char * location)
{
  GtkCTreeNode *node, *n;
  GtkCTree *ctree;
  GtkCTreeRow *row;
  gint rownum;

  ctree = GTK_CTREE (help->topics_tree);

  rownum = GTK_CLIST(ctree)->focus_row;

  node = gtk_ctree_node_nth (ctree, rownum);

  if (node)
  {
    char *node_loc;

    node_loc = gtk_ctree_node_get_row_data (ctree, node);

    if (safe_strcmp (location, node_loc) != 0)
    {
      char *help_loc;

      help_loc = gnc_build_url (URL_TYPE_HELP, location, NULL);
      if (safe_strcmp (help_loc, node_loc) != 0)
        node = NULL;
      g_free (help_loc);
    }
  }

  if (!node)
    node = gtk_ctree_find_by_row_data_custom (ctree, NULL,
                                              (gpointer) location,
                                              compare_locations);

  if (!node)
  {
    char *help_loc = gnc_build_url (URL_TYPE_HELP, location, NULL);
    node = gtk_ctree_find_by_row_data_custom (ctree, NULL, help_loc,
                                              compare_locations);
    g_free (help_loc);
  }

  if (!node)
  {
    gtk_ctree_unselect_recursive (ctree, NULL);
    return;
  }

  /* Select it */
  gtk_signal_handler_block_by_data (GTK_OBJECT (ctree), help);
  gtk_ctree_select (ctree, node);
  gtk_signal_handler_unblock_by_data (GTK_OBJECT (ctree), help);

  /* Expand all the parents */
  row = GTK_CTREE_ROW (node);
  while ((n = row->parent) != NULL)
  {
    gtk_ctree_expand (ctree, n);
    row = GTK_CTREE_ROW (n);
  }

  /* Make sure it's visible */
  if (gtk_ctree_node_is_visible(ctree, node) != GTK_VISIBILITY_FULL)
    gtk_ctree_node_moveto(ctree, node, 0, 0.5, 0.0);
}

static void 
gnc_help_window_load_cb(gnc_html * html, URLType type, 
                        const gchar * location, const gchar * label, 
                        gpointer data) {
  gnc_help_window * win = data;

  if(gnc_html_history_forward_p(gnc_html_get_history(win->html))) {
    gnc_help_window_set_fwd_button(win, TRUE); 
  }
  else {
    gnc_help_window_set_fwd_button(win, FALSE); 
  }
  
  if(gnc_html_history_back_p(gnc_html_get_history(win->html))) {
    gnc_help_window_set_back_button(win, TRUE); 
  }
  else {
    gnc_help_window_set_back_button(win, FALSE); 
  }

  gnc_help_show_topic (win, location);
}


/********************************************************************
 * toolbar callbacks
 ********************************************************************/

static int 
gnc_help_window_fwd_cb(GtkWidget * w, gpointer data) {
  gnc_help_window       * help = data;
  gnc_html_history_node * node;

  gnc_html_history_forward(gnc_html_get_history(help->html));
  node = gnc_html_history_get_current(gnc_html_get_history(help->html));
  gnc_html_show_url(help->html, node->type, node->location, node->label, 0);
  return TRUE;
}

static int 
gnc_help_window_back_cb(GtkWidget * w, gpointer data) {
  gnc_help_window       * help = data;
  gnc_html_history_node * node;

  gnc_html_history_back(gnc_html_get_history(help->html));
  node = gnc_html_history_get_current(gnc_html_get_history(help->html));
  gnc_html_show_url(help->html, node->type, node->location, node->label, 0);
  return TRUE;
}

static int
gnc_help_window_stop_button_cb(GtkWidget * w, gpointer data) {
  gnc_help_window       * help = data;
  gnc_html_cancel(help->html);
  return TRUE;
}

static int
gnc_help_window_reload_button_cb(GtkWidget * w, gpointer data) {
  gnc_help_window       * help = data;
  gnc_html_reload(help->html);
  return TRUE;
}

static void
goto_string_cb(char * string, gpointer data) {
  if(!data) return;
  if(!string) {
    *(char **)data = NULL;
  }
  else {
    *(char **)data = g_strdup(string);
  }
}

static int
gnc_help_window_goto_button_cb(GtkWidget * w, gpointer data) {
  gnc_help_window * help = data;
  int             retval = -1;
  char            * url = NULL;
  URLType         type;
  char            * location = NULL;
  char            * label = NULL;

  GtkWidget * dlg = gnome_request_dialog(FALSE, 
                                         _("Enter URI to load:"), "", 250,
                                         &goto_string_cb, &url,
                                         GTK_WINDOW (help->toplevel));
  retval = gnome_dialog_run_and_close(GNOME_DIALOG(dlg));
  
  if((retval == 0) && url && (strlen(url) > 0)) {
    type = gnc_html_parse_url(help->html, url, &location, &label);
    gnc_html_show_url(help->html, type, location, label, 0);
    g_free(location);
    g_free(label);
  }

  if(url) {
    g_free(url);
  }

  return TRUE;
}


/********************************************************************
 * topics-browser callbacks 
 * silly tree building and clicking stuff
 ********************************************************************/

static void
gnc_help_window_topic_select_cb(GtkCTree * tree, GtkCTreeNode * row, int col,
                                gpointer user_data) {
  gnc_help_window * wind = user_data;
  URLType         type;
  char            * location = NULL;
  char            * label = NULL;
  char            * url = gtk_ctree_node_get_row_data(tree, row);

  if(url && strlen(url) > 0) {
    type = gnc_html_parse_url(wind->html, url, &location, &label);
    gnc_html_show_url(wind->html, type, location, label, 0);
    g_free(location);
    g_free(label);
  }
}

static void
free_url_cb(gpointer user_data) {
  if(user_data) free(user_data);
}

static void 
topics_add_children(SCM topics, GtkCTree * tree, GtkCTreeNode * parent, 
                    gnc_help_window * help) {
  SCM          this_topic;
  SCM          subtopics;
  GtkCTreeNode * node; 
  char         * topic_str;
  char         * ctopics[1];
  char         * curl = NULL;
  gboolean     leafnode;

  if(!gh_list_p(topics)) return;

  for(; !gh_null_p(topics); topics = gh_cdr(topics)) {
    this_topic = gh_car(topics);

    if(!gh_list_p(this_topic)) continue;

    if(!gh_null_p(gh_cdr(this_topic)) && !gh_null_p(gh_cddr(this_topic))) {
      subtopics  = gh_caddr(this_topic);
    }
    else {
      subtopics = SCM_BOOL_F;
    }

    topic_str  = gh_scm2newstr(gh_car(this_topic), NULL);
    ctopics[0] = _(topic_str);

    if(!gh_null_p(gh_cdr(this_topic))) {
      curl = gh_scm2newstr(gh_cadr(this_topic), NULL);
    }
   
    if(gh_list_p(subtopics)) {
      leafnode = FALSE;
    }
    else {
      leafnode = TRUE;
    }
    
    node = gtk_ctree_insert_node(GTK_CTREE(tree),
                                 GTK_CTREE_NODE(parent), NULL,
                                 ctopics, 1,
                                 NULL, NULL, NULL, NULL,
                                 leafnode, FALSE);

    gtk_ctree_node_set_row_data_full(GTK_CTREE(tree), 
                                     GTK_CTREE_NODE(node), curl,
                                     free_url_cb);
    free(topic_str);
    if(gh_list_p(subtopics)) {
      topics_add_children(subtopics, tree, node, help);
    }
  }
}

static void
gnc_help_window_load_topics(gnc_help_window * help, const gchar * file)
{  
  SCM topics;
  SCM load_topics =  gh_eval_str("gnc:load-help-topics");

  /* FIXME: when we drop support older guiles, drop the (char *) coercion. */
  topics = gh_call1(load_topics, gh_str02scm((char *) file));
  topics_add_children(topics, GTK_CTREE(help->topics_tree), NULL, help);
  gtk_ctree_expand_to_depth (GTK_CTREE(help->topics_tree), NULL, 1);
  gtk_widget_show_all(help->topics_tree);
}


static void
gnc_help_window_destroy_cb(GtkWidget * w, gpointer data) {
  gnc_help_window * help = data;

  gnc_unregister_gui_component_by_data (WINDOW_HELP_CM_CLASS, help);

  /* close the help index db */
  if(help->index_db) {
    help->index_db->close(help->index_db);
  }

  gnc_html_destroy(help->html);

  help->html        = NULL;
  help->toplevel    = NULL;
  help->statusbar   = NULL;
  help->html_vbox   = NULL;
  help->topics_tree = NULL;

  g_free(help);
}

static void
gnc_help_window_close_cb(GtkWidget * w, gpointer data) {
  gnc_help_window * help = data;

  gnc_close_gui_component_by_data (WINDOW_HELP_CM_CLASS, help);
}

static void
gnc_help_window_print_cb(GtkWidget * w, gpointer data) {
  gnc_help_window * help = data;
  
  gnc_html_print(help->html);
}


static void 
item_destroy_cb(GtkListItem * li, gpointer user_data) {
  gpointer x = gtk_object_get_user_data(GTK_OBJECT(li));
  g_free(x);
}


static void
show_search_results(gnc_help_window * help, const char * matches, 
                    int match_len) {
  const char * current;
  const char * end;
  char       * this_link=NULL;
  int        link_len;
  GList      * results = NULL; 
  GtkWidget  * listitem;

  if(!matches) {
    if(GTK_LIST(help->search_results)->children) {
      gtk_list_remove_items(GTK_LIST(help->search_results),
                            GTK_LIST(help->search_results)->children);
    }
    return;
  }

  current = matches;  
  while((end = strchr(current, '\n')) != NULL) {
    link_len  = end - current;
    this_link = g_new0(char, link_len + 1);
    strncpy(this_link, current, link_len);
    listitem = gtk_list_item_new_with_label(this_link);
    gtk_object_set_user_data(GTK_OBJECT(listitem), this_link);
    gtk_signal_connect(GTK_OBJECT(listitem), "destroy",
                       GTK_SIGNAL_FUNC(item_destroy_cb), NULL);
    
    gtk_widget_show(listitem);
    results = g_list_append(results, listitem);
    current = end+1;
  }
  
  /* get rid of the old items */ 
  if(GTK_LIST(help->search_results)->children) {
    gtk_list_remove_items(GTK_LIST(help->search_results),
                          GTK_LIST(help->search_results)->children);
  }

  /* add the new ones */ 
  if(results) {
    gtk_list_append_items(GTK_LIST(help->search_results),
                          results);
  }
}


static void
gnc_help_window_search_button_cb(GtkButton * button, gpointer data) {
  gnc_help_window * help = data;
  char            * search_string = 
    gtk_entry_get_text(GTK_ENTRY(help->search_entry));
  DBT             key, value;
  int             err=1;
  
  /* initialize search key/value */
  memset(&key, 0, sizeof(DBT));
  memset(&value, 0, sizeof(DBT));

  key.data    = search_string;
  key.size    = strlen(search_string);

  /* do the search */
  if(help->index_db) {
    err = help->index_db->get(help->index_db, &key, &value, 0);
  }

  if(err == 0) {
    /* the data in the DB is a newline-separated list of filenames */
    show_search_results(help, value.data, value.size);    
  }
}

static void
gnc_help_window_search_help_button_cb(GtkButton * button, gpointer data) {
#if 0
  gnc_help_window * help = data;
#endif

  printf("help on help\n");
}

static void
gnc_help_window_search_result_select_cb(GtkWidget * list, GtkWidget * child,
                                        gpointer user_data) {
  gnc_help_window * help = user_data;
  char * helpfile = gtk_object_get_user_data(GTK_OBJECT(child));
  gnc_help_window_show_help(help, helpfile, NULL);
}

static void
close_handler (gpointer user_data)
{
  gnc_help_window *help = user_data;

  gdk_window_get_geometry (GTK_WIDGET(help->toplevel)->window, NULL, NULL,
                           &last_width, &last_height, NULL);

  gnc_save_window_size ("help_win", last_width, last_height);

  gnc_help_window_destroy (help);
}

/********************************************************************
 * gnc_help_window_new 
 * allocates and opens up a help window
 ********************************************************************/

gnc_help_window *
gnc_help_window_new (void) {
  
  gnc_help_window * help = g_new0(gnc_help_window, 1);
  char            * indexfile;
  GnomeUIInfo     toolbar_data[] = 
  {
    { GNOME_APP_UI_ITEM,
      N_("Back"),
      N_("Move back one step in the history"),
      gnc_help_window_back_cb, help,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_BACK,
      0, 0, NULL
    },
    { GNOME_APP_UI_ITEM,
      N_("Forward"),
      N_("Move forward one step in the history"),
      gnc_help_window_fwd_cb, help,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_FORWARD,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    { GNOME_APP_UI_ITEM,
      N_("Reload"),
      N_("Reload the current document"),
      gnc_help_window_reload_button_cb, help,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_REFRESH,
      0, 0, NULL
    },
    { GNOME_APP_UI_ITEM,
      N_("Open"),
      N_("Open a new document"),
      gnc_help_window_goto_button_cb, help,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_OPEN,
      0, 0, NULL
    },
    { GNOME_APP_UI_ITEM,
      N_("Stop"),
      N_("Cancel outstanding HTML requests"),
      gnc_help_window_stop_button_cb, help,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_STOP,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    { GNOME_APP_UI_ITEM,
      N_("Print"),
      N_("Print Help window"),
      gnc_help_window_print_cb, help,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_PRINT,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    { GNOME_APP_UI_ITEM,
      N_("Close"),
      N_("Close this Help window"),
      gnc_help_window_close_cb, help,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_CLOSE,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };
  GladeXML *xml;

  xml = gnc_glade_xml_new ("help.glade", "Help Window");

  help->toplevel = glade_xml_get_widget (xml, "Help Window");

  gnc_register_gui_component (WINDOW_HELP_CM_CLASS, NULL, close_handler, help);

  help->toolbar        = glade_xml_get_widget (xml, "help_toolbar");
  help->statusbar      = glade_xml_get_widget (xml, "help_statusbar");
  help->statusbar_hbox = glade_xml_get_widget (xml, "statusbar_hbox");
  help->html_vbox      = glade_xml_get_widget (xml, "help_html_vbox");
  help->topics_tree    = glade_xml_get_widget (xml, "help_topics_tree");
  help->paned          = glade_xml_get_widget (xml, "help_paned");
  help->search_entry   = glade_xml_get_widget (xml, "help_search_entry");
  help->search_results = glade_xml_get_widget (xml, "search_results_list");
  help->type_pixmap    = glade_xml_get_widget (xml, "file_type_pixmap");

  help->html         = gnc_html_new();

  glade_xml_signal_connect_data
    (xml, "gnc_help_window_search_button_cb",
     GTK_SIGNAL_FUNC (gnc_help_window_search_button_cb), help);

  glade_xml_signal_connect_data
    (xml, "gnc_help_window_search_help_button_cb",
     GTK_SIGNAL_FUNC (gnc_help_window_search_help_button_cb), help);

  gnome_app_fill_toolbar(GTK_TOOLBAR(help->toolbar), toolbar_data, NULL);
  gtk_box_pack_start(GTK_BOX(help->html_vbox), 
                     gnc_html_get_widget(help->html),
                     TRUE, TRUE, 0);
  gtk_paned_set_position(GTK_PANED(help->paned), 200);
  
  gnc_html_set_urltype_cb(help->html, gnc_help_window_check_urltype);
  gnc_html_set_flyover_cb(help->html, gnc_help_window_url_flyover, 
                          (gpointer)help);
  gnc_html_set_load_cb(help->html, gnc_help_window_load_cb, 
                       (gpointer)help);
  
  gnc_help_window_load_topics(help, "help-topics-index.scm"); 

  gtk_signal_connect(GTK_OBJECT(help->toplevel), "destroy",
                     GTK_SIGNAL_FUNC(gnc_help_window_destroy_cb), 
                     (gpointer)help);
  
  gtk_signal_connect(GTK_OBJECT(help->topics_tree), "tree_select_row",
                     GTK_SIGNAL_FUNC(gnc_help_window_topic_select_cb), 
                     (gpointer)help);

  gtk_signal_connect(GTK_OBJECT(help->search_results), "select_child",
                     GTK_SIGNAL_FUNC(gnc_help_window_search_result_select_cb), 
                     (gpointer)help);

  indexfile = gncFindFile("help-search-index.db");
  help->index_db = dbopen(indexfile, O_RDONLY, 0644, DB_HASH, NULL);
  if (!help->index_db) {
    PERR("Failed to open help index DB '%s' : %s\n",
         indexfile ? indexfile : "(null)",
         strerror(errno) ? strerror(errno) : "");
  }
  g_free(indexfile);

  if (last_width == 0)
    gnc_get_window_size("help_win", &last_width, &last_height);

  gtk_window_set_default_size(GTK_WINDOW(help->toplevel),
                              last_width, last_height);

  gnc_window_adjust_for_screen (GTK_WINDOW(help->toplevel));

  gtk_widget_show_all(help->toplevel);
  
  return help;
}


/********************************************************************
 * gnc_help_window_destroy
 * delete a help window 
 ********************************************************************/

void
gnc_help_window_destroy(gnc_help_window * help) {
  if (!help) return;

  gnc_unregister_gui_component_by_data (WINDOW_HELP_CM_CLASS, help);

  gtk_signal_disconnect_by_func(GTK_OBJECT(help->toplevel), 
                                GTK_SIGNAL_FUNC(gnc_help_window_destroy_cb), 
                                (gpointer)help);
  /* close the help index db */
  if(help->index_db) {
    help->index_db->close(help->index_db);
  }

  /* take care of the gnc-html object specially */
  gtk_widget_ref(gnc_html_get_widget(help->html));
  gnc_html_destroy(help->html);

  gtk_widget_destroy(GTK_WIDGET(help->toplevel)); 
}

void
gnc_help_window_show_help(gnc_help_window * help, const char * location,
                          const char * label) {
  gnc_html_show_url(help->html, URL_TYPE_FILE, location, label, 0);
}


/********************************************************************
 * compatibility stuff (temporary)
 ********************************************************************/

void
helpWindow(GtkWidget * parent, const char * title, const char * htmlfile) {
  gnc_help_window * help = gnc_help_window_new();
  gnc_help_window_show_help(help, htmlfile, NULL);
}

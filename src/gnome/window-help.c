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

#include "File.h"
#include "glade-cb-gnc-dialogs.h"
#include "glade-gnc-dialogs.h"
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

/********************************************************************
 * gnc_help_window_check_urltype
 * is it OK to show a certain URLType in this window?
 ********************************************************************/

static int
gnc_help_window_check_urltype(URLType t) {
  switch (t) {
  case URL_TYPE_FILE:
  case URL_TYPE_HELP:
  case URL_TYPE_HTTP:
  case URL_TYPE_SECURE:
    return TRUE;
    break;
  default:
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

    ctopics[0] = gh_scm2newstr(gh_car(this_topic), NULL);

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
    free(ctopics[0]);
    if(gh_list_p(subtopics)) {
      topics_add_children(subtopics, tree, node, help);
    }
  }
}

static void
gnc_help_window_load_topics(gnc_help_window * help, const gchar * file) {
  
  SCM topics;
  SCM load_topics =  gh_eval_str("gnc:load-help-topics");

  topics = gh_call1(load_topics, gh_str02scm(file));
  topics_add_children(topics, GTK_CTREE(help->topics_tree), NULL, help);
  gtk_widget_show_all(help->topics_tree);
}


static void
gnc_help_window_destroy_cb(GtkWidget * w, gpointer data) {
  gnc_help_window * help = data;

  gnc_help_window_destroy(help);
}

static void
gnc_help_window_print_cb(GtkWidget * w, gpointer data) {
  gnc_help_window * help = data;
  
  gnc_html_print(help->html);
}


static void 
item_destroy_cb(GtkListItem * li, gpointer user_data) {
  g_free(gtk_object_get_user_data(GTK_OBJECT(li)));
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


void
gnc_help_window_search_button_cb(GtkButton * button, gpointer data) {
  GtkObject       * hw   = data;
  gnc_help_window * help = gtk_object_get_data(hw, "help_window_struct");
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

void
gnc_help_window_search_help_button_cb(GtkButton * button, gpointer data) {
#if 0
  GtkObject       * hw   = data;
  gnc_help_window * help = gtk_object_get_data(hw, "help_window_struct");
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

  gnc_help_window_destroy (help);
}

/********************************************************************
 * gnc_help_window_new 
 * allocates and opens up a help window
 ********************************************************************/

gnc_help_window *
gnc_help_window_new (void) {
  
  gnc_help_window * help = g_new0(gnc_help_window, 1);
  GtkObject       * tlo;
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
      gnc_help_window_destroy_cb, help,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_CLOSE,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };
  
  help->toplevel = create_Help_Window();
  tlo = GTK_OBJECT(help->toplevel);

  gnc_register_gui_component (WINDOW_HELP_CM_CLASS, NULL, close_handler, help);

  help->toolbar      = gtk_object_get_data(tlo, "help_toolbar");
  help->statusbar    = gtk_object_get_data(tlo, "help_statusbar");
  help->statusbar_hbox = gtk_object_get_data(tlo, "statusbar_hbox");
  help->html_vbox    = gtk_object_get_data(tlo, "help_html_vbox");
  help->topics_tree  = gtk_object_get_data(tlo, "help_topics_tree");
  help->paned        = gtk_object_get_data(tlo, "help_paned");
  help->search_entry = gtk_object_get_data(tlo, "help_search_entry");
  help->search_results = gtk_object_get_data(tlo, "search_results_list");
  help->type_pixmap  = gtk_object_get_data(tlo, "file_type_pixmap");
  
  help->html         = gnc_html_new();

  gtk_object_set_data(tlo, "help_window_struct", (gpointer)help);

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
         indexfile, strerror(errno));
  }
  g_free(indexfile);

  gtk_widget_show_all(help->toplevel);
  
  return help;
}


/********************************************************************
 * gnc_help_window_destroy
 * delete a help window 
 ********************************************************************/

void
gnc_help_window_destroy(gnc_help_window * help) {

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

  help->html        = NULL;
  help->toplevel    = NULL;
  help->statusbar   = NULL;
  help->html_vbox   = NULL;
  help->topics_tree = NULL;

  g_free(help);
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

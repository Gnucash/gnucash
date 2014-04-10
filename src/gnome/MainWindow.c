/********************************************************************\
 * MainWindow.c -- the main window, and associated helper functions * 
 *                 and callback functions for xacc (X-Accountant)   *
 * Copyright (C) 1998 Jeremy Collins				    *
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

#include <nana.h>
#include <gnome.h>

#include "config.h"

#include "gnucash.h"
#include "options-dialog.h"
#include "AccWindow.h"
#include "MenuCommands.h"
#include "messages.h"
#include "RegWindow.h"
#include "top-level.h"
#include "version.h"
#include "MainWindow.h"
#include "MainWindowP.h"

/** STRUCTURES ******************************************************/
struct _main_window {
  GtkTree *maintree;
  GtkTree *root_item;
};

typedef struct _main_window main_window;

/** GLOBALS **********************************************************/
main_window    *mwindow;
char		*HELP_ROOT = "";

gchar *clist_titles[] =
{
  ACC_NAME_STR,
  ACC_TYPE_STR,
  BALN_STR 
};

static void
acct_tree_open_selected(GtkWidget *child) {  
  Account *acct = gtk_object_get_user_data(GTK_OBJECT(child));
  fprintf(stderr, "calling regWindowSimple(%p)\n", acct);
  regWindowSimple(acct);
}

static gint
acct_tree_select(GtkWidget *widget, GdkEventButton *event, GtkWidget *child) 
{
  /* Catch a double or single click */
  if (GTK_IS_TREE_ITEM(widget) &&
     (event->type==GDK_2BUTTON_PRESS ||
      event->type==GDK_3BUTTON_PRESS) ) 
  {
    acct_tree_open_selected(child);         
  }

  return FALSE;

}

void
refreshMainWindow()
{
  gnc_ui_refresh_tree();
}


/********************************************************************\
 * acct_tree_fill                                                   *
 *   fills the tree with accounts, name ($balance)                  *
 *                                                                  *
 * Args:   item      - top level of tree                            *
 *         accts     - the account group to use in filling tree     *
 *         subtree   - whether this is the toplevel, or a subtree   *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_ui_acct_tree_fill(GtkTree *item, AccountGroup *accts, int subtree) 
{
  int accounts_in_group = xaccGroupGetNumAccounts(accts);
  int current_account;
  GtkTree* item_subtree;
  GtkTreeItem* tree_item;
  int no_root_item;

  if ( subtree == -1 ) {
    item_subtree = item;
    no_root_item = 1;
  } else {
    item_subtree = GTK_TREE(gtk_tree_new());
    no_root_item = 0;
  }
  
  for( current_account=0; 
       current_account < accounts_in_group; 
       current_account++ )
  {
    Account *acc = xaccGroupGetAccount(accts, current_account);
    AccountGroup *acc_children;
    char buffer[255]; 
    gchar *rowstrs[3];
    
    rowstrs[0] = xaccAccountGetName (acc);
    rowstrs[1] = xaccAccountGetDescription (acc);
    rowstrs[2] = xaccAccountGetNotes (acc);

    sprintf (buffer, "%s ($%.2f)", rowstrs[0], xaccAccountGetBalance(acc));

    tree_item = GTK_TREE_ITEM(gtk_tree_item_new_with_label( buffer ));
    /* Set the tree item to point to the actual account so we can reach it
       trivially when the user selects the row.  (Should we use
       gtk_*_data_full and have a destroy notify?) */
    gtk_object_set_user_data(GTK_OBJECT(tree_item), acc); 

    gtk_tree_append(GTK_TREE(item_subtree), GTK_WIDGET(tree_item)); 

    gtk_signal_connect (GTK_OBJECT (tree_item), 
                        "button_press_event",
                        (GtkSignalFunc) acct_tree_select, 
                        GTK_WIDGET(tree_item));

    acc_children = xaccAccountGetChildren(acc);
    if ( acc_children )
    {
      gnc_ui_acct_tree_fill ( GTK_TREE(tree_item), acc_children, 1 );
    }
  
    gtk_widget_show(GTK_WIDGET(tree_item));

  }
  
  if(!no_root_item) {
    gtk_tree_item_set_subtree(GTK_TREE_ITEM(item), GTK_WIDGET(item_subtree));
  }
  
}

/********************************************************************\
 * refresh_tree                                                     *
 *   refreshes the main window                                      *
 *                                                                  *
 * Args:    tree - the tree that will get destroyed..               *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_ui_refresh_tree() {

  /** This is ugly... how do we do this nicer? */
  GList *items;
  
  mwindow->maintree = GTK_TREE_ROOT_TREE ( mwindow->maintree );
  
  items = GTK_TREE_SELECTION ( mwindow->maintree );
  
  gtk_tree_clear_items ( mwindow->maintree, 0, g_list_length(items) );  
  
  mwindow->root_item = mwindow->maintree;  
    
  gnc_ui_acct_tree_fill(mwindow->root_item,
                        xaccSessionGetGroup(current_session),
                        -1); 

}

/* Standard Gnome About Dialog, need I say more? */
/* hack alert -- should display about.html documentation page instead */
static void
gnc_ui_about_cb (GtkWidget *widget, gpointer data)
{
  GtkWidget *about;
  const gchar *authors[] = {
  /* Here should be your names */
          "Rob Clark",
          "Linas Vepstas",
          "Jeremy Collins",
          "Rob Browning",
          "For more see http://www.gnucash.org/developers.html",
          NULL
          };

  about = gnome_about_new ( "GnuCash", VERSION,
                            "(C) 1998 The GnuCash Project",
                            authors,
                            "GnuCash: The GNU way to manage your money!",
                            NULL);
  gtk_widget_show (about);

}                          

/* Help system callback */
static void
gnc_ui_help_cb ( GtkWidget *widget, gpointer data )
{
  /* hack alert --  We need some config menus to setup were the docs are located */
  
  gchar *docs_path = HELP_ROOT;
  
  docs_path = gnome_util_prepend_user_home( docs_path );

  gnome_help_goto( NULL, docs_path );

  g_free( docs_path );

}

#if 0

/* Some dialog stubs to be worked on */
/* We might want to move these to there own file =\ */

static void
gnc_ui_file_new_cb ( GtkWidget *widget, gpointer data )
{

}

#endif

static void
gnc_ui_add_account ( GtkWidget *widget, gpointer data )
{
  GtkWidget *tree = data;
  GList *selection;
  
  selection = GTK_TREE_SELECTION ( tree );
  if ( selection )
  {
    if ( selection->data != NULL )
    {
      Account *acc = gtk_object_get_user_data(GTK_OBJECT(selection->data));
      accWindow( (AccountGroup *) acc);
    }
   
  }
  else
  {
    accWindow(NULL);
  }
  
}

static void
gnc_ui_delete_account_finish_cb ( GtkWidget *widget, gpointer data )
{
  Account *account = data;
  
  /* Did I delete this correctly? */
  xaccRemoveAccount ( account );
  xaccFreeAccount ( account );

  gnc_ui_refresh_tree ();  
  
}

static void
gnc_ui_delete_account_cb ( GtkWidget *widget, gpointer data )
{
  GtkWidget *tree = data;
  GList *selection;
  
  selection = GTK_TREE_SELECTION ( tree );
  if ( selection )
  {
    if ( selection->data != NULL )
    {
      Account *acc = gtk_object_get_user_data(GTK_OBJECT(selection->data));
      GtkWidget *msgbox;
      
      msgbox = gnome_message_box_new ( " Are you sure you want to delete this account. ",
                                       GNOME_MESSAGE_BOX_WARNING, 
                                       GNOME_STOCK_BUTTON_OK,
                                       GNOME_STOCK_BUTTON_CANCEL, NULL );
      gnome_dialog_button_connect (GNOME_DIALOG (msgbox), 0,
                                   GTK_SIGNAL_FUNC (gnc_ui_delete_account_finish_cb), 
                                   acc);
      gtk_widget_show ( msgbox );
    }
  }
  else
  {
    GtkWidget *msgbox;
    msgbox = gnome_message_box_new ( " You must select an account to delete. ",
                                     GNOME_MESSAGE_BOX_ERROR, "Ok", NULL );
    gtk_widget_show ( msgbox );    
  }
}

static void
gnc_ui_mainWindow_toolbar_open ( GtkWidget *widget, gpointer data )
{
  GtkWidget *tree = data;
  GList *selection;
  
  selection = GTK_TREE_SELECTION ( tree );
  if ( selection )
  {
    if ( selection->data != NULL )
    {
      Account *acc = gtk_object_get_user_data(GTK_OBJECT(selection->data));
      fprintf(stderr, "calling regWindowSimple(%p)\n", acct);
      regWindowSimple ( acc );
    }
  }
  else
  {
    GtkWidget *msgbox;
    msgbox = gnome_message_box_new ( " You must select an account to open first. ",
                                     GNOME_MESSAGE_BOX_ERROR, "Ok", NULL );
    gtk_widget_show ( msgbox );    
  }  
}

//static void
//quit_menu_item_helper() {
//  gnc_shutdown(0);
//}

static void
gnc_ui_options_cb ( GtkWidget *widget, gpointer data ) {
  gnc_show_options_dialog();
}

/* Menus */

static GnomeUIInfo filemenu[] = {
       {GNOME_APP_UI_ITEM, 
       N_("New"), N_("Create New File."),
       NULL, NULL, NULL, 
       GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_ABOUT,
       0, 0, NULL},
       {GNOME_APP_UI_ITEM,
       N_("Open"), N_("Open File."),
       file_cmd_open, NULL, NULL,
       GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_OPEN,
       0,0, NULL},
       {GNOME_APP_UI_ITEM,
       N_("Import"), N_("Import QIF File."),
       file_cmd_import, NULL, NULL,
       GNOME_APP_PIXMAP_NONE, NULL,
       0, 0, NULL},
       GNOMEUIINFO_END             
};

static GnomeUIInfo optionsmenu[] = {
	{GNOME_APP_UI_ITEM,
	 N_("Preferences"), N_("Preferences"),
	 gnc_ui_options_cb, NULL, NULL,
	 GNOME_APP_PIXMAP_NONE, NULL,
	 0, 0, NULL},
	 GNOMEUIINFO_END
};
  
static GnomeUIInfo helpmenu[] = {
    {GNOME_APP_UI_ITEM, 
     N_("About"), N_("About Gnucash."),
     gnc_ui_about_cb, NULL, NULL, 
     GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_ABOUT, 0, 0, NULL},
     GNOMEUIINFO_SEPARATOR,
    {GNOME_APP_UI_ITEM,
     N_("Help"), N_("Gnucash Help."),
     gnc_ui_help_cb, NULL, NULL,
     GNOME_APP_PIXMAP_NONE, NULL,
     0, 0, NULL},
     GNOMEUIINFO_END
};

 /*GNOMEUIINFO_SUBTREE(N_("Options"), optionsmenu),*/

static GnomeUIInfo mainmenu[] = {
    GNOMEUIINFO_SUBTREE(N_("File"), filemenu),
    GNOMEUIINFO_SUBTREE(N_("Options"), optionsmenu),
    GNOMEUIINFO_SUBTREE(N_("Help"), helpmenu),
    GNOMEUIINFO_END
};

void
mainWindow() {

  GtkWidget 	*scrolled_win;
  GtkWidget 	*main_vbox;
  GtkWidget 	*clist_vbox;
  GtkWidget 	*clist;
  GtkWidget 	*notebook;
  AccountGroup *accts = xaccSessionGetGroup(current_session);

  mwindow = g_malloc ( sizeof ( main_window ) );
  mwindow->maintree = GTK_TREE(gtk_tree_new());

  mwindow->root_item = mwindow->maintree;

  /* Create the toolbar, and hook up the buttons to the tree widget */
  {
    GnomeUIInfo toolbar[] = 
    {
      GNOMEUIINFO_ITEM_DATA(N_("Open"), 
                       N_("Open selected account."),
                       gnc_ui_mainWindow_toolbar_open, 
                       mwindow->root_item,
                       GNOME_APP_PIXMAP_NONE),
      GNOMEUIINFO_ITEM_DATA(N_("New"),
                       N_("Create a new account."),
                       gnc_ui_add_account, mwindow->root_item, GNOME_APP_PIXMAP_NONE),
      GNOMEUIINFO_ITEM_DATA(N_("Edit"), 
                       N_("Edit account information."), 
                       NULL, NULL, GNOME_APP_PIXMAP_NONE),
      GNOMEUIINFO_ITEM_DATA(N_("Delete"), N_("Delete selected account."), 
                       gnc_ui_delete_account_cb, mwindow->root_item, GNOME_APP_PIXMAP_NONE),
      GNOMEUIINFO_ITEM(N_("Exit"), N_("Exit GnuCash."),
                       gnc_shutdown, GNOME_APP_PIXMAP_NONE),
      GNOMEUIINFO_END
     };


    gnome_app_create_toolbar(GNOME_APP(app), toolbar);
    gnome_app_create_menus(GNOME_APP(app), mainmenu);

  }

  /* Cram accounts into the tree widget */
  gnc_ui_acct_tree_fill(GTK_TREE(mwindow->root_item), accts, -1);

  /* Create the notebook */
  notebook = gtk_notebook_new ();
  gtk_notebook_set_tab_pos ( GTK_NOTEBOOK( notebook ), GTK_POS_TOP );
  gtk_widget_show ( notebook );
  
  /* Create main vbox */
  main_vbox = gtk_vbox_new( FALSE, 1 );
  gtk_container_border_width( GTK_CONTAINER( main_vbox ), 2 );
  gnome_app_set_contents ( GNOME_APP ( app ), main_vbox );

  /* Create main hbox which will hold the clist widget */
  clist_vbox = gtk_vbox_new( FALSE, 1 );

  /* Now create clist, and pack it in the hbox we just created */
  clist = gtk_clist_new_with_titles ( 3, clist_titles );
  gtk_box_pack_start ( GTK_BOX( clist_vbox ), clist, TRUE, TRUE, 1 );
  
  /* Fix the column widths */
  gtk_clist_set_column_width ( GTK_CLIST(clist), 1, 85 );
  gtk_clist_set_column_width ( GTK_CLIST(clist), 0, 85 );
  
  gtk_widget_show ( clist );
      
  /* create scrolled window */
  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
				  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_show (scrolled_win);

  gtk_container_add( GTK_CONTAINER( main_vbox ), notebook );
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_win), GTK_WIDGET(mwindow->maintree));

  /* Append some pages to notebook */
  {
    GtkWidget *label;
    
    label = gtk_label_new ( " Bank Accounts " );
    gtk_notebook_append_page (GTK_NOTEBOOK(notebook), scrolled_win, label);
  }
  
  gtk_widget_show(GTK_WIDGET(mwindow->maintree));

  /* Setup some callbacks */
	
  //gtk_signal_connect (GTK_OBJECT(window), "destroy",
  //                    GTK_SIGNAL_FUNC (gnucash_shutdown), NULL);
  //gtk_signal_connect (GTK_OBJECT (window), "delete_event",
  //                    GTK_SIGNAL_FUNC (gnucash_shutdown), NULL);		       

  gtk_widget_set_usize ( GTK_WIDGET(app), 400, 400 );		      
		      
  /* Show everything now that it is created */

  gtk_widget_show(main_vbox);
  gtk_widget_show ( app );

} 



/* OLD_GNOME_CODE */

void
gnc_ui_shutdown (GtkWidget *widget, gpointer *data) {
  gtk_main_quit ();
}
    
#if 0

/* FIXME: This was the old shutdown code.  It probably needs to be
   migrated to whatever function is being called by gncFileQuerySave() */

  GtkWidget *msgbox;
  msgbox = gnome_message_box_new ( FMB_SAVE_MSG,
                                   GNOME_MESSAGE_BOX_ERROR, 
                                   GNOME_STOCK_BUTTON_OK,
                                   GNOME_STOCK_BUTTON_CANCEL, NULL );
  gnome_dialog_button_connect (GNOME_DIALOG (msgbox), 0,
                               GTK_SIGNAL_FUNC (file_cmd_save), 
                               NULL);
  gnome_dialog_button_connect (GNOME_DIALOG (msgbox), 0,
                               GTK_SIGNAL_FUNC (file_cmd_quit), 
                               NULL);                                 
  gnome_dialog_button_connect (GNOME_DIALOG (msgbox), 1,
                               GTK_SIGNAL_FUNC (file_cmd_quit), 
                               NULL);                                                    
    gtk_widget_show ( msgbox );   

#endif



/********************* END OF FILE **********************************/

/*
  Local Variables:
  tab-width: 2
  indent-tabs-mode: nil
  mode: c-mode
  c-indentation-style: gnu
  eval: (c-set-offset 'block-open '-)
  End:
*/

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
#include "Add_Dialog.h"
#include "MenuBar.h"
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
      create_add_account_dialog(acc);  
    }
   
  }
  else
  {
    create_add_account_dialog(NULL);
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

static void
quit_menu_item_helper() {
  gnc_shutdown(0);
}

static void
gnc_ui_options_cb ( GtkWidget *widget, gpointer data ) {
  gnc_show_options_dialog();
}

void
mainWindow() {

  GtkWidget 	*scrolled_win;
  GtkWidget 	*main_vbox;
  GtkWidget 	*clist_vbox;
  GtkWidget	  *menubar;
  GtkWidget 	*clist;
  GtkWidget 	*notebook;
  AccountGroup *accts = xaccSessionGetGroup(current_session);
  int nmenu_items;
  /*GtkAcceleratorTable *accel;*/


  /* this is the GtkMenuEntry structure used to create new menus.  The
     first member is the menu definition string.  The second, the default
     accelerator key used to access this menu function with the keyboard.
     The third is the callback function to call when this menu item is
     selected (by the accelerator key, or with the mouse.) The last
     member is the data to pass to your callback function.  */
  
  GtkMenuEntry menu_items[] =
  {
    {"<Main>/File/New", "<control>N", NULL, NULL},
    {"<Main>/File/Open", "<control>O", file_cmd_open, NULL},
    {"<Main>/File/Import", NULL, file_cmd_import, NULL},
    {"<Main>/File/Save", "<control>S", file_cmd_save, NULL},
    {"<Main>/File/Save as", NULL, NULL, NULL},
    {"<Main>/File/<separator>", NULL, NULL, NULL},
    {"<Main>/File/Quit", "<control>Q", quit_menu_item_helper, NULL },
    {"<Main>/Options/Preferences..", "<control>A", gnc_ui_options_cb, NULL},
    {"<Main>/Help/Help", NULL, gnc_ui_help_cb, NULL},
    {"<Main>/Help/<separator>", NULL, NULL, NULL},
    {"<Main>/Help/About..", NULL, gnc_ui_about_cb, NULL}
  };

  MenuBar *main_menu_bar;

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

    /* calculate the number of menu_item's */
    nmenu_items = sizeof(menu_items) / sizeof(menu_items[0]);

    gnome_app_create_toolbar(GNOME_APP(app), toolbar);
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

  {
    MenuBarGroup *mbg = menuBarGroupCreate();
    main_menu_bar = menuBarCreate(mbg, "<Main>");
    menuBarGroupAddItems(mbg, menu_items, nmenu_items);
  }

  menubar = main_menu_bar->widget;
  /*accel = main_menu_bar->table;*/

//gtk_window_add_accelerator_table(GTK_WINDOW(window), accel);
  gnome_app_set_menus ( GNOME_APP (app), GTK_MENU_BAR (menubar));
  gtk_container_add( GTK_CONTAINER( main_vbox ), notebook );
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_win),
                                        GTK_WIDGET(mwindow->maintree));

  /* Append some pages to notebook */
  {
    GtkWidget *label;
   // label = gtk_label_new ( " All Accounts " );
   // gtk_notebook_append_page (GTK_NOTEBOOK(notebook), clist_vbox, label);
    
    label = gtk_label_new ( " Bank Accounts " );
    gtk_notebook_append_page (GTK_NOTEBOOK(notebook), scrolled_win, label);
  }
  
  //gtk_widget_show(clist_vbox);
  gtk_widget_show(menubar);
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

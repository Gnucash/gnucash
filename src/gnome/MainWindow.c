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

#include <gnome.h>

#include "config.h"

#include "gnucash.h"
#include "dialog-options.h"
#include "AccWindow.h"
#include "MenuCommands.h"
#include "messages.h"
#include "RegWindow.h"
#include "top-level.h"
#include "version.h"
#include "MainWindow.h"
#include "MainWindowP.h"
#include "util.h"

/** STRUCTURES ******************************************************/

/** PROTOTYPES ******************************************************/
static void gnc_ui_options_cb( GtkWidget *, gpointer );
static void gnc_ui_add_account( GtkWidget *, gpointer );
static void gnc_ui_delete_account_cb( GtkWidget *, gpointer );
static void gnc_ui_about_cb( GtkWidget *, gpointer );
static void gnc_ui_help_cb( GtkWidget *, gpointer );
static void gnc_ui_mainWindow_toolbar_open( GtkWidget *, gpointer );
static void gnc_ui_mainWindow_toolbar_edit( GtkWidget *, gpointer );

/** GLOBALS *********************************************************/
char	    *HELP_ROOT = "";

/** Menus ***********************************************************/
static GnomeUIInfo filemenu[] = {
       {GNOME_APP_UI_ITEM, 
       N_("New"), N_("Create New File."),
       NULL, NULL, NULL, 
       GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_NEW,
       0, 0, NULL},
       {GNOME_APP_UI_ITEM,
       N_("Open"), N_("Open File."),
       file_cmd_open, NULL, NULL,
       GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_OPEN,
       0,0, NULL},
       {GNOME_APP_UI_ITEM,
       N_("Save"), N_("Save File."),
       file_cmd_save, NULL, NULL,
       GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_SAVE,
       0, 0, NULL},
       {GNOME_APP_UI_ITEM,
       N_("Import"), N_("Import QIF File."),
       file_cmd_import, NULL, NULL,
       GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CONVERT,
       0, 0, NULL},
       GNOMEUIINFO_SEPARATOR,
       {GNOME_APP_UI_ITEM,
       N_("Exit"), N_("Exit Gnucash."),
       gnc_shutdown, NULL, NULL,
       GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_QUIT,
       0, 0, NULL},       
       GNOMEUIINFO_END             
};

static GnomeUIInfo optionsmenu[] = {
	{GNOME_APP_UI_ITEM,
	 N_("Preferences"), N_("Preferences"),
	 gnc_ui_options_cb, NULL, NULL,
	 GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF,
	 0, 0, NULL},
	 GNOMEUIINFO_END
};
  
static GnomeUIInfo accountsmenu[] = {
	{GNOME_APP_UI_ITEM,
	 N_("View"), N_("View Account"),
	 gnc_ui_mainWindow_toolbar_open, NULL, NULL,
	 GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_OPEN,
	 0, 0, NULL},
	{GNOME_APP_UI_ITEM,
	 N_("Edit"), N_("Edit Account"),
	 gnc_ui_mainWindow_toolbar_edit, NULL, NULL,
	 GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PROP,
	 0, 0, NULL},
	GNOMEUIINFO_SEPARATOR,
	{GNOME_APP_UI_ITEM,
	 N_("Add"), N_("Add Account"),
	 gnc_ui_add_account, NULL, NULL,
	 GNOME_APP_PIXMAP_NONE, NULL,
	 0, 0, NULL},
	{GNOME_APP_UI_ITEM,
	 N_("Remove"), N_("Remove Account"),
	 gnc_ui_delete_account_cb, NULL, NULL,
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

static GnomeUIInfo mainmenu[] = {
    GNOMEUIINFO_SUBTREE(N_("File"), filemenu),
    GNOMEUIINFO_SUBTREE(N_("Accounts"), accountsmenu),
    GNOMEUIINFO_SUBTREE(N_("Options"), optionsmenu),
    GNOMEUIINFO_SUBTREE(N_("Help"), helpmenu),
    GNOMEUIINFO_END
};

/** TOOLBAR ************************************************************/
GnomeUIInfo toolbar[] = 
{
  { GNOME_APP_UI_ITEM,
    N_("Open"), 
    N_("Open File."),
    file_cmd_open, 
    NULL, 
    NULL,
    GNOME_APP_PIXMAP_STOCK, 
    GNOME_STOCK_PIXMAP_OPEN, 0, 0, NULL
  },
  { GNOME_APP_UI_ITEM,
    N_("Save"), 
    N_("Save File."),
    file_cmd_save, 
    NULL, 
    NULL,
    GNOME_APP_PIXMAP_STOCK, 
    GNOME_STOCK_PIXMAP_SAVE, 0, 0, NULL
  },
  { GNOME_APP_UI_ITEM,
    N_("Import"), 
    N_("Import QIF File."),
    file_cmd_import, 
    NULL, 
    NULL,
    GNOME_APP_PIXMAP_STOCK, 
    GNOME_STOCK_PIXMAP_CONVERT, 0, 0, NULL
  },
  GNOMEUIINFO_SEPARATOR,
  { GNOME_APP_UI_ITEM, 
    N_("View"), 
    N_("View selected account."),
    gnc_ui_mainWindow_toolbar_open, 
    NULL,
    NULL,
    GNOME_APP_PIXMAP_STOCK,
    GNOME_STOCK_PIXMAP_OPEN, 0, 0, NULL 
  },
  { GNOME_APP_UI_ITEM,
    N_("Edit"), 
    N_("Edit account information."), 
    gnc_ui_mainWindow_toolbar_edit, 
    NULL,
    NULL,
    GNOME_APP_PIXMAP_STOCK,
    GNOME_STOCK_PIXMAP_PROPERTIES, 0, 0, NULL
  },
  GNOMEUIINFO_SEPARATOR,
  { GNOME_APP_UI_ITEM,
    N_("Add"),
    N_("Add a new account."),
    gnc_ui_add_account, 
    NULL,
    NULL,
    GNOME_APP_PIXMAP_STOCK,
    GNOME_STOCK_PIXMAP_ADD, 0, 0, NULL
  },
  { GNOME_APP_UI_ITEM,
    N_("Remove"), 
    N_("Remove selected account."), 
    gnc_ui_delete_account_cb, 
    NULL,
    NULL,
    GNOME_APP_PIXMAP_STOCK,
    GNOME_STOCK_PIXMAP_REMOVE, 0, 0, NULL
  },
  GNOMEUIINFO_SEPARATOR,
  { GNOME_APP_UI_ITEM,
    N_("Exit"), 
    N_("Exit GnuCash."),
    gnc_shutdown, 
    NULL,
    NULL,
    GNOME_APP_PIXMAP_STOCK,
    GNOME_STOCK_PIXMAP_QUIT, 0, 0, NULL
  },
  GNOMEUIINFO_END
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

//static void
//acct_ctree_open_selected(GtkWidget *child) {  
//  Account *acct = gtk_ctree_node_get_row_data(GTK_CTREE(child));
//  fprintf(stderr, "calling regWindowSimple(%p)\n", acct);
//  regWindowSimple(acct);
//}

static gint
acct_ctree_select(GtkWidget *widget, GdkEventButton *event, GtkWidget *child) 
{
  /* Catch a double or single click */
  if (GTK_IS_CTREE(widget) &&
     (event->type==GDK_2BUTTON_PRESS ||
      event->type==GDK_3BUTTON_PRESS) ) 
  {
    //acct_tree_open_selected(child);         
  }

  return FALSE;

}


void
refreshMainWindow()
{
  GtkWidget *statusbar;
  guint     context_id;
  int i;
  double  assets  = 0.0;
  double  profits = 0.0;
  char buf[BUFSIZE];
  char *amt;
  AccountGroup *grp;
  Account *acc;
  int nacc;
   
  grp = xaccSessionGetGroup (current_session);
  //if (!grp) grp = topgroup;
  nacc = xaccGroupGetNumAccounts (grp);
  for (i=0; i<nacc; i++) {
     int acc_type;
     AccountGroup *acc_children;

     acc = xaccGroupGetAccount (grp,i);
 
     acc_type = xaccAccountGetType (acc);
     acc_children = xaccAccountGetChildren (acc);

     switch (acc_type) {
        case BANK:
        case CASH:
        case ASSET:
        case STOCK:
        case MUTUAL:
        case CREDIT:
        case LIABILITY:
           assets += xaccAccountGetBalance (acc);
           if (acc_children) {
              assets += xaccGroupGetBalance (acc_children);
           }
           break;
        case INCOME:
        case EXPENSE:
           profits -= xaccAccountGetBalance (acc); /* flip the sign !! */
           if (acc_children) {
              profits -= xaccGroupGetBalance (acc_children); /* flip the sign !! */
           }
           break;
        case EQUITY:
        default:
           break;
     }
  }
  
  amt = xaccPrintAmount (assets, PRTSYM | PRTSEP);
  strcpy (buf, " [ Assets: ");
  strcat (buf, amt);
  strcat (buf, " ] [ Profits: ");
  amt = xaccPrintAmount (profits, PRTSYM | PRTSEP);
  strcat (buf, amt);
  strcat (buf, "]");
   
  statusbar = gtk_object_get_data(GTK_OBJECT(app), "statusbar");

  context_id = gtk_statusbar_get_context_id( GTK_STATUSBAR(statusbar), 
                                            "Statusbar");
  
  gtk_statusbar_push( GTK_STATUSBAR(statusbar), context_id, buf);

  gnc_ui_refresh_tree();

}

void
gnc_ui_acct_ctree_fill(GtkCTree *ctree, GtkCTreeNode *parent, AccountGroup *accts)
{
  GtkCTreeNode *sibling;
  gint          totalAccounts = xaccGroupGetNumAccounts(accts);
  gint          currentAccount;
  gchar        *text[3];
  
  sibling = NULL;
  
  /* Add each account to the tree */  
  for ( currentAccount = 0;
        currentAccount < totalAccounts;
        currentAccount++ )
  {
    Account      *acc = xaccGroupGetAccount(accts, currentAccount);
    AccountGroup *hasChildren;
    gchar         buf[BUFSIZE];
//    GtkWidget    *popup;
        
    sprintf(buf, "%s%.2f", CURRENCY_SYMBOL, xaccAccountGetBalance(acc));
    
    text[0] = xaccAccountGetName(acc);
    text[1] = xaccAccountGetDescription(acc);
    text[2] = buf;
    
    sibling = gtk_ctree_insert_node (ctree, parent, sibling, text, 0,
                                     NULL, NULL, NULL, NULL,
				     FALSE, FALSE);
				           
    /* Set the user_data for the tree item to the account it */
    /* represents.                                           */
    gtk_ctree_node_set_row_data(GTK_CTREE(ctree), sibling, acc);
    
//    popup = gnome_popup_menu_new(accountsmenu);
//    gnome_popup_menu_attach (GTK_WIDGET(popup), GTK_WIDGET(treeItem), NULL);
    
    /* Connect the signal to the button press event */
    gtk_signal_connect (GTK_OBJECT (ctree), 
                        "button_press_event",
                        GTK_SIGNAL_FUNC(acct_ctree_select), 
                        GTK_WIDGET(ctree));
    
    /* Show the new tree item */
//    gtk_widget_show(GTK_WIDGET(treeItem));
    
    /* Check to see if this account has any children. If it
     * does then we need to build a subtree and fill it 
     */
    hasChildren = xaccAccountGetChildren(acc); 
     
    if(hasChildren)
    {
      /* Call gnc_ui_accWindow_tree_fill to fill this new subtree */
      gnc_ui_acct_ctree_fill(ctree, sibling, hasChildren );  
    }
  }
  				       
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
gnc_ui_acct_tree_fill(GtkTree *tree, AccountGroup *accts) 
{
  GtkWidget *treeItem;
  GtkWidget *subtree;
  gint       totalAccounts = xaccGroupGetNumAccounts(accts);
  gint       currentAccount;

  /* Add each account to the tree */  
  for ( currentAccount = 0;
        currentAccount < totalAccounts;
        currentAccount++ )
  {
    Account      *acc = xaccGroupGetAccount(accts, currentAccount);
    AccountGroup *hasChildren;
    GtkWidget    *popup;
        
    /* Create a new tree item for this account */
    treeItem = gtk_tree_item_new_with_label(xaccAccountGetName(acc));
    
    /* Set the user_data for the tree item to the account it */
    /* represents.                                           */
    gtk_object_set_user_data(GTK_OBJECT(treeItem), acc);
    
    /* Now append this tree item to the tree */
    gtk_tree_append(GTK_TREE(tree), GTK_WIDGET(treeItem));

    popup = gnome_popup_menu_new(accountsmenu);
    gnome_popup_menu_attach (GTK_WIDGET(popup), GTK_WIDGET(treeItem), NULL);
    
    /* Connect the signal to the button press event */
    gtk_signal_connect (GTK_OBJECT (treeItem), 
                        "button_press_event",
                        GTK_SIGNAL_FUNC(acct_tree_select), 
                        GTK_WIDGET(treeItem));
    
    /* Show the new tree item */
    gtk_widget_show(GTK_WIDGET(treeItem));
    
    /* Check to see if this account has any children. If it
     * does then we need to build a subtree and fill it 
     */
    hasChildren = xaccAccountGetChildren(acc); 
     
    if(hasChildren)
    {
      /* Create the subtree */
      subtree = gtk_tree_new();
    
      /* Append this new subtree to the current tree item */
      gtk_tree_item_set_subtree(GTK_TREE_ITEM(treeItem), GTK_WIDGET(subtree));
      
      /* Call gnc_ui_accWindow_tree_fill to fill this new subtree */
      gnc_ui_acct_tree_fill(GTK_TREE(subtree), hasChildren );  
            
    }
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
gnc_ui_refresh_tree() 
{
  GtkTree   *accountTree;
  GList     *list;

  accountTree = gtk_object_get_data(GTK_OBJECT(app), "accountTree");

  /* Make sure we are at the top of the tree */
  list = GTK_TREE(GTK_TREE_ROOT_TREE(accountTree))->children;

  gtk_tree_remove_items(GTK_TREE_ROOT_TREE(accountTree), list);
  
  /* Refill Tree with fresh data */
  gnc_ui_acct_tree_fill(accountTree, xaccSessionGetGroup(current_session));
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
                            "(C) 1998,1999 The GnuCash Project",
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

static void
gnc_ui_add_account ( GtkWidget *widget, gpointer data )
{
  GtkWidget *toplevel;
  GtkWidget *tree;
  GList *selection;
  
  toplevel = gtk_widget_get_toplevel(GTK_WIDGET(widget));
  tree     = gtk_object_get_data(GTK_OBJECT(app), "accountTree");

  /* FIXME: Right now this really is not doing anything        */
  /*        The new account dialog should use this information */  
  /*        to set the parent account...                       */
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
  GtkWidget *toplevel;
  GtkWidget *tree;
  GList *selection;
  
  toplevel = gtk_widget_get_toplevel(GTK_WIDGET(widget));
  tree     = gtk_object_get_data(GTK_OBJECT(app), "accountTree");
  
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
  GtkWidget *toplevel;
  GtkWidget *tree;
  GList *selection;
  
  toplevel = gtk_widget_get_toplevel(GTK_WIDGET(widget));
  tree     = gtk_object_get_data(GTK_OBJECT(app), "accountTree");
  
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
gnc_ui_mainWindow_toolbar_edit ( GtkWidget *widget, gpointer data )
{
  GtkWidget *toplevel;
  GtkWidget *tree;
  GList     *selection;
  
  toplevel = gtk_widget_get_toplevel(GTK_WIDGET(widget));
  tree     = gtk_object_get_data(GTK_OBJECT(app), "accountTree");
  
  selection = GTK_TREE_SELECTION ( tree );
  if ( selection )
  {
    if ( selection->data != NULL )
    {
      Account *acc = gtk_object_get_user_data(GTK_OBJECT(selection->data));
      editAccWindow( acc );
    }
  }
  else
  {
    GtkWidget *msgbox;
    msgbox = gnome_message_box_new ( " You must select an account to edit first. ",
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

void
mainWindow() {
  GtkTree      *accountTree;
  GtkWidget    *scrolled_win;
  GtkWidget    *main_vbox;
  GtkWidget    *statusbar;
  GtkWidget    *ctree;
  GtkWidget    *notebook;
  GtkWidget    *label;
  AccountGroup *accts = xaccSessionGetGroup(current_session);
  gchar        *ctitles[] = 
               { 
                    "Name",
                    "Description",
                    "Balance"
               };

  accountTree = GTK_TREE(gtk_tree_new());

  gtk_object_set_data (GTK_OBJECT(app), "accountTree",  accountTree);

  /* Create ctree */
  
  ctree = gtk_ctree_new_with_titles(3, 0, ctitles);

  gtk_object_set_data(GTK_OBJECT(app), "ctree", ctree);

  gnome_app_create_toolbar(GNOME_APP(app), toolbar);
  gnome_app_create_menus  (GNOME_APP(app), mainmenu);

  /* Cram accounts into the tree widget */
  gnc_ui_acct_tree_fill(GTK_TREE(accountTree), accts);
  gnc_ui_acct_ctree_fill(GTK_CTREE(ctree), NULL, accts);

  /* Create main vbox */
  main_vbox = gtk_vbox_new( FALSE, 1 );
  gtk_container_border_width( GTK_CONTAINER( main_vbox ), 2 );
  gnome_app_set_contents ( GNOME_APP ( app ), main_vbox );

  /* create scrolled window */
  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
				  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_show (scrolled_win);

//  gtk_box_pack_start( GTK_BOX(main_vbox), scrolled_win, TRUE, TRUE, 0 );
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_win), GTK_WIDGET(accountTree));

  notebook = gtk_notebook_new();

  gtk_notebook_set_tab_pos (GTK_NOTEBOOK (notebook), GTK_POS_LEFT);
  
  label = gtk_label_new("tree widget");
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook), scrolled_win, label);
  gtk_widget_show(label);
  
  label = gtk_label_new("ctree widget");
  gtk_notebook_append_page(GTK_NOTEBOOK(notebook), ctree, label);
  gtk_widget_show(label);
  
  gtk_box_pack_start(GTK_BOX(main_vbox), notebook, TRUE, TRUE, 0);
  gtk_widget_show(notebook);

  statusbar = gtk_statusbar_new ();
  gtk_object_set_data (GTK_OBJECT (app), "statusbar", statusbar);
  gtk_widget_show (statusbar);
  gtk_box_pack_start (GTK_BOX (main_vbox), statusbar, FALSE, FALSE, 0); 

  gtk_widget_show(GTK_WIDGET(accountTree));

  gtk_widget_set_usize ( GTK_WIDGET(app), 500, 400 );		      


  gtk_signal_connect_after (GTK_OBJECT (ctree), "button_press_event",
		            GTK_SIGNAL_FUNC (acct_ctree_select), NULL);

  /* Show everything now that it is created */

  gtk_widget_show(main_vbox);
  gtk_widget_show(ctree);
  gtk_widget_show(app);

  refreshMainWindow();

} 

/* OLD_GNOME_CODE */

void
gnc_ui_shutdown (GtkWidget *widget, gpointer *data) 
{
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

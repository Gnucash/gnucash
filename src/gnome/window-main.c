/********************************************************************\
 * MainWindow.c -- the main window, and associated helper functions * 
 *                 and callback functions for xacc (X-Accountant)   *
 * Copyright (C) 1998,1999 Jeremy Collins	                    *
 * Copyright (C) 1998,1999 Linas Vepstas                            *
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
#include <guile/gh.h>

#include "config.h"


#include "AccWindow.h"
#include "dialog-options.h"
#include "FileDialog.h"
#include "g-wrap.h"
#include "gnucash.h"
#include "MainWindow.h"
#include "messages.h"
#include "RegWindow.h"
#include "top-level.h"
#include "version.h"
#include "window-mainP.h"
#include "window-help.h"
#include "util.h"


/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;


enum {
  FMB_NEW,
  FMB_OPEN,
  FMB_IMPORT,
  FMB_SAVE,
  FMB_SAVEAS,
  FMB_QUIT,
};

enum {
  VIEW_CTREE_BALANCE,
  VIEW_CTREE_DESCRIPTION,
  VIEW_CTREE_TYPE,
};

/** STRUCTURES ******************************************************/

/** PROTOTYPES ******************************************************/
static void gnc_ui_options_cb( GtkWidget *, gpointer );
static void gnc_ui_add_account( GtkWidget *, gpointer );
static void gnc_ui_delete_account_cb( GtkWidget *, gpointer );
static void gnc_ui_about_cb( GtkWidget *, gpointer );
static void gnc_ui_help_cb( GtkWidget *, gpointer );
static void gnc_ui_reports_cb( GtkWidget *, gchar * );
static void gnc_ui_view_cb( GtkWidget *, gint );
static void gnc_ui_filemenu_cb( GtkWidget *, gint *);
static void gnc_ui_mainWindow_toolbar_open( GtkWidget *, gpointer );
static void gnc_ui_mainWindow_toolbar_edit( GtkWidget *, gpointer );
static void gnc_ui_refresh_statusbar();

/** GLOBALS *********************************************************/
char	*HELP_ROOT = "";
short   show_categories = 1;
extern GtkWidget *app;

/** Menus ***********************************************************/
static GnomeUIInfo filemenu[] = {
       {GNOME_APP_UI_ITEM, 
       N_("New"), N_("Create New File."),
       gnc_ui_filemenu_cb, (gpointer)FMB_NEW, NULL, 
       GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_NEW,
       0, 0, NULL},
       {GNOME_APP_UI_ITEM,
       N_("Open"), N_("Open File."),
       gnc_ui_filemenu_cb, (gpointer)FMB_OPEN, NULL,
       GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_OPEN,
       0,0, NULL},
       {GNOME_APP_UI_ITEM,
       N_("Save"), N_("Save File."),
       gnc_ui_filemenu_cb, (gpointer)FMB_SAVE, NULL,
       GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_SAVE,
       0, 0, NULL},
       {GNOME_APP_UI_ITEM,
       N_("Import"), N_("Import QIF File."),
       gnc_ui_filemenu_cb, (gpointer)FMB_IMPORT, NULL,
       GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CONVERT,
       0, 0, NULL},
       GNOMEUIINFO_SEPARATOR,
       {GNOME_APP_UI_ITEM,
       N_("Exit"), N_("Exit Gnucash."),
       gnc_ui_filemenu_cb, (gpointer)FMB_QUIT, NULL,
       GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_QUIT,
       0, 0, NULL},       
       GNOMEUIINFO_END             
};

static GnomeUIInfo viewmenu[] = {
	{GNOME_APP_UI_ITEM,
	 N_("Hide categories"), N_("Hide"),
	 gnc_ui_view_cb, 0, NULL,
	 GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF,
	 0, 0, NULL},
	 GNOMEUIINFO_END
};

static GnomeUIInfo showmenu[] = {
	{GNOME_APP_UI_ITEM,
	 N_("Show categories"), N_("Show"),
	 gnc_ui_view_cb, 0, NULL,
	 GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF,
	 0, 0, NULL},
	 GNOMEUIINFO_END
};

static GnomeUIInfo reportsmenu[] = {
	{GNOME_APP_UI_ITEM,
	 N_("Balance"), N_("BalanceReport"),
	 gnc_ui_reports_cb, "report-baln.phtml", NULL,
	 GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF,
	 0, 0, NULL},
	{GNOME_APP_UI_ITEM,
	 N_("Profit & Loss"), N_("ProfitLoss"),
	 gnc_ui_reports_cb, "report-pnl.phtml", NULL,
	 GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF,
	 0, 0, NULL},
	 
	 GNOMEUIINFO_END
};

static GnomeUIInfo optionsmenu[] = {
	{GNOME_APP_UI_ITEM,
	 N_("Preferences"), N_("Preferences"),
	 gnc_ui_options_cb, NULL, NULL,
	 GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF,
	 0, 0, NULL},
#if 0
	{GNOME_APP_UI_ITEM,
	 N_("Gnucash News"), N_("News"),
	 gnc_ui_news_callback, NULL, NULL,
	 GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF,
	 0, 0, NULL},	 
#endif	 
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


static GnomeUIInfo scriptsmenu[] = {
  GNOMEUIINFO_END
};

static GnomeUIInfo mainmenu[] = {
    GNOMEUIINFO_SUBTREE(N_("File"), filemenu),
    GNOMEUIINFO_SUBTREE(N_("View"), viewmenu),    
    GNOMEUIINFO_SUBTREE(N_("Accounts"), accountsmenu),
    GNOMEUIINFO_SUBTREE(N_("Reports"), reportsmenu),
    GNOMEUIINFO_SUBTREE(N_("Options"), optionsmenu),
    GNOMEUIINFO_SUBTREE(N_("Extensions"), scriptsmenu),    
    GNOMEUIINFO_SUBTREE(N_("Help"), helpmenu),
    GNOMEUIINFO_END
};

/** TOOLBAR ************************************************************/
GnomeUIInfo toolbar[] = 
{
  { GNOME_APP_UI_ITEM,
    N_("Open"), 
    N_("Open File."),
    gnc_ui_filemenu_cb, 
    (gpointer)FMB_OPEN, 
    NULL,
    GNOME_APP_PIXMAP_STOCK, 
    GNOME_STOCK_PIXMAP_OPEN, 'o', (GDK_CONTROL_MASK), NULL
  },
  { GNOME_APP_UI_ITEM,
    N_("Save"), 
    N_("Save File."),
    gnc_ui_filemenu_cb, 
    (gpointer)FMB_SAVE, 
    NULL,
    GNOME_APP_PIXMAP_STOCK, 
    GNOME_STOCK_PIXMAP_SAVE, 's', (GDK_CONTROL_MASK), NULL
  },
  { GNOME_APP_UI_ITEM,
    N_("Import"), 
    N_("Import QIF File."),
    gnc_ui_filemenu_cb, 
    (gpointer)FMB_IMPORT, 
    NULL,
    GNOME_APP_PIXMAP_STOCK, 
    GNOME_STOCK_PIXMAP_CONVERT, 'i', (GDK_CONTROL_MASK), NULL
  },
  GNOMEUIINFO_SEPARATOR,
  { GNOME_APP_UI_ITEM, 
    N_("View"), 
    N_("View selected account."),
    gnc_ui_mainWindow_toolbar_open, 
    NULL,
    NULL,
    GNOME_APP_PIXMAP_STOCK,
    GNOME_STOCK_PIXMAP_OPEN, 'v', (GDK_CONTROL_MASK), NULL 
  },
  { GNOME_APP_UI_ITEM,
    N_("Edit"), 
    N_("Edit account information."), 
    gnc_ui_mainWindow_toolbar_edit, 
    NULL,
    NULL,
    GNOME_APP_PIXMAP_STOCK,
    GNOME_STOCK_PIXMAP_PROPERTIES, 'e', (GDK_CONTROL_MASK), NULL
  },
  GNOMEUIINFO_SEPARATOR,
  { GNOME_APP_UI_ITEM,
    N_("Add"),
    N_("Add a new account."),
    gnc_ui_add_account, 
    NULL,
    NULL,
    GNOME_APP_PIXMAP_STOCK,
    GNOME_STOCK_PIXMAP_ADD, 'a', (GDK_CONTROL_MASK), NULL
  },
  { GNOME_APP_UI_ITEM,
    N_("Remove"), 
    N_("Remove selected account."), 
    gnc_ui_delete_account_cb, 
    NULL,
    NULL,
    GNOME_APP_PIXMAP_STOCK,
    GNOME_STOCK_PIXMAP_REMOVE, 'r', (GDK_CONTROL_MASK), NULL
  },
  GNOMEUIINFO_SEPARATOR,
  { GNOME_APP_UI_ITEM,
    N_("Exit"), 
    N_("Exit GnuCash."),
    gnc_shutdown, 
    NULL,
    NULL,
    GNOME_APP_PIXMAP_STOCK,
    GNOME_STOCK_PIXMAP_QUIT, 'q', (GDK_CONTROL_MASK), NULL
  },
  GNOMEUIINFO_END
};


static gint
ctree_button_press(GtkWidget *widget, GdkEventButton *event)
{
	GtkCTree *ctree = GTK_CTREE(widget);
	GtkCList *clist = GTK_CLIST(widget);
	GtkCTreeNode *node;
	Account *account, *old_acct;
	gint x, y, row, column;
	
	if (event->window == clist->clist_window) {
		x = event->x;
		y = event->y;

		if (!gtk_clist_get_selection_info(clist, x, y, &row, &column))
			return FALSE;

		node = gtk_ctree_node_nth (ctree, row);
		account = gtk_ctree_node_get_row_data(ctree, node);

		
		if (event->type == GDK_2BUTTON_PRESS) {
			/* so the the button_release will leave it selected */
			gtk_ctree_unselect (ctree, node);
           /* this will stop the node from being collapsed/expanded */
			gtk_signal_emit_stop_by_name (GTK_OBJECT(widget), "button_press_event");
			regWindowSimple ( account );
			return TRUE;
		}
  }
	return FALSE;
}

static gint
acct_ctree_select(GtkWidget *widget, GtkCTreeNode *row, gint column) 
{
  Account *account;
  
  account = (Account *)gtk_ctree_node_get_row_data(GTK_CTREE(widget), GTK_CTREE_NODE(row));
  gtk_object_set_data(GTK_OBJECT(app), "selected_account", account);
  
  return TRUE;
}

static gint
acct_ctree_unselect(GtkWidget *widget, GtkCTreeNode *row, gint column)
{
  gtk_object_set_data(GTK_OBJECT(app), "selected_account", NULL);
  
  return TRUE; 
}

static void
gnc_ui_refresh_statusbar()
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
   
  grp = gncGetCurrentGroup ();
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
  
}

/* Required for compatibility with Motif code... */
void
refreshMainWindow()
{
  gnc_ui_refresh_statusbar();
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
    int           acc_type = xaccAccountGetType (acc); 
    AccountGroup *acc_children = xaccAccountGetChildren (acc);
    gchar         buf[BUFSIZE];
    GtkWidget    *popup;
    double        dbalance = 0.0;

    /* fill in the balance column */
    dbalance = xaccAccountGetBalance (acc);
    /* if the account has children, add in thier balance */
    if (acc_children) {
       dbalance += xaccGroupGetBalance (acc_children);
    }

    /* the meaning of "balance" for income and expense
     * accounts is reversed, since a deposit of a paycheck in a
     * bank account will appear as a debit of the corresponding
     * amount in the income account */
    if ((EXPENSE == acc_type) ||
        (INCOME  == acc_type) ) {
      dbalance = -dbalance;
    }
    xaccSPrintAmount (buf, dbalance, PRTSYM | PRTSEP);

    text[0] = xaccAccountGetName(acc);
    text[1] = xaccAccountGetDescription(acc);
    text[2] = xaccPrintAmount (dbalance, PRTSYM | PRTSEP);
    
    sibling = gtk_ctree_insert_node (ctree, parent, sibling, text, 0,
                                     NULL, NULL, NULL, NULL,
                                     FALSE, FALSE);
				           
    /* Set the user_data for the tree item to the account it */
    /* represents.                                           */
    gtk_ctree_node_set_row_data(GTK_CTREE(ctree), sibling, acc);
    
    popup = gnome_popup_menu_new(accountsmenu);
    gnome_popup_menu_attach (GTK_WIDGET(popup), GTK_WIDGET(ctree), NULL);

//    gtk_ctree_toggle_expansion(GTK_CTREE(ctree), GTK_CTREE_NODE(sibling));

    
    /* If this account has children,
     * then we need to build a subtree and fill it 
     */
    if(acc_children)
    {
      /* Call gnc_ui_accWindow_tree_fill to fill this new subtree */
      gnc_ui_acct_ctree_fill(ctree, sibling, acc_children );  
    }
  }
  				       
}

 
static void
tree_set_row_text (GtkCTree *ctree, GtkCTreeNode *node, gpointer data)
{
       Account *acc = (Account *)gtk_ctree_node_get_row_data(ctree, node);
         AccountGroup *hasChildren = xaccAccountGetChildren(acc);
         int acc_type = xaccAccountGetType (acc);
         double balance;
         gchar *text[3];
         int i;
 
         hasChildren = xaccAccountGetChildren(acc);
 
         balance = xaccAccountGetBalance(acc) + xaccGroupGetBalance(hasChildren);
         
         if ((EXPENSE == acc_type) || (INCOME == acc_type))
                 balance = -balance;
         
         text[0] = xaccAccountGetName(acc);
         text[1] = xaccAccountGetDescription(acc);
         text[2] = xaccPrintAmount (balance, PRTSYM | PRTSEP);
 
         for (i=0; i<3; i++)
                 gtk_ctree_node_set_text (ctree, node, i, text[i]);
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
  GtkCTree     *ctree;
  GtkCTreeNode *parent;
  
  parent = gtk_object_get_data(GTK_OBJECT(app), "ctree_parent");
  ctree  = gtk_object_get_data(GTK_OBJECT(app), "ctree");

  gtk_clist_freeze(GTK_CLIST(ctree));
   
  gtk_ctree_pre_recursive(ctree, parent, (GtkCTreeFunc)tree_set_row_text, NULL);

  gtk_clist_thaw(GTK_CLIST(ctree));
  gtk_clist_columns_autosize(GTK_CLIST(ctree));  
 
}

static void
gnc_ui_about_cb (GtkWidget *widget, gpointer data)
{
  helpWindow( GTK_WIDGET(app), ABOUT_STR, HH_ABOUT ); 
}                          

static void
gnc_ui_help_cb ( GtkWidget *widget, gpointer data )
{
  helpWindow( GTK_WIDGET(app), HELP_STR, HH_MAIN );
}

static void
gnc_ui_reports_cb(GtkWidget *widget, gchar *report)
{
  reportWindow (widget, "duuuude", report);  
}

static void
gnc_ui_add_account ( GtkWidget *widget, gpointer data )
{
  GtkWidget *toplevel;
  Account   *account;
  
  toplevel = gtk_widget_get_toplevel(GTK_WIDGET(widget));
  account  = gtk_object_get_data(GTK_OBJECT(app), "selected_account");

  accWindow((AccountGroup*)account);
}

static void
gnc_ui_delete_account_finish_cb ( GtkWidget *widget, gpointer data )
{
  GtkCTreeNode *deleteRow;
  GtkCTreeNode *parentRow;
  GtkCTree     *ctree;
  Account      *account = data;

  ctree      = gtk_object_get_data(GTK_OBJECT(app), "ctree");
  parentRow  = gtk_object_get_data(GTK_OBJECT(app), "ctree_parent");

  /* Step 1: Delete the actual account */  
  xaccRemoveAccount ( account );
  xaccFreeAccount ( account );

  /* Step 2: Find the GtkCTreeNode that matches this account */
  deleteRow = gtk_ctree_find_by_row_data(GTK_CTREE(ctree), parentRow, account);
  
  /* Step 3: Delete the GtkCTreeNode we just found */
  gtk_ctree_remove_node(GTK_CTREE(ctree), deleteRow);
  
  /* Step 4: Refresh the toolbar */
  gnc_ui_refresh_statusbar();
}

static void
gnc_ui_delete_account_cb ( GtkWidget *widget, gpointer data )
{
  GtkWidget *toplevel;
  Account   *account;
  
  toplevel = gtk_widget_get_toplevel(GTK_WIDGET(widget));
  account  = gtk_object_get_data(GTK_OBJECT(app), "selected_account");
  

  if (account)
  {
    GtkWidget *msgbox;
      
     msgbox = gnome_message_box_new ( " Are you sure you want to delete this account. ",
                                       GNOME_MESSAGE_BOX_WARNING, 
                                       GNOME_STOCK_BUTTON_OK,
                                       GNOME_STOCK_BUTTON_CANCEL, NULL );
     gnome_dialog_button_connect (GNOME_DIALOG (msgbox), 0,
                                  GTK_SIGNAL_FUNC (gnc_ui_delete_account_finish_cb), 
                                  account);
     gtk_window_set_modal(GTK_WINDOW(msgbox) ,TRUE );
     gtk_widget_show ( msgbox );
  }
  else
  {
    GtkWidget *msgbox;
    msgbox = gnome_message_box_new(ACC_DEL_MSG, GNOME_MESSAGE_BOX_ERROR, "Ok", NULL);
    gtk_window_set_modal(GTK_WINDOW(msgbox) ,TRUE );
    gtk_widget_show ( msgbox );    
  }
}

static void
gnc_ui_mainWindow_toolbar_open ( GtkWidget *widget, gpointer data )
{
  Account   *account;
  GtkWidget *toplevel;
  
  toplevel = gtk_widget_get_toplevel(GTK_WIDGET(widget));
  account  = gtk_object_get_data(GTK_OBJECT(app), "selected_account");
  
  if(account)
  {
   PINFO ("calling regWindowSimple(%p)\n", account);
   regWindowSimple ( account );
  }
  else
  {
    GtkWidget *msgbox;
    msgbox = gnome_message_box_new ( " You must select an account to open first. ",
                                     GNOME_MESSAGE_BOX_ERROR, "Ok", NULL );
    gtk_window_set_modal(GTK_WINDOW(msgbox) ,TRUE );
    gtk_widget_show ( msgbox );    
  }
}

static void
gnc_ui_mainWindow_toolbar_edit ( GtkWidget *widget, gpointer data )
{
  GtkWidget *toplevel;
  Account   *account;
  
  toplevel = gtk_widget_get_toplevel(GTK_WIDGET(widget));
  account  = gtk_object_get_data(GTK_OBJECT(app), "selected_account");
  
  if (account)
  {
    editAccWindow( account );
  }
  else
  {
    GtkWidget *msgbox;
    msgbox = gnome_message_box_new(ACC_EDIT_MSG, GNOME_MESSAGE_BOX_ERROR, "Ok", NULL);
    gtk_window_set_modal(GTK_WINDOW(msgbox) ,TRUE );
    gtk_widget_show ( msgbox );    
  }  
}

static void
gnc_ui_options_cb ( GtkWidget *widget, gpointer data ) {
  gnc_show_options_dialog();
}

/* This function currently just hides/shows the INCOME/EXPENSE
 * accounts.  It could and should be extended to allow full
 * customization of the ctree widget... for instance the user
 * should be able to choose which fields get displayed such as
 * balance, description, account type, etc...
 */
static void
gnc_ui_view_cb(GtkWidget *widget, gint viewType)
{

  GtkWidget *ctree;

  ctree = gtk_object_get_data(GTK_OBJECT(app), "ctree");  
  
  if(show_categories == 1)
  {
    /* Widget label -> Hide Categories */
    gnome_app_remove_menus (GNOME_APP(app), "View/Hide categories", 1);
    gnome_app_insert_menus (GNOME_APP(app), "View/", showmenu);
    gtk_clist_set_column_visibility(GTK_CLIST(ctree), 2, FALSE);  
    show_categories = 0;
  }
  else
  {
    /* Widget lable -> Show Categories */
    gnome_app_remove_menus (GNOME_APP(app), "View/Show categories", 1);
    gnome_app_insert_menus (GNOME_APP(app), "View/", viewmenu);
    gtk_clist_set_column_visibility(GTK_CLIST(ctree), 2, TRUE);  
    show_categories = 1;
  }



  /* We really need a smarter refresh routine... one that will 
   * only remove the INCOME/EXPENSE accounts... or add them... instead of
   * just wiping the whole thing out and starting over.
   */    
  refreshMainWindow();  
}

void
gnc_ui_filemenu_cb(GtkWidget *widget, gint *menuItem)
{
  switch((gint)menuItem)
  {
    case FMB_OPEN:   gncFileOpen();
                     refreshMainWindow();
                     break;
    case FMB_SAVE:   gncFileSave();
                     break;
    case FMB_IMPORT: gncFileQIFImport();
                     break;
    case FMB_QUIT:   gnc_shutdown(0);
                     break;
    default: break;  
  }  
}

void
mainWindow() {
  GtkWidget    *scrolled_win;
  GtkWidget    *main_vbox;
  GtkWidget    *statusbar;
  GtkWidget    *ctree;
  GtkCTreeNode *parent = NULL;
  AccountGroup *accts = gncGetCurrentGroup();
  gchar        *ctitles[] = {ACC_NAME_STR, DESC_STR, BALN_STR};

  /* Create ctree */
  ctree = gtk_ctree_new_with_titles(3, 0, ctitles);

  gtk_signal_connect (GTK_OBJECT(ctree),
                      "button_press_event",
                      GTK_SIGNAL_FUNC(ctree_button_press),
                      NULL);

    /* Connect the signal to the tree_select_row event */
    gtk_signal_connect (GTK_OBJECT(ctree), 
                        "tree_select_row",
                        GTK_SIGNAL_FUNC(acct_ctree_select), 
                        NULL);

    gtk_signal_connect (GTK_OBJECT(ctree), 
                        "tree_unselect_row",
                        GTK_SIGNAL_FUNC(acct_ctree_unselect), 
                        NULL);                        

  gtk_object_set_data(GTK_OBJECT(app), "ctree", ctree);
  gtk_object_set_data(GTK_OBJECT(app), "ctree_parent", parent);

  gnome_app_create_toolbar(GNOME_APP(app), toolbar);
  gnome_app_create_menus  (GNOME_APP(app), mainmenu);

  /* Cram accounts into the ctree widget & make sure the columns are sized correctly */
  gtk_clist_freeze(GTK_CLIST(ctree));
  gnc_ui_acct_ctree_fill(GTK_CTREE(ctree), parent, accts);
  gtk_clist_thaw(GTK_CLIST(ctree));

  gtk_clist_columns_autosize(GTK_CLIST(ctree));
  gtk_clist_set_shadow_type (GTK_CLIST(ctree), GTK_SHADOW_IN);

  /* Create main vbox */
  main_vbox = gtk_vbox_new( FALSE, 0 );
  gnome_app_set_contents ( GNOME_APP ( app ), main_vbox );

  /* create scrolled window */
  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
                                  GTK_POLICY_AUTOMATIC, 
                                  GTK_POLICY_AUTOMATIC);
  gtk_widget_show (scrolled_win);

  gtk_container_add(GTK_CONTAINER(scrolled_win), GTK_WIDGET(ctree));
  gtk_box_pack_start(GTK_BOX(main_vbox), scrolled_win, TRUE, TRUE, 0);

  /* create statusbar and pack it into the main_vbox */
  statusbar = gtk_statusbar_new ();
  gtk_object_set_data (GTK_OBJECT (app), "statusbar", statusbar);
  gtk_widget_show (statusbar);
  gtk_box_pack_start (GTK_BOX (main_vbox), statusbar, FALSE, FALSE, 0); 

  gtk_widget_set_usize ( GTK_WIDGET(app), 500, 400 );		      

  {
    SCM run_danglers = gh_eval_str("gnc:hook-run-danglers");
    SCM hook = gh_eval_str("gnc:*main-window-opened-hook*");
    SCM window = POINTER_TOKEN_to_SCM(make_POINTER_TOKEN("gncUIWidget", app));
    gh_call2(run_danglers, hook, window); 
  }
  
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
    
/********************* END OF FILE **********************************/

/*
  Local Variables:
  tab-width: 2
  indent-tabs-mode: nil
  mode: c
  c-indentation-style: gnu
  eval: (c-set-offset 'block-open '-)
  End:
*/

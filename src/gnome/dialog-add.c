/********************************************************************\
 * AccWindow.c -- window for creating new accounts for xacc         *
 *                (X-Accountant)                                    *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998, 1999 Linas Vepstas                     *
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
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#include <gnome.h>
#include <stdio.h>

#include "config.h"

#include "AccWindow.h"
#include "AccInfo.h"
#include "Account.h"
#include "top-level.h"
#include "MainWindow.h"
#include "messages.h"
#include "util.h"
#include "dialog-editnotes.h"

/* Please look at ../motif/AccWindow.c for info on what should be
   going on in these functions */

struct _accwindow
{
  GnomeDialog 	*dialog;

  AccountGroup	*parentAccount;
  gint		 type;
};

static GtkWidget*
gnc_ui_get_widget (GtkWidget *widget, gchar *widget_name)
{
  GtkWidget *found_widget;

  if (widget->parent)
    widget = gtk_widget_get_toplevel (widget);
  found_widget = (GtkWidget*) gtk_object_get_data (GTK_OBJECT (widget),
                                                   widget_name);
  if (!found_widget)
    g_warning ("Widget not found: %s", widget_name);
  return found_widget;
}

/********************************************************************\
 *                                                                  * 
 * Args:   parent   - the parent of the window to be created        * 
 * Return: none                                                     *
\********************************************************************/
static void
gnc_ui_accWindow_list_cb   ( GtkWidget *listItem, gpointer data )
{
  GtkWidget *toplevel;
  GtkWidget *entrySecurity;
  AccWindow *accData;
  gint       accountTypeByNumber = 0;
      
  /* Find accData so we can set the account type */
  toplevel      = gtk_widget_get_toplevel(GTK_WIDGET(listItem));
  entrySecurity = gtk_object_get_data(GTK_OBJECT(toplevel), "entrySecurity");
  accData       = gtk_object_get_data(GTK_OBJECT(toplevel), "accData");
    
  while ( strcmp(xaccAccountGetTypeStr (accountTypeByNumber), 
                 gtk_object_get_data(GTK_OBJECT(listItem), 
                 "listItemData")))
  {
    accountTypeByNumber++;
  }
  
  accData->type = accountTypeByNumber;
  
  /* Set the Security field depending on what account type is selected */
  switch (accountTypeByNumber)
  {
    case STOCK    : 
    case MUTUAL   :
    case CURRENCY : gtk_widget_set_sensitive(GTK_WIDGET(entrySecurity), TRUE );
                    break;
    default       : gtk_widget_set_sensitive(GTK_WIDGET(entrySecurity), FALSE);

  } 
}

/********************************************************************\
 *                                                                  * 
 * Args:   parent   - the parent of the window to be created        * 
 * Return: none                                                     *
\********************************************************************/
static void
gnc_ui_accWindow_list_fill ( GtkWidget *listOfTypes )
{
  gchar      buffer[255];
  gint       i;
  GtkWidget *list_item;  
  
  for (i=0; i<NUM_ACCOUNT_TYPES; i++) 
  {
    GtkWidget       *label;
    gchar           *string;
             
    sprintf(buffer, "%s", xaccAccountGetTypeStr (i));
    label=gtk_label_new(buffer);
    list_item=gtk_list_item_new();
    gtk_container_add(GTK_CONTAINER(list_item), label);
    gtk_widget_show(label);
    gtk_container_add(GTK_CONTAINER(listOfTypes), list_item);
    gtk_widget_show(list_item);
    gtk_label_get(GTK_LABEL(label), &string);
    gtk_object_set_data(GTK_OBJECT(list_item),
                        "listItemData",
                        string);
    gtk_signal_connect ( GTK_OBJECT(list_item), "button_press_event",
                         gnc_ui_accWindow_list_cb, NULL);
  }
      
}

static void
gnc_ui_accWindow_tree_select ( GtkWidget *widget, gpointer data )
{
  GtkWidget   *toplevel;
  GtkWidget   *listOfTypes;
  GList       *children;
  GtkListItem *listItem;
  gchar       *accountTypeByName;
  AccWindow   *accData;
  
  /* Find accData so we can set the account type */
  toplevel    = gtk_widget_get_toplevel(GTK_WIDGET(widget));
  listOfTypes = gtk_object_get_data(GTK_OBJECT(toplevel), "listOfTypes");
  accData     = gtk_object_get_data(GTK_OBJECT(toplevel), "accData");  

  accData->parentAccount = gtk_object_get_user_data(GTK_OBJECT(widget));
  
  children = GTK_LIST(listOfTypes)->children;
  
  /* If parentAccount is Income/Expense then we need to make sure they */
  /* can't select any other type except Income/Expense.                */
  while(children)
  {
    listItem          = children->data;
    accountTypeByName = gtk_object_get_data(GTK_OBJECT(listItem), "listItemData");
    
    if( (strcmp(xaccAccountGetTypeStr(
                xaccAccountGetType((Account *)accData->parentAccount)),
                accountTypeByName) != 0 ) && 
        (strcmp(xaccAccountGetTypeStr(
                xaccAccountGetType((Account *)accData->parentAccount)),
                xaccAccountGetTypeStr(INCOME)) == 0 ) )
    {
      gtk_widget_set_sensitive(GTK_WIDGET(listItem), FALSE); 
      gtk_list_item_select(GTK_LIST_ITEM(listItem)); 
    }
    else
    {
      gtk_widget_set_sensitive(GTK_WIDGET(listItem), TRUE);
      gtk_list_item_deselect(GTK_LIST_ITEM(listItem));
    }
    
    children = children->next;
        
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
static void
gnc_ui_accWindow_tree_fill (GtkTree *tree, AccountGroup *accts) 
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
        
    /* Create a new tree item for this account */
    treeItem = gtk_tree_item_new_with_label(xaccAccountGetName(acc));
    
    /* Set the user_data for the tree item to the account it */
    /* represents.                                           */
    gtk_object_set_user_data(GTK_OBJECT(treeItem), acc);
    
    /* Now append this tree item to the tree */
    gtk_tree_append(GTK_TREE(tree), GTK_WIDGET(treeItem));
    
    /* Connect the signal to the button press event */
    gtk_signal_connect (GTK_OBJECT (treeItem), 
                        "button_press_event",
                        GTK_SIGNAL_FUNC(gnc_ui_accWindow_tree_select), 
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
      gnc_ui_accWindow_tree_fill(GTK_TREE(subtree), hasChildren );  
            
    }

  }
   
}

/********************************************************************\
 *                                                                  * 
 * Args:   parent   - the parent of the window to be created        * 
 * Return: none                                                     *
\********************************************************************/
static void
gnc_ui_accWindow_cancelled_callback( GtkWidget *ignore, gpointer data )
{
  AccWindow  *accData = data;
  
  gnome_dialog_close( GNOME_DIALOG(accData->dialog));
  g_free(accData);
}

/********************************************************************\
 * gnc_ui_accWindow_create_callback                                 *
 *    This function does the actually creating of the new account.  *
 *    It also validates the data entered first.                     *
 *                                                                  * 
 * Args:   dialog - button that was clicked                         * 
 *         data   - should be NULL                                  *
 * Return: none                                                     *
\********************************************************************/
static void 
gnc_ui_accWindow_create_callback(GtkWidget * dialog, gpointer data)
{

  Transaction 		*trans;
  Account      		*account;
  AccWindow             *accData = data; 
  GtkWidget             *entryAccountName;
  GtkWidget             *entryDescription;
  GtkWidget             *entryCurrency;
  GtkWidget             *entrySecurity;
  
  entryAccountName = gnc_ui_get_widget(GTK_WIDGET(dialog), "entryAccountName");
  entryDescription = gnc_ui_get_widget(GTK_WIDGET(dialog), "entryDescription");
  entryCurrency    = gnc_ui_get_widget(GTK_WIDGET(dialog), "entryCurrency"   );
  entrySecurity    = gnc_ui_get_widget(GTK_WIDGET(dialog), "entrySecurity"   );

  /* Check to make sure something was entered */
  if( strcmp( gtk_entry_get_text(GTK_ENTRY(entryAccountName)), "" ) == 0 ) 
  {
    GtkWidget *msgbox;
    msgbox = gnome_message_box_new ( "You must enter a filename",
                                     GNOME_MESSAGE_BOX_ERROR, "Ok", NULL );
    gtk_widget_show ( msgbox );
    return;
    }
  
  account = xaccMallocAccount();
            
  xaccAccountBeginEdit (account, 0);
 
  xaccAccountSetName        (account, gtk_entry_get_text(GTK_ENTRY(entryAccountName)));
  xaccAccountSetDescription (account, gtk_entry_get_text(GTK_ENTRY(entryDescription)));
  xaccAccountSetType        (account, accData->type);
  xaccAccountSetCurrency    (account, gtk_entry_get_text(GTK_ENTRY(entryCurrency)));
  xaccAccountSetSecurity    (account, gtk_entry_get_text(GTK_ENTRY(entrySecurity)));

  /* Add an opening balance transaction (as the first transaction) */
  trans = xaccMallocTransaction();
 
  xaccTransBeginEdit(trans, 1);
  xaccTransSetDateToday (trans);
  xaccTransSetDescription (trans, OPEN_BALN_STR);
  xaccTransCommitEdit(trans);
            
  /* add the new transaction to the account */
  xaccAccountInsertSplit (account, xaccTransGetSplit (trans, 0) );
  
  /* once the account is set up, add it to account group 
   * If the user indicated a parent acccount, make it a 
   * sub account of that */
       
  /* The g_print statements should be removed after we are sure 
   * everything is a ok with this callback 
   */       
       
  if ((Account *)accData->parentAccount) {
    xaccInsertSubAccount ((Account *)accData->parentAccount, account);
  } else {
    xaccGroupInsertAccount(xaccSessionGetGroup(current_session), account );
  }
  xaccAccountCommitEdit (account);

  refreshMainWindow();

  gnome_dialog_close(GNOME_DIALOG(gtk_widget_get_toplevel(dialog)));

}

/********************************************************************\
 * accWindow                                                        *
 *   opens up a window to create a new account... the account is    * 
 *   actually created in the "create" callback                      * 
 *                                                                  * 
 * Args:   parent   - the parent of the window to be created        * 
 * Return: none                                                     *
\********************************************************************/
AccWindow *
accWindow (AccountGroup *grp) 
{
  AccWindow *accData;
  GtkWidget *dialog_vbox3;
  GtkWidget *hbox2;
  GtkWidget *frame;
  GtkWidget *vbox1;
  GtkWidget *hbox3;
  GtkWidget *labelName;
  GtkWidget *entryAccountName;
  GtkWidget *hbox4;
  GtkWidget *labelDescription;
  GtkWidget *entryDescription;
  GtkWidget *hbox5;
  GtkWidget *labelCurrency;
  GtkWidget *entryCurrency;
  GtkWidget *hbox6;
  GtkWidget *labelSecurity;
  GtkWidget *entrySecurity;
  GtkWidget *hbox7;
  GtkWidget *frameListTypes;
  GtkWidget *frameList;
  GtkWidget *listOfTypes;
  GtkWidget *frameParentAccount;
  GtkWidget *frameParent;
  GtkWidget *scrolledWindow1;
  GtkWidget *scrolledWindow2;
  GtkWidget *tree1;
  gchar     *title      = SETUP_ACCT_STR;

  accData = (AccWindow *)g_malloc(sizeof(AccWindow));

  accData->parentAccount = grp;

  accData->dialog = GNOME_DIALOG( gnome_dialog_new ( title, 
                                  NOTES_STR,
                                  CREATE_STR,
                                  CANCEL_STR,
                                  NULL));

  /* Make this dialog modal */
  gtk_window_set_modal(GTK_WINDOW( &(accData->dialog)->window ),TRUE );

  /* Add accData to the dialogs object data so we can get it from anywhere later */
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "accData", accData );

  dialog_vbox3 = GNOME_DIALOG (accData->dialog)->vbox;
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "dialog_vbox3", dialog_vbox3);
  gtk_widget_show (dialog_vbox3);

  hbox2 = gtk_hbox_new (FALSE, 0);
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "hbox2", hbox2);
  gtk_widget_show (hbox2);
  gtk_box_pack_start (GTK_BOX (dialog_vbox3), hbox2, TRUE, TRUE, 0);

#if 0
  /* Un#if 0 this stuff to add the pixmap */
  /* You also might need to set_usize it a bit bigger */

  gtk_widget_realize (GTK_WIDGET(accData->dialog));
  glade_pixmap = gdk_pixmap_create_from_xpm (accData->dialog->window, &glade_mask,
                                             NULL,
                                             "/home/collins/gnucash/testcode/image/wizard");
  pixmap = gtk_pixmap_new (glade_pixmap, glade_mask);
  gdk_pixmap_unref (glade_pixmap);
  gdk_bitmap_unref (glade_mask);
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "pixmap", pixmap);
  gtk_widget_show (pixmap);
  gtk_box_pack_start (GTK_BOX (hbox2), pixmap, FALSE, TRUE, 5);
  gtk_misc_set_padding (GTK_MISC (pixmap), 5, 0);
#endif

  frame = gtk_frame_new (NULL);
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "frame", frame);
  gtk_widget_show (frame);
  gtk_box_pack_start (GTK_BOX (hbox2), frame, TRUE, TRUE, 0);
  gtk_container_border_width (GTK_CONTAINER (frame), 10);
  gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_NONE);

  vbox1 = gtk_vbox_new (FALSE, 0);
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "vbox1", vbox1);
  gtk_widget_show (vbox1);
  gtk_container_add (GTK_CONTAINER (frame), vbox1);

  hbox3 = gtk_hbox_new (FALSE, 0);
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "hbox3", hbox3);
  gtk_widget_show (hbox3);
  gtk_box_pack_start (GTK_BOX (vbox1), hbox3, FALSE, TRUE, 0);

  labelName = gtk_label_new ("Account Name:");
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "labelName", labelName);
  gtk_widget_show (labelName);
  gtk_box_pack_start (GTK_BOX (hbox3), labelName, TRUE, TRUE, 0);
  gtk_misc_set_alignment (GTK_MISC (labelName), 0.95, 0.5);

  entryAccountName = gtk_entry_new ();
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "entryAccountName", entryAccountName);
  gtk_widget_show (entryAccountName);
  gtk_box_pack_start (GTK_BOX (hbox3), entryAccountName, FALSE, TRUE, 5);

  hbox4 = gtk_hbox_new (FALSE, 0);
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "hbox4", hbox4);
  gtk_widget_show (hbox4);
  gtk_box_pack_start (GTK_BOX (vbox1), hbox4, FALSE, TRUE, 0);

  labelDescription = gtk_label_new ("Description:");
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "labelDescription", labelDescription);
  gtk_widget_show (labelDescription);
  gtk_box_pack_start (GTK_BOX (hbox4), labelDescription, TRUE, TRUE, 0);
  gtk_misc_set_alignment (GTK_MISC (labelDescription), 0.95, 0.5);

  entryDescription = gtk_entry_new ();
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "entryDescription", entryDescription);
  gtk_widget_show (entryDescription);
  gtk_box_pack_start (GTK_BOX (hbox4), entryDescription, FALSE, TRUE, 5);

  hbox5 = gtk_hbox_new (FALSE, 0);
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "hbox5", hbox5);
  gtk_widget_show (hbox5);
  gtk_box_pack_start (GTK_BOX (vbox1), hbox5, FALSE, TRUE, 0);

  labelCurrency = gtk_label_new ("Currency:");
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "labelCurrency", labelCurrency);
  gtk_widget_show (labelCurrency);
  gtk_box_pack_start (GTK_BOX (hbox5), labelCurrency, TRUE, TRUE, 0);
  gtk_misc_set_alignment (GTK_MISC (labelCurrency), 0.95, 0.5);

  entryCurrency = gtk_entry_new ();
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "entryCurrency", entryCurrency);
  gtk_widget_show (entryCurrency);
  gtk_box_pack_start (GTK_BOX (hbox5), entryCurrency, FALSE, FALSE, 5);

  hbox6 = gtk_hbox_new (FALSE, 0);
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "hbox6", hbox6);
  gtk_widget_show (hbox6);
  gtk_box_pack_start (GTK_BOX (vbox1), hbox6, FALSE, TRUE, 0);

  labelSecurity = gtk_label_new ("Security:");
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "labelSecurity", labelSecurity);
  gtk_widget_show (labelSecurity);
  gtk_box_pack_start (GTK_BOX (hbox6), labelSecurity, TRUE, TRUE, 0);
  gtk_misc_set_alignment (GTK_MISC (labelSecurity), 0.95, 0.5);

  entrySecurity = gtk_entry_new ();
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "entrySecurity", entrySecurity);
  gtk_widget_show (entrySecurity);
  gtk_box_pack_start (GTK_BOX (hbox6), entrySecurity, FALSE, FALSE, 5);

  /* Set the security entry box to insensitive by default */
  gtk_widget_set_sensitive(GTK_WIDGET(entrySecurity), FALSE );

  hbox7 = gtk_hbox_new (TRUE, 5);
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "hbox7", hbox7);
  gtk_widget_show (hbox7);
  gtk_box_pack_start (GTK_BOX (vbox1), hbox7, TRUE, TRUE, 0);
  gtk_container_border_width (GTK_CONTAINER (hbox7), 5);

  frameListTypes = gtk_frame_new ("Type of Account");
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "frameListTypes", frameListTypes);
  gtk_widget_show (frameListTypes);
  gtk_box_pack_start (GTK_BOX (hbox7), frameListTypes, TRUE, TRUE, 0);

  frameList = gtk_frame_new (NULL);
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "frameList", frameList);
  gtk_widget_show (frameList);
  gtk_container_add (GTK_CONTAINER (frameListTypes), frameList);
  gtk_container_border_width (GTK_CONTAINER (frameList), 5);
  gtk_frame_set_shadow_type (GTK_FRAME (frameList), GTK_SHADOW_IN);

  listOfTypes = gtk_list_new ();
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "listOfTypes", listOfTypes);
  gtk_widget_show (listOfTypes);
//  gtk_container_add (GTK_CONTAINER (frameList), listOfTypes);

  frameParentAccount = gtk_frame_new ("Parent Account");
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "frameParentAccount", frameParentAccount);
  gtk_widget_show (frameParentAccount);
  gtk_box_pack_start (GTK_BOX (hbox7), frameParentAccount, TRUE, TRUE, 0);

  frameParent = gtk_frame_new (NULL);
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "frameParent", frameParent);
  gtk_widget_show (frameParent);
  gtk_container_add (GTK_CONTAINER (frameParentAccount), frameParent);
  gtk_container_border_width (GTK_CONTAINER (frameParent), 5);
  gtk_frame_set_shadow_type (GTK_FRAME (frameParent), GTK_SHADOW_IN);

  tree1 = gtk_tree_new ();
  gtk_object_set_data (GTK_OBJECT (accData->dialog), "tree1", tree1);
  gtk_widget_show (tree1);

  scrolledWindow1 = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolledWindow1),
				  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_show (scrolledWindow1);

  scrolledWindow2 = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolledWindow2),
				  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_show (scrolledWindow2);

  gtk_container_add( GTK_CONTAINER( frameList   ), scrolledWindow2);
  gtk_container_add( GTK_CONTAINER( frameParent ), scrolledWindow1 );
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolledWindow1), 
                                        GTK_WIDGET(tree1));
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolledWindow2), 
                                        GTK_WIDGET(listOfTypes));                                        

  gtk_widget_set_usize ( GTK_WIDGET(accData->dialog), 404, 376 );

  /*** FILL WIDGETS *************************************************/
  gnc_ui_accWindow_list_fill ( GTK_WIDGET(listOfTypes) );

  /* Add a toplevel tree item so we can add new top level accounts  */
  {
    GtkWidget *subtree;
    GtkWidget *treeItem;
    
    treeItem = gtk_tree_item_new_with_label("New Toplevel Account");

    /* Set the user_data for the tree item to the account it */
    /* represents.                                           */
    gtk_object_set_user_data(GTK_OBJECT(treeItem), NULL);
    
    /* Now append this tree item to the tree */
    gtk_tree_append(GTK_TREE(tree1), GTK_WIDGET(treeItem));
    
    /* Connect the signal to the button press event */
    gtk_signal_connect (GTK_OBJECT (treeItem), 
                        "button_press_event",
                        GTK_SIGNAL_FUNC(gnc_ui_accWindow_tree_select), 
                        GTK_WIDGET(treeItem));
    
    /* Show the new tree item */
    gtk_widget_show(GTK_WIDGET(treeItem));

    /* Create the subtree */
    subtree = gtk_tree_new();
    
    /* Append this new subtree to the current tree item */
    gtk_tree_item_set_subtree(GTK_TREE_ITEM(treeItem), GTK_WIDGET(subtree));
    
    gnc_ui_accWindow_tree_fill(GTK_TREE(subtree),
                               xaccSessionGetGroup(current_session)); 
  }

  /*** Callbacks ****************************************************/

  gnome_dialog_button_connect (GNOME_DIALOG (accData->dialog), 0,
                               GTK_SIGNAL_FUNC (gnc_ui_editnotes_callback), 
                               accData->parentAccount);          
   
  gnome_dialog_button_connect (GNOME_DIALOG (accData->dialog), 1,
                               GTK_SIGNAL_FUNC (gnc_ui_accWindow_create_callback), 
                               accData);
  
  gnome_dialog_button_connect (GNOME_DIALOG (accData->dialog), 2,
                               GTK_SIGNAL_FUNC (gnc_ui_accWindow_cancelled_callback), 
                               accData);
                               
                     

  gtk_widget_show(GTK_WIDGET(accData->dialog));  
                       
  /*** End of Callbacks *********************************************/  

  return accData;
}


/********************** END OF FILE *********************************\
\********************************************************************/

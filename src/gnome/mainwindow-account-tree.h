#ifndef __GNC_MAINWIN_ACCOUNT_TREE_H
#define __GNC_MAINWIN_ACCOUNT_TREE_H

#include <gnome.h>
#include "account-tree.h"
#include "Account.h"

#define GNC_MAINWIN_ACCOUNT_TREE(obj)          GTK_CHECK_CAST (obj, gnc_mainwin_account_tree_get_type (), GNCMainWinAccountTree)
#define GNC_MAINWIN_ACCOUNT_TREE_CLASS(klass)  GTK_CHECK_CLASS_CAST (klass, gnc_mainwin_account_tree_get_type(), GNCMainWinAccountTreeClass)
#define IS_GNC_MAINWIN_ACCOUNT_TREE(obj)       GTK_CHECK_TYPE (obj, gnc_mainwin_account_tree_get_type ())


typedef struct _GNCMainWinAccountTree       GNCMainWinAccountTree;
typedef struct _GNCMainWinAccountTreeClass  GNCMainWinAccountTreeClass;

struct _GNCMainWinAccountTree
{ 
  GtkVBox vbox;
  GtkScrolledWindow *scrolled_window;
  GNCAccountTree *acc_tree; 
};

struct _GNCMainWinAccountTreeClass
{
  GtkVBoxClass parent_class;  

  void (*select_account)   (GNCMainWinAccountTree *tree,
                            Account        *account);

  void (*unselect_account) (GNCMainWinAccountTree *tree,
                            Account        *account);

  void (*activate_account) (GNCMainWinAccountTree *tree,
                            Account        *account);
};

guint          gnc_mainwin_account_tree_get_type(void);
GtkWidget*     gnc_mainwin_account_tree_new(void);

void 
gnc_mainwin_account_tree_attach_popup(GNCMainWinAccountTree *tree, GnomeUIInfo *popup_info);

void
gnc_mainwin_account_tree_set_view_info(GNCMainWinAccountTree *tree, AccountViewInfo new_info);
Account *
gnc_mainwin_account_tree_get_current_account(GNCMainWinAccountTree *tree);
GList *
gnc_mainwin_account_tree_get_current_accounts(GNCMainWinAccountTree *tree);

void gnc_mainwin_account_tree_toggle_account_expansion(GNCMainWinAccountTree *tree, Account *account);

#endif /* __GNC_MAINWINDOW_ACCOUNT_TREE_H */

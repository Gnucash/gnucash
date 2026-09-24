/*
 * gnc-tree-model-account-types.h -- GTK4 list model for account types.
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2005, 2006 Chris Shoemaker <c.shoemaker@cox.net>
 * Copyright (C) 2006 Eskil Bylund <eskil.bylund@gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 */

#ifndef __GNC_TREE_MODEL_ACCOUNT_TYPES_H
#define __GNC_TREE_MODEL_ACCOUNT_TYPES_H

#include <gio/gio.h>

#include "Account.h"

G_BEGIN_DECLS

#define GNC_TYPE_ACCOUNT_TYPE_ITEM (gnc_account_type_item_get_type ())
G_DECLARE_FINAL_TYPE (GncAccountTypeItem, gnc_account_type_item, GNC,
                      ACCOUNT_TYPE_ITEM, GObject)

/*
 * Return a newly allocated, locale-sorted list containing exactly the account
 * types selected by @types. The caller owns the returned GListModel.
 */
GListModel *gnc_account_type_list_new (guint32 types);

/*
 * Return a newly allocated list with a neutral ACCT_TYPE_NONE item followed
 * by exactly the account types selected by @types. If @types is empty, return
 * an empty list. The neutral item represents no user selection and cannot be
 * saved as an account type.
 */
GListModel *gnc_account_type_list_new_with_placeholder (guint32 types);

GNCAccountType gnc_account_type_item_get_account_type (GncAccountTypeItem *item);
const gchar *gnc_account_type_item_get_name (GncAccountTypeItem *item);
gboolean gnc_account_type_item_get_selected (GncAccountTypeItem *item);
void gnc_account_type_item_set_selected (GncAccountTypeItem *item,
                                         gboolean selected);

G_END_DECLS

#endif /* __GNC_TREE_MODEL_ACCOUNT_TYPES_H */

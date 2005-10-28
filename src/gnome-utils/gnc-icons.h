/*
 * gnc-icons.h -- Functions to create a GtkIconFactory for GnuCash
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#ifndef __GNC_ICONS_H
#define __GNC_ICONS_H

G_BEGIN_DECLS

#define GNC_STOCK_ACCOUNT "gnc-account"
#define GNC_STOCK_DELETE_ACCOUNT "gnc-delete-account"
#define GNC_STOCK_EDIT_ACCOUNT "gnc-edit-account"
#define GNC_STOCK_NEW_ACCOUNT "gnc-new-account"
#define GNC_STOCK_OPEN_ACCOUNT "gnc-open-account"

//FIXME: use own budget icons?
#define GNC_STOCK_BUDGET "gnc-budget"
#define GNC_STOCK_NEW_BUDGET "gnc-account"
#define GNC_STOCK_OPEN_BUDGET "gnc-open-account"
//#define GNC_STOCK_CLOSE_BUDGET "gnc-close-account"
//#define GNC_STOCK_EDIT_BUDGET "gnc-edit-account"
#define GNC_STOCK_DELETE_BUDGET "gnc-delete-account"

void gnc_load_stock_icons (void);

G_END_DECLS

#endif /* __GNC_ICONS_H */

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
#define GNC_STOCK_OPEN_ACCOUNT "gnc-open-account"

void gnc_load_stock_icons (void);

G_END_DECLS

#endif /* __GNC_ICONS_H */

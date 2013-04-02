/*
 * dialog-choose-owner.h -- Dialog to choose an owner for a business Split
 * Copyright (C) 2006 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */


#ifndef GNC_DIALOG_CHOOSE_OWNER_H_
#define GNC_DIALOG_CHOOSE_OWNER_H_

#include "Split.h"

typedef struct _choose_owner_dialog DialogChooseOwner;

/**
 * This split was added to an A/R or A/P account.  Make sure it
 * has an owner attached to it so the business reports work.
 */
gboolean gnc_split_assign_owner(GtkWidget* window, Split* split);

#endif /* GNC_DIALOG_CHOOSE_OWNER_H_ */

/*
 * dialog-coowner.h -- Dialog(s) for Employee search and entry
 * Copyright (C) 2022 Ralf Zerres
 * Author: Ralf Zerres <ralf.zerres@mail.de>
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


#ifndef GNC_DIALOG_COOWNER_H_
#define GNC_DIALOG_COOWNER_H_

typedef struct _coowner_window CoOwnerWindow;

#include "gncCoOwner.h"
#include "dialog-search.h"

/* Functions to edit and create coowners */
CoOwnerWindow * gnc_ui_coowner_edit (GtkWindow *parent, GncCoOwner *coowner);
CoOwnerWindow * gnc_ui_coowner_new (GtkWindow *parent, QofBook *book);

/* Search for an coowner */
GNCSearchWindow * gnc_coowner_search (GtkWindow *parent, GncCoOwner *start, QofBook *book);

/*
 * These callbacks are for use with the gnc_general_search widget
 *
 * select() provides a Select Dialog and returns it.
 * edit() opens the existing vendor for editing and returns NULL.
 */
GNCSearchWindow * gnc_coowner_search_select (GtkWindow *parent, gpointer start, gpointer book);
GNCSearchWindow * gnc_coowner_search_edit (GtkWindow *parent, gpointer start, gpointer book);

#endif /* GNC_DIALOG_COOWNER_H_ */

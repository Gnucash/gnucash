/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * go-action-combo.h: A custom GOActionCombo to handle undo/redo menus/toolbars
 *
 * Copyright (C) 2003-2004 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 **/

#ifndef __GO_ACTION_COMBO_TEXT_H__
#define __GO_ACTION_COMBO_TEXT_H__

#include <glib-object.h>

G_BEGIN_DECLS

#define GO_ACTION_COMBO_TEXT_TYPE  (go_action_combo_text_get_type ())
#define GO_ACTION_COMBO_TEXT(o)    (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_ACTION_COMBO_TEXT_TYPE, GOActionComboText))
#define IS_GO_ACTION_COMBO_TEXT(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_ACTION_COMBO_TEXT_TYPE))

typedef struct _GOActionComboText	 GOActionComboText;
typedef enum {		/* begin the search from : */
	GO_ACTION_COMBO_SEARCH_FROM_TOP,	/* the top of the list */
	GO_ACTION_COMBO_SEARCH_CURRENT,		/* the current selection */
	GO_ACTION_COMBO_SEARCH_NEXT		/* the next element after current */
} GOActionComboTextSearchDir;

GType	    go_action_combo_text_get_type (void);
void	    go_action_combo_text_add_item  (GOActionComboText       *a,
					    char const *item);
void        go_action_combo_text_set_width (GOActionComboText       *a,
					    char const *largest_elem);
char const *go_action_combo_text_get_entry (GOActionComboText const *a);
void	    go_action_combo_text_set_entry (GOActionComboText       *a,
					    char const *text,
					    GOActionComboTextSearchDir dir);

G_END_DECLS

#endif  /* __GO_ACTION_COMBO_TEXT_H__ */

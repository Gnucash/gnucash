/* File import from libegg to gnumeric by import-egg.  Do not edit.  */

#include <goffice/goffice-config.h>
/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
 *
 * Authors:
 *   James Willcox <jwillcox@cs.indiana.edu>
 */


#include <stdio.h>
#include <string.h>
#include <glib.h>
#include <libgnomevfs/gnome-vfs.h>
#include <libgnomevfs/gnome-vfs-mime-utils.h>
#include "egg-recent-item.h"



EggRecentItem *
egg_recent_item_new (void)
{
	EggRecentItem *item;

	item = g_new (EggRecentItem, 1);

	item->groups = NULL;
	item->private_data = FALSE;
	item->uri = NULL;
	item->mime_type = NULL;

	item->refcount = 1;

	return item;
}

static void
egg_recent_item_free (EggRecentItem *item)
{
	if (item->uri)
		g_free (item->uri);

	if (item->mime_type)
		g_free (item->mime_type);

	if (item->groups) {
		g_list_foreach (item->groups, (GFunc)g_free, NULL);
		g_list_free (item->groups);
		item->groups = NULL;
	}

	g_free (item);
}

EggRecentItem *
egg_recent_item_ref (EggRecentItem *item)
{
	item->refcount++;
	return item;
}

EggRecentItem *
egg_recent_item_unref (EggRecentItem *item)
{
	item->refcount--;

	if (item->refcount == 0) {
		egg_recent_item_free (item);
	}

	return item;
}


EggRecentItem *
egg_recent_item_new_from_uri (const gchar *uri)
{
	EggRecentItem *item;

	g_return_val_if_fail (uri != NULL, NULL);

	item = egg_recent_item_new ();

	if (!egg_recent_item_set_uri (item ,uri)) {
		egg_recent_item_free (item);
		return NULL;
	}

	item->mime_type = gnome_vfs_get_mime_type (item->uri);

	if (!item->mime_type)
		item->mime_type = g_strdup (GNOME_VFS_MIME_TYPE_UNKNOWN);

	return item;
}

/*
static GList *
egg_recent_item_copy_groups (const GList *list)
{
	GList *newlist = NULL;

	while (list) {
		gchar *group = (gchar *)list->data;

		newlist = g_list_prepend (newlist, g_strdup (group));

		list = list->next;
	}

	return newlist;
}


EggRecentItem *
egg_recent_item_copy (const EggRecentItem *item)
{
	EggRecentItem *newitem;

	newitem = egg_recent_item_new ();
	newitem->uri = g_strdup (item->uri);
	if (item->mime_type)
		newitem->mime_type = g_strdup (item->mime_type);
	newitem->timestamp = item->timestamp;
	newitem->private_data = item->private_data;
	newitem->groups = egg_recent_item_copy_groups (item->groups);

	return newitem;
}
*/

/*
EggRecentItem *
egg_recent_item_new_valist (const gchar *uri, va_list args)
{
	EggRecentItem *item;
	EggRecentArg arg;
	gchar *str1;
	gchar *str2;
	gboolean priv;

	item = egg_recent_item_new ();

	arg = va_arg (args, EggRecentArg);

	while (arg != EGG_RECENT_ARG_NONE) {
		switch (arg) {
			case EGG_RECENT_ARG_MIME_TYPE:
				str1 = va_arg (args, gchar*);

				egg_recent_item_set_mime_type (item, str1);
			break;
			case EGG_RECENT_ARG_GROUP:
				str1 = va_arg (args, gchar*);

				egg_recent_item_add_group (item, str1);
			break;
			case EGG_RECENT_ARG_PRIVATE:
				priv = va_arg (args, gboolean);

				egg_recent_item_set_private (item, priv);
			break;
			default:
			break;
		}

		arg = va_arg (args, EggRecentArg);
	}

	return item;
}
*/

gboolean
egg_recent_item_set_uri (EggRecentItem *item, const gchar *uri)
{
	gchar *utf8_uri;

	/* if G_BROKEN_FILENAMES is not set, this should succede */
	if (g_utf8_validate (uri, -1, NULL)) {
		item->uri = gnome_vfs_make_uri_from_input (uri);
	} else {
		utf8_uri = g_filename_to_utf8 (uri, -1, NULL, NULL, NULL);

		if (utf8_uri == NULL) {
			g_warning ("Couldn't convert URI to UTF-8");
			return FALSE;
		}

		if (g_utf8_validate (utf8_uri, -1, NULL)) {
			item->uri = gnome_vfs_make_uri_from_input (utf8_uri);
		} else {
			g_free (utf8_uri);
			return FALSE;
		}

		g_free (utf8_uri);
	}

	return TRUE;
}

gchar *
egg_recent_item_get_uri (const EggRecentItem *item)
{
	return g_strdup (item->uri);
}

G_CONST_RETURN gchar *
egg_recent_item_peek_uri (const EggRecentItem *item)
{
	return item->uri;
}

gchar *
egg_recent_item_get_uri_utf8 (const EggRecentItem *item)
{
	/* this could fail, but it's not likely, since we've already done it
	 * once in set_uri()
	 */
	return g_filename_to_utf8 (item->uri, -1, NULL, NULL, NULL);
}

gchar *
egg_recent_item_get_uri_for_display (const EggRecentItem *item)
{
	return gnome_vfs_format_uri_for_display (item->uri);
}

/* Stolen from gnome_vfs_make_valid_utf8() */
static char *
make_valid_utf8 (const char *name)
{
	GString *string;
	const char *remainder, *invalid;
	int remaining_bytes, valid_bytes;

	string = NULL;
	remainder = name;
	remaining_bytes = strlen (name);

	while (remaining_bytes != 0) {
		if (g_utf8_validate (remainder, remaining_bytes, &invalid))
			break;

		valid_bytes = invalid - remainder;

		if (string == NULL)
			string = g_string_sized_new (remaining_bytes);

		g_string_append_len (string, remainder, valid_bytes);
		g_string_append_c (string, '?');

		remaining_bytes -= valid_bytes + 1;
		remainder = invalid + 1;
	}

	if (string == NULL)
		return g_strdup (name);

	g_string_append (string, remainder);
/* 	g_string_append (string, _(" (invalid file name)")); */
	g_assert (g_utf8_validate (string->str, -1, NULL));

	return g_string_free (string, FALSE);
}

/**
 * egg_recent_item_get_short_name:
 * @item: an #EggRecentItem
 *
 * Computes a valid UTF-8 string that can be used as the name of the item in a
 * menu or list.  For example, calling this function on an item that refers to
 * "file:///foo/bar.txt" will yield "bar.txt".
 *
 * Return value: A newly-allocated string in UTF-8 encoding; free it with
 * g_free().
 **/
gchar *
egg_recent_item_get_short_name (const EggRecentItem *item)
{
	GnomeVFSURI *uri;
	char *short_name;
	gboolean valid;

	g_return_val_if_fail (item != NULL, NULL);

	if (item->uri == NULL)
		return NULL;

	uri = gnome_vfs_uri_new (item->uri);
	if (uri == NULL)
		return NULL;

	short_name = gnome_vfs_uri_extract_short_name (uri);
	valid = FALSE;

	if (strcmp (gnome_vfs_uri_get_scheme (uri), "file") == 0) {
		char *tmp;

		tmp = g_filename_to_utf8 (short_name, -1, NULL, NULL, NULL);
		if (tmp) {
			g_free (short_name);
			short_name = tmp;
			valid = TRUE;
		}
	}

	if (!valid) {
		char *tmp;

		tmp = make_valid_utf8 (short_name);
		g_assert (tmp != NULL);
		g_free (short_name);
		short_name = tmp;
	}

	gnome_vfs_uri_unref (uri);

	return short_name;
}

void
egg_recent_item_set_mime_type (EggRecentItem *item, const gchar *mime)
{
	item->mime_type = g_strdup (mime);
}

gchar *
egg_recent_item_get_mime_type (const EggRecentItem *item)
{
	return g_strdup (item->mime_type);
}

void
egg_recent_item_set_timestamp (EggRecentItem *item, time_t timestamp)
{
	if (timestamp == (time_t) -1)
		time (&timestamp);

	item->timestamp = timestamp;
}

time_t
egg_recent_item_get_timestamp (const EggRecentItem *item)
{
	return item->timestamp;
}

G_CONST_RETURN GList *
egg_recent_item_get_groups (const EggRecentItem *item)
{
	return item->groups;
}

gboolean
egg_recent_item_in_group (const EggRecentItem *item, const gchar *group_name)
{
	GList *tmp;

	tmp = item->groups;
	while (tmp != NULL) {
		gchar *val = (gchar *)tmp->data;

		if (strcmp (group_name, val) == 0)
			return TRUE;

		tmp = tmp->next;
	}

	return FALSE;
}

void
egg_recent_item_add_group (EggRecentItem *item, const gchar *group_name)
{
	g_return_if_fail (group_name != NULL);

	if (!egg_recent_item_in_group (item, group_name))
		item->groups = g_list_append (item->groups, g_strdup (group_name));
}

void
egg_recent_item_remove_group (EggRecentItem *item, const gchar *group_name)
{
	GList *tmp;

	g_return_if_fail (group_name != NULL);

	tmp = item->groups;
	while (tmp != NULL) {
		gchar *val = (gchar *)tmp->data;

		if (strcmp (group_name, val) == 0) {
			item->groups = g_list_remove (item->groups,
						      val);
			g_free (val);
			break;
		}

		tmp = tmp->next;
	}
}

void
egg_recent_item_set_private (EggRecentItem *item, gboolean priv)
{
	item->private_data = priv;
}

gboolean
egg_recent_item_get_private (const EggRecentItem *item)
{
	return item->private_data;
}

GType
egg_recent_item_get_type (void)
{
	static GType boxed_type = 0;

	if (!boxed_type) {
		boxed_type = g_boxed_type_register_static ("EggRecentItem",
					(GBoxedCopyFunc)egg_recent_item_ref,
					(GBoxedFreeFunc)egg_recent_item_unref);
	}

	return boxed_type;
}

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

#ifdef HAVE_CONFIG_H
/* #include <config.h> */
#endif

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/time.h>
#include <time.h>
#include <gtk/gtk.h>
#include <libgnomevfs/gnome-vfs.h>
#include <libgnomevfs/gnome-vfs-mime-utils.h>
#include <gconf/gconf-client.h>
#include "egg-recent-model.h"
#include "egg-recent-item.h"

#define EGG_RECENT_MODEL_FILE_PATH "/.recently-used"
#define EGG_RECENT_MODEL_BUFFER_SIZE 8192

#define EGG_RECENT_MODEL_MAX_ITEMS 500
#define EGG_RECENT_MODEL_DEFAULT_LIMIT 10
#define EGG_RECENT_MODEL_TIMEOUT_LENGTH 200

#define EGG_RECENT_MODEL_KEY_DIR "/desktop/gnome/recent_files"
#define EGG_RECENT_MODEL_DEFAULT_LIMIT_KEY EGG_RECENT_MODEL_KEY_DIR "/default_limit"
#define EGG_RECENT_MODEL_EXPIRE_KEY EGG_RECENT_MODEL_KEY_DIR "/expire"

struct _EggRecentModelPrivate {
	GSList *mime_filter_values;	/* list of mime types we allow */
	GSList *group_filter_values;	/* list of groups we allow */
	GSList *scheme_filter_values;	/* list of URI schemes we allow */

	EggRecentModelSort sort_type; /* type of sorting to be done */

	int limit;			/* soft limit for length of the list */
	int expire_days;		/* number of days to hold an item */

	char *path;			/* path to the file we store stuff in */

	GHashTable *monitors;

	GnomeVFSMonitorHandle *monitor;

	GConfClient *client;
	gboolean use_default_limit;

	guint limit_change_notify_id;
	guint expiration_change_notify_id;

	guint changed_timeout;
};

/* signals */
enum {
	CHANGED,
	LAST_SIGNAL
};

static GType model_signals[LAST_SIGNAL] = { 0 };

/* properties */
enum {
	PROP_BOGUS,
	PROP_MIME_FILTERS,
	PROP_GROUP_FILTERS,
	PROP_SCHEME_FILTERS,
	PROP_SORT_TYPE,
	PROP_LIMIT
};

typedef struct {
	GSList *states;
	GList *items;
	EggRecentItem *current_item;
}ParseInfo;

typedef enum {
	STATE_START,
	STATE_RECENT_FILES,
	STATE_RECENT_ITEM,
	STATE_URI,
	STATE_MIME_TYPE,
	STATE_TIMESTAMP,
	STATE_PRIVATE,
	STATE_GROUPS,
	STATE_GROUP
} ParseState;

typedef struct _ChangedData {
	EggRecentModel *model;
	GList *list;
}ChangedData;

#define TAG_RECENT_FILES "RecentFiles"
#define TAG_RECENT_ITEM "RecentItem"
#define TAG_URI "URI"
#define TAG_MIME_TYPE "Mime-Type"
#define TAG_TIMESTAMP "Timestamp"
#define TAG_PRIVATE "Private"
#define TAG_GROUPS "Groups"
#define TAG_GROUP "Group"

static void start_element_handler (GMarkupParseContext *context,
			      const gchar *element_name,
			      const gchar **attribute_names,
			      const gchar **attribute_values,
			      gpointer user_data,
			      GError **error);

static void end_element_handler (GMarkupParseContext *context,
			    const gchar *element_name,
			    gpointer user_data,
			    GError **error);

static void text_handler (GMarkupParseContext *context,
		     const gchar *text,
		     gsize text_len,
		     gpointer user_data,
		     GError **error);

static void error_handler (GMarkupParseContext *context,
		      GError *error,
		      gpointer user_data);

static GMarkupParser parser = {start_element_handler, end_element_handler,
			text_handler,
			NULL,
			error_handler};

static gboolean
egg_recent_model_string_match (const GSList *list, const gchar *str)
{
	const GSList *tmp;

	if (list == NULL || str == NULL)
		return TRUE;

	tmp = list;

	while (tmp) {
		if (g_pattern_match_string (tmp->data, str))
			return TRUE;

		tmp = tmp->next;
	}

	return FALSE;
}

static gboolean
egg_recent_model_write_raw (EggRecentModel *model, FILE *file,
			      const gchar *content)
{
	int len;
	int fd;
	struct stat sbuf;

	rewind (file);

	len = strlen (content);
	fd = fileno (file);

	if (fstat (fd, &sbuf) < 0)
		g_warning ("Couldn't stat XML document.");

	if ((off_t)len < sbuf.st_size) {
		ftruncate (fd, len);
	}

	if (fputs (content, file) == EOF)
		return FALSE;

	fsync (fd);
	rewind (file);

	return TRUE;
}

static GList *
egg_recent_model_delete_from_list (GList *list,
				       const gchar *uri)
{
	GList *tmp;

	if (!uri)
		return list;

	tmp = list;

	while (tmp) {
		EggRecentItem *item = tmp->data;
		GList         *next;

		next = tmp->next;

		if (!strcmp (egg_recent_item_peek_uri (item), uri)) {
			egg_recent_item_unref (item);

			list = g_list_remove_link (list, tmp);
			g_list_free_1 (tmp);
		}

		tmp = next;
	}

	return list;
}

static void
egg_recent_model_add_new_groups (EggRecentItem *item,
				 EggRecentItem *upd_item)
{
	const GList *tmp;

	tmp = egg_recent_item_get_groups (upd_item);

	while (tmp) {
		char *group = tmp->data;

		if (!egg_recent_item_in_group (item, group))
			egg_recent_item_add_group (item, group);

		tmp = tmp->next;
	}
}

static gboolean
egg_recent_model_update_item (GList *items, EggRecentItem *upd_item)
{
	GList      *tmp;
	const char *uri;

	uri = egg_recent_item_peek_uri (upd_item);

	tmp = items;

	while (tmp) {
		EggRecentItem *item = tmp->data;

		if (gnome_vfs_uris_match (egg_recent_item_peek_uri (item), uri)) {
			egg_recent_item_set_timestamp (item, (time_t) -1);

			egg_recent_model_add_new_groups (item, upd_item);

			return TRUE;
		}

		tmp = tmp->next;
	}

	return FALSE;
}

static gchar *
egg_recent_model_read_raw (EggRecentModel *model, FILE *file)
{
	GString *string;
	char buf[EGG_RECENT_MODEL_BUFFER_SIZE];

	rewind (file);

	string = g_string_new (NULL);
	while (fgets (buf, EGG_RECENT_MODEL_BUFFER_SIZE, file)) {
		string = g_string_append (string, buf);
	}

	rewind (file);

	return g_string_free (string, FALSE);
}



static void
parse_info_init (ParseInfo *info)
{
	info->states = g_slist_prepend (NULL, STATE_START);
	info->items = NULL;
}

static void
parse_info_free (ParseInfo *info)
{
	g_slist_free (info->states);
}

static void
push_state (ParseInfo  *info,
            ParseState  state)
{
  info->states = g_slist_prepend (info->states, GINT_TO_POINTER (state));
}

static void
pop_state (ParseInfo *info)
{
  g_return_if_fail (info->states != NULL);

  info->states = g_slist_remove (info->states, info->states->data);
}

static ParseState
peek_state (ParseInfo *info)
{
  g_return_val_if_fail (info->states != NULL, STATE_START);

  return GPOINTER_TO_INT (info->states->data);
}

#define ELEMENT_IS(name) (strcmp (element_name, (name)) == 0)

static void
start_element_handler (GMarkupParseContext *context,
			      const gchar *element_name,
			      const gchar **attribute_names,
			      const gchar **attribute_values,
			      gpointer user_data,
			      GError **error)
{
	ParseInfo *info = (ParseInfo *)user_data;

	if (ELEMENT_IS (TAG_RECENT_FILES))
		push_state (info, STATE_RECENT_FILES);
	else if (ELEMENT_IS (TAG_RECENT_ITEM)) {
		info->current_item = egg_recent_item_new ();
		push_state (info, STATE_RECENT_ITEM);
	} else if (ELEMENT_IS (TAG_URI))
		push_state (info, STATE_URI);
	else if (ELEMENT_IS (TAG_MIME_TYPE))
		push_state (info, STATE_MIME_TYPE);
	else if (ELEMENT_IS (TAG_TIMESTAMP))
		push_state (info, STATE_TIMESTAMP);
	else if (ELEMENT_IS (TAG_PRIVATE)) {
		push_state (info, STATE_PRIVATE);
		egg_recent_item_set_private (info->current_item, TRUE);
	} else if (ELEMENT_IS (TAG_GROUPS))
		push_state (info, STATE_GROUPS);
	else if (ELEMENT_IS (TAG_GROUP))
		push_state (info, STATE_GROUP);
}

static gint
list_compare_func_mru (gpointer a, gpointer b)
{
	EggRecentItem *item_a = (EggRecentItem *)a;
	EggRecentItem *item_b = (EggRecentItem *)b;

	return item_a->timestamp < item_b->timestamp;
}

static gint
list_compare_func_lru (gpointer a, gpointer b)
{
	EggRecentItem *item_a = (EggRecentItem *)a;
	EggRecentItem *item_b = (EggRecentItem *)b;

	return item_a->timestamp > item_b->timestamp;
}



static void
end_element_handler (GMarkupParseContext *context,
			    const gchar *element_name,
			    gpointer user_data,
			    GError **error)
{
	ParseInfo *info = (ParseInfo *)user_data;

	switch (peek_state (info)) {
		case STATE_RECENT_ITEM:
			info->items = g_list_append (info->items,
						    info->current_item);
			if (info->current_item->uri == NULL ||
			    strlen (info->current_item->uri) == 0)
				g_warning ("URI NOT LOADED");
		break;
		default:
		break;
	}

	pop_state (info);
}

static void
text_handler (GMarkupParseContext *context,
		     const gchar *text,
		     gsize text_len,
		     gpointer user_data,
		     GError **error)
{
	ParseInfo *info = (ParseInfo *)user_data;

	switch (peek_state (info)) {
		case STATE_START:
		case STATE_RECENT_FILES:
		case STATE_RECENT_ITEM:
		case STATE_PRIVATE:
		case STATE_GROUPS:
		break;
		case STATE_URI:
			egg_recent_item_set_uri (info->current_item, text);
		break;
		case STATE_MIME_TYPE:
			egg_recent_item_set_mime_type (info->current_item,
							 text);
		break;
		case STATE_TIMESTAMP:
			egg_recent_item_set_timestamp (info->current_item,
							 (time_t)atoi (text));
		break;
		case STATE_GROUP:
			egg_recent_item_add_group (info->current_item,
						     text);
		break;
	}

}

static void
error_handler (GMarkupParseContext *context,
		      GError *error,
		      gpointer user_data)
{
	g_warning ("Error in parse: %s", error->message);
}

static void
egg_recent_model_enforce_limit (GList *list, int limit)
{
	int len;
	GList *end;

	/* limit < 0 means unlimited */
	if (limit <= 0)
		return;

	len = g_list_length (list);

	if (len > limit) {
		GList *next;

		end = g_list_nth (list, limit-1);
		next = end->next;

		end->next = NULL;

		EGG_RECENT_ITEM_LIST_UNREF (next);
	}
}

static GList *
egg_recent_model_sort (EggRecentModel *model, GList *list)
{
	switch (model->priv->sort_type) {
		case EGG_RECENT_MODEL_SORT_MRU:
			list = g_list_sort (list,
					(GCompareFunc)list_compare_func_mru);
		break;
		case EGG_RECENT_MODEL_SORT_LRU:
			list = g_list_sort (list,
					(GCompareFunc)list_compare_func_lru);
		break;
		case EGG_RECENT_MODEL_SORT_NONE:
		break;
	}

	return list;
}

static gboolean
egg_recent_model_group_match (EggRecentItem *item, GSList *groups)
{
	GSList *tmp;

	tmp = groups;

	while (tmp != NULL) {
		const gchar * group = (const gchar *)tmp->data;

		if (egg_recent_item_in_group (item, group))
			return TRUE;

		tmp = tmp->next;
	}

	return FALSE;
}

static GList *
egg_recent_model_filter (EggRecentModel *model,
				GList *list)
{
	EggRecentItem *item;
	GList *newlist = NULL;
	gchar *mime_type;
	gchar *uri;

	g_return_val_if_fail (list != NULL, NULL);

	while (list) {
		gboolean pass_mime_test = FALSE;
		gboolean pass_group_test = FALSE;
		gboolean pass_scheme_test = FALSE;
		item = (EggRecentItem *)list->data;
		list = list->next;

		uri = egg_recent_item_get_uri (item);

		/* filter by mime type */
		if (model->priv->mime_filter_values != NULL) {
			mime_type = egg_recent_item_get_mime_type (item);

			if (egg_recent_model_string_match
					(model->priv->mime_filter_values,
					 mime_type))
				pass_mime_test = TRUE;

			g_free (mime_type);
		} else
			pass_mime_test = TRUE;

		/* filter by group */
		if (pass_mime_test && model->priv->group_filter_values != NULL) {
			if (egg_recent_model_group_match
					(item, model->priv->group_filter_values))
				pass_group_test = TRUE;
		} else if (egg_recent_item_get_private (item)) {
			pass_group_test = FALSE;
		} else
			pass_group_test = TRUE;

		/* filter by URI scheme */
		if (pass_mime_test && pass_group_test &&
		    model->priv->scheme_filter_values != NULL) {
			gchar *scheme;

			scheme = gnome_vfs_get_uri_scheme (uri);

			if (egg_recent_model_string_match
				(model->priv->scheme_filter_values, scheme))
				pass_scheme_test = TRUE;

			g_free (scheme);
		} else
			pass_scheme_test = TRUE;

		if (pass_mime_test && pass_group_test && pass_scheme_test)
			newlist = g_list_prepend (newlist, item);

		g_free (uri);
	}

	if (newlist) {
		newlist = g_list_reverse (newlist);
		g_list_free (list);
	}


	return newlist;
}



#if 0
static void
egg_recent_model_monitor_list_cb (GnomeVFSMonitorHandle *handle,
			       const gchar *monitor_uri,
			       const gchar *info_uri,
			       GnomeVFSMonitorEventType event_type,
			       gpointer user_data)
{
	EggRecentModel *model;

	model = EGG_RECENT_MODEL (user_data);

	if (event_type == GNOME_VFS_MONITOR_EVENT_DELETED) {
		egg_recent_model_delete (model, monitor_uri);
		g_hash_table_remove (model->priv->monitors, monitor_uri);
	}
}



static void
egg_recent_model_monitor_list (EggRecentModel *model, GList *list)
{
	GList *tmp;

	tmp = list;
	while (tmp) {
		EggRecentItem *item = (EggRecentItem *)tmp->data;
		GnomeVFSMonitorHandle *handle;
		GnomeVFSResult res;
		gchar *uri;

		tmp = tmp->next;

		uri = egg_recent_item_get_uri (item);
		if (g_hash_table_lookup (model->priv->monitors, uri)) {
			/* already monitoring this one */
			g_free (uri);
			continue;
		}

		res = gnome_vfs_monitor_add (&handle, uri,
					     GNOME_VFS_MONITOR_FILE,
					     egg_recent_model_monitor_list_cb,
					     model);

		if (res == GNOME_VFS_OK)
			g_hash_table_insert (model->priv->monitors, uri, handle);
		else
			g_free (uri);
	}
}
#endif


static gboolean
egg_recent_model_changed_timeout (EggRecentModel *model)
{
	egg_recent_model_changed (model);

	return FALSE;
}

static void
egg_recent_model_monitor_cb (GnomeVFSMonitorHandle *handle,
			       const gchar *monitor_uri,
			       const gchar *info_uri,
			       GnomeVFSMonitorEventType event_type,
			       gpointer user_data)
{
	EggRecentModel *model;

	g_return_if_fail (user_data != NULL);
	g_return_if_fail (EGG_IS_RECENT_MODEL (user_data));
	model = EGG_RECENT_MODEL (user_data);

	if (event_type == GNOME_VFS_MONITOR_EVENT_CHANGED) {
		if (model->priv->changed_timeout > 0) {
			g_source_remove (model->priv->changed_timeout);
		}

		model->priv->changed_timeout = g_timeout_add (
			EGG_RECENT_MODEL_TIMEOUT_LENGTH,
			(GSourceFunc)egg_recent_model_changed_timeout,
			model);
	}
}

static void
egg_recent_model_monitor (EggRecentModel *model, gboolean should_monitor)
{
	if (should_monitor && model->priv->monitor == NULL) {
		char *uri;

		uri = gnome_vfs_get_uri_from_local_path (model->priv->path);

		gnome_vfs_monitor_add (&model->priv->monitor,
				       uri,
				       GNOME_VFS_MONITOR_FILE,
				       egg_recent_model_monitor_cb,
				       model);

		g_free (uri);

		/* if the above fails, don't worry about it.
		 * local notifications will still happen
		 */

	} else if (!should_monitor && model->priv->monitor != NULL) {
		gnome_vfs_monitor_cancel (model->priv->monitor);
		model->priv->monitor = NULL;
	}
}

static void
egg_recent_model_set_limit_internal (EggRecentModel *model, int limit)
{
	model->priv->limit = limit;

	if (limit <= 0)
		egg_recent_model_monitor (model, FALSE);
	else {
		egg_recent_model_monitor (model, TRUE);
		egg_recent_model_changed (model);
	}
}

static GList *
egg_recent_model_read (EggRecentModel *model, FILE *file)
{
	GList *list=NULL;
	gchar *content;
	GMarkupParseContext *ctx;
	ParseInfo info;
	GError *error;

	content = egg_recent_model_read_raw (model, file);

	if (strlen (content) <= 0) {
		g_free (content);
		return NULL;
	}

	parse_info_init (&info);

	ctx = g_markup_parse_context_new (&parser, 0, &info, NULL);

	error = NULL;
	if (!g_markup_parse_context_parse (ctx, content, strlen (content),
					   &error)) {
		g_warning (error->message);
		g_error_free (error);
		error = NULL;
		goto out;
	}

	error = NULL;
	if (!g_markup_parse_context_end_parse (ctx, &error))
		goto out;

	g_markup_parse_context_free (ctx);
out:
	list = info.items;

	parse_info_free (&info);

	g_free (content);

	/*
	g_print ("Total items: %d\n", g_list_length (list));
	*/

	return list;
}


static gboolean
egg_recent_model_write (EggRecentModel *model, FILE *file, GList *list)
{
	GString *string;
	gchar *data;
	EggRecentItem *item;
	const GList *groups;
	int i;
	int ret;

	string = g_string_new ("<?xml version=\"1.0\"?>\n");
	string = g_string_append (string, "<" TAG_RECENT_FILES ">\n");

	i=0;
	while (list) {
		gchar *uri;
		gchar *mime_type;
		gchar *escaped_uri;
		time_t timestamp;
		item = (EggRecentItem *)list->data;


		uri = egg_recent_item_get_uri_utf8 (item);
		escaped_uri = g_markup_escape_text (uri,
						    strlen (uri));
		g_free (uri);

		mime_type = egg_recent_item_get_mime_type (item);
		timestamp = egg_recent_item_get_timestamp (item);

		string = g_string_append (string, "  <" TAG_RECENT_ITEM ">\n");

		g_string_append_printf (string,
				"    <" TAG_URI ">%s</" TAG_URI ">\n", escaped_uri);

		if (mime_type)
			g_string_append_printf (string,
				"    <" TAG_MIME_TYPE ">%s</" TAG_MIME_TYPE ">\n", mime_type);
		else
			g_string_append_printf (string,
				"    <" TAG_MIME_TYPE "></" TAG_MIME_TYPE ">\n");


		g_string_append_printf (string,
				"    <" TAG_TIMESTAMP ">%d</" TAG_TIMESTAMP ">\n", (int)timestamp);

		if (egg_recent_item_get_private (item))
			string = g_string_append (string,
					"    <" TAG_PRIVATE "/>\n");

		/* write the groups */
		string = g_string_append (string,
				"    <" TAG_GROUPS ">\n");
		groups = egg_recent_item_get_groups (item);

		if (groups == NULL && egg_recent_item_get_private (item))
			g_warning ("Item with URI \"%s\" marked as private, but"
				   " does not belong to any groups.\n", uri);

		while (groups) {
			const gchar *group = (const gchar *)groups->data;
			gchar *escaped_group;

			escaped_group = g_markup_escape_text (group, strlen(group));

			g_string_append_printf (string,
					"      <" TAG_GROUP ">%s</" TAG_GROUP ">\n",
					escaped_group);

			g_free (escaped_group);

			groups = groups->next;
		}

		string = g_string_append (string, "    </" TAG_GROUPS ">\n");

		string = g_string_append (string,
				"  </" TAG_RECENT_ITEM ">\n");

		g_free (mime_type);
		g_free (escaped_uri);

		list = list->next;
		i++;
	}

	string = g_string_append (string, "</" TAG_RECENT_FILES ">");

	data = g_string_free (string, FALSE);

	ret = egg_recent_model_write_raw (model, file, data);

	g_free (data);

	return ret;
}

static FILE *
egg_recent_model_open_file (EggRecentModel *model)
{
	FILE *file;
	mode_t prev_umask;

	file = fopen (model->priv->path, "r+");
	if (file == NULL) {
		/* be paranoid */
		prev_umask = umask (077);

		file = fopen (model->priv->path, "w+");

		umask (prev_umask);

		g_return_val_if_fail (file != NULL, NULL);
	}

	return file;
}

static gboolean
egg_recent_model_lock_file (FILE *file)
{
	int fd;
	gint	try = 5;

	rewind (file);
	fd = fileno (file);

	/* Attempt to lock the file 5 times,
	 * waiting a random interval (< 1 second)
	 * in between attempts.
	 * We should really be doing asynchronous
	 * locking, but requires substantially larger
	 * changes.
	 */

	while (try > 0)
	{
		int rand_interval;

		if (lockf (fd, F_TLOCK, 0) == 0)
			return TRUE;

		rand_interval = 1 + (int) (10.0 * rand()/(RAND_MAX + 1.0));

           	g_usleep (100000 * rand_interval);

		--try;
	}

	return FALSE;
}

static gboolean
egg_recent_model_unlock_file (FILE *file)
{
	int fd;

	rewind (file);
	fd = fileno (file);

	return (lockf (fd, F_ULOCK, 0) == 0) ? TRUE : FALSE;
}

static void
egg_recent_model_finalize (GObject *object)
{
	EggRecentModel *model = EGG_RECENT_MODEL (object);

	egg_recent_model_monitor (model, FALSE);


	g_slist_foreach (model->priv->mime_filter_values,
			 (GFunc) g_pattern_spec_free, NULL);
	g_slist_free (model->priv->mime_filter_values);
	model->priv->mime_filter_values = NULL;

	g_slist_foreach (model->priv->scheme_filter_values,
			 (GFunc) g_pattern_spec_free, NULL);
	g_slist_free (model->priv->scheme_filter_values);
	model->priv->scheme_filter_values = NULL;

	g_slist_foreach (model->priv->group_filter_values,
			 (GFunc) g_free, NULL);
	g_slist_free (model->priv->group_filter_values);
	model->priv->group_filter_values = NULL;


	if (model->priv->limit_change_notify_id)
		gconf_client_notify_remove (model->priv->client,
					    model->priv->limit_change_notify_id);
	model->priv->expiration_change_notify_id = 0;

	if (model->priv->expiration_change_notify_id)
		gconf_client_notify_remove (model->priv->client,
					    model->priv->expiration_change_notify_id);
	model->priv->expiration_change_notify_id = 0;

	g_object_unref (model->priv->client);
	model->priv->client = NULL;


	g_free (model->priv->path);
	model->priv->path = NULL;

	g_hash_table_destroy (model->priv->monitors);
	model->priv->monitors = NULL;


	g_free (model->priv);
}

static void
egg_recent_model_set_property (GObject *object,
			       guint prop_id,
			       const GValue *value,
			       GParamSpec *pspec)
{
	EggRecentModel *model = EGG_RECENT_MODEL (object);

	switch (prop_id)
	{
		case PROP_MIME_FILTERS:
			model->priv->mime_filter_values =
				(GSList *)g_value_get_pointer (value);
		break;

		case PROP_GROUP_FILTERS:
			model->priv->group_filter_values =
				(GSList *)g_value_get_pointer (value);
		break;

		case PROP_SCHEME_FILTERS:
			model->priv->scheme_filter_values =
				(GSList *)g_value_get_pointer (value);
		break;

		case PROP_SORT_TYPE:
			model->priv->sort_type = g_value_get_int (value);
		break;

		case PROP_LIMIT:
			egg_recent_model_set_limit (model,
						g_value_get_int (value));
		break;

		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
egg_recent_model_get_property (GObject *object,
			       guint prop_id,
			       GValue *value,
			       GParamSpec *pspec)
{
	EggRecentModel *model = EGG_RECENT_MODEL (object);

	switch (prop_id)
	{
		case PROP_MIME_FILTERS:
			g_value_set_pointer (value, model->priv->mime_filter_values);
		break;

		case PROP_GROUP_FILTERS:
			g_value_set_pointer (value, model->priv->group_filter_values);
		break;

		case PROP_SCHEME_FILTERS:
			g_value_set_pointer (value, model->priv->scheme_filter_values);
		break;

		case PROP_SORT_TYPE:
			g_value_set_int (value, model->priv->sort_type);
		break;

		case PROP_LIMIT:
			g_value_set_int (value, model->priv->limit);
		break;

		default:
			G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
egg_recent_model_class_init (EggRecentModelClass * klass)
{
	GObjectClass *object_class;

	object_class = G_OBJECT_CLASS (klass);
	object_class->set_property = egg_recent_model_set_property;
	object_class->get_property = egg_recent_model_get_property;
	object_class->finalize     = egg_recent_model_finalize;

	model_signals[CHANGED] = g_signal_new ("changed",
			G_OBJECT_CLASS_TYPE (object_class),
			G_SIGNAL_RUN_LAST,
			G_STRUCT_OFFSET (EggRecentModelClass, changed),
			NULL, NULL,
			g_cclosure_marshal_VOID__POINTER,
			G_TYPE_NONE, 1,
			G_TYPE_POINTER);


	g_object_class_install_property (object_class,
					 PROP_MIME_FILTERS,
					 g_param_spec_pointer ("mime-filters",
					 "Mime Filters",
					 "List of mime types to be allowed.",
					 G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
					 PROP_GROUP_FILTERS,
					 g_param_spec_pointer ("group-filters",
					 "Group Filters",
					 "List of groups to be allowed.",
					 G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
					 PROP_SCHEME_FILTERS,
					 g_param_spec_pointer ("scheme-filters",
					 "Scheme Filters",
					 "List of URI schemes to be allowed.",
					 G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
					 PROP_SORT_TYPE,
					 g_param_spec_int ("sort-type",
					 "Sort Type",
					 "Type of sorting to be done.",
					 0, EGG_RECENT_MODEL_SORT_NONE,
					 EGG_RECENT_MODEL_SORT_MRU,
					 G_PARAM_READWRITE));

	g_object_class_install_property (object_class,
					 PROP_LIMIT,
					 g_param_spec_int ("limit",
					 "Limit",
					 "Max number of items allowed.",
					 -1, EGG_RECENT_MODEL_MAX_ITEMS,
					 EGG_RECENT_MODEL_DEFAULT_LIMIT,
					 G_PARAM_READWRITE));

	klass->changed = NULL;
}



static void
egg_recent_model_limit_changed (GConfClient *client, guint cnxn_id,
				GConfEntry *entry, gpointer user_data)
{
	EggRecentModel *model;
	GConfValue *value;

	model = EGG_RECENT_MODEL (user_data);

	g_return_if_fail (model != NULL);

	if (model->priv->use_default_limit == FALSE)
		return; /* ignore this key */

	/* the key was unset, and the schema has apparently failed */
	if (entry == NULL)
		return;

	value = gconf_entry_get_value (entry);

	if (value->type != GCONF_VALUE_INT) {
		g_warning ("Expected GConfValue of type integer, "
			   "got something else");
	}


	egg_recent_model_set_limit_internal (model, gconf_value_get_int (value));
}

static void
egg_recent_model_expiration_changed (GConfClient *client, guint cnxn_id,
				     GConfEntry *entry, gpointer user_data)
{

}

static void
egg_recent_model_init (EggRecentModel * model)
{
	if (!gnome_vfs_init ()) {
		g_warning ("gnome-vfs initialization failed.");
		return;
	}


	model->priv = g_new0 (EggRecentModelPrivate, 1);

	model->priv->path = g_strdup_printf ("%s" EGG_RECENT_MODEL_FILE_PATH,
					     g_get_home_dir ());

	model->priv->mime_filter_values   = NULL;
	model->priv->group_filter_values  = NULL;
	model->priv->scheme_filter_values = NULL;

	model->priv->client = gconf_client_get_default ();
	gconf_client_add_dir (model->priv->client, EGG_RECENT_MODEL_KEY_DIR,
			      GCONF_CLIENT_PRELOAD_ONELEVEL, NULL);

	model->priv->limit_change_notify_id =
			gconf_client_notify_add (model->priv->client,
						 EGG_RECENT_MODEL_DEFAULT_LIMIT_KEY,
						 egg_recent_model_limit_changed,
						 model, NULL, NULL);

	model->priv->expiration_change_notify_id =
			gconf_client_notify_add (model->priv->client,
						 EGG_RECENT_MODEL_EXPIRE_KEY,
						 egg_recent_model_expiration_changed,
						 model, NULL, NULL);

	model->priv->expire_days = gconf_client_get_int (
					model->priv->client,
					EGG_RECENT_MODEL_EXPIRE_KEY,
					NULL);

#if 0
	/* keep this out, for now */
	model->priv->limit = gconf_client_get_int (
					model->priv->client,
					EGG_RECENT_MODEL_DEFAULT_LIMIT_KEY, NULL);
	model->priv->use_default_limit = TRUE;
#endif
	model->priv->limit = EGG_RECENT_MODEL_DEFAULT_LIMIT;
	model->priv->use_default_limit = FALSE;

	model->priv->monitors = g_hash_table_new_full (
					g_str_hash, g_str_equal,
					(GDestroyNotify) g_free,
					(GDestroyNotify) gnome_vfs_monitor_cancel);

	model->priv->monitor = NULL;
	egg_recent_model_monitor (model, TRUE);
}


/**
 * egg_recent_model_new:
 * @sort:  the type of sorting to use
 * @limit:  maximum number of items in the list
 *
 * This creates a new EggRecentModel object.
 *
 * Returns: a EggRecentModel object
 */
EggRecentModel *
egg_recent_model_new (EggRecentModelSort sort)
{
	EggRecentModel *model;

	model = EGG_RECENT_MODEL (g_object_new (egg_recent_model_get_type (),
				  "sort-type", sort, NULL));

	g_return_val_if_fail (model, NULL);

	return model;
}

/**
 * egg_recent_model_add_full:
 * @model:  A EggRecentModel object.
 * @item:  A EggRecentItem
 *
 * This function adds an item to the list of recently used URIs.
 *
 * Returns: gboolean
 */
gboolean
egg_recent_model_add_full (EggRecentModel * model, EggRecentItem *item)
{
	FILE *file;
	GList *list = NULL;
	gboolean ret = FALSE;
	gboolean updated = FALSE;
	char *uri;
	time_t t;

	g_return_val_if_fail (model != NULL, FALSE);
	g_return_val_if_fail (EGG_IS_RECENT_MODEL (model), FALSE);

	uri = egg_recent_item_get_uri (item);
	if (strncmp (uri, "recent-files://", strlen ("recent-files://")) == 0) {
		g_free (uri);
		return FALSE;
	} else {
		g_free (uri);
	}

	file = egg_recent_model_open_file (model);
	g_return_val_if_fail (file != NULL, FALSE);

	time (&t);
	egg_recent_item_set_timestamp (item, t);

	if (egg_recent_model_lock_file (file)) {

		/* read existing stuff */
		list = egg_recent_model_read (model, file);

		/* if it's already there, we just update it */
		updated = egg_recent_model_update_item (list, item);

		if (!updated) {
			list = g_list_prepend (list, item);

			egg_recent_model_enforce_limit (list,
						EGG_RECENT_MODEL_MAX_ITEMS);
		}

		/* write new stuff */
		if (!egg_recent_model_write (model, file, list))
			g_warning ("Write failed: %s", strerror (errno));

		if (!updated)
			list = g_list_remove (list, item);

		EGG_RECENT_ITEM_LIST_UNREF (list);
		ret = TRUE;
	} else {
		g_warning ("Failed to lock:  %s", strerror (errno));
		fclose (file);
		return FALSE;
	}

	if (!egg_recent_model_unlock_file (file))
		g_warning ("Failed to unlock: %s", strerror (errno));

	fclose (file);

	if (model->priv->monitor == NULL) {
		/* since monitoring isn't working, at least give a
		 * local notification
		 */
		egg_recent_model_changed (model);
	}

	return ret;
}

/**
 * egg_recent_model_add:
 * @model:  A EggRecentModel object.
 * @uri:  A string URI
 *
 * This function adds an item to the list of recently used URIs.
 *
 * Returns: gboolean
 */
gboolean
egg_recent_model_add (EggRecentModel *model, const gchar *uri)
{
	EggRecentItem *item;
	gboolean ret = FALSE;

	g_return_val_if_fail (model != NULL, FALSE);
	g_return_val_if_fail (uri != NULL, FALSE);

	item = egg_recent_item_new_from_uri (uri);

	g_return_val_if_fail (item != NULL, FALSE);

	ret = egg_recent_model_add_full (model, item);

	egg_recent_item_unref (item);

	return ret;
}



/**
 * egg_recent_model_delete:
 * @model:  A EggRecentModel object.
 * @uri: The URI you want to delete.
 *
 * This function deletes a URI from the file of recently used URIs.
 *
 * Returns: gboolean
 */
gboolean
egg_recent_model_delete (EggRecentModel * model, const gchar * uri)
{
	FILE *file;
	GList *list;
	unsigned int length;
	gboolean ret = FALSE;

	g_return_val_if_fail (model != NULL, FALSE);
	g_return_val_if_fail (EGG_IS_RECENT_MODEL (model), FALSE);
	g_return_val_if_fail (uri != NULL, FALSE);

	file = egg_recent_model_open_file (model);
	g_return_val_if_fail (file != NULL, FALSE);

	if (egg_recent_model_lock_file (file)) {
		list = egg_recent_model_read (model, file);

		if (list == NULL)
			goto out;

		length = g_list_length (list);

		list = egg_recent_model_delete_from_list (list, uri);

		if (length == g_list_length (list)) {
			/* nothing was deleted */
			EGG_RECENT_ITEM_LIST_UNREF (list);
		} else {
			egg_recent_model_write (model, file, list);
			EGG_RECENT_ITEM_LIST_UNREF (list);
			ret = TRUE;

		}
	} else {
		g_warning ("Failed to lock:  %s", strerror (errno));
		return FALSE;
	}

out:

	if (!egg_recent_model_unlock_file (file))
		g_warning ("Failed to unlock: %s", strerror (errno));

	fclose (file);

	g_hash_table_remove (model->priv->monitors, uri);

	if (model->priv->monitor == NULL && ret) {
		/* since monitoring isn't working, at least give a
		 * local notification
		 */
		egg_recent_model_changed (model);
	}

	return ret;
}


/**
 * egg_recent_model_get_list:
 * @model:  A EggRecentModel object.
 *
 * This function gets the current contents of the file
 *
 * Returns: a GList
 */
GList *
egg_recent_model_get_list (EggRecentModel *model)
{
	FILE *file;
	GList *list=NULL;

	file = egg_recent_model_open_file (model);
	g_return_val_if_fail (file != NULL, NULL);

	if (egg_recent_model_lock_file (file)) {
		list = egg_recent_model_read (model, file);

	} else {
		g_warning ("Failed to lock:  %s", strerror (errno));
		fclose (file);
		return NULL;
	}

	if (!egg_recent_model_unlock_file (file))
		g_warning ("Failed to unlock: %s", strerror (errno));

	if (list != NULL) {
		list = egg_recent_model_filter (model, list);
		list = egg_recent_model_sort (model, list);

		egg_recent_model_enforce_limit (list, model->priv->limit);
	}

	fclose (file);

	return list;
}



/**
 * egg_recent_model_set_limit:
 * @model:  A EggRecentModel object.
 * @limit:  The maximum length of the list
 *
 * This function sets the maximum length of the list.  Note:  This only affects
 * the length of the list emitted in the "changed" signal, not the list stored
 * on disk.
 *
 * Returns:  void
 */
void
egg_recent_model_set_limit (EggRecentModel *model, int limit)
{
	model->priv->use_default_limit = FALSE;

	egg_recent_model_set_limit_internal (model, limit);
}

/**
 * egg_recent_model_get_limit:
 * @model:  A EggRecentModel object.
 *
 * This function gets the maximum length of the list.
 *
 * Returns:  int
 */
int
egg_recent_model_get_limit (EggRecentModel *model)
{
	return model->priv->limit;
}


/**
 * egg_recent_model_clear:
 * @model:  A EggRecentModel object.
 *
 * This function clears the contents of the file
 *
 * Returns: void
 */
void
egg_recent_model_clear (EggRecentModel *model)
{
	FILE *file;
	int fd;

	file = egg_recent_model_open_file (model);
	g_return_if_fail (file != NULL);

	fd = fileno (file);

	if (egg_recent_model_lock_file (file)) {
		ftruncate (fd, 0);
	} else {
		g_warning ("Failed to lock:  %s", strerror (errno));
		return;
	}

	if (!egg_recent_model_unlock_file (file))
		g_warning ("Failed to unlock: %s", strerror (errno));

	fclose (file);
}


/**
 * egg_recent_model_set_filter_mime_types:
 * @model:  A EggRecentModel object.
 *
 * Sets which mime types are allowed in the list.
 *
 * Returns: void
 */
void
egg_recent_model_set_filter_mime_types (EggRecentModel *model,
			       ...)
{
	va_list valist;
	GSList *list = NULL;
	gchar *str;

	g_return_if_fail (model != NULL);

	if (model->priv->mime_filter_values != NULL) {
		g_slist_foreach (model->priv->mime_filter_values,
				 (GFunc) g_pattern_spec_free, NULL);
		g_slist_free (model->priv->mime_filter_values);
		model->priv->mime_filter_values = NULL;
	}

	va_start (valist, model);

	str = va_arg (valist, gchar*);

	while (str != NULL) {
		list = g_slist_prepend (list, g_pattern_spec_new (str));

		str = va_arg (valist, gchar*);
	}

	va_end (valist);

	model->priv->mime_filter_values = list;
}

/**
 * egg_recent_model_set_filter_groups:
 * @model:  A EggRecentModel object.
 *
 * Sets which groups are allowed in the list.
 *
 * Returns: void
 */
void
egg_recent_model_set_filter_groups (EggRecentModel *model,
			       ...)
{
	va_list valist;
	GSList *list = NULL;
	gchar *str;

	g_return_if_fail (model != NULL);

	if (model->priv->group_filter_values != NULL) {
		g_slist_foreach (model->priv->group_filter_values, (GFunc)g_free, NULL);
		g_slist_free (model->priv->group_filter_values);
		model->priv->group_filter_values = NULL;
	}

	va_start (valist, model);

	str = va_arg (valist, gchar*);

	while (str != NULL) {
		list = g_slist_prepend (list, g_strdup (str));

		str = va_arg (valist, gchar*);
	}

	va_end (valist);

	model->priv->group_filter_values = list;
}

/**
 * egg_recent_model_set_filter_uri_schemes:
 * @model:  A EggRecentModel object.
 *
 * Sets which URI schemes (file, http, ftp, etc) are allowed in the list.
 *
 * Returns: void
 */
void
egg_recent_model_set_filter_uri_schemes (EggRecentModel *model, ...)
{
	va_list valist;
	GSList *list = NULL;
	gchar *str;

	g_return_if_fail (model != NULL);

	if (model->priv->scheme_filter_values != NULL) {
		g_slist_foreach (model->priv->scheme_filter_values,
				(GFunc) g_pattern_spec_free, NULL);
		g_slist_free (model->priv->scheme_filter_values);
		model->priv->scheme_filter_values = NULL;
	}

	va_start (valist, model);

	str = va_arg (valist, gchar*);

	while (str != NULL) {
		list = g_slist_prepend (list, g_pattern_spec_new (str));

		str = va_arg (valist, gchar*);
	}

	va_end (valist);

	model->priv->scheme_filter_values = list;
}

/**
 * egg_recent_model_set_sort:
 * @model:  A EggRecentModel object.
 * @sort:  A EggRecentModelSort type
 *
 * Sets the type of sorting to be used.
 *
 * Returns: void
 */
void
egg_recent_model_set_sort (EggRecentModel *model,
			     EggRecentModelSort sort)
{
	g_return_if_fail (model != NULL);

	model->priv->sort_type = sort;
}

/**
 * egg_recent_model_changed:
 * @model:  A EggRecentModel object.
 *
 * This function causes a "changed" signal to be emitted.
 *
 * Returns: void
 */
void
egg_recent_model_changed (EggRecentModel *model)
{
	GList *list = NULL;

	if (model->priv->limit > 0) {
		list = egg_recent_model_get_list (model);
		/* egg_recent_model_monitor_list (model, list); */

		g_signal_emit (G_OBJECT (model), model_signals[CHANGED], 0,
			       list);
	}

	if (list)
		EGG_RECENT_ITEM_LIST_UNREF (list);
}

static void
egg_recent_model_remove_expired_list (EggRecentModel *model, GList *list)
{
	time_t current_time;
	time_t day_seconds;

	time (&current_time);
	day_seconds = model->priv->expire_days*24*60*60;

	while (list != NULL) {
		EggRecentItem *item = list->data;
		time_t timestamp;

		timestamp = egg_recent_item_get_timestamp (item);

		if ((timestamp+day_seconds) < current_time) {
			gchar *uri = egg_recent_item_get_uri (item);
			egg_recent_model_delete (model, uri);

			g_strdup (uri);
		}

		list = list->next;
	}
}


/**
 * egg_recent_model_remove_expired:
 * @model:  A EggRecentModel object.
 *
 * Goes through the entire list, and removes any items that are older than
 * the user-specified expiration period.
 *
 * Returns: void
 */
void
egg_recent_model_remove_expired (EggRecentModel *model)
{
	FILE *file;
	GList *list=NULL;

	g_return_if_fail (model != NULL);

	file = egg_recent_model_open_file (model);
	g_return_if_fail (file != NULL);

	if (egg_recent_model_lock_file (file)) {
		list = egg_recent_model_read (model, file);

	} else {
		g_warning ("Failed to lock:  %s", strerror (errno));
		return;
	}

	if (!egg_recent_model_unlock_file (file))
		g_warning ("Failed to unlock: %s", strerror (errno));

	if (list != NULL) {
		egg_recent_model_remove_expired_list (model, list);
		EGG_RECENT_ITEM_LIST_UNREF (list);
	}

	fclose (file);
}

/**
 * egg_recent_model_get_type:
 *
 * This returns a GType representing a EggRecentModel object.
 *
 * Returns: a GType
 */
GType
egg_recent_model_get_type (void)
{
	static GType egg_recent_model_type = 0;

	if(!egg_recent_model_type) {
		static const GTypeInfo egg_recent_model_info = {
			sizeof (EggRecentModelClass),
			NULL, /* base init */
			NULL, /* base finalize */
			(GClassInitFunc)egg_recent_model_class_init, /* class init */
			NULL, /* class finalize */
			NULL, /* class data */
			sizeof (EggRecentModel),
			0,
			(GInstanceInitFunc) egg_recent_model_init
		};

		egg_recent_model_type = g_type_register_static (G_TYPE_OBJECT,
							"EggRecentModel",
							&egg_recent_model_info, 0);
	}

	return egg_recent_model_type;
}


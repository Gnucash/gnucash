/* File import from libegg to gnumeric by import-egg.  Do not edit.  */


#ifndef __EGG_RECENT_ITEM_H__
#define __EGG_RECENT_ITEM_H__

#include <time.h>
#include <glib.h>
#include <glib-object.h>

G_BEGIN_DECLS

#define EGG_TYPE_RECENT_ITEM       (egg_recent_item_get_type ())

#define EGG_RECENT_ITEM_LIST_UNREF(list) \
	g_list_foreach (list, (GFunc)egg_recent_item_unref, NULL); \
	g_list_free (list);

typedef struct _EggRecentItem EggRecentItem;

struct _EggRecentItem {
	/* do not access any of these directly */
	gchar *uri;
	gchar *mime_type;
	time_t timestamp;

	gboolean private_data;

	GList *groups;

	int refcount;
};

GType		egg_recent_item_get_type (void) G_GNUC_CONST;

/* constructors */
EggRecentItem * egg_recent_item_new (void);

EggRecentItem *	egg_recent_item_ref (EggRecentItem *item);
EggRecentItem *	egg_recent_item_unref (EggRecentItem *item);

/* automatically fetches the mime type, etc */
EggRecentItem * egg_recent_item_new_from_uri (const gchar *uri);

gboolean egg_recent_item_set_uri (EggRecentItem *item, const gchar *uri);
gchar * egg_recent_item_get_uri (const EggRecentItem *item);
gchar * egg_recent_item_get_uri_utf8 (const EggRecentItem *item);
gchar * egg_recent_item_get_uri_for_display (const EggRecentItem *item);
gchar * egg_recent_item_get_short_name (const EggRecentItem *item);

void egg_recent_item_set_mime_type (EggRecentItem *item, const gchar *mime);
gchar * egg_recent_item_get_mime_type (const EggRecentItem *item);

void egg_recent_item_set_timestamp (EggRecentItem *item, time_t timestamp);
time_t egg_recent_item_get_timestamp (const EggRecentItem *item);

G_CONST_RETURN gchar *egg_recent_item_peek_uri (const EggRecentItem *item);


/* groups */
G_CONST_RETURN GList *  egg_recent_item_get_groups (const EggRecentItem *item);

gboolean       egg_recent_item_in_group (const EggRecentItem *item,
					   const gchar *group_name);

void           egg_recent_item_add_group (EggRecentItem *item,
					    const gchar *group_name);

void           egg_recent_item_remove_group (EggRecentItem *item,
					       const gchar *group_name);

void           egg_recent_item_set_private (EggRecentItem *item,
					      gboolean priv);

gboolean       egg_recent_item_get_private (const EggRecentItem *item);


G_END_DECLS

#endif /* __EGG_RECENT_ITEM_H__ */

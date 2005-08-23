#include "split.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include <glib.h>
#include <glib-object.h>
#include <gtk/gtkwidget.h>

#include <goffice/utils/go-locale.h>
#include <libxml/tree.h>
#include "xml-io.h"

#include "application.h"
#include "datetime.h"
#include "gnumeric.h"

#include "value.h"

#define CC2XML(s) ((const xmlChar *)(s))
#define CXML2C(s) ((const char *)(s))

// 1904 = false
GnmDateConventions gdc_singleton = { 0 };

GnmExprConventions stack_gnm_expr_conventions_default = {
  FALSE,
  NULL,
  NULL
};

GnmExprConventions *gnm_expr_conventions_default = &stack_gnm_expr_conventions_default;

GnmDateConventions const *
workbook_date_conv( Workbook const *wb )
{
  return &gdc_singleton;
}

GnmValue const *
value_area_fetch_x_y (GnmValue const *v, int x, int y, GnmEvalPos const *ep)
{
	GnmValue const * const res = value_area_get_x_y (v, x, y, ep);
	if (res && res->type != VALUE_EMPTY)
		return res;

	return value_zero;
}

/*
 * An internal routine to get a cell from an array or range.  If any
 * problems occur a NULL is returned.
 */
GnmValue const *
value_area_get_x_y (GnmValue const *v, int x, int y, GnmEvalPos const *ep)
{
	g_return_val_if_fail (v, NULL);

	if (v->type == VALUE_ARRAY){
		g_return_val_if_fail (x < v->v_array.x &&
				      y < v->v_array.y,
				      NULL);
		return v->v_array.vals [x][y];
	} else if (v->type == VALUE_CELLRANGE) {
          // jsled: throw a frickin' exception.
          printf( "failure. cellrange.\n" );
	} else
		return v;

	return NULL;
}

#if 0

char *
global_range_name(Sheet *sheet, GnmRange const *r)
{
  return "unimplemented";
}
#endif // 0

double
gnm_app_display_dpi_get (gboolean horizontal)
{
  // jsled: Taken as default value from gnumeric/src/gnumeric-gconf.c
  return 96.;
}

struct _GnmApp {
	GObject  base;

	/* Clipboard */
	SheetView	*clipboard_sheet_view;
	GnmCellRegion	*clipboard_copied_contents;
	GnmRange	*clipboard_cut_range;

	/* History for file menu */
	GSList           *history_list;

	/* Others */
	GtkWidget       *pref_dialog;

	GList		*workbook_list;

	GHashTable      *named_pixbufs;
};

/* Signals */
enum {
	WORKBOOK_ADDED,
	WORKBOOK_REMOVED,
	WINDOW_LIST_CHANGED,
	CUSTOM_UI_ADDED,
	CUSTOM_UI_REMOVED,
	CLIPBOARD_MODIFIED,
	LAST_SIGNAL
};

static guint signals [LAST_SIGNAL] = { 0 };

static GnmApp *app;

GnmAction *
gnm_action_new (char const *id, char const *label,
		char const *icon_name, gboolean always_available,
		GnmActionHandler handler)
{
	GnmAction *res = g_new0 (GnmAction, 1);
	res->id		= g_strdup (id);
	res->label	= g_strdup (label);
	res->icon_name	= g_strdup (icon_name);
	res->always_available = always_available;
	res->handler	= handler;
	return res;
}

void
gnm_action_free (GnmAction *action)
{
	if (NULL != action) {
		g_free (action->id);
		g_free (action->label);
		g_free (action->icon_name);
		g_free (action);
	}
}

static GSList *extra_uis = NULL;

GnmAppExtraUI *
gnm_app_add_extra_ui (GSList *actions, char *layout,
		      char const *domain,
		      gpointer user_data)
{
	GnmAppExtraUI *extra_ui = g_new0 (GnmAppExtraUI, 1);
	extra_uis = g_slist_prepend (extra_uis, extra_ui);
	extra_ui->actions = actions;
	extra_ui->layout = layout;
	extra_ui->user_data = user_data;
	g_signal_emit (G_OBJECT (app), signals [CUSTOM_UI_ADDED], 0, extra_ui);
	return extra_ui;
}

void
gnm_app_remove_extra_ui (GnmAppExtraUI *extra_ui)
{
	g_signal_emit (G_OBJECT (app), signals [CUSTOM_UI_REMOVED], 0, extra_ui);
}

/*
 * Get a named pixbuf.
 */
GdkPixbuf *
gnm_app_get_pixbuf (const char *name)
{
	g_return_val_if_fail (app != NULL, NULL);
	return g_hash_table_lookup (app->named_pixbufs, name);
}

gboolean
xml_node_get_bool (xmlNodePtr node, char const *name, gboolean *val)
{
	xmlChar *buf = xml_node_get_cstr (node, name);
	if (buf == NULL)
		return FALSE;

	*val = (!strcmp (buf, "1")
		|| 0 == g_ascii_strcasecmp (buf, "true"));
	g_free (buf);
	return TRUE;
}

xmlChar *
xml_node_get_cstr (xmlNodePtr node, char const *name)
{
	if (name != NULL)
		return xmlGetProp (node, CC2XML (name));
	/* in libxml1 <foo/> would return NULL
	 * in libxml2 <foo/> would return ""
	 */
	if (node->xmlChildrenNode != NULL)
		return xmlNodeGetContent (node);
	return NULL;
}

gboolean
xml_node_get_int (xmlNodePtr node, char const *name, int *val)
{
	xmlChar *buf;
	char *end;

	buf = xml_node_get_cstr (node, name);
	if (buf == NULL)
		return FALSE;

	errno = 0; /* strto(ld) sets errno, but does not clear it.  */
	*val = strtol (CXML2C (buf), &end, 10);
	xmlFree (buf);

	/* FIXME: it is, strictly speaking, not valid to use buf here.  */
	return (CXML2C (buf) != end) && (errno != ERANGE);
}

xmlNode *
e_xml_get_child_by_name (xmlNode const *parent, char const *child_name)
{
	xmlNode *child;

	g_return_val_if_fail (parent != NULL, NULL);
	g_return_val_if_fail (child_name != NULL, NULL);

	for (child = parent->xmlChildrenNode; child != NULL; child = child->next) {
		if (xmlStrcmp (child->name, child_name) == 0) {
			return child;
		}
	}
	return NULL;
}

xmlNode *
e_xml_get_child_by_name_no_lang (xmlNode const *parent, char const *name)
{
	xmlNodePtr node;

	g_return_val_if_fail (parent != NULL, NULL);
	g_return_val_if_fail (name != NULL, NULL);

	for (node = parent->xmlChildrenNode; node != NULL; node = node->next) {
		xmlChar *lang;

		if (node->name == NULL || strcmp (node->name, name) != 0) {
			continue;
		}
		lang = xmlGetProp (node, "xml:lang");
		if (lang == NULL) {
			return node;
		}
		xmlFree (lang);
	}

	return NULL;
}

xmlNode *
e_xml_get_child_by_name_by_lang (const xmlNode *parent, const gchar *name)
{
	xmlNodePtr   best_node = NULL, node;
	gint         best_lang_score = INT_MAX;
	GList const *lang_list = go_locale_languages ();

	g_return_val_if_fail (parent != NULL, NULL);
	g_return_val_if_fail (name != NULL, NULL);

	for (node = parent->xmlChildrenNode; node != NULL; node = node->next) {
		xmlChar *lang;

		if (node->name == NULL || strcmp (node->name, name) != 0)
			continue;

		lang = xmlGetProp (node, "xml:lang");
		if (lang != NULL) {
			const GList *l;
			gint i;

			for (l = lang_list, i = 0;
			     l != NULL && i < best_lang_score;
			     l = l->next, i++) {
				if (strcmp ((gchar *) l->data, lang) == 0) {
					best_node = node;
					best_lang_score = i;
				}
			}
		} else if (best_node == NULL)
			best_node = node;

		xmlFree (lang);
		if (best_lang_score == 0) 
			return best_node;
	}

	return best_node;
}


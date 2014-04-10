/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gnumeric-gconf.c:
 *
 *
 * Author:
 * 	Andreas J. Guelzow <aguelzow@taliesin.ca>
 *
 * (C) Copyright 2002-2004 Andreas J. Guelzow <aguelzow@taliesin.ca>
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
 */

#include <config.h>
#include <gnumeric.h>
#include "application.h"
#include "gnumeric-gconf.h"
#include "gnumeric-gconf-priv.h"
#include "gutils.h"
#include "mstyle.h"

static GnmAppPrefs prefs;
GnmAppPrefs const *gnm_app_prefs = &prefs;

#ifdef WITH_GNOME
#include <format.h>
#include <value.h>
#include <number-match.h>
#include <gconf/gconf-client.h>

static GConfClient *gconf_client = NULL;
static GConfClient *
gnm_app_get_gconf_client (void)
{
	if (!gconf_client) {
		gconf_client = gconf_client_get_default ();
		gconf_client_add_dir (gconf_client, "/apps/gnumeric",
				      GCONF_CLIENT_PRELOAD_RECURSIVE,
				      NULL);
	}
	return gconf_client;
}
void
go_conf_sync (void)
{
	gconf_client_suggest_sync (gnm_app_get_gconf_client (), NULL);
}

void     
go_conf_set_bool (char const *key, gboolean val)
{
	gconf_client_set_bool (gnm_app_get_gconf_client (), key, val, NULL);
}
void     
go_conf_set_int (char const *key, gint val)
{
	gconf_client_set_int (gnm_app_get_gconf_client (), key, val, NULL);
}
void     
go_conf_set_double (char const *key, gnm_float val)
{
	gconf_client_set_float (gnm_app_get_gconf_client (), key, val, NULL);
}
void     
go_conf_set_string (char const *key, char const *str)
{
	gconf_client_set_string (gnm_app_get_gconf_client (), key, str, NULL);
}
void
go_conf_set_str_list (char const *key, GSList *list)
{
	gconf_client_set_list (gnm_app_get_gconf_client (),
		key, GCONF_VALUE_STRING, list, NULL);
}

static GConfValue *
go_conf_get (char const *key, GConfValueType t)
{
	GError *err = NULL;
	GConfValue *val = gconf_client_get (gnm_app_get_gconf_client (), key, &err);

	if (err != NULL) {
		g_warning ("Unable to load key '%s' : because %s",
			   key, err->message);
		g_error_free (err);
		return NULL;
	}
	if (val == NULL) {
		g_warning ("Unable to load key '%s'", key);
		return NULL;
	}

	if (val->type != t) {
#if 1 /* gconf_value_type_to_string is internal */
		g_warning ("Expected `%d' got `%d' for key %s",
			t, val->type, key);
#else
		g_warning ("Expected `%s' got `%s' for key %s",
			gconf_value_type_to_string (t),
			gconf_value_type_to_string (val->type),
			key);
#endif
		gconf_value_free (val);
		return NULL;
	}

	return val;
}
gboolean
go_conf_load_bool (char const *key, gboolean default_val)
{
	gboolean res;
	GConfValue *val = go_conf_get (key, GCONF_VALUE_BOOL);

	if (val != NULL) {
		res = gconf_value_get_bool (val);
		gconf_value_free (val);
	} else {
		g_warning ("Using default value '%s'", default_val ? "true" : "false");
		return default_val;
	}
	return res;
}

int
go_conf_load_int (char const *key, int minima, int maxima, int default_val)
{
	int res = -1;
	GConfValue *val = go_conf_get (key, GCONF_VALUE_INT);

	if (val != NULL) {
		res = gconf_value_get_int (val);
		gconf_value_free (val);
		if (res < minima || maxima < res) {
			g_warning ("Invalid value '%d' for %s.  If should be >= %d and <= %d",
				   res, key, minima, maxima);
			val = NULL;
		}
	}
	if (val == NULL) {
		g_warning ("Using default value '%d'", default_val);
		return default_val;
	}
	return res;
}

double
go_conf_load_double (char const *key,
		     double minima, double maxima, double default_val)
{
	double res = -1;
	GConfValue *val = go_conf_get (key, GCONF_VALUE_FLOAT);

	if (val != NULL) {
		res = gconf_value_get_float (val);
		gconf_value_free (val);
		if (res < minima || maxima < res) {
			g_warning ("Invalid value '%g' for %s.  If should be >= %g and <= %g",
				   res, key, minima, maxima);
			val = NULL;
		}
	}
	if (val == NULL) {
		g_warning ("Using default value '%g'", default_val);
		return default_val;
	}
	return res;
}
char *
go_conf_load_string (char const *key)
{
	return gconf_client_get_string (gnm_app_get_gconf_client (), key, NULL);
}
GSList *
go_conf_load_str_list (char const *key)
{
	return gconf_client_get_list (gnm_app_get_gconf_client (),
		key, GCONF_VALUE_STRING, NULL);
}

static GConfSchema *
get_schema (char const *key)
{
	char *schema_key = g_strconcat ("/schemas", key, NULL);
	GConfSchema *schema = gconf_client_get_schema (
		gnm_app_get_gconf_client (), schema_key, NULL);
	g_free (schema_key);
	return schema;
}
char *
go_conf_get_short_desc (char const *key)
{
	GConfSchema *schema = get_schema (key);

	if (schema != NULL) {
		char *desc = g_strdup (gconf_schema_get_short_desc (schema));
		gconf_schema_free (schema);
		return desc;
	}
	return NULL;
}
char *
go_conf_get_long_desc  (char const *key)
{
	GConfSchema *schema = get_schema (key);

	if (schema != NULL) {
		char *desc =  g_strdup (gconf_schema_get_long_desc (schema));
		gconf_schema_free (schema);
		return desc;
	}
	return NULL;
}

GType
go_conf_get_type (char const *key)
{
	GConfSchema *schema = get_schema (key);
	GType t;

	switch (gconf_schema_get_type (schema)) {
	case GCONF_VALUE_STRING: t = G_TYPE_STRING; break;
	case GCONF_VALUE_FLOAT: t = G_TYPE_FLOAT; break;
	case GCONF_VALUE_INT: t = G_TYPE_INT; break;
	case GCONF_VALUE_BOOL: t = G_TYPE_BOOLEAN; break;
	default :
		t = G_TYPE_NONE;
	}

	if (schema != NULL)
		gconf_schema_free (schema);
	return t;
}

char *
go_conf_get_value_as_str (char const *key)
{
	char *value_string;
	GConfClient *gconf = gnm_app_get_gconf_client ();

	switch (go_conf_get_type (key)) {
	case G_TYPE_STRING:
		value_string = gconf_client_get_string (gconf, key, NULL);

		break;
	case G_TYPE_INT:
		value_string = g_strdup_printf ("%i", gconf_client_get_int (gconf, key,
									    NULL));
		break;
	case G_TYPE_FLOAT:
		value_string = g_strdup_printf ("%f", gconf_client_get_float (gconf, key,
									    NULL));
		break;
	case G_TYPE_BOOLEAN:
		value_string = g_strdup (format_boolean (gconf_client_get_bool (gconf, key, NULL)));
		break;
	default:
		value_string = g_strdup ("ERROR FIXME");
	}

	return value_string;
}

int
go_conf_get_bool (char const *key)
{
	GConfClient *gconf = gnm_app_get_gconf_client ();
	return gconf_client_get_bool (gconf, key, NULL);
}

int
go_conf_get_int	(char const *key)
{
	GConfClient *gconf = gnm_app_get_gconf_client ();
	return gconf_client_get_int (gconf, key, NULL);
}

double
go_conf_get_double (char const *key)
{
	GConfClient *gconf = gnm_app_get_gconf_client ();
	return gconf_client_get_float (gconf, key, NULL);
}


gboolean
go_conf_set_value_from_str (char const *key, char const *val_str)
{
	GConfClient *client = gnm_app_get_gconf_client ();

	switch (go_conf_get_type (key)) {
	case G_TYPE_STRING:
		go_conf_set_string (key, val_str);
		break;
	case G_TYPE_FLOAT: {
		GnmDateConventions const *conv = NULL;  /* workbook_date_conv (state->wb); */
		GnmValue *value = format_match_number (val_str, NULL, conv);
		if (value != NULL) {
			gnm_float the_float = value_get_as_float (value);
			gconf_client_set_float (client, key, the_float, NULL);
		}
		if (value)
			value_release (value);
		break;
	}
	case G_TYPE_INT: {
		GnmDateConventions const *conv = NULL;  /* workbook_date_conv (state->wb); */
		GnmValue *value = format_match_number (val_str, NULL, conv);
		if (value != NULL) {
			int the_int = value_get_as_int (value);
			go_conf_set_int (key, the_int);
		}
		if (value)
			value_release (value);
		break;
	}
	case G_TYPE_BOOLEAN: {
		GnmDateConventions const *conv = NULL;  /* workbook_date_conv (state->wb); */
		GnmValue *value = format_match_number (val_str, NULL, conv);
		gboolean err, the_bool;
		if (value != NULL) {
			err = FALSE;
			the_bool =  value_get_as_bool (value, &err);
			gconf_client_set_bool (client, key, the_bool, NULL);
		}
		if (value)
			value_release (value);
		break;
	}
	default:
		g_warning ("Unsupported gconf type in preference dialog");
	}

	return TRUE;
}

void
go_conf_remove_monitor (guint monitor_id)
{
	gconf_client_notify_remove (gnm_app_get_gconf_client (),
		GPOINTER_TO_INT (monitor_id));
}

typedef struct {
	void (*monitor) (char const *key, gpointer data);
	gpointer data;
} GOConfClosure;
static void
cb_key_changed (GConfClient *client, guint cnxn_id,
		GConfEntry *entry, GOConfClosure *close)
{
	close->monitor (gconf_entry_get_key (entry), close->data);
}
guint
go_conf_add_monitor (char const *key,
		     GOConfMonitorFunc monitor, gpointer data)
{
	GOConfClosure *close = g_new0 (GOConfClosure, 1);
	close->monitor = monitor;
	close->data = data;
	return gconf_client_notify_add (gnm_app_get_gconf_client (), key,
		(GConfClientNotifyFunc) cb_key_changed, close, g_free, NULL);
}
#else
void     
go_conf_set_bool (G_GNUC_UNUSED char const *key, G_GNUC_UNUSED gboolean val)
{
}
void     
go_conf_set_int (G_GNUC_UNUSED char const *key, G_GNUC_UNUSED gint val)
{
}
void     
go_conf_set_double (G_GNUC_UNUSED char const *key, G_GNUC_UNUSED gnm_float val)
{
}
void     
go_conf_set_string (G_GNUC_UNUSED char const *key, G_GNUC_UNUSED char const *str)
{
}
void
go_conf_set_str_list (G_GNUC_UNUSED char const *key, G_GNUC_UNUSED GSList *list)
{
}
gboolean
go_conf_get_bool (char const *key)
{
	return FALSE;
}

int
go_conf_get_int	(char const *key)
{
	return 0;
}

double
go_conf_get_double (char const *key)
{
	return 0.;
}

char *
go_conf_get_string (char const *key)
{
	return g_strdup ("");
}

GSList *
go_conf_get_str_list (char const *key)
{
	return NULL;
}

gboolean
go_conf_load_bool (G_GNUC_UNUSED char const *key,
		   gboolean default_val)
{
	return default_val;
}
int
go_conf_load_int (G_GNUC_UNUSED char const *key,
		  G_GNUC_UNUSED int minima, G_GNUC_UNUSED int maxima,
		  int default_val)
{
	return default_val;
}

double
go_conf_load_double (G_GNUC_UNUSED char const *key,
		     G_GNUC_UNUSED double minima, G_GNUC_UNUSED double maxima,
		     double default_val)
{
	return default_val;
}
char *
go_conf_load_string (G_GNUC_UNUSED char const *key)
{
	return NULL;
}
GSList *
go_conf_load_str_list (G_GNUC_UNUSED char const *key)
{
	return NULL;
}
char *
go_conf_get_short_desc (char const *key)
{
	return NULL;
}
char *
go_conf_get_long_desc  (char const *key)
{
	return NULL;
}

GType
go_conf_get_type (char const *key)
{
	return G_TYPE_NONE;
}

char *
go_conf_get_value_as_str (char const *key)
{
	return g_strdup ("");
}

gboolean
go_conf_set_value_from_str (char const *key, char const *val_str)
{
	return TRUE;
}

void
go_conf_sync (void)
{
}

void
go_conf_remove_monitor (guint monitor_id)
{
}

guint
go_conf_add_monitor (char const *key,
		     GOConfMonitorFunc monitor, gpointer data)
{
	return 1;
}

#endif

static void
gnm_conf_init_printer_decoration_font (void)
{
	gchar *name;
	if (prefs.printer_decoration_font == NULL)
		prefs.printer_decoration_font = mstyle_new ();

	name = go_conf_load_string (PRINTSETUP_GCONF_HF_FONT_NAME);
	if (name) {
		mstyle_set_font_name (prefs.printer_decoration_font, name);
		g_free (name);
	} else
		mstyle_set_font_name (prefs.printer_decoration_font, DEFAULT_FONT);
	mstyle_set_font_size (prefs.printer_decoration_font,
		go_conf_load_double (PRINTSETUP_GCONF_HF_FONT_SIZE, 1., 100., DEFAULT_SIZE));
	mstyle_set_font_bold (prefs.printer_decoration_font,
		go_conf_load_bool (PRINTSETUP_GCONF_HF_FONT_BOLD, FALSE));
	mstyle_set_font_italic (prefs.printer_decoration_font,
		go_conf_load_bool (PRINTSETUP_GCONF_HF_FONT_ITALIC, FALSE));
}

static void
gnm_conf_init_essential (void)
{
	prefs.default_font.name = go_conf_load_string (CONF_DEFAULT_FONT_NAME);
	if (prefs.default_font.name == NULL)
		prefs.default_font.name = g_strdup (DEFAULT_FONT);
	prefs.default_font.size = go_conf_load_double (
		CONF_DEFAULT_FONT_SIZE, 1., 100., DEFAULT_SIZE);
	prefs.default_font.is_bold = go_conf_load_bool (
		CONF_DEFAULT_FONT_BOLD, FALSE);
	prefs.default_font.is_italic = go_conf_load_bool (
		CONF_DEFAULT_FONT_ITALIC, FALSE);

	prefs.file_history_max = go_conf_load_int (
		GNM_CONF_FILE_HISTORY_N, 0, 20, 4);
	prefs.file_history_files = go_conf_load_str_list (GNM_CONF_FILE_HISTORY_FILES);
	prefs.plugin_file_states = go_conf_load_str_list (PLUGIN_GCONF_FILE_STATES);
	prefs.plugin_extra_dirs = go_conf_load_str_list (PLUGIN_GCONF_EXTRA_DIRS);
	prefs.active_plugins = go_conf_load_str_list (PLUGIN_GCONF_ACTIVE);
	prefs.activate_new_plugins = go_conf_load_bool (
		PLUGIN_GCONF_ACTIVATE_NEW, TRUE);

	// printf( "prefs.activate_new_plugins: %d\n", prefs.activate_new_plugins );

	prefs.horizontal_dpi = go_conf_load_double (
		GNM_CONF_GUI_RES_H, 10., 1000., 96.);
	prefs.vertical_dpi = go_conf_load_double (
		GNM_CONF_GUI_RES_V, 10., 1000., 96.);
	prefs.initial_sheet_number = go_conf_load_int (
		GNM_CONF_WORKBOOK_NSHEETS, 1, 64, 3);
	prefs.horizontal_window_fraction = go_conf_load_double (
		  GNM_CONF_GUI_WINDOW_X, .1, 1., .6);
	prefs.vertical_window_fraction = go_conf_load_double (
		  GNM_CONF_GUI_WINDOW_Y, .1, 1., .6);
	prefs.zoom = go_conf_load_double (
		  GNM_CONF_GUI_ZOOM, .1, 5., 1.);

	/* Unfortunately we need the printing stuff in essentials since the */
	/* first pi is created for the new sheet before the idle loop has a */
	/* chance to run                                                    */
	prefs.printer_config = go_conf_load_string (PRINTSETUP_GCONF_PRINTER_CONFIG);
	prefs.print_center_horizontally = go_conf_load_bool 
		(PRINTSETUP_GCONF_CENTER_HORIZONTALLY, FALSE); 
	prefs.print_center_vertically = go_conf_load_bool 
		(PRINTSETUP_GCONF_CENTER_VERTICALLY, FALSE);
	prefs.print_grid_lines = go_conf_load_bool 
		(PRINTSETUP_GCONF_PRINT_GRID_LINES, FALSE);
	prefs.print_even_if_only_styles = go_conf_load_bool 
		(PRINTSETUP_GCONF_EVEN_IF_ONLY_STYLES, FALSE);
	prefs.print_black_and_white = go_conf_load_bool 
		(PRINTSETUP_GCONF_PRINT_BLACK_AND_WHITE, FALSE);
	prefs.print_titles = go_conf_load_bool 
		(PRINTSETUP_GCONF_PRINT_TITLES, FALSE);
	prefs.print_order_right_then_down = go_conf_load_bool 
		(PRINTSETUP_GCONF_RIGHT_THEN_DOWN, FALSE);
	prefs.print_scale_percentage = go_conf_load_bool 
		(PRINTSETUP_GCONF_SCALE_PERCENTAGE, TRUE);
	prefs.print_scale_percentage_value = go_conf_load_double 
		(PRINTSETUP_GCONF_SCALE_PERCENTAGE_VALUE, 1, 500, 100);
	prefs.print_scale_width = go_conf_load_int 
		(PRINTSETUP_GCONF_SCALE_WIDTH, 0, 100, 1);
        prefs.print_scale_height = go_conf_load_int 
		(PRINTSETUP_GCONF_SCALE_HEIGHT, 0, 100, 1);
	prefs.print_repeat_top = go_conf_load_string (PRINTSETUP_GCONF_REPEAT_TOP);
	prefs.print_repeat_left = go_conf_load_string (PRINTSETUP_GCONF_REPEAT_LEFT);
#if 0
	prefs.print_tb_margins.top.points = go_conf_load_double 
		(PRINTSETUP_GCONF_MARGIN_TOP, 0.0, 10000.0, 120.0);
	prefs.print_tb_margins.bottom.points = go_conf_load_double 
		(PRINTSETUP_GCONF_MARGIN_BOTTOM, 0.0, 10000.0, 120.0);
	{
		/* Note: the desired display unit is stored in the  */
		/* printer config. So we are never using this field */
		/* inside the margin structure, but only setting it */
		/* in various input routines.                       */
		prefs.print_tb_margins.top.desired_display 
			= gnome_print_unit_get_by_abbreviation ("cm");
		prefs.print_tb_margins.bottom.desired_display 
			= prefs.print_tb_margins.top.desired_display;
	}
#endif // 0
	prefs.print_all_sheets = go_conf_load_bool (
		PRINTSETUP_GCONF_ALL_SHEETS, TRUE);
	prefs.printer_header = go_conf_load_str_list (PRINTSETUP_GCONF_HEADER);
	prefs.printer_footer = go_conf_load_str_list (PRINTSETUP_GCONF_FOOTER);
	prefs.printer_header_formats_left = go_conf_load_str_list (PRINTSETUP_GCONF_HEADER_FORMAT_LEFT);
	prefs.printer_header_formats_middle = go_conf_load_str_list (PRINTSETUP_GCONF_HEADER_FORMAT_MIDDLE);
	prefs.printer_header_formats_right = go_conf_load_str_list (PRINTSETUP_GCONF_HEADER_FORMAT_RIGHT);

	prefs.auto_complete = go_conf_load_bool (GNM_CONF_GUI_ED_AUTOCOMPLETE, TRUE);
	prefs.live_scrolling = go_conf_load_bool (GNM_CONF_GUI_ED_LIVESCROLLING, TRUE);
}

static gboolean
gnm_conf_init_extras (void)
{
	char *tmp;

	prefs.num_of_recent_funcs = go_conf_load_int (
		FUNCTION_SELECT_GCONF_NUM_OF_RECENT, 0, 40, 10);
	prefs.recent_funcs = go_conf_load_str_list (FUNCTION_SELECT_GCONF_RECENT);

	prefs.transition_keys = go_conf_load_bool (
		GNM_CONF_GUI_ED_TRANSITION_KEYS, FALSE);
	prefs.recalc_lag = go_conf_load_int (
		GNM_CONF_GUI_ED_RECALC_LAG, -5000, 5000, 200);
	prefs.show_sheet_name = go_conf_load_bool (
		GNM_CONF_UNDO_SHOW_SHEET_NAME, TRUE);
	prefs.max_descriptor_width = go_conf_load_int (
		GNM_CONF_UNDO_MAX_DESCRIPTOR_WIDTH, 5, 256, 15);
	prefs.undo_size = go_conf_load_int (
		GNM_CONF_UNDO_SIZE, 1, 1000000, 100000);
	prefs.undo_max_number = go_conf_load_int (
		GNM_CONF_UNDO_MAXNUM, 0, 10000, 100);

	prefs.autoformat.extra_dirs = go_conf_load_str_list (AUTOFORMAT_GCONF_EXTRA_DIRS);
	tmp = go_conf_load_string (AUTOFORMAT_GCONF_SYS_DIR);
	if (tmp == NULL)
		tmp = g_strdup ("autoformat-templates");
	prefs.autoformat.sys_dir = gnm_sys_data_dir (tmp);
	g_free (tmp);
	tmp = go_conf_load_string (AUTOFORMAT_GCONF_USR_DIR);
	if (tmp == NULL)
		tmp = g_strdup ("autoformat-templates");
	prefs.autoformat.usr_dir = gnm_usr_dir (tmp);
	g_free (tmp);

	prefs.xml_compression_level = go_conf_load_int (
		GNM_CONF_XML_COMPRESSION, 0, 9, 9);
	prefs.file_overwrite_default_answer = go_conf_load_bool (
		GNM_CONF_FILE_OVERWRITE_DEFAULT, FALSE);
	prefs.file_ask_single_sheet_save = go_conf_load_bool (
		GNM_CONF_FILE_SINGLE_SHEET_SAVE, TRUE);
	prefs.sort_default_by_case = go_conf_load_bool (
		GNM_CONF_SORT_DEFAULT_BY_CASE, FALSE);
	prefs.sort_default_retain_formats = go_conf_load_bool (
		GNM_CONF_SORT_DEFAULT_RETAIN_FORM, TRUE);
	prefs.sort_default_ascending = go_conf_load_bool (
		GNM_CONF_SORT_DEFAULT_ASCENDING, TRUE);
	prefs.sort_max_initial_clauses = go_conf_load_int (
		GNM_CONF_SORT_DIALOG_MAX_INITIAL, 0, 256, 10);
	prefs.unfocused_range_selection = go_conf_load_bool (
		DIALOGS_GCONF_UNFOCUSED_RS, TRUE);
	prefs.prefer_clipboard_selection = go_conf_load_bool (
		GNM_CONF_CUTANDPASTE_PREFER_CLIPBOARD, TRUE);
	prefs.latex_use_utf8 = go_conf_load_bool (
		PLUGIN_GCONF_LATEX_USE_UTF8, TRUE); 

	gnm_conf_init_printer_decoration_font ();

	return FALSE;
}

/**
 * gnm_conf_init
 *
 * @fast : Load non-essential prefs in an idle handler
 **/
void
gnm_conf_init (gboolean fast)
{
	gnm_conf_init_essential ();
	if (fast)
		g_timeout_add (1000, (GSourceFunc) gnm_conf_init_extras, NULL);
	else
		gnm_conf_init_extras ();
}

void     
gnm_conf_shutdown (void)
{
	mstyle_unref (prefs.printer_decoration_font);
	prefs.printer_decoration_font = NULL;
#ifdef WITH_GNOME
	if (gconf_client) {
		gconf_client_remove_dir (gconf_client, "/apps/gnumeric", NULL);
		g_object_unref (G_OBJECT (gconf_client));
		gconf_client = NULL;
	}
#endif
}

void
gnm_gconf_set_plugin_file_states (GSList *list)
{
	g_return_if_fail (prefs.plugin_file_states != list);

	/* the const_casts are ok, the const in the header is just to keep
	 * people for doing stupid things */
	g_slist_foreach ((GSList *)prefs.plugin_file_states, (GFunc)g_free, NULL);
	g_slist_free ((GSList *)prefs.plugin_file_states);
	prefs.plugin_file_states = list;

	go_conf_set_str_list (PLUGIN_GCONF_FILE_STATES, list);
}

void
gnm_gconf_set_plugin_extra_dirs (GSList *list)
{
	g_return_if_fail (prefs.plugin_extra_dirs != list);

	/* the const_casts are ok, the const in the header is just to keep
	 * people for doing stupid things */
	g_slist_foreach ((GSList *)prefs.plugin_extra_dirs, (GFunc)g_free, NULL);
	g_slist_free ((GSList *)prefs.plugin_extra_dirs);
	prefs.plugin_extra_dirs = list;

	go_conf_set_str_list (PLUGIN_GCONF_EXTRA_DIRS, list);
}

void
gnm_gconf_set_active_plugins (GSList *list)
{
	go_conf_set_str_list (PLUGIN_GCONF_ACTIVE, list);
}

void
gnm_gconf_set_activate_new_plugins (gboolean val)
{
	go_conf_set_bool (PLUGIN_GCONF_ACTIVATE_NEW, val);
}

void
gnm_gconf_set_recent_funcs (GSList *list)
{
	go_conf_set_str_list (FUNCTION_SELECT_GCONF_RECENT, list);

	/* the const_casts are ok, the const in the header is just to keep
	 * people for doing stupid things */
	g_slist_foreach ((GSList *)prefs.recent_funcs, (GFunc)g_free, NULL);
	g_slist_free ((GSList *)prefs.recent_funcs);

	prefs.recent_funcs = list;
}

void
gnm_gconf_set_num_recent_functions (gint val)
{
	if (val < 0)
		val = 0;
	prefs.num_of_recent_funcs = val;
	go_conf_set_int ( FUNCTION_SELECT_GCONF_NUM_OF_RECENT, val);
}

void
gnm_gconf_set_file_history_files (GSList *list)
{
	g_return_if_fail (prefs.file_history_files != list);

	/* the const_casts are ok, the const in the header is just to keep
	 * people for doing stupid things */
	g_slist_foreach ((GSList *)prefs.file_history_files, (GFunc)g_free, NULL);
	g_slist_free ((GSList *)prefs.file_history_files);
	prefs.file_history_files = list;
	go_conf_set_str_list (GNM_CONF_FILE_HISTORY_FILES, list);
}

void
gnm_gconf_set_file_history_number (gint val)
{
	if (val < 0)
		val = 0;
	prefs.file_history_max = val; 
	go_conf_set_int (GNM_CONF_FILE_HISTORY_N, val);
}


void
gnm_gconf_set_undo_size (gint val)
{
	if (val < 1)
		val = 1;
	prefs.undo_size = val; 
	go_conf_set_int (GNM_CONF_UNDO_SIZE, val);
}


void
gnm_gconf_set_undo_max_number (gint val)
{
	if (val < 1)
		val = 1;
	prefs.undo_max_number = val;
	go_conf_set_int (GNM_CONF_UNDO_MAXNUM, val);
}

void
gnm_gconf_set_autoformat_sys_dirs (char const * string)
{
	go_conf_set_string (AUTOFORMAT_GCONF_SYS_DIR, string);
}

void
gnm_gconf_set_autoformat_usr_dirs (char const * string)
{
	go_conf_set_string (AUTOFORMAT_GCONF_USR_DIR, string);
}

void
gnm_gconf_set_all_sheets (gboolean val)
{
	go_conf_set_bool (PRINTSETUP_GCONF_ALL_SHEETS, val);
}

void
gnm_gconf_set_printer_config (gchar *str)
{
	go_conf_set_string  (PRINTSETUP_GCONF_PRINTER_CONFIG, str);
	g_free (prefs.printer_config);
	prefs.printer_config = str;
}

void
gnm_gconf_set_printer_header (gchar const *left, gchar const *middle, 
			      gchar const *right)
{
	GSList *list = NULL;
	list = g_slist_prepend (list, g_strdup (right));
	list = g_slist_prepend (list, g_strdup (middle));
	list = g_slist_prepend (list, g_strdup (left));
	go_conf_set_str_list (PRINTSETUP_GCONF_HEADER, list);
	gnm_slist_free_custom ((GSList *)prefs.printer_header, g_free);
	prefs.printer_header = list;
}

void
gnm_gconf_set_printer_footer (gchar const *left, gchar const *middle, 
			      gchar const *right)
{
	GSList *list = NULL;
	list = g_slist_prepend (list, g_strdup (right));
	list = g_slist_prepend (list, g_strdup (middle));
	list = g_slist_prepend (list, g_strdup (left));
	go_conf_set_str_list (PRINTSETUP_GCONF_FOOTER, list);
	gnm_slist_free_custom ((GSList *)prefs.printer_footer, g_free);
	prefs.printer_footer = list;
}

void     
gnm_gconf_set_print_center_horizontally (gboolean val)
{
	go_conf_set_bool (PRINTSETUP_GCONF_CENTER_HORIZONTALLY, val);
}

void     
gnm_gconf_set_print_center_vertically (gboolean val)
{
	go_conf_set_bool (PRINTSETUP_GCONF_CENTER_VERTICALLY, val);
}

void     
gnm_gconf_set_print_grid_lines (gboolean val)
{
	go_conf_set_bool (PRINTSETUP_GCONF_PRINT_GRID_LINES, val);
}

void     
gnm_gconf_set_print_even_if_only_styles (gboolean val)
{
	go_conf_set_bool (PRINTSETUP_GCONF_EVEN_IF_ONLY_STYLES, val);
}

void     
gnm_gconf_set_print_black_and_white (gboolean val)
{
	go_conf_set_bool (PRINTSETUP_GCONF_PRINT_BLACK_AND_WHITE, val);
}

void     
gnm_gconf_set_print_titles (gboolean val)
{
	go_conf_set_bool (PRINTSETUP_GCONF_PRINT_TITLES, val);
}

void     
gnm_gconf_set_print_order_right_then_down (gboolean val)
{
	go_conf_set_bool (PRINTSETUP_GCONF_RIGHT_THEN_DOWN, val);
}

void     
gnm_gconf_set_print_scale_percentage (gboolean val)
{
	go_conf_set_bool (PRINTSETUP_GCONF_SCALE_PERCENTAGE, val);
}

void     
gnm_gconf_set_print_scale_percentage_value (gnm_float val)
{
	go_conf_set_double (PRINTSETUP_GCONF_SCALE_PERCENTAGE_VALUE, val);
}

#if 0
void     
gnm_gconf_set_print_tb_margins (PrintMargins const *pm)
{
	/* We are not saving the GnomePrintUnits since they are */
	/* duplicated in the gnomeprintconfig                   */
	go_conf_set_double (PRINTSETUP_GCONF_MARGIN_TOP, pm->top.points);
	go_conf_set_double (PRINTSETUP_GCONF_MARGIN_BOTTOM, pm->bottom.points);
}
#endif // 0

void     
gnm_gconf_set_print_header_formats (GSList *left, GSList *middle, 
				    GSList *right)
{
	go_conf_set_str_list (PRINTSETUP_GCONF_HEADER_FORMAT_LEFT, left);
	gnm_slist_free_custom (left, g_free);
	go_conf_set_str_list (PRINTSETUP_GCONF_HEADER_FORMAT_MIDDLE, middle);
	gnm_slist_free_custom (middle, g_free);
	go_conf_set_str_list (PRINTSETUP_GCONF_HEADER_FORMAT_RIGHT, right);
	gnm_slist_free_custom (right, g_free);
}

void     
gnm_gconf_set_gui_window_x (gnm_float val)
{
	prefs.horizontal_window_fraction = val;
	go_conf_set_double (GNM_CONF_GUI_WINDOW_X, val);
}

void     
gnm_gconf_set_gui_window_y (gnm_float val)
{
	prefs.vertical_window_fraction = val;
	go_conf_set_double (GNM_CONF_GUI_WINDOW_Y, val);
}

void     
gnm_gconf_set_gui_zoom (gnm_float val)
{
	prefs.zoom = val;
	go_conf_set_double (GNM_CONF_GUI_WINDOW_Y, val);
}

void     
gnm_gconf_set_default_font_size (gnm_float val)
{
	prefs.default_font.size = val;
	go_conf_set_double (GNM_CONF_FONT_SIZE, val);
}

void     
gnm_gconf_set_default_font_name (char const *str)
{
	g_return_if_fail (str != NULL);

	/* the const_casts are ok, the const in the header is just to keep
	 * people for doing stupid things */
	if (prefs.default_font.name != NULL)
		g_free ((char *) prefs.default_font.name);
	prefs.default_font.name = g_strdup (str);
	go_conf_set_string (GNM_CONF_FONT_NAME, str);
}

void     
gnm_gconf_set_default_font_bold (gboolean val)
{
	prefs.default_font.is_bold = val;
	go_conf_set_bool (GNM_CONF_FONT_BOLD, val);
}

void     
gnm_gconf_set_default_font_italic (gboolean val)
{
	prefs.default_font.is_italic = val;
	go_conf_set_bool (GNM_CONF_FONT_ITALIC, val);
}

void
gnm_gconf_set_hf_font (GnmStyle const *mstyle)
{
	GnmStyle *old_style = (prefs.printer_decoration_font != NULL) ?
		prefs.printer_decoration_font :
		mstyle_new_default ();
	
	prefs.printer_decoration_font = mstyle_copy_merge (old_style, mstyle);
	mstyle_unref (old_style);
	
	if (mstyle_is_element_set (mstyle, MSTYLE_FONT_SIZE))
		go_conf_set_double (PRINTSETUP_GCONF_HF_FONT_SIZE,
			mstyle_get_font_size (mstyle));
	if (mstyle_is_element_set (mstyle, MSTYLE_FONT_NAME))
		go_conf_set_string (PRINTSETUP_GCONF_HF_FONT_NAME,
			mstyle_get_font_name (mstyle));
	if (mstyle_is_element_set (mstyle, MSTYLE_FONT_BOLD))
		go_conf_set_bool (PRINTSETUP_GCONF_HF_FONT_BOLD,
			mstyle_get_font_bold (mstyle));
	if (mstyle_is_element_set (mstyle, MSTYLE_FONT_ITALIC))
		go_conf_set_bool (PRINTSETUP_GCONF_HF_FONT_ITALIC,
			mstyle_get_font_italic (mstyle));
}


void
gnm_gconf_set_max_descriptor_width (gint val)
{
	if (val < 1)
		val = 1;
	prefs.max_descriptor_width = val;
	go_conf_set_int (GNM_CONF_UNDO_MAX_DESCRIPTOR_WIDTH, val);
}

void
gnm_gconf_set_sort_dialog_max_initial (gint val)
{
	if (val < 1)
		val = 1;
	prefs.sort_max_initial_clauses = val;
	go_conf_set_int (GNM_CONF_SORT_DIALOG_MAX_INITIAL, val);
}

void
gnm_gconf_set_workbook_nsheets (gint val)
{
	if (val < 1)
		val = 1;
	prefs.initial_sheet_number = val;
	go_conf_set_int (GNM_CONF_WORKBOOK_NSHEETS, val);
}

void
gnm_gconf_set_xml_compression (gint val)
{
	if (val < 0)
		val = 0;
	prefs.xml_compression_level = val;
	go_conf_set_int (GNM_CONF_XML_COMPRESSION, val);
}

void     
gnm_gconf_set_show_sheet_name (gboolean val)
{
	prefs.show_sheet_name = val;
	go_conf_set_bool( GNM_CONF_UNDO_SHOW_SHEET_NAME, 
			  val != FALSE);
}

void     
gnm_gconf_set_latex_use_utf8 (gboolean val)
{
	prefs.latex_use_utf8 = val;
	go_conf_set_bool( PLUGIN_GCONF_LATEX_USE_UTF8, 
			  val != FALSE);
}

void     
gnm_gconf_set_sort_retain_form (gboolean val)
{
	prefs.sort_default_retain_formats = val;
	go_conf_set_bool( GNM_CONF_SORT_DEFAULT_RETAIN_FORM, 
			  val != FALSE);
}

void     
gnm_gconf_set_sort_by_case (gboolean val)
{
	prefs.sort_default_by_case = val;
	go_conf_set_bool( GNM_CONF_SORT_DEFAULT_BY_CASE, 
			  val != FALSE);
}

void     
gnm_gconf_set_sort_ascending (gboolean val)
{
	prefs.sort_default_ascending = val;
	go_conf_set_bool( GNM_CONF_SORT_DEFAULT_ASCENDING, 
			  val != FALSE);
}

void     
gnm_gconf_set_gui_transition_keys (gboolean val)
{
	prefs.transition_keys = val;
	go_conf_set_bool( GNM_CONF_GUI_ED_TRANSITION_KEYS, 
			  val != FALSE);
}

void     
gnm_gconf_set_gui_livescrolling (gboolean val)
{
	prefs.live_scrolling = val;
	go_conf_set_bool( GNM_CONF_GUI_ED_LIVESCROLLING, 
			  val != FALSE);
}

void     
gnm_gconf_set_file_overwrite (gboolean val)
{
	prefs.file_overwrite_default_answer = val;
	go_conf_set_bool( GNM_CONF_FILE_OVERWRITE_DEFAULT, 
			  val != FALSE);
}

void     
gnm_gconf_set_file_single_sheet_save (gboolean val)
{
	prefs.file_ask_single_sheet_save = val;
	go_conf_set_bool( GNM_CONF_FILE_SINGLE_SHEET_SAVE, 
			  val != FALSE);
}

void    
gnm_gconf_set_gui_resolution_h (gnm_float val)
{
	if (val < 50)
		val = 50;
	if (val > 250)
		val = 250;
	prefs.horizontal_dpi = val;
	go_conf_set_double (GNM_CONF_GUI_RES_H, val);
}

void     
gnm_gconf_set_gui_resolution_v (gnm_float val)
{
	if (val < 50)
		val = 50;
	if (val > 250)
		val = 250;
	prefs.vertical_dpi = val;
	go_conf_set_double (GNM_CONF_GUI_RES_V, val);
}

void     
gnm_gconf_set_unfocused_rs (gboolean val)
{
	prefs.unfocused_range_selection = val;
	go_conf_set_bool( DIALOGS_GCONF_UNFOCUSED_RS, 
			  val != FALSE);
}

void     
gnm_gconf_set_autocomplete (gboolean val)
{
	prefs.auto_complete = val;
	go_conf_set_bool( GNM_CONF_GUI_ED_AUTOCOMPLETE, 
			  val != FALSE);
}

void     
gnm_gconf_set_prefer_clipboard  (gboolean val)
{
	prefs.prefer_clipboard_selection = val;
	go_conf_set_bool( GNM_CONF_CUTANDPASTE_PREFER_CLIPBOARD, 
			  val != FALSE);
}


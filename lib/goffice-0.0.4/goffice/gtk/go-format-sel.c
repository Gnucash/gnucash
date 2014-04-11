/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * go-format-sel.c: A widget to select a format
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

#include <goffice/goffice-config.h>
#include "go-format-sel.h"

#include <goffice/gtk/goffice-gtk.h>
#include <goffice/gtk/go-combo-text.h>
#include <goffice/utils/format-impl.h>
#include <goffice/utils/go-color.h>
#include <goffice/utils/go-marshalers.h>

#include <gtk/gtkcellrenderertext.h>
#include <gtk/gtkhbox.h>
#include <gtk/gtklabel.h>
#include <gtk/gtkliststore.h>
#include <gtk/gtksizegroup.h>
#include <gtk/gtkspinbutton.h>
#include <gtk/gtktextview.h>
#include <gtk/gtktogglebutton.h>
#include <gtk/gtktreeselection.h>
#include <gtk/gtktreeview.h>
#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n.h>

#include <string.h>
#include <locale.h>

/* The maximum number of chars in the formatting sample */
#define FORMAT_PREVIEW_MAX 25

#define SETUP_LOCALE_SWITCH char *oldlocale = NULL

#define START_LOCALE_SWITCH						\
	do {									\
		if (gfs->locale) {						\
			currency_date_format_shutdown ();			\
			oldlocale = g_strdup (setlocale (LC_ALL, NULL));	\
			go_setlocale (LC_ALL, gfs->locale);		\
			currency_date_format_init ();				\
		}								\
	} while (0)

#define END_LOCALE_SWITCH						\
	do {									\
		if (oldlocale) {						\
			currency_date_format_shutdown ();			\
			go_setlocale (LC_ALL, oldlocale);			\
			g_free (oldlocale);					\
			currency_date_format_init ();				\
		}								\
	} while (0)

#define FMT_CUSTOM ((GOFormatFamily)(GO_FORMAT_SPECIAL + 1))

/*Format Categories*/
static char const *const format_category_names[] = {
	N_("General"),
	N_("Number"),
	N_("Currency"),
	N_("Accounting"),
	N_("Date"),
	N_("Time"),
	N_("Percentage"),
	N_("Fraction"),
	N_("Scientific"),
	N_("Text"),
	N_("Special"),
	N_("Custom"),
	NULL
};

/* The available format widgets */
typedef enum {
	F_GENERAL_EXPLANATION,
	F_NUMBER_EXPLANATION,
	F_CURRENCY_EXPLANATION,
	F_ACCOUNTING_EXPLANATION,
	F_DATE_EXPLANATION,
	F_TIME_EXPLANATION,
	F_PERCENTAGE_EXPLANATION,
	F_FRACTION_EXPLANATION,
	F_SCIENTIFIC_EXPLANATION,
	F_TEXT_EXPLANATION,
	F_SPECIAL_EXPLANATION,
	F_CUSTOM_EXPLANATION,

	F_SEPARATOR,
	F_SYMBOL_LABEL,		F_SYMBOL,
	F_ENTRY,
	F_LIST_LABEL,		F_LIST_SCROLL,		F_LIST,
	F_DECIMAL_SPIN,	
	F_NEGATIVE_LABEL,	F_NEGATIVE_SCROLL,	F_NEGATIVE,
	F_DECIMAL_LABEL,	F_CODE_LABEL,	F_SYMBOL_BOX,
	F_DECIMAL_BOX,	F_CODE_BOX,	F_MAX_WIDGET
} FormatWidget;

struct  _GOFormatSel {
	GtkHBox   box;
	GladeXML *gui;

	gpointer  value;
	char	 *locale;

	gboolean  enable_edit;

	GODateConventions const *date_conv;

	struct {
		GtkTextView	*preview;
		GtkWidget	*preview_box;
		GtkTextBuffer	*preview_buffer;

		GtkWidget	*widget[F_MAX_WIDGET];
		GtkWidget	*menu;
		GtkTreeModel	*menu_model;
		GtkSizeGroup    *size_group;

		struct {
			GtkTreeView 		*view;
			GtkListStore		*model;
			GtkTreeSelection 	*selection;
		} negative_types;

		struct {
			GtkTreeView	 *view;
			GtkListStore	 *model;
			GtkTreeSelection *selection;
		} formats;

		gulong          entry_changed_id;
		GOFormat	*spec;
		gint		current_type;
		int		num_decimals;
		int		negative_format;
		int		currency_index;
		gboolean	use_separator;
	} format;
};

typedef struct {
	GtkHBoxClass parent_class;

	gboolean  (*format_changed)   (GOFormatSel *gfs, char const *fmt);
	char     *(*generate_preview) (GOFormatSel *gfs, char *fmt);
} GOFormatSelClass;

/* Signals we emit */
enum {
	FORMAT_CHANGED,
	GENERATE_PREVIEW,
	LAST_SIGNAL
};

static guint go_format_sel_signals [LAST_SIGNAL] = { 0 };

static void format_entry_set_text (GOFormatSel *gfs, gchar *text);

static void
generate_format (GOFormatSel *gfs)
{
	GOFormatFamily const page = gfs->format.current_type;
	GOFormat *new_format;

	/* 
	 * It is a strange idea not to reuse GOFormatDetails
	 * in this file, so build one.
	 */
	GOFormatDetails format = gfs->format.spec->family_info;
	format.thousands_sep = gfs->format.use_separator;
	format.num_decimals = gfs->format.num_decimals;
	format.negative_fmt = gfs->format.negative_format;
	format.currency_symbol_index = gfs->format.currency_index;

	new_format = go_format_new (page, &format);
	if (new_format) {
		char *tmp = go_format_as_XL (new_format, TRUE);
		format_entry_set_text (gfs, tmp);
		g_free (tmp);
	}

	go_format_unref (new_format);
}

static char *
generate_preview (GOFormatSel *gfs, GOColor *c)
{
	char *res = NULL;
	g_signal_emit (G_OBJECT (gfs),
		       go_format_sel_signals [GENERATE_PREVIEW], 0,
		       c, &res);
	return res;
}

static void
draw_format_preview (GOFormatSel *gfs, gboolean regen_format)
{
	char		*preview;
	GOFormat	*sf = NULL;
	GOColor		 c = 0;
	GdkColor	 gdk_color;

	if (regen_format)
		generate_format (gfs);

	if (NULL == (sf = gfs->format.spec))
		return;

	if (NULL == (preview = generate_preview (gfs, &c)))
		return;

	if (strlen (preview) > FORMAT_PREVIEW_MAX)
		strcpy (&preview[FORMAT_PREVIEW_MAX - 5], " ...");
	gtk_text_buffer_set_text (gfs->format.preview_buffer, preview, -1);
	if (c != 0)
		go_color_to_gdk (c, &gdk_color);
	else
		gdk_color_parse ("black", &gdk_color);
	gtk_widget_modify_text (GTK_WIDGET (gfs->format.preview), 
				GTK_STATE_NORMAL, &gdk_color);
	g_free (preview);
}

static void
fillin_negative_samples (GOFormatSel *gfs)
{
	static char const *const decimals = "098765432109876543210987654321";
	static char const *const formats[4] = {
		"-%s%s3%s210%s%s%s%s",
		"%s%s3%s210%s%s%s%s",
		"(%s%s3%s210%s%s%s%s)",
		"(%s%s3%s210%s%s%s%s)"
	};
	int const n = 30 - gfs->format.num_decimals;

	GOFormatFamily const page = gfs->format.current_type;
	char const *space_b = "", *currency_b;
	char const *space_a = "", *currency_a;
	const char *decimal;
	const char *thousand_sep;
	int i;
	GtkTreeIter  iter;
	GtkTreePath *path;
	gboolean more;
	SETUP_LOCALE_SWITCH;

	g_return_if_fail (page == GO_FORMAT_NUMBER || page == GO_FORMAT_CURRENCY);
	g_return_if_fail (gfs->format.num_decimals <= 30);

	START_LOCALE_SWITCH;

	if (gfs->format.use_separator)
		thousand_sep = format_get_thousand ()->str;
	else
		thousand_sep = "";
	if (gfs->format.num_decimals > 0)
		decimal = format_get_decimal ()->str;
	else
		decimal = "";

	if (page == GO_FORMAT_CURRENCY) {
		currency_b = go_format_currencies[gfs->format.currency_index].symbol;
		/*
		 * FIXME : This should be better hidden.
		 * Ideally the render would do this for us.
		 */
		if (currency_b[0] == '[' && currency_b[1] == '$') {
			char const *end = strchr (currency_b+2, '-');
			if (end == NULL)
				end = strchr (currency_b+2, ']');
			currency_b = g_strndup (currency_b+2, end-currency_b-2);
		} else
			currency_b = g_strdup (currency_b);

		if (go_format_currencies[gfs->format.currency_index].has_space)
			space_b = " ";

		if (!go_format_currencies[gfs->format.currency_index].precedes) {
			currency_a = currency_b;
			currency_b = "";
			space_a = space_b;
			space_b = "";
		} else {
			currency_a = "";
		}
	} else
		currency_a = currency_b = "";

	more = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (gfs->format.negative_types.model), &iter);
	for (i = 0 ; i < 4; i++) {
		char *buf = g_strdup_printf (formats[i],
					     currency_b, space_b, thousand_sep, decimal,
					     decimals + n, space_a, currency_a);
		if (!more)
			gtk_list_store_append (gfs->format.negative_types.model, &iter);
		gtk_list_store_set (gfs->format.negative_types.model, &iter,
				    0, i,
				    1, buf,
				    2, (i % 2) ? "red" : NULL,
				    -1);
		if (more)
			more = gtk_tree_model_iter_next (GTK_TREE_MODEL (gfs->format.negative_types.model),
							 &iter);

		g_free (buf);
	}

	/* If non empty then free the string */
	if (*currency_a)
		g_free ((char*)currency_a);
	if (*currency_b)
		g_free ((char*)currency_b);

	path = gtk_tree_path_new ();
	gtk_tree_path_append_index (path, gfs->format.negative_format);
	gtk_tree_selection_select_path (gfs->format.negative_types.selection, path);
	gtk_tree_path_free (path);

	END_LOCALE_SWITCH;
}

static void
cb_decimals_changed (GtkSpinButton *spin, GOFormatSel *gfs)
{
	GOFormatFamily const page = gfs->format.current_type;

	gfs->format.num_decimals = gtk_spin_button_get_value_as_int (spin);

	if (page == GO_FORMAT_NUMBER || page == GO_FORMAT_CURRENCY)
		fillin_negative_samples (gfs);

	draw_format_preview (gfs, TRUE);
}

static void
cb_separator_toggle (GtkObject *obj, GOFormatSel *gfs)
{
	gfs->format.use_separator =
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (obj));
	fillin_negative_samples (gfs);

	draw_format_preview (gfs, TRUE);
}

static void
fmt_dialog_init_fmt_list (GOFormatSel *gfs, char const *const *formats,
			  GtkTreeIter *select)
{
	GtkTreeIter iter;
	char *fmt;
	char const *cur_fmt = gfs->format.spec->format;

	for (; *formats; formats++) {
		gtk_list_store_append (gfs->format.formats.model, &iter);
		fmt = go_format_str_as_XL (*formats, TRUE);
		gtk_list_store_set (gfs->format.formats.model, &iter,
				    0, fmt, -1);
		g_free (fmt);

		if (!strcmp (*formats, cur_fmt))
			*select = iter;
	}
}

static void
fmt_dialog_enable_widgets (GOFormatSel *gfs, int page)
{
	SETUP_LOCALE_SWITCH;
	static FormatWidget const contents[][12] = {
		/* General */
		{
			F_GENERAL_EXPLANATION,
			F_MAX_WIDGET
		},
		/* Number */
		{
			F_NUMBER_EXPLANATION,
			F_DECIMAL_BOX,
			F_DECIMAL_LABEL,
			F_DECIMAL_SPIN,
			F_SEPARATOR,
			F_NEGATIVE_LABEL,
			F_NEGATIVE_SCROLL,
			F_NEGATIVE,
			F_MAX_WIDGET
		},
		/* Currency */
		{
			F_CURRENCY_EXPLANATION,
			F_DECIMAL_BOX,
			F_DECIMAL_LABEL,
			F_DECIMAL_SPIN,
			F_SEPARATOR,
			F_SYMBOL_BOX,
			F_SYMBOL_LABEL,
			F_SYMBOL,
			F_NEGATIVE_LABEL,
			F_NEGATIVE_SCROLL,
			F_NEGATIVE,
			F_MAX_WIDGET
		},
		/* Accounting */
		{
			F_ACCOUNTING_EXPLANATION,
			F_DECIMAL_BOX,
			F_DECIMAL_LABEL,
			F_DECIMAL_SPIN,
			F_SYMBOL_BOX,
			F_SYMBOL_LABEL,
			F_SYMBOL,
			F_MAX_WIDGET
		},
		/* Date */
		{
			F_DATE_EXPLANATION,
			F_LIST_LABEL,
			F_LIST_SCROLL,
			F_LIST,
			F_MAX_WIDGET
		},
		/* Time */
		{
			F_TIME_EXPLANATION,
			F_LIST_LABEL,
			F_LIST_SCROLL,
			F_LIST,
			F_MAX_WIDGET
		},
		/* Percentage */
		{
			F_PERCENTAGE_EXPLANATION,
			F_DECIMAL_BOX,
			F_DECIMAL_LABEL,
			F_DECIMAL_SPIN,
			F_MAX_WIDGET
		},
		/* Fraction */
		{
			F_FRACTION_EXPLANATION,
			F_LIST_LABEL,
			F_LIST_SCROLL,
			F_LIST,
			F_MAX_WIDGET
		},
		/* Scientific */
		{
			F_SCIENTIFIC_EXPLANATION,
			F_DECIMAL_BOX,
			F_DECIMAL_LABEL,
			F_DECIMAL_SPIN,
			F_MAX_WIDGET
		},
		/* Text */
		{
			F_TEXT_EXPLANATION,
			F_MAX_WIDGET
		},
		/* Special */
		{
			F_SPECIAL_EXPLANATION,
			F_MAX_WIDGET
		},
		/* Custom */
		{
			F_CUSTOM_EXPLANATION,
			F_CODE_BOX,
			F_CODE_LABEL,
			F_ENTRY,
			F_LIST_LABEL,
			F_LIST_SCROLL,
			F_LIST,
			F_MAX_WIDGET
		}
	};

	GOFormatFamily const old_page = gfs->format.current_type;
	int i;
	FormatWidget tmp;

	START_LOCALE_SWITCH;

	/* Hide widgets from old page */
	if (old_page >= 0) {
		int i, j;
		FormatWidget wi, wj;
		for (i = 0; (wi = contents[old_page][i]) != F_MAX_WIDGET ; ++i) {
			for (j = 0; (wj = contents[page][j]) != F_MAX_WIDGET ; ++j)
				if (wi == wj)
					goto stays;
			gtk_widget_hide (gfs->format.widget[wi]);
stays:
			; /* No more */
		}
	}

	/* Set the default format if appropriate */
	if (page == GO_FORMAT_GENERAL ||
	    page == GO_FORMAT_ACCOUNTING ||
	    page == GO_FORMAT_FRACTION ||
	    page == GO_FORMAT_TEXT) {
		int list_elem = 0;
		char *tmp;
		if (page == gfs->format.spec->family)
			list_elem = gfs->format.spec->family_info.list_element;

		tmp = go_format_str_as_XL (go_format_builtins[page][list_elem], TRUE);
		format_entry_set_text (gfs, tmp);
		g_free (tmp);
	}

	gfs->format.current_type = page;
	for (i = 0; (tmp = contents[page][i]) != F_MAX_WIDGET ; ++i) {
		GtkWidget *w = gfs->format.widget[tmp];

		switch (tmp) {
		case F_LIST: {
			int start = 0, end = -1;
			GtkTreeIter select;

			switch (page) {
			default :
				;
			case GO_FORMAT_DATE:
			case GO_FORMAT_TIME:
			case GO_FORMAT_FRACTION:
				start = end = page;
				break;

			case FMT_CUSTOM:
				start = 0; end = 8;
				break;
			}

			select.stamp = 0;
			gtk_list_store_clear (gfs->format.formats.model);
			for (; start <= end ; ++start)
				fmt_dialog_init_fmt_list (gfs,
					go_format_builtins[start], &select);

			/* If this is the custom page and the format has
			* not been found append it */
			/* TODO We should add the list of other custom formats created.
			*      It should be easy.  All that is needed is a way to differentiate
			*      the std formats and the custom formats in the GOFormat hash.
			*/
			if  (page == FMT_CUSTOM && select.stamp == 0) {
				char *tmp = go_format_as_XL (gfs->format.spec, TRUE);
				format_entry_set_text (gfs, tmp);
				g_free (tmp);
			} else if (select.stamp == 0) {
				if (!gtk_tree_model_get_iter_first (
					   GTK_TREE_MODEL (gfs->format.formats.model),
					   &select))
					select.stamp = 0;
			}

			if (select.stamp != 0) {
				GtkTreePath *path = gtk_tree_model_get_path (
					GTK_TREE_MODEL (gfs->format.formats.model),
					&select);
				gtk_tree_selection_select_iter (
					gfs->format.formats.selection,
					&select);
				gtk_tree_view_scroll_to_cell (gfs->format.formats.view,
					path, NULL, FALSE, 0., 0.);
				gtk_tree_path_free (path);
			}
			break;
		}

		case F_NEGATIVE:
			fillin_negative_samples (gfs);
			break;

		case F_DECIMAL_SPIN:
			gtk_spin_button_set_value (GTK_SPIN_BUTTON (gfs->format.widget[F_DECIMAL_SPIN]),
						   gfs->format.num_decimals);
			break;

		case F_SEPARATOR:
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (gfs->format.widget[F_SEPARATOR]),
						      gfs->format.use_separator);
			break;

		default:
			; /* Nothing */
		}

		gtk_widget_show (w);
	}

	draw_format_preview (gfs, TRUE);

	END_LOCALE_SWITCH;
}

/*
 * Callback routine to manage the relationship between the number
 * formating radio buttons and the widgets required for each mode.
 */

static void
cb_format_class_changed (G_GNUC_UNUSED GtkTreeSelection *ignored, 
			 GOFormatSel *gfs)
{
	int selected_item = 0;
	GList *list;
	GtkTreeSelection *selection = gtk_tree_view_get_selection 
		(GTK_TREE_VIEW(gfs->format.menu));

	list = gtk_tree_selection_get_selected_rows 
		(selection, &gfs->format.menu_model);
	if (list) {
		GtkTreePath *path;
		path = list->data;
		selected_item = *(gtk_tree_path_get_indices (path));

		if (selected_item >= 0) {
			fmt_dialog_enable_widgets (gfs, selected_item);
		}
		g_list_foreach (list, (GFunc)gtk_tree_path_free, NULL);
		g_list_free (list);
	}
}

static void
cb_format_entry_changed (GtkEditable *w, GOFormatSel *gfs)
{
	char *fmt;
	if (!gfs->enable_edit)
		return;

	fmt = go_format_str_delocalize (gtk_entry_get_text (GTK_ENTRY (w)));
	if (strcmp (gfs->format.spec->format, fmt)) {
		go_format_unref (gfs->format.spec);
		gfs->format.spec = go_format_new_from_XL (fmt, FALSE);
		g_signal_emit (G_OBJECT (gfs),
			       go_format_sel_signals [FORMAT_CHANGED], 0,
			       fmt);
		draw_format_preview (gfs, FALSE);
	}
	g_free (fmt);
}

/*
 * We only want to emit the number format changed signal once for each
 * format change. When not blocking signals when calling
 * gtk_entry_set_text, one would be emitted for deleting the old text
 * and one for inserting the new. That's why we block the signal and
 * invoke cb_format_entry_changed explicitly.
 */
static void
format_entry_set_text (GOFormatSel *gfs, gchar *text)
{
	GtkEntry *entry = GTK_ENTRY (gfs->format.widget[F_ENTRY]);

	g_signal_handler_block (entry, gfs->format.entry_changed_id);
	gtk_entry_set_text (entry, text);
	g_signal_handler_unblock (entry, gfs->format.entry_changed_id);
	cb_format_entry_changed (GTK_EDITABLE (entry), gfs);
}

static void
cb_format_list_select (GtkTreeSelection *selection, GOFormatSel *gfs)
{
	GtkTreeIter iter;
	gchar *text;

	if (!gtk_tree_selection_get_selected (selection, NULL, &iter))
		return;

	gtk_tree_model_get (GTK_TREE_MODEL (gfs->format.formats.model),
			    &iter, 0, &text, -1);
	format_entry_set_text (gfs, text);
}

static gboolean
cb_format_currency_select (G_GNUC_UNUSED GtkWidget *ct,
			   char * new_text, GOFormatSel *gfs)
{
	int i;

	/* ignore the clear while assigning a new value */
	if (!gfs->enable_edit || new_text == NULL || *new_text == '\0')
		return FALSE;

	for (i = 0; go_format_currencies[i].symbol != NULL ; ++i)
		if (!strcmp (_(go_format_currencies[i].description), new_text)) {
			gfs->format.currency_index = i;
			break;
		}

	if (gfs->format.current_type == 1 || gfs->format.current_type == 2)
		fillin_negative_samples (gfs);
	draw_format_preview (gfs, TRUE);

	return TRUE;
}

static void
cb_format_negative_form_selected (GtkTreeSelection *selection, GOFormatSel *gfs)
{
	GtkTreeIter iter;
	int type;

	if (!gtk_tree_selection_get_selected (selection, NULL, &iter))
		return;

	gtk_tree_model_get (GTK_TREE_MODEL (gfs->format.negative_types.model),
			    &iter, 0, &type, -1);
	gfs->format.negative_format = type;
	draw_format_preview (gfs, TRUE);
}

static gint
funny_currency_order (gconstpointer _a, gconstpointer _b)
{
	char const *a = (char const *)_a;
	char const *b = (char const *)_b;

	/* Keep the special 1 char versions, and both euro forms at the top */
	gboolean a1 = a[0] && (*(g_utf8_next_char (a)) == '\0' ||
			       0x20AC == g_utf8_get_char (a)); /* euro */
	gboolean b1 = b[0] && (*(g_utf8_next_char (b)) == '\0' ||
			       0x20AC == g_utf8_get_char (b)); /* euro */

	if (a1) {
		if (b1) {
			return strcmp (a, b);
		} else {
			return -1;
		}
	} else {
		if (b1) {
			return +1;
		} else {
			return strcmp (a, b);
		}
	}
}

static void
set_format_category (GOFormatSel *gfs, int row)
{
	GtkTreePath *path;
	GtkTreeSelection *selection = gtk_tree_view_get_selection 
		((GTK_TREE_VIEW(gfs->format.menu)));

	path = gtk_tree_path_new_from_indices (row, -1);
	gtk_tree_selection_select_path  (selection, path);
	gtk_tree_path_free (path);
}


static void
set_format_category_menu_from_style (GOFormatSel *gfs)
{
	GOFormatFamily page;

	g_return_if_fail (IS_GO_FORMAT_SEL (gfs));

	/* Attempt to extract general parameters from the current format */
	if ((page = gfs->format.spec->family) < 0)
		page = FMT_CUSTOM; /* Default to custom */

	set_format_category (gfs, page);
	fmt_dialog_enable_widgets (gfs, page);
}

static void
populate_menu (GOFormatSel *gfs)
{
	GtkTreeViewColumn *column;
	GtkTreeSelection  *selection;
	GtkTreeIter iter;
	GtkCellRenderer *renderer;
	char const * const *categories = format_category_names;

	gfs->format.menu_model = GTK_TREE_MODEL (gtk_list_store_new 
						 (1, G_TYPE_STRING));
	gtk_tree_view_set_model (GTK_TREE_VIEW (gfs->format.menu), 
				 gfs->format.menu_model);
	selection = gtk_tree_view_get_selection 
		(GTK_TREE_VIEW(gfs->format.menu));
	gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);

	while (*categories) {
		gtk_list_store_append 
			(GTK_LIST_STORE (gfs->format.menu_model), &iter);
		gtk_list_store_set (GTK_LIST_STORE (gfs->format.menu_model),
				    &iter, 0, _(*categories), -1);
		categories++;
	}

	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("", renderer,
							   "text", 0,
							   NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW(gfs->format.menu), column);

	g_signal_connect (selection,
			  "changed",
			  G_CALLBACK (cb_format_class_changed), gfs);
}


/*
 * static void
 * fmt_dialog_init_format_page (FormatState *state)
 */

static void
nfs_init (GOFormatSel *gfs)
{
	/* The various format widgets */
	static char const *const widget_names[] = {
		"format_general_explanation",
		"format_number_explanation",
		"format_currency_explanation",
		"format_accounting_explanation",
		"format_date_explanation",
		"format_time_explanation",
		"format_percentage_explanation",
		"format_fraction_explanation",
		"format_scientific_explanation",
		"format_text_explanation",
		"format_special_explanation",
		"format_custom_explanation",

		"format_separator",
		"format_symbol_label",
		"format_symbol_select",
		"format_entry",
		"format_list_label",
		"format_list_scroll",
		"format_list",
		"format_number_decimals",
		"format_negatives_label",
		"format_negatives_scroll",
		"format_negatives",
		"format_decimal_label",
		"format_code_label",
		"format_symbol_box",
		"format_decimal_box",
		"format_code_box",
		NULL
	};

	GtkWidget *tmp;
	GtkTreeViewColumn *column;
	GoComboText *combo;
	char const *name;
	int i;
	GOFormatFamily page;

	GtkWidget *toplevel;
	GtkWidget *old_parent;

	gfs->enable_edit = FALSE;
	gfs->locale = NULL;

	gfs->gui = go_libglade_new ("go-format-sel.glade", NULL, NULL, NULL);
	if (gfs->gui == NULL)
		return;

	toplevel = glade_xml_get_widget (gfs->gui, "number_box");
	old_parent = gtk_widget_get_toplevel (toplevel);
	gtk_widget_reparent (toplevel, GTK_WIDGET (gfs));
	gtk_widget_destroy (old_parent);
	gtk_widget_queue_resize (toplevel);

	gfs->format.spec = go_format_general ();
	go_format_ref (gfs->format.spec);

	gfs->format.preview = NULL;

	/* The handlers will set the format family later.  -1 flags that
	 * all widgets are already hidden. */
	gfs->format.current_type = -1;

	/* Even if the format was not recognized it has set intelligent defaults */
	gfs->format.use_separator = gfs->format.spec->family_info.thousands_sep;
	gfs->format.num_decimals = gfs->format.spec->family_info.num_decimals;
	gfs->format.negative_format = gfs->format.spec->family_info.negative_fmt;
	gfs->format.currency_index = gfs->format.spec->family_info.currency_symbol_index;

	gfs->format.preview_box = glade_xml_get_widget (gfs->gui, "preview_box");
	gfs->format.preview = GTK_TEXT_VIEW (glade_xml_get_widget (gfs->gui, "preview"));
	{
		PangoFontMetrics *metrics;
		PangoContext *context;
		GtkWidget *w = GTK_WIDGET (gfs->format.preview);
		gint char_width;

		/* request width in number of chars */
		context = gtk_widget_get_pango_context (w);
		metrics = pango_context_get_metrics (context,
						     gtk_widget_get_style(w)->font_desc,
						     pango_context_get_language (context));
		char_width = pango_font_metrics_get_approximate_char_width (metrics);
		gtk_widget_set_size_request (w, PANGO_PIXELS (char_width) * FORMAT_PREVIEW_MAX, -1);
		pango_font_metrics_unref (metrics);
	}
	gfs->format.preview_buffer = gtk_text_view_get_buffer (gfs->format.preview);

	gfs->format.menu = glade_xml_get_widget (gfs->gui, "format_menu");
	populate_menu (gfs);

	/* Collect all the required format widgets and hide them */
	for (i = 0; (name = widget_names[i]) != NULL; ++i) {
		tmp = glade_xml_get_widget (gfs->gui, name);

		if (tmp == NULL) {
			g_warning ("nfs_init : failed to load widget %s", name);
		}

		g_return_if_fail (tmp != NULL);

		gtk_widget_hide (tmp);
		gfs->format.widget[i] = tmp;
	}

	/* set minimum heights */
	gtk_widget_set_size_request (gfs->format.widget[F_LIST], -1, 100);
	gtk_widget_set_size_request (gfs->format.widget[F_NEGATIVE], -1, 100);

	/* use size group for better widget alignment */
	gfs->format.size_group = gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL);
	gtk_size_group_add_widget (gfs->format.size_group,
				   gfs->format.widget[F_SYMBOL_LABEL]);
	gtk_size_group_add_widget (gfs->format.size_group,
				   gfs->format.widget[F_DECIMAL_LABEL]);

	/* hide preview by default until a value is set */
	gtk_widget_hide (gfs->format.preview_box);

	/* setup the structure of the negative type list */
	gfs->format.negative_types.model = gtk_list_store_new (3,
							       G_TYPE_INT,
							       G_TYPE_STRING,
							       G_TYPE_STRING);
	gfs->format.negative_types.view = GTK_TREE_VIEW (gfs->format.widget[F_NEGATIVE]);
	gtk_tree_view_set_model (gfs->format.negative_types.view,
				 GTK_TREE_MODEL (gfs->format.negative_types.model));
	column = gtk_tree_view_column_new_with_attributes (_("Negative Number Format"),
							   gtk_cell_renderer_text_new (),
							   "text",		1,
							   "foreground",	2,
							   NULL);
	gtk_tree_view_append_column (gfs->format.negative_types.view, column);
	gfs->format.negative_types.selection =
		gtk_tree_view_get_selection (gfs->format.negative_types.view);
	gtk_tree_selection_set_mode (gfs->format.negative_types.selection,
				     GTK_SELECTION_SINGLE);
	g_signal_connect (G_OBJECT (gfs->format.negative_types.selection), "changed",
		G_CALLBACK (cb_format_negative_form_selected), gfs);
	g_signal_connect (G_OBJECT (gfs->format.widget[F_DECIMAL_SPIN]), "value_changed",
		G_CALLBACK (cb_decimals_changed), gfs);
	g_signal_connect (G_OBJECT (gfs->format.widget[F_SEPARATOR]), "toggled",
		G_CALLBACK (cb_separator_toggle), gfs);

	/* setup custom format list */
	gfs->format.formats.model = gtk_list_store_new (1, G_TYPE_STRING);
	gfs->format.formats.view = GTK_TREE_VIEW (gfs->format.widget[F_LIST]);
	gtk_tree_view_set_model (gfs->format.formats.view,
		GTK_TREE_MODEL (gfs->format.formats.model));
	column = gtk_tree_view_column_new_with_attributes (_("Number Formats"),
		gtk_cell_renderer_text_new (),
		"text",		0,
		NULL);
	gtk_tree_view_append_column (gfs->format.formats.view, column);
	gfs->format.formats.selection =
		gtk_tree_view_get_selection (gfs->format.formats.view);
	gtk_tree_selection_set_mode (gfs->format.formats.selection,
		GTK_SELECTION_BROWSE);
	g_signal_connect (G_OBJECT (gfs->format.formats.selection), "changed",
		G_CALLBACK (cb_format_list_select), gfs);

	/* Setup handler Currency & Accounting currency symbols */
	combo = GO_COMBO_TEXT (gfs->format.widget[F_SYMBOL]);
	if (combo != NULL) {
		GList *ptr, *l = NULL;

		for (i = 0; go_format_currencies[i].symbol != NULL ; ++i)
			l = g_list_append (l, _((gchar *)go_format_currencies[i].description));
		l = g_list_sort (l, funny_currency_order);

		for (ptr = l; ptr != NULL ; ptr = ptr->next)
			go_combo_text_add_item	(combo, ptr->data);
		g_list_free (l);
		go_combo_text_set_text (combo,
			_((char const *)go_format_currencies[gfs->format.currency_index].description),
			GO_COMBO_TEXT_FROM_TOP);
		g_signal_connect (G_OBJECT (combo), "entry_changed",
			G_CALLBACK (cb_format_currency_select), gfs);
		gtk_label_set_mnemonic_widget (
			GTK_LABEL (glade_xml_get_widget (gfs->gui, "format_symbol_label")),
			GTK_WIDGET (combo));
	}

	/* Setup special handler for Custom */
	gfs->format.entry_changed_id = g_signal_connect (
		G_OBJECT (gfs->format.widget[F_ENTRY]), "changed",
		G_CALLBACK (cb_format_entry_changed), gfs);

	/* Connect signal for format menu */
	set_format_category_menu_from_style (gfs);

	if ((page = gfs->format.spec->family) < 0)
		page = FMT_CUSTOM; /* Default to custom */
	fmt_dialog_enable_widgets (gfs, page);

	gfs->enable_edit = TRUE;
}

static void
go_format_sel_finalize (GObject *obj)
{
	GOFormatSel *gfs = GO_FORMAT_SEL (obj);

	g_free (gfs->locale);
	gfs->locale = NULL;

	if (gfs->format.spec) {
		go_format_unref (gfs->format.spec);
		gfs->format.spec = NULL;
	}

	if (gfs->format.size_group) {
		g_object_unref (gfs->format.size_group);
		gfs->format.size_group = NULL;
	}

	if (gfs->gui) {
		g_object_unref (G_OBJECT (gfs->gui));
		gfs->gui = NULL;
	}

	G_OBJECT_CLASS (g_type_class_peek (GTK_TYPE_HBOX))->finalize (obj);
}

static gboolean
accumulate_first_string (GSignalInvocationHint *ihint,
			 GValue                *accum_result,
			 const GValue          *handler_result,
			 gpointer               data)
{
	gchar const *str = g_value_get_string (handler_result);
	if (NULL != str) {
		g_value_set_string (accum_result, str);
		return FALSE;
	}
	return TRUE;
}

static void
nfs_class_init (GObjectClass *klass)
{
	klass->finalize = go_format_sel_finalize;

	go_format_sel_signals [FORMAT_CHANGED] =
		g_signal_new ("format_changed",
			      G_OBJECT_CLASS_TYPE (klass),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GOFormatSelClass, format_changed),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__POINTER,
			      G_TYPE_NONE, 1, G_TYPE_POINTER);

	go_format_sel_signals [GENERATE_PREVIEW] =
		g_signal_new ("generate-preview",
			      G_OBJECT_CLASS_TYPE (klass),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (GOFormatSelClass, generate_preview),
			      accumulate_first_string, NULL,
			      go__STRING__POINTER,
			      G_TYPE_STRING, 1, G_TYPE_POINTER);
}

GSF_CLASS (GOFormatSel, go_format_sel,
	   nfs_class_init, nfs_init, GTK_TYPE_HBOX)

GtkWidget *
go_format_sel_new (void)
{
	return g_object_new (GO_FORMAT_SEL_TYPE, NULL);
}

void
go_format_sel_set_focus (GOFormatSel *gfs)
{
	g_return_if_fail (IS_GO_FORMAT_SEL (gfs));

	gtk_widget_grab_focus (GTK_WIDGET (gfs->format.menu));
}

void
go_format_sel_set_style_format (GOFormatSel *gfs,
				GOFormat *style_format)
{
	GoComboText *combo;

	g_return_if_fail (IS_GO_FORMAT_SEL (gfs));
	g_return_if_fail (style_format != NULL);

	go_format_ref (style_format);

	go_format_unref (gfs->format.spec);

	gfs->format.spec = style_format;

	gfs->format.use_separator = style_format->family_info.thousands_sep;
	gfs->format.num_decimals = style_format->family_info.num_decimals;
	gfs->format.negative_format = style_format->family_info.negative_fmt;
	gfs->format.currency_index = style_format->family_info.currency_symbol_index;

	combo = GO_COMBO_TEXT (gfs->format.widget[F_SYMBOL]);
	go_combo_text_set_text
		(combo,
		 _(go_format_currencies[gfs->format.currency_index].description),
		 GO_COMBO_TEXT_FROM_TOP);

	set_format_category_menu_from_style (gfs);
	draw_format_preview (gfs, TRUE);
}

void
go_format_sel_set_dateconv (GOFormatSel *gfs,
			    GODateConventions const *date_conv)
{
	g_return_if_fail (IS_GO_FORMAT_SEL (gfs));
	g_return_if_fail (date_conv != NULL);

	/* FIXME is it safe ? */

	gfs->date_conv = date_conv;

	draw_format_preview (gfs, TRUE);
}

GOFormat *
go_format_sel_get_fmt (GOFormatSel *gfs)
{
	g_return_val_if_fail (IS_GO_FORMAT_SEL (gfs), NULL);
	return gfs->format.spec;
}

GODateConventions const *
go_format_sel_get_dateconv (GOFormatSel *gfs)
{
	g_return_val_if_fail (IS_GO_FORMAT_SEL (gfs), NULL);
	return gfs->date_conv;
}

void
go_format_sel_show_preview (GOFormatSel *gfs)
{
	g_return_if_fail (IS_GO_FORMAT_SEL (gfs));
	gtk_widget_show (gfs->format.preview_box);
	draw_format_preview (gfs, TRUE);
}

void
go_format_sel_hide_preview (GOFormatSel *gfs)
{
	g_return_if_fail (IS_GO_FORMAT_SEL (gfs));
	gtk_widget_hide (gfs->format.preview_box);
}

void
go_format_sel_editable_enters (GOFormatSel *gfs,
			       GtkWindow *window)
{
	g_return_if_fail (IS_GO_FORMAT_SEL (gfs));
	go_editable_enters (window, gfs->format.widget[F_DECIMAL_SPIN]);
	go_editable_enters (window, gfs->format.widget[F_ENTRY]);
}

void		
go_format_sel_set_locale (GOFormatSel *gfs, 
			  char const *locale)
{
	g_free (gfs->locale);
	gfs->locale = g_strdup (locale);

	cb_format_class_changed (NULL, gfs);
}

/* The following utility function should possibly be in format.h but we */
/* access to the array of category names which are better located here. */
char const *    
go_format_sel_format_classification (GOFormat const *style_format)
{
	GOFormatFamily page = style_format->family;

	if (page < 0 || page > FMT_CUSTOM)
		page = FMT_CUSTOM; /* Default to custom */

	return _(format_category_names[page]);
}

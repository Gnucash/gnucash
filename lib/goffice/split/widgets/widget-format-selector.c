/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * widget-number-format-selector.c:  Implements a widget to select number format.
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

#include <config.h>
#include "widget-format-selector.h"

#include <glib/gi18n.h>

#include <format.h>
#include <mstyle.h>
#include <style-color.h>
//#include <sheet.h>
#include <value.h>

#include <goffice/gui-utils/go-combo-text.h>

#include <gtk/gtksizegroup.h>
#include <gtk/gtktreeview.h>
#include <gtk/gtktreeselection.h>
#include <gtk/gtkspinbutton.h>
#include <gtk/gtktogglebutton.h>
#include <gtk/gtkcellrenderertext.h>
#include <gtk/gtkliststore.h>
#include <gtk/gtkhbox.h>
#include <gsf/gsf-impl-utils.h>

#include <string.h>
#include <locale.h>

/* The maximum number of chars in the formatting sample */
#define FORMAT_PREVIEW_MAX 25

#define SETUP_LOCALE_SWITCH char *oldlocale = NULL

#define START_LOCALE_SWITCH						\
  do {									\
	if (nfs->locale) {						\
		currency_date_format_shutdown ();			\
		oldlocale = g_strdup (setlocale (LC_ALL, NULL));	\
		gnm_setlocale (LC_ALL, nfs->locale);		\
		currency_date_format_init ();				\
	}								\
  } while (0)

#define END_LOCALE_SWITCH						\
  do {									\
	if (oldlocale) {						\
		currency_date_format_shutdown ();			\
		gnm_setlocale (LC_ALL, oldlocale);			\
		g_free (oldlocale);					\
		currency_date_format_init ();				\
	}								\
  } while (0)

#define FMT_CUSTOM ((FormatFamily)(FMT_SPECIAL + 1))

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

struct  _NumberFormatSelector {
	GtkHBox   box;
	GladeXML *gui;

	GnmValue *value;
	char	 *locale;

	gboolean  enable_edit;

	GnmDateConventions const *date_conv;

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
		GnmFormat	*spec;
		gint		current_type;
		int		num_decimals;
		int		negative_format;
		int		currency_index;
		gboolean	use_separator;
	} format;
};

typedef struct {
	GtkHBoxClass parent_class;

	gboolean (*number_format_changed) (NumberFormatSelector *nfs, const char *fmt);
} NumberFormatSelectorClass;

static GtkHBoxClass *nfs_parent_class;

/* Signals we emit */
enum {
	NUMBER_FORMAT_CHANGED,
	LAST_SIGNAL
};

static guint nfs_signals[LAST_SIGNAL] = { 0 };

static void format_entry_set_text (NumberFormatSelector *nfs, gchar *text);

static void
generate_format (NumberFormatSelector *nfs)
{
	FormatFamily const page = nfs->format.current_type;
	GnmFormat *new_format;

	/* 
	 * It is a strange idea not to reuse FormatCharacteristics
	 * in this file, so build one.
	 */
	FormatCharacteristics format = nfs->format.spec->family_info;
	format.thousands_sep = nfs->format.use_separator;
	format.num_decimals = nfs->format.num_decimals;
	format.negative_fmt = nfs->format.negative_format;
	format.currency_symbol_index = nfs->format.currency_index;

	new_format = style_format_build (page, &format);
	if (new_format) {
		char *tmp = style_format_as_XL (new_format, TRUE);
		format_entry_set_text (nfs, tmp);
		g_free (tmp);
	}

	style_format_unref (new_format);
}

static void
draw_format_preview (NumberFormatSelector *nfs, gboolean regen_format)
{
	gchar		*preview;
	GnmFormat	*sf = NULL;
	GnmColor	*c = NULL;

	if (regen_format)
		generate_format (nfs);

	/* Nothing to sample. */
	if (nfs->value == NULL)
		return;

	sf = nfs->format.spec;

	if (sf == NULL || nfs->value == NULL)
		return;

	if (style_format_is_general (sf) &&
	    VALUE_FMT (nfs->value) != NULL)
		sf = VALUE_FMT (nfs->value);

	preview = format_value (sf, nfs->value, &c, -1, nfs->date_conv);
	if (strlen (preview) > FORMAT_PREVIEW_MAX)
		strcpy (&preview[FORMAT_PREVIEW_MAX - 5], " ...");

	gtk_text_buffer_set_text (nfs->format.preview_buffer, preview, -1);
	if (c != NULL) {
		gtk_widget_modify_text (GTK_WIDGET(nfs->format.preview), 
					GTK_STATE_NORMAL, &(c->color));
		style_color_unref (c);
	} else {
		GdkColor color;
		gdk_color_parse ("black", &color);
		gtk_widget_modify_text (GTK_WIDGET(nfs->format.preview), 
					GTK_STATE_NORMAL, &color);	
	}

	g_free (preview);
}

static void
fillin_negative_samples (NumberFormatSelector *nfs)
{
	static char const *const decimals = "098765432109876543210987654321";
	static char const *const formats[4] = {
		"-%s%s3%s210%s%s%s%s",
		"%s%s3%s210%s%s%s%s",
		"(%s%s3%s210%s%s%s%s)",
		"(%s%s3%s210%s%s%s%s)"
	};
	int const n = 30 - nfs->format.num_decimals;

	FormatFamily const page = nfs->format.current_type;
	char const *space_b = "", *currency_b;
	char const *space_a = "", *currency_a;
	const char *decimal;
	const char *thousand_sep;
	int i;
	GtkTreeIter  iter;
	GtkTreePath *path;
	gboolean more;
	SETUP_LOCALE_SWITCH;

	g_return_if_fail (page == FMT_NUMBER || page == FMT_CURRENCY);
	g_return_if_fail (nfs->format.num_decimals <= 30);

	START_LOCALE_SWITCH;
		
	if (nfs->format.use_separator)
		thousand_sep = format_get_thousand ()->str;
	else
		thousand_sep = "";
	if (nfs->format.num_decimals > 0)
		decimal = format_get_decimal ()->str;
	else
		decimal = "";

	if (page == FMT_CURRENCY) {
		currency_b = (const gchar *)currency_symbols[nfs->format.currency_index].symbol;
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

		if (currency_symbols[nfs->format.currency_index].has_space)
			space_b = " ";

		if (!currency_symbols[nfs->format.currency_index].precedes) {
			currency_a = currency_b;
			currency_b = "";
			space_a = space_b;
			space_b = "";
		} else {
			currency_a = "";
		}
	} else
		currency_a = currency_b = "";

	more = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (nfs->format.negative_types.model), &iter);
	for (i = 0 ; i < 4; i++) {
		char *buf = g_strdup_printf (formats[i],
					     currency_b, space_b, thousand_sep, decimal,
					     decimals + n, space_a, currency_a);
		if (!more)
			gtk_list_store_append (nfs->format.negative_types.model, &iter);
		gtk_list_store_set (nfs->format.negative_types.model, &iter,
				    0, i,
				    1, buf,
				    2, (i % 2) ? "red" : NULL,
				    -1);
		if (more)
			more = gtk_tree_model_iter_next (GTK_TREE_MODEL (nfs->format.negative_types.model),
							 &iter);

		g_free (buf);
	}

	/* If non empty then free the string */
	if (*currency_a)
		g_free ((char*)currency_a);
	if (*currency_b)
		g_free ((char*)currency_b);

	path = gtk_tree_path_new ();
	gtk_tree_path_append_index (path, nfs->format.negative_format);
	gtk_tree_selection_select_path (nfs->format.negative_types.selection, path);
	gtk_tree_path_free (path);

	END_LOCALE_SWITCH;
}

static void
cb_decimals_changed (GtkSpinButton *spin, NumberFormatSelector *nfs)
{
	FormatFamily const page = nfs->format.current_type;

	nfs->format.num_decimals = gtk_spin_button_get_value_as_int (spin);

	if (page == FMT_NUMBER || page == FMT_CURRENCY)
		fillin_negative_samples (nfs);

	draw_format_preview (nfs, TRUE);
}

static void
cb_separator_toggle (GtkObject *obj, NumberFormatSelector *nfs)
{
	nfs->format.use_separator =
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (obj));
	fillin_negative_samples (nfs);

	draw_format_preview (nfs, TRUE);
}

static void
fmt_dialog_init_fmt_list (NumberFormatSelector *nfs, char const *const *formats,
			  GtkTreeIter *select)
{
	GtkTreeIter iter;
	char *fmt;
	char const *cur_fmt = nfs->format.spec->format;

	for (; *formats; formats++) {
		gtk_list_store_append (nfs->format.formats.model, &iter);
		fmt = style_format_str_as_XL (*formats, TRUE);
		gtk_list_store_set (nfs->format.formats.model, &iter,
			0, fmt, -1);
		g_free (fmt);

		if (!strcmp (*formats, cur_fmt))
			*select = iter;
	}
}

static void
fmt_dialog_enable_widgets (NumberFormatSelector *nfs, int page)
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

	FormatFamily const old_page = nfs->format.current_type;
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
			gtk_widget_hide (nfs->format.widget[wi]);
		stays:
			; /* No more */
		}
	}

	/* Set the default format if appropriate */
	if (page == FMT_GENERAL || page == FMT_ACCOUNT || page == FMT_FRACTION || page == FMT_TEXT) {
		int list_elem = 0;
		char *tmp;
		if (page == nfs->format.spec->family)
			list_elem = nfs->format.spec->family_info.list_element;

		tmp = style_format_str_as_XL (cell_formats[page][list_elem], TRUE);
		format_entry_set_text (nfs, tmp);
		g_free (tmp);
	}

	nfs->format.current_type = page;
	for (i = 0; (tmp = contents[page][i]) != F_MAX_WIDGET ; ++i) {
		GtkWidget *w = nfs->format.widget[tmp];

		switch (tmp) {
		case F_LIST: {
			int start = 0, end = -1;
			GtkTreeIter select;

			switch (page) {
			case FMT_DATE:
			case FMT_TIME:
			case FMT_FRACTION:
				start = end = page;
				break;

			case FMT_CUSTOM:
				start = 0; end = 8;
				break;

			default :
				g_assert_not_reached ();
			};

			select.stamp = 0;
			gtk_list_store_clear (nfs->format.formats.model);
			for (; start <= end ; ++start)
			  fmt_dialog_init_fmt_list (nfs,
						    cell_formats[start], &select);

			/* If this is the custom page and the format has
			 * not been found append it */
			/* TODO We should add the list of other custom formats created.
			 *      It should be easy.  All that is needed is a way to differentiate
			 *      the std formats and the custom formats in the GnmFormat hash.
			 */
			if  (page == FMT_CUSTOM && select.stamp == 0) {
				char *tmp = style_format_as_XL (nfs->format.spec, TRUE);
				format_entry_set_text (nfs, tmp);
				g_free (tmp);
			} else if (select.stamp == 0)
				gtk_tree_model_get_iter_first (
					GTK_TREE_MODEL (nfs->format.formats.model),
					&select);

			if (select.stamp != 0)
				gtk_tree_selection_select_iter (
					nfs->format.formats.selection, &select);

			break;
		}

		case F_NEGATIVE:
			fillin_negative_samples (nfs);
			break;

		case F_DECIMAL_SPIN:
			gtk_spin_button_set_value (GTK_SPIN_BUTTON (nfs->format.widget[F_DECIMAL_SPIN]),
						   nfs->format.num_decimals);
			break;

		case F_SEPARATOR:
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (nfs->format.widget[F_SEPARATOR]),
						      nfs->format.use_separator);
			break;

		default:
			; /* Nothing */
		}

		gtk_widget_show (w);
	}

#if 0
	if ((cl = GTK_CLIST (nfs->format.widget[F_LIST])) != NULL)
		gnumeric_clist_make_selection_visible (cl);
#endif

	draw_format_preview (nfs, TRUE);

	END_LOCALE_SWITCH;
}

/*
 * Callback routine to manage the relationship between the number
 * formating radio buttons and the widgets required for each mode.
 */

static void
cb_format_class_changed (G_GNUC_UNUSED GtkTreeSelection *ignored, 
			 NumberFormatSelector *nfs)
{
	int selected_item = 0;
	GList *list;
	GtkTreeSelection *selection = gtk_tree_view_get_selection 
		(GTK_TREE_VIEW(nfs->format.menu));

	list = gtk_tree_selection_get_selected_rows 
		(selection, &nfs->format.menu_model);
	if (list) {
		GtkTreePath *path;
		path = list->data;
		selected_item = *(gtk_tree_path_get_indices (path));
		
		if (selected_item >= 0) {
			fmt_dialog_enable_widgets (nfs, selected_item);
		}
		g_list_foreach (list, (GFunc)gtk_tree_path_free, NULL);
		g_list_free (list);
	}
}

static void
cb_format_entry_changed (GtkEditable *w, NumberFormatSelector *nfs)
{
	char *fmt;
	if (!nfs->enable_edit)
		return;

	fmt = style_format_delocalize (gtk_entry_get_text (GTK_ENTRY (w)));
	if (strcmp (nfs->format.spec->format, fmt)) {
		style_format_unref (nfs->format.spec);
		nfs->format.spec = style_format_new_XL (fmt, FALSE);
		g_signal_emit (GTK_OBJECT (nfs),
			       nfs_signals[NUMBER_FORMAT_CHANGED], 0,
			       fmt);
		draw_format_preview (nfs, FALSE);
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
format_entry_set_text (NumberFormatSelector *nfs, gchar *text)
{
	GtkEntry *entry = GTK_ENTRY (nfs->format.widget[F_ENTRY]);

	g_signal_handler_block (entry, nfs->format.entry_changed_id);
	gtk_entry_set_text (entry, text);
	g_signal_handler_unblock (entry, nfs->format.entry_changed_id);
	cb_format_entry_changed (GTK_EDITABLE (entry), nfs);
}

static void
cb_format_list_select (GtkTreeSelection *selection, NumberFormatSelector *nfs)
{
	GtkTreeIter iter;
	gchar *text;

	if (!gtk_tree_selection_get_selected (selection, NULL, &iter))
		return;

	gtk_tree_model_get (GTK_TREE_MODEL (nfs->format.formats.model),
		&iter, 0, &text, -1);
	format_entry_set_text (nfs, text);
}

static gboolean
cb_format_currency_select (G_GNUC_UNUSED GtkWidget *ct,
			   char * new_text, NumberFormatSelector *nfs)
{
	int i;

	/* ignore the clear while assigning a new value */
	if (!nfs->enable_edit || new_text == NULL || *new_text == '\0')
		return FALSE;

	for (i = 0; currency_symbols[i].symbol != NULL ; ++i)
		if (!strcmp (_(currency_symbols[i].description), new_text)) {
			nfs->format.currency_index = i;
			break;
		}

	if (nfs->format.current_type == 1 || nfs->format.current_type == 2)
		fillin_negative_samples (nfs);
	draw_format_preview (nfs, TRUE);

	return TRUE;
}

static void
cb_format_negative_form_selected (GtkTreeSelection *selection, NumberFormatSelector *nfs)
{
	GtkTreeIter iter;
	int type;

	if (!gtk_tree_selection_get_selected (selection, NULL, &iter))
		return;

	gtk_tree_model_get (GTK_TREE_MODEL (nfs->format.negative_types.model),
		&iter, 0, &type, -1);
	nfs->format.negative_format = type;
	draw_format_preview (nfs, TRUE);
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
set_format_category (NumberFormatSelector *nfs, int row)
{
	GtkTreePath *path;
	GtkTreeSelection *selection = gtk_tree_view_get_selection 
		((GTK_TREE_VIEW(nfs->format.menu)));

	path = gtk_tree_path_new_from_indices (row, -1);
	gtk_tree_selection_select_path  (selection, path);
	gtk_tree_path_free (path);
}


static void
set_format_category_menu_from_style (NumberFormatSelector *nfs)
{
  	FormatFamily page;

  	g_return_if_fail (IS_NUMBER_FORMAT_SELECTOR (nfs));

	/* Attempt to extract general parameters from the current format */
	if ((page = nfs->format.spec->family) < 0)
		page = FMT_CUSTOM; /* Default to custom */

	set_format_category (nfs, page);
	fmt_dialog_enable_widgets (nfs, page);
}

static void
populate_menu (NumberFormatSelector *nfs)
{
	GtkTreeViewColumn *column;
	GtkTreeSelection  *selection;
	GtkTreeIter iter;
	GtkCellRenderer *renderer;
	char const * const *categories = format_category_names;

	nfs->format.menu_model = GTK_TREE_MODEL (gtk_list_store_new 
						(1, G_TYPE_STRING));
	gtk_tree_view_set_model (GTK_TREE_VIEW (nfs->format.menu), 
				 nfs->format.menu_model);
	selection = gtk_tree_view_get_selection 
		(GTK_TREE_VIEW(nfs->format.menu));
	gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);
	
	while (*categories) {
		gtk_list_store_append 
			(GTK_LIST_STORE (nfs->format.menu_model), &iter);
		gtk_list_store_set (GTK_LIST_STORE (nfs->format.menu_model),
				    &iter, 0, _(*categories), -1);
		categories++;
	}

	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("", renderer,
							   "text", 0,
							   NULL);
	gtk_tree_view_append_column (GTK_TREE_VIEW(nfs->format.menu), column);

	g_signal_connect (selection,
		"changed",
		G_CALLBACK (cb_format_class_changed), nfs);
}


/*
 * static void
 * fmt_dialog_init_format_page (FormatState *state)
 */

static void
nfs_init (NumberFormatSelector *nfs)
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
	FormatFamily page;

	GtkWidget *toplevel;
	GtkWidget *old_parent;

	nfs->enable_edit = FALSE;
	nfs->locale = NULL;

	nfs->gui = gnm_glade_xml_new (NULL, "format-selector.glade", NULL, NULL);
	if (nfs->gui == NULL)
		return;

	toplevel = glade_xml_get_widget (nfs->gui, "number_box");
	old_parent = gtk_widget_get_toplevel (toplevel);
	gtk_widget_reparent (toplevel, GTK_WIDGET (nfs));
	gtk_widget_destroy (old_parent);
	gtk_widget_queue_resize (toplevel);

	nfs->format.spec = style_format_general ();
	style_format_ref (nfs->format.spec);

	nfs->format.preview = NULL;

	/* The handlers will set the format family later.  -1 flags that
	 * all widgets are already hidden. */
	nfs->format.current_type = -1;

	/* Even if the format was not recognized it has set intelligent defaults */
	nfs->format.use_separator = nfs->format.spec->family_info.thousands_sep;
	nfs->format.num_decimals = nfs->format.spec->family_info.num_decimals;
	nfs->format.negative_format = nfs->format.spec->family_info.negative_fmt;
	nfs->format.currency_index = nfs->format.spec->family_info.currency_symbol_index;

	nfs->format.preview_box = glade_xml_get_widget (nfs->gui, "preview_box");
	nfs->format.preview = GTK_TEXT_VIEW (glade_xml_get_widget (nfs->gui, "preview"));
	{
		PangoFontMetrics *metrics;
		PangoContext *context;
		GtkWidget *w = GTK_WIDGET (nfs->format.preview);
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
	nfs->format.preview_buffer = gtk_text_view_get_buffer (nfs->format.preview);

	nfs->format.menu = glade_xml_get_widget (nfs->gui, "format_menu");
	populate_menu (nfs);

	/* Collect all the required format widgets and hide them */
	for (i = 0; (name = widget_names[i]) != NULL; ++i) {
		tmp = glade_xml_get_widget (nfs->gui, name);

		if (tmp == NULL) {
			g_warning ("nfs_init : failed to load widget %s", name);
		}

		g_return_if_fail (tmp != NULL);

		gtk_widget_hide (tmp);
		nfs->format.widget[i] = tmp;
	}

	/* set minimum heights */
	gtk_widget_set_size_request (nfs->format.widget[F_LIST], -1, 100);
	gtk_widget_set_size_request (nfs->format.widget[F_NEGATIVE], -1, 100);

	/* use size group for better widget alignment */
	nfs->format.size_group = gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL);
	gtk_size_group_add_widget (nfs->format.size_group,
				   nfs->format.widget[F_SYMBOL_LABEL]);
	gtk_size_group_add_widget (nfs->format.size_group,
				   nfs->format.widget[F_DECIMAL_LABEL]);

	/* hide preview by default until a value is set */
	gtk_widget_hide (nfs->format.preview_box);

	/* setup the structure of the negative type list */
	nfs->format.negative_types.model = gtk_list_store_new (3,
							       G_TYPE_INT,
							       G_TYPE_STRING,
							       G_TYPE_STRING);
	nfs->format.negative_types.view = GTK_TREE_VIEW (nfs->format.widget[F_NEGATIVE]);
	gtk_tree_view_set_model (nfs->format.negative_types.view,
				 GTK_TREE_MODEL (nfs->format.negative_types.model));
	column = gtk_tree_view_column_new_with_attributes (_("Negative Number Format"),
							   gtk_cell_renderer_text_new (),
							   "text",		1,
							   "foreground",	2,
							   NULL);
	gtk_tree_view_append_column (nfs->format.negative_types.view, column);
	nfs->format.negative_types.selection =
		gtk_tree_view_get_selection (nfs->format.negative_types.view);
	gtk_tree_selection_set_mode (nfs->format.negative_types.selection,
				     GTK_SELECTION_SINGLE);
	g_signal_connect (G_OBJECT (nfs->format.negative_types.selection),
			  "changed",
			  G_CALLBACK (cb_format_negative_form_selected), nfs);

	/* Catch changes to the spin box */
	g_signal_connect (G_OBJECT (nfs->format.widget[F_DECIMAL_SPIN]),
			  "value_changed",
			  G_CALLBACK (cb_decimals_changed), nfs);

	/* Setup special handlers for : Numbers */
	g_signal_connect (G_OBJECT (nfs->format.widget[F_SEPARATOR]),
			  "toggled",
			  G_CALLBACK (cb_separator_toggle), nfs);

	/* setup custom format list */
	nfs->format.formats.model =
		gtk_list_store_new (1, G_TYPE_STRING);
	nfs->format.formats.view =
		GTK_TREE_VIEW (nfs->format.widget[F_LIST]);
	gtk_tree_view_set_model (nfs->format.formats.view,
				 GTK_TREE_MODEL (nfs->format.formats.model));
	column = gtk_tree_view_column_new_with_attributes (_("Number Formats"),
							   gtk_cell_renderer_text_new (),
							   "text",		0,
							   NULL);
	gtk_tree_view_append_column (nfs->format.formats.view, column);
	nfs->format.formats.selection =
		gtk_tree_view_get_selection (nfs->format.formats.view);
	gtk_tree_selection_set_mode (nfs->format.formats.selection,
				     GTK_SELECTION_BROWSE);
	g_signal_connect (G_OBJECT (nfs->format.formats.selection),
			  "changed",
			  G_CALLBACK (cb_format_list_select), nfs);

	/* Setup handler Currency & Accounting currency symbols */
	combo = GO_COMBO_TEXT (nfs->format.widget[F_SYMBOL]);
	if (combo != NULL) {
		GList *ptr, *l = NULL;

		for (i = 0; currency_symbols[i].symbol != NULL ; ++i)
			l = g_list_append (l, _((gchar *)currency_symbols[i].description));
		l = g_list_sort (l, funny_currency_order);

		for (ptr = l; ptr != NULL ; ptr = ptr->next)
			go_combo_text_add_item	(combo, ptr->data);
		g_list_free (l);
		go_combo_text_set_text (combo,
					 _((const gchar *)currency_symbols[nfs->format.currency_index].description),
					 GO_COMBO_TEXT_FROM_TOP);
		g_signal_connect (G_OBJECT (combo),
				  "entry_changed",
				  G_CALLBACK (cb_format_currency_select), nfs);
	}

	/* Setup special handler for Custom */
	nfs->format.entry_changed_id 
		= g_signal_connect (G_OBJECT (nfs->format.widget[F_ENTRY]),
				    "changed",
				    G_CALLBACK (cb_format_entry_changed), nfs);

	/* Connect signal for format menu */
	set_format_category_menu_from_style (nfs);

	if ((page = nfs->format.spec->family) < 0)
		page = FMT_CUSTOM; /* Default to custom */
	fmt_dialog_enable_widgets (nfs, page);

	nfs->enable_edit = TRUE;
}

static void
nfs_destroy (GtkObject *object)
{
  	NumberFormatSelector *nfs = NUMBER_FORMAT_SELECTOR (object);

	g_free (nfs->locale);
	nfs->locale = NULL;

	if (nfs->format.spec) {
		style_format_unref (nfs->format.spec);
		nfs->format.spec = NULL;
	}

	if (nfs->format.size_group) {
		g_object_unref (nfs->format.size_group);
		nfs->format.size_group = NULL;
	}

	if (nfs->value) {
		value_release (nfs->value);
		nfs->value = NULL;
	}

	if (nfs->gui) {
		g_object_unref (G_OBJECT (nfs->gui));
		nfs->gui = NULL;
	}

	((GtkObjectClass *)nfs_parent_class)->destroy (object);
}

static void
nfs_class_init (GtkObjectClass *klass)
{
	klass->destroy = nfs_destroy;

	nfs_parent_class = g_type_class_peek (gtk_hbox_get_type ());

	nfs_signals[NUMBER_FORMAT_CHANGED] =
		g_signal_new ("number_format_changed",
			      G_OBJECT_CLASS_TYPE (klass),
			      G_SIGNAL_RUN_LAST,
			      G_STRUCT_OFFSET (NumberFormatSelectorClass, number_format_changed),
			      NULL, NULL,
			      g_cclosure_marshal_VOID__POINTER,
			      G_TYPE_NONE, 1, G_TYPE_POINTER);
}

GSF_CLASS (NumberFormatSelector, number_format_selector,
	   nfs_class_init, nfs_init, GTK_TYPE_HBOX)

GtkWidget *
number_format_selector_new (void)
{
	return g_object_new (NUMBER_FORMAT_SELECTOR_TYPE, NULL);
}

void
number_format_selector_set_focus (NumberFormatSelector *nfs)
{
	g_return_if_fail (IS_NUMBER_FORMAT_SELECTOR (nfs));

	gtk_widget_grab_focus (GTK_WIDGET (nfs->format.menu));
}

void
number_format_selector_set_style_format (NumberFormatSelector *nfs,
					 GnmFormat *style_format)
{
	GoComboText *combo;

  	g_return_if_fail (IS_NUMBER_FORMAT_SELECTOR (nfs));
	g_return_if_fail (style_format != NULL);

	style_format_ref (style_format);

  	style_format_unref (nfs->format.spec);

	nfs->format.spec = style_format;

	nfs->format.use_separator = style_format->family_info.thousands_sep;
	nfs->format.num_decimals = style_format->family_info.num_decimals;
	nfs->format.negative_format = style_format->family_info.negative_fmt;
	nfs->format.currency_index = style_format->family_info.currency_symbol_index;

	combo = GO_COMBO_TEXT (nfs->format.widget[F_SYMBOL]);
	go_combo_text_set_text
		(combo,
		 _((const gchar *)currency_symbols[nfs->format.currency_index].description),
		 GO_COMBO_TEXT_FROM_TOP);

	set_format_category_menu_from_style (nfs);
	draw_format_preview (nfs, TRUE);
}

void
number_format_selector_set_value (NumberFormatSelector *nfs,
				  GnmValue const *value)
{
  	g_return_if_fail (IS_NUMBER_FORMAT_SELECTOR (nfs));
	g_return_if_fail (value != NULL);

	if (nfs->value)	{
	  	value_release (nfs->value);
	}
	nfs->value = value_dup (value);

	gtk_widget_show (nfs->format.preview_box);

	draw_format_preview (nfs, TRUE);
}

void
number_format_selector_set_date_conv (NumberFormatSelector *nfs,
				      GnmDateConventions const *date_conv)
{
  	g_return_if_fail (IS_NUMBER_FORMAT_SELECTOR (nfs));
	g_return_if_fail (date_conv != NULL);

	/* FIXME is it safe ? */

  	nfs->date_conv = date_conv;

	draw_format_preview (nfs, TRUE);
}

void
number_format_selector_editable_enters (NumberFormatSelector *nfs,
					GtkWindow *window)
{
	g_return_if_fail (IS_NUMBER_FORMAT_SELECTOR (nfs));

	gnumeric_editable_enters (window,
				  GTK_WIDGET (nfs->format.widget[F_DECIMAL_SPIN]));
	gnumeric_editable_enters (window,
				  GTK_WIDGET (nfs->format.widget[F_ENTRY]));
}


void		
number_format_selector_set_locale (NumberFormatSelector *nfs, 
				   char const *locale)
{
	g_free (nfs->locale);
	nfs->locale = g_strdup (locale);

	cb_format_class_changed (NULL, nfs);
}

/* The following utility function should possibly be in format.h but we */
/* access to the array of category names which are better located here. */
char const *    
number_format_selector_format_classification (GnmFormat const *style_format)
{
  	FormatFamily page = style_format->family;

	if (page < 0 || page > FMT_CUSTOM)
		page = FMT_CUSTOM; /* Default to custom */

	return _(format_category_names[page]);
}

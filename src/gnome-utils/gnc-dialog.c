/* Copyright (C) 2005 Chris Shoemaker <c.shoemaker@cox.net>
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

#include "config.h"
#include <gtk/gtk.h>
#include <glade/glade.h>

#include "gnc-engine.h"
#include "gnome.h"         // for gnome_date_edit
#include "gnc-dialog.h"
#include "gnc-gobject-utils.h"
#include "dialog-utils.h"  // for gnc_glade_xml_new

static QofLogModule log_module = GNC_MOD_GUI;

#define GET_PRIVATE(obj) (G_TYPE_INSTANCE_GET_PRIVATE ((obj), \
  GNC_TYPE_DIALOG, GncDialogPrivate))

struct _GncDialog {
    GtkDialog parent;
};

struct _GncDialogClass {
    GtkDialogClass parent;
    void (*changed) (GncDialog *d);
};

enum {
    GNC_DIALOG_CHANGED,
    LAST_SIGNAL
};

static gint gnc_dialog_signals [LAST_SIGNAL] = { GNC_DIALOG_CHANGED };
static GtkDialogClass *parent_class = NULL;

typedef struct {
    GladeXML *xml;
    GncDialogCallback apply_cb;
    GncDialogCallback close_cb;
    GncDialogCallback help_cb;
    GtkWidget *cancel_btn;
    GtkWidget *ok_btn;
    GtkWidget *help_btn;

    gpointer user_data;
    gboolean changed;
} GncDialogPrivate;

static void
gnc_dialog_finalize(GObject *d)
{
    g_return_if_fail(d);
    /* We don't own any references, so do nothing */
    gnc_gobject_tracking_forget(d);
    G_OBJECT_CLASS(parent_class)->finalize(d);
}

static void
gnc_dialog_class_init (GncDialogClass *klass)
{
    GObjectClass *gobject_class;

    parent_class = g_type_class_peek_parent (klass);

    gobject_class = G_OBJECT_CLASS (klass);

    g_type_class_add_private (gobject_class, sizeof (GncDialogPrivate));

    gnc_dialog_signals [GNC_DIALOG_CHANGED] =
        g_signal_new ("changed",
		  G_OBJECT_CLASS_TYPE (gobject_class),
		  G_SIGNAL_RUN_FIRST,
		  G_STRUCT_OFFSET (struct _GncDialogClass, changed),
		  NULL,
		  NULL,
		  g_cclosure_marshal_VOID__VOID,
		  G_TYPE_NONE,
		  0);

    /* GObject signals */
    gobject_class->finalize = gnc_dialog_finalize;
}

static void
gnc_dialog_init (GncDialog *d, GncDialogClass *klass)
{
  gnc_gobject_tracking_remember(G_OBJECT(d), G_OBJECT_CLASS(klass));
}

GType gnc_dialog_get_type (void)
{
    static GType t = 0;

    if (!t) {
	static const GTypeInfo info = {
	    sizeof (struct _GncDialogClass),
	    NULL, /* base_init */
	    NULL, /* base_final */
	    (GClassInitFunc) gnc_dialog_class_init,
	    NULL, /* class final */
	    NULL, /* class data */
	    sizeof (struct _GncDialog),
	    0, /* n_preallocs */
	    (GInstanceInitFunc) gnc_dialog_init,
	    NULL,
	};
	t = g_type_register_static (GTK_TYPE_DIALOG,
                                    "GncDialog", &info, 0);
    }
    return t;
}

static void gnc_dialog_set_changed(GncDialog *_d, gboolean changed)
{
    GncDialogPrivate *priv;
    struct _GncDialog *d = _d;

    priv = GET_PRIVATE(d);
    if (!priv->changed && changed)
        gtk_dialog_set_response_sensitive(&d->parent, GTK_RESPONSE_OK,
                                          changed);
    priv->changed = changed;
    if (changed)
        g_signal_emit(G_OBJECT(d), gnc_dialog_signals[GNC_DIALOG_CHANGED], 0);
}

static void gnc_dialog_response_cb(GtkDialog *dlg,
                                              gint response, GncDialog *d)
{
    gboolean success = TRUE;
    GncDialogPrivate *priv = GET_PRIVATE(d);

    switch (response) {
    case GTK_RESPONSE_HELP:
	if (priv->help_cb)
            priv->help_cb(d, priv->user_data);
	break;
    case GTK_RESPONSE_OK:
        //case GTK_RESPONSE_APPLY:
	if (priv->apply_cb) {
	    success = priv->apply_cb(d, priv->user_data);
            if (success)
                gnc_dialog_set_changed(d, FALSE);
        }

	if (!success)
	    break;
        // fall through
    default:
	if (priv->close_cb)
	    success = priv->close_cb(d, priv->user_data);
        else
            success = TRUE;

	if (success)
            gtk_widget_destroy(GTK_WIDGET(dlg));
    }
}

static void changed_cb(GObject *obj, gpointer d)
{
    gnc_dialog_set_changed(GNC_DIALOG(d), TRUE);
}

static void
gnc_dialog_watch_for_changes(GtkWidget *wid, gpointer d)
{
    if (GTK_IS_BUTTON(wid))
        g_signal_connect(G_OBJECT(wid), "clicked", G_CALLBACK(changed_cb), d);

    if (GTK_IS_EDITABLE(wid) || GTK_IS_COMBO_BOX(wid))
        g_signal_connect(G_OBJECT(wid), "changed", G_CALLBACK(changed_cb), d);

    if (GTK_IS_TREE_VIEW(wid)) {
        GtkTreeSelection *sel =
            gtk_tree_view_get_selection(GTK_TREE_VIEW(wid));
        g_signal_connect(G_OBJECT(sel), "changed", G_CALLBACK(changed_cb), d);
    }

    if (GTK_IS_TEXT_VIEW(wid)) {
        GtkTextBuffer *buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(wid));
        g_signal_connect(G_OBJECT(buf), "changed", G_CALLBACK(changed_cb), d);
    }
    //Possibly TODO: GtkCalendar?

    /* Recurse over all "contained" widgets */
    if (GTK_IS_CONTAINER(wid)) {
        gtk_container_foreach(GTK_CONTAINER(wid),
                              gnc_dialog_watch_for_changes, d);
    }
}

GncDialog *gnc_dialog_new(const char* filename,
				      const char* root)
{
    GncDialog *d;
    GncDialogPrivate *priv;
    GtkDialog *dlg;
    GtkWidget *child;

    d = g_object_new(GNC_TYPE_DIALOG, NULL);
    dlg = GTK_DIALOG(d);
    priv = GET_PRIVATE(d);

    /* Load in the glade portion and plug it in. */
    priv->xml = gnc_glade_xml_new(filename, root);
    child = glade_xml_get_widget(priv->xml, root);
    if (GTK_WIDGET_TOPLEVEL(child)) {
        PERR("GncDialog root widget must not be a toplevel widget");
        return NULL;
    }

    gtk_container_add(GTK_CONTAINER(dlg->vbox), child);

    /* Prepare the dialog. */
    priv->help_btn = gtk_dialog_add_button(dlg, GTK_STOCK_HELP,
                                           GTK_RESPONSE_HELP);
    priv->cancel_btn = gtk_dialog_add_button(dlg, GTK_STOCK_CANCEL,
                                             GTK_RESPONSE_CANCEL);
    priv->ok_btn = gtk_dialog_add_button(dlg, GTK_STOCK_OK,
                                         GTK_RESPONSE_OK);

    g_signal_connect(dlg, "response",
                     G_CALLBACK(gnc_dialog_response_cb), d);

    glade_xml_signal_autoconnect_full(priv->xml,
                                      gnc_glade_autoconnect_full_func, d);
    gnc_dialog_watch_for_changes(child, (gpointer) d);
    gtk_dialog_set_response_sensitive(dlg, GTK_RESPONSE_OK, FALSE);
    return d;
}

void gnc_dialog_set_cb(GncDialog *d, GncDialogCallback apply_cb,
                       GncDialogCallback close_cb,
                       GncDialogCallback help_cb,
                       gpointer user_data)
{
    GncDialogPrivate *priv;

    priv = GET_PRIVATE(d);
    priv->apply_cb = apply_cb;
    priv->close_cb = close_cb;
    priv->help_cb = help_cb;
    priv->user_data = user_data;

    if (apply_cb == NULL)
        gtk_widget_hide(priv->ok_btn);
    if (help_cb == NULL)
        gtk_widget_hide(priv->help_btn);
}

void gnc_dialog_block_until_close(GncDialog *d)
{
    gint result;
    g_return_if_fail(d);

    do {
        result = gtk_dialog_run(GTK_DIALOG(d));
    } while (result != GTK_RESPONSE_DELETE_EVENT);
}

/* There are certain containers that the type-specific functions don't
   operate on.  But, the API user might have used
   gnc_dialog_get_widget() to get the container widget, and then added
   their own widget to the container.  For the purpose of the
   type-specific functions, we'll consider references to those
   containers as references to their child.  (But only one
   child.)
  */
static GtkWidget *gnc_dialog_get_widget_smart(GtkWidget *w)
{
    g_return_val_if_fail(w, NULL);

    if (GTK_IS_BOX(w)) {
        GList *children = gtk_container_get_children(GTK_CONTAINER(w));
        if (g_list_length(children) == 1) {
            GtkWidget *child = GTK_WIDGET(children->data);
            g_list_free(children);
            return gnc_dialog_get_widget_smart(child);
        }
        g_list_free(children);
    }
    return w;
}


/* Method 1 */
GtkWidget *gnc_dialog_get_widget(GncDialog *d, const gchar* name)
{
    GncDialogPrivate *priv;

    priv = GET_PRIVATE(d);
    g_return_val_if_fail(name, NULL);
    return glade_xml_get_widget(priv->xml, name);
}

void gnc_dialog_set_sensitive(GncDialog *d, const gchar* name, gboolean sen)
{
    gtk_widget_set_sensitive(gnc_dialog_get_widget(d, name), sen);
}

#define IS_A(wid, tname) (g_type_is_a(GTK_WIDGET_TYPE(wid), \
				      g_type_from_name(tname) ))

#define TYPE_ERROR(wid, tname, failval) do {             \
    PERR("Expected %s, but found %s", (tname),  \
        g_type_name(GTK_WIDGET_TYPE(wid)));     \
    return (failval);                              \
} while (0)

#define SPECIFIC_INIT(d, name, wid, failval)               \
    GtkWidget *(wid);                                      \
    g_return_val_if_fail((d) && (name), (failval));        \
    (wid) = gnc_dialog_get_widget((d), (name));            \
    (wid) = gnc_dialog_get_widget_smart((wid));            \
    g_return_val_if_fail((wid), (failval));

/*
 *  Type-specific getter/setters.
 *
 */
gboolean gnc_dialog_set_string(GncDialog *d, const gchar* name,
                               const gchar* val)
{
    SPECIFIC_INIT(d, name, wid, FALSE);

    if (IS_A(wid, "GtkEntry"))
	gtk_entry_set_text(GTK_ENTRY(wid), val);
    else if (IS_A(wid, "GtkLabel"))
        gtk_label_set_text(GTK_LABEL(wid), val);
    else if (IS_A(wid, "GtkCombo")) //deprecated
        gtk_entry_set_text(GTK_ENTRY(GTK_COMBO(wid)->entry), val);
    else if (IS_A(wid, "GtkTextView")) {
        GtkTextBuffer *buf;
        buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(wid));
        gtk_text_buffer_set_text(buf, val, -1);
    } else TYPE_ERROR(wid, "GtkEntry or GtkLabel or GtkTextView", FALSE);
    //TODO: font support?

    return TRUE;
}

gchar * gnc_dialog_get_string(GncDialog *d, const gchar* name)
{
    SPECIFIC_INIT(d, name, wid, NULL);

    if (IS_A(wid, "GtkEntry"))
        return g_strdup(gtk_entry_get_text(GTK_ENTRY(wid)));
    else if (IS_A(wid, "GtkLabel"))
        return g_strdup(gtk_label_get_text(GTK_LABEL(wid)));
    else if (IS_A(wid, "GtkCombo")) //deprecated
        return g_strdup(gtk_entry_get_text(GTK_ENTRY(GTK_COMBO(wid)->entry)));
    else if (IS_A(wid, "GtkTextView")) {
        GtkTextBuffer *buf;
        GtkTextIter start, end;
        buf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(wid));
        gtk_text_buffer_get_bounds(buf, &start, &end);
        return gtk_text_buffer_get_text(buf, &start, &end, TRUE);
    } else if (IS_A(wid, "GtkComboBoxEntry")) {
        gint col;
        GtkTreeModel *tm;
        GtkTreeIter iter;
        GType type;
        col = gtk_combo_box_entry_get_text_column(GTK_COMBO_BOX_ENTRY(wid));
        tm = gtk_combo_box_get_model(GTK_COMBO_BOX(wid));
        type = gtk_tree_model_get_column_type(tm, col);
        if (type != G_TYPE_STRING)
            return NULL;
        if (!gtk_combo_box_get_active_iter(GTK_COMBO_BOX(wid), &iter)) {
            gchar *str;
            gtk_tree_model_get(tm, &iter, col, &str);
            return str;
        } else return NULL;
    } else TYPE_ERROR(wid, "GtkEntry or GtkLabel or GtkTextView", NULL);
}

gboolean gnc_dialog_set_double(GncDialog *d, const gchar* name, gdouble val)
{
    SPECIFIC_INIT(d, name, wid, FALSE);

    if (IS_A(wid, "GtkSpinButton"))
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(wid), val);
    else TYPE_ERROR(wid, "GtkSpinButton", FALSE);
    return TRUE;
    //TODO: string conversion?
}

gdouble gnc_dialog_get_double(GncDialog *d, const gchar* name)
{
    SPECIFIC_INIT(d, name, wid, 0.0);

    if (IS_A(wid, "GtkSpinButton"))
	return gtk_spin_button_get_value(GTK_SPIN_BUTTON(wid));
    else TYPE_ERROR(wid, "GtkSpinButton", 0.0);
}
gboolean gnc_dialog_set_int(GncDialog *d, const gchar* name, gint val)
{
    SPECIFIC_INIT(d, name, wid, FALSE);

    if (IS_A(wid, "GtkSpinButton"))
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(wid), (gdouble)val);
    else TYPE_ERROR(wid, "GtkSpinButton", FALSE);
    return TRUE;
}

gint gnc_dialog_get_int(GncDialog *d, const gchar* name)
{
    SPECIFIC_INIT(d, name, wid, 0);

    if (IS_A(wid, "GtkSpinButton"))
	return gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(wid));
    else TYPE_ERROR(wid, "GtkSpinButton", 0);
}

gboolean gnc_dialog_set_date(GncDialog *d, const gchar* name, time_t val)
{
    SPECIFIC_INIT(d, name, wid, FALSE);

    if (IS_A(wid, "GnomeDateEdit"))
	gnome_date_edit_set_time((GnomeDateEdit *)wid, val);
    else TYPE_ERROR(wid, "GnomeDateEdit", FALSE);
    return TRUE;
}

time_t gnc_dialog_get_date(GncDialog *d, const gchar* name)
{
    SPECIFIC_INIT(d, name, wid, ((time_t)(-1)));

    if (IS_A(wid, "GnomeDateEdit"))
	return gnome_date_edit_get_time((GnomeDateEdit *)wid);
    else TYPE_ERROR(wid, "GnomeDateEdit", ((time_t)(-1)));
}


gboolean gnc_dialog_set_index(GncDialog *d, const gchar* name, gint val)
{
    SPECIFIC_INIT(d, name, wid, FALSE);

    if (IS_A(wid, "GtkComboBox"))
        gtk_combo_box_set_active(GTK_COMBO_BOX(wid), val);
    else if (IS_A(wid, "GtkOptionMenu"))
        gtk_option_menu_set_history(GTK_OPTION_MENU(wid),
                                    (guint)(val < 0 ? -val : val));
    else TYPE_ERROR(wid, "GtkComboBox", FALSE); // GtkOptionMenu is deprecated.
    return TRUE;
}

gint gnc_dialog_get_index(GncDialog *d, const gchar* name)
{
    SPECIFIC_INIT(d, name, wid, -1);

    if (IS_A(wid, "GtkComboBox"))
	return gtk_combo_box_get_active(GTK_COMBO_BOX(wid));
    else if (IS_A(wid, "GtkOptionMenu"))
        return gtk_option_menu_get_history(GTK_OPTION_MENU(wid));
    else TYPE_ERROR(wid, "GtkComboBox", -1); // GtkOptionMenu is deprecated.
}

gboolean gnc_dialog_set_boolean(GncDialog *d, const gchar* name,
                                gboolean val)
{
    SPECIFIC_INIT(d, name, wid, FALSE);

    if (IS_A(wid, "GtkToggleButton"))
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(wid), val);
    else TYPE_ERROR(wid, "GtkToggleButton", FALSE);
    return TRUE;
}

gboolean gnc_dialog_get_boolean(GncDialog *d, const gchar* name)
{
    SPECIFIC_INIT(d, name, wid, FALSE);

    if (IS_A(wid, "GtkToggleButton"))
	return gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wid));
    else TYPE_ERROR(wid, "GtkToggleButton", FALSE);
}


/* Method 3 */
/* getters and setters */
static gpointer gd_gtk_entry_get_text(gpointer w)
{
    return (gpointer)gtk_entry_get_text(GTK_ENTRY(w));
}
static gboolean gd_gtk_entry_set_text(gpointer wid, gpointer val)
{
    g_return_val_if_fail(GTK_IS_ENTRY(wid), FALSE);
    gtk_entry_set_text(GTK_ENTRY(wid), (gchar *) val);
    return TRUE;

}
static gpointer gd_gtk_spin_button_get_value(gpointer w)
{
    static gdouble d;
    d = gtk_spin_button_get_value(GTK_SPIN_BUTTON(w));
    return ((gpointer) &d);
}
static gboolean gd_gtk_spin_button_set_value(gpointer w, gpointer d)
{
    g_return_val_if_fail(GTK_IS_SPIN_BUTTON(w), FALSE);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(w), *(gdouble *)d);
    return TRUE;
}
#if 0
static const gpointer gd_gtk_toggle_button_get_active(GtkWidget *w)
{
    static gboolean b;
    b = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(w));
    return ((gpointer) &b);
}
static void gd_gtk_toggle_button_set_active(GtkWidget *w, gpointer b)
{
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(w), *(gboolean *)b);
}
static const gpointer gd_gtk_combo_box_get_active(GtkWidget *w)
{
    static gint i;
    i = gtk_combo_box_get_active(GTK_COMBO_BOX(w));
    return ((gpointer) &i);
}
static void gd_gtk_combo_box_set_active(GtkWidget *w, gpointer b)
{
    gtk_combo_box_set_active(GTK_COMBO_BOX(w), *(gint *)b);
}
static const gpointer gd_gtk_text_view_get_buffer(GtkWidget *w)
{
    return (gpointer)gtk_text_view_get_buffer(GTK_TEXT_VIEW(w));
}
static void gd_gtk_text_view_set_buffer(GtkWidget *w, gpointer b)
{
    gtk_text_view_set_buffer(GTK_TEXT_VIEW(w), GTK_TEXT_BUFFER(b));
}
static const gpointer gd_gnome_date_edit_get_time(GtkWidget *w)
{
    static time_t t;
    t = gnome_date_edit_get_time(GNOME_DATE_EDIT(w));
    return ((gpointer) &t);
}
static void gd_gnome_date_edit_set_time(GtkWidget *w, gpointer t)
{
    gnome_date_edit_set_time(GNOME_DATE_EDIT(w), *(time_t *)t);
}

/* Order is important. Children before parents. */
static struct prop_type {
    gchar *widget_type;
    GD_Getter_Func getter;
    GD_Setter_Func setter;
} prop_types[] = {
    {"GnomeDateEdit", gd_gnome_date_edit_get_time,
     gd_gnome_date_edit_set_time },
    {"GtkLabel", (GD_Getter_Func) gtk_label_get_label,
     (GD_Setter_Func) gtk_label_set_label},
    {"GtkToggleButton", gd_gtk_toggle_button_get_active,
     gd_gtk_toggle_button_set_active},
    {"GtkComboBox", gd_gtk_combo_box_get_active,
     gd_gtk_combo_box_set_active},
};

#define NUM_PROP_TYPES \
  (sizeof(prop_types) / sizeof(struct prop_type))

static gint
find_prop_type(GncDialog *d, GtkWidget *wid)
{
    gint i;
    struct prop_type pt;

    for(i = 0; i < NUM_PROP_TYPES; i++) {
	pt = prop_types[i];
	if (IS_A(wid, pt.widget_type))
	    return i;
    }
    return -1;
}
#endif

typedef gpointer (*GD_Getter_Func)(GtkWidget *w);
typedef void (*GD_Setter_Func)(GtkWidget *w, gpointer val);

typedef struct {
    GncDialogGetter getter;
    GncDialogSetter setter;
    GncDialogSetter filler;
} custom_type;


void gnc_dialog_register_testing_types(void)
{
    gnc_dialog_register_custom(g_type_from_name("GtkSpinButton"),
                               gd_gtk_spin_button_get_value,
                               gd_gtk_spin_button_set_value, NULL);
    gnc_dialog_register_custom(g_type_from_name("GtkEntry"),
                               gd_gtk_entry_get_text,
                               gd_gtk_entry_set_text, NULL);

}

static GHashTable *custom_types;

gboolean gnc_dialog_set_custom(GncDialog *d, const gchar* name,
                               const gpointer val)
{
    GType i;
    custom_type *custom_spec = NULL;
    SPECIFIC_INIT(d, name, wid, FALSE);

    g_return_val_if_fail(custom_types, FALSE);
    i = G_TYPE_FROM_INSTANCE(wid);
    custom_spec = g_hash_table_lookup(
        custom_types, &i);

    g_return_val_if_fail(custom_spec, FALSE);

    if (custom_spec->setter(wid, val)) {
        gnc_dialog_set_changed(d, TRUE);
        return TRUE;
    }
    return FALSE;
}

gpointer gnc_dialog_get_custom(GncDialog *d, const gchar* name)
{
    GType i;
    custom_type *custom_spec = NULL;
    SPECIFIC_INIT(d, name, wid, NULL);

    g_return_val_if_fail(custom_types, NULL);
    i = G_TYPE_FROM_INSTANCE(wid);
    custom_spec = g_hash_table_lookup(
        custom_types, &i);
    g_return_val_if_fail(custom_spec, NULL);

    return custom_spec->getter(wid);
}

gboolean gnc_dialog_fill_custom(GncDialog *d, const char* name);


void gnc_dialog_register_custom(GType widgetType, GncDialogGetter getter,
                                GncDialogSetter setter,
                                GncDialogSetter filler)
{
    custom_type *ct = g_new0(custom_type, 1);
    GType *key = g_new0(GType, 1);

    if (custom_types == NULL) {
        custom_types = g_hash_table_new_full(
            g_int_hash, g_int_equal, g_free, g_free);
    }
    ct->getter = getter;
    ct->setter = setter;
    ct->filler = filler;
    *key = widgetType;
    PINFO("registering with GType %d", (int)widgetType);
    g_hash_table_insert(custom_types, key, ct);
}

void gnc_dialog_unregister_custom(GType widgetType)
{
    g_hash_table_remove(custom_types, &widgetType);
}

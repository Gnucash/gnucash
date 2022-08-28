/*
 *  Copyright (C) 2002 Derek Atkins
 *
 *  Authors: Derek Atkins <warlord@MIT.EDU>
 *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <string.h>
#include <sys/types.h>
#include <regex.h>

#include "search-string.h"
#include "search-core-utils.h"
#include "qof.h"

#define d(x)

static void editable_enters (GNCSearchCoreType *fe);
static void pass_parent (GNCSearchCoreType *fe, gpointer parent);
static void grab_focus (GNCSearchCoreType *fe);
static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe);
static gboolean gncs_validate (GNCSearchCoreType *fe);
static GtkWidget *gncs_get_widget(GNCSearchCoreType *fe);
static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe);

static void gnc_search_string_class_init	(GNCSearchStringClass *klass);
static void gnc_search_string_init	(GNCSearchString *gspaper);
static void gnc_search_string_finalize	(GObject *obj);

typedef struct _GNCSearchStringPrivate GNCSearchStringPrivate;

struct _GNCSearchStringPrivate
{
    GtkWidget *entry;
    GtkWindow *parent;
};

G_DEFINE_TYPE_WITH_PRIVATE(GNCSearchString, gnc_search_string, GNC_TYPE_SEARCH_CORE_TYPE)

#define _PRIVATE(o) \
   ((GNCSearchStringPrivate*)gnc_search_string_get_instance_private((GNCSearchString*)o))

static GNCSearchCoreTypeClass *parent_class;

static void
gnc_search_string_class_init (GNCSearchStringClass *klass)
{
    GObjectClass *object_class;
    GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)klass;

    object_class = G_OBJECT_CLASS (klass);
    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_search_string_finalize;

    /* override methods */
    gnc_search_core_type->editable_enters = editable_enters;
    gnc_search_core_type->pass_parent = pass_parent;
    gnc_search_core_type->grab_focus = grab_focus;
    gnc_search_core_type->validate = gncs_validate;
    gnc_search_core_type->get_widget = gncs_get_widget;
    gnc_search_core_type->get_predicate = gncs_get_predicate;
    gnc_search_core_type->clone = gncs_clone;
}

static void
gnc_search_string_init (GNCSearchString *o)
{
    o->value = NULL;
    o->how = SEARCH_STRING_CONTAINS;
    o->ign_case = TRUE;
}

static void
gnc_search_string_finalize (GObject *obj)
{
    GNCSearchString *o = (GNCSearchString *)obj;
    g_assert (IS_GNCSEARCH_STRING (o));

    g_free (o->value);

    G_OBJECT_CLASS (parent_class)->finalize(obj);
}

/**
 * gnc_search_string_new:
 *
 * Create a new GNCSearchString object.
 *
 * Return value: A new #GNCSearchString object.
 **/
GNCSearchString *
gnc_search_string_new (void)
{
    GNCSearchString *o = g_object_new(GNC_TYPE_SEARCH_STRING, NULL);
    return o;
}

void
gnc_search_string_set_value (GNCSearchString *fi, const char *value)
{
    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_STRING (fi));

    if (fi->value)
        g_free (fi->value);

    fi->value = g_strdup (value);
}

void
gnc_search_string_set_how (GNCSearchString *fi, GNCSearchString_Type how)
{
    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_STRING (fi));
    fi->how = how;
}

void
gnc_search_string_set_case (GNCSearchString *fi, gboolean ignore_case)
{
    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_STRING (fi));
    fi->ign_case = ignore_case;
}

static gboolean
gncs_validate (GNCSearchCoreType *fe)
{
    GNCSearchString *fi = (GNCSearchString *)fe;
    GNCSearchStringPrivate *priv;
    gboolean valid = TRUE;

    g_return_val_if_fail (fi, FALSE);
    g_return_val_if_fail (IS_GNCSEARCH_STRING (fi), FALSE);

    priv = _PRIVATE(fi);

    if (!fi->value || *(fi->value) == '\0')
    {
        GtkWidget *dialog;
        dialog = gtk_message_dialog_new (GTK_WINDOW(priv->parent),
                                         GTK_DIALOG_MODAL,
                                         GTK_MESSAGE_ERROR,
                                         GTK_BUTTONS_OK,
                                         "%s",
                                         _("You need to enter some search text."));
        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy(dialog);
        return FALSE;
    }

    if (fi->how == SEARCH_STRING_MATCHES_REGEX ||
            fi->how == SEARCH_STRING_NOT_MATCHES_REGEX)
    {
        regex_t regexpat;        /* regex pattern */
        gint regerr;
        int flags = REG_EXTENDED;

        if (fi->ign_case)
            flags |= REG_ICASE;

        regerr = regcomp (&regexpat, fi->value, flags);
        if (regerr)
        {
            GtkWidget *dialog;
            gchar *regmsg, *errmsg;
            size_t reglen;

            /* regerror gets called twice to get the full error string
            	 length to do proper posix error reporting */
            reglen = regerror (regerr, &regexpat, 0, 0);
            regmsg = g_malloc0 (reglen + 1);
            regerror (regerr, &regexpat, regmsg, reglen);

            errmsg = g_strdup_printf (_("Error in regular expression '%s':\n%s"),
                                      fi->value, regmsg);
            g_free (regmsg);

            dialog = gtk_message_dialog_new (GTK_WINDOW(priv->parent),
                                             GTK_DIALOG_MODAL,
                                             GTK_MESSAGE_ERROR,
                                             GTK_BUTTONS_OK,
                                             "%s", errmsg);
            gtk_dialog_run (GTK_DIALOG (dialog));
            gtk_widget_destroy(dialog);
            g_free (errmsg);
            valid = FALSE;
        }

        regfree (&regexpat);
    }

    return valid;
}

static void
toggle_changed (GtkToggleButton *button, GNCSearchString *fe)
{
    fe->ign_case = !gtk_toggle_button_get_active (button);
}

static void
entry_changed (GtkEntry *entry, GNCSearchString *fe)
{
    const char *new_str;

    new_str = gtk_entry_get_text(entry);
    gnc_search_string_set_value (fe, new_str);
}

static GtkWidget *
make_menu (GNCSearchCoreType *fe)
{
    GNCSearchString *fi = (GNCSearchString *)fe;
    GtkComboBox *combo;

    combo = GTK_COMBO_BOX(gnc_combo_box_new_search());

    gnc_combo_box_search_add(combo, _("contains"), SEARCH_STRING_CONTAINS);
    gnc_combo_box_search_add(combo, _("equals"), SEARCH_STRING_EQUAL);
    gnc_combo_box_search_add(combo, _("matches regex"),
                             SEARCH_STRING_MATCHES_REGEX);
    gnc_combo_box_search_add(combo, _("does not match regex"),
                             SEARCH_STRING_NOT_MATCHES_REGEX);
    gnc_combo_box_search_changed(combo, &fi->how);
    gnc_combo_box_search_set_active(combo, fi->how ? fi->how : SEARCH_STRING_CONTAINS);

    return GTK_WIDGET(combo);
}

static void
grab_focus (GNCSearchCoreType *fe)
{
    GNCSearchString *fi = (GNCSearchString *)fe;
    GNCSearchStringPrivate *priv;

    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_STRING (fi));

    priv = _PRIVATE(fi);
    if (priv->entry)
        gtk_widget_grab_focus (priv->entry);
}

static void
editable_enters (GNCSearchCoreType *fe)
{
    GNCSearchString *fi = (GNCSearchString *)fe;
    GNCSearchStringPrivate *priv;

    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_STRING (fi));

    priv = _PRIVATE(fi);
    if (priv->entry)
        gtk_entry_set_activates_default(GTK_ENTRY (priv->entry), TRUE);
}

static void
pass_parent (GNCSearchCoreType *fe, gpointer parent)
{
    GNCSearchString *fi = (GNCSearchString *)fe;
    GNCSearchStringPrivate *priv;

    g_return_if_fail (fi);
    g_return_if_fail (IS_GNCSEARCH_STRING (fi));

    priv = _PRIVATE(fi);
    priv->parent = GTK_WINDOW(parent);
}

static GtkWidget *
gncs_get_widget (GNCSearchCoreType *fe)
{
    GtkWidget *entry, *toggle, *menu, *box;
    GNCSearchString *fi = (GNCSearchString *)fe;
    GNCSearchStringPrivate *priv;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_STRING (fi), NULL);

    priv = _PRIVATE(fi);
    box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 3);
    gtk_box_set_homogeneous (GTK_BOX (box), FALSE);

    /* Build and connect the option menu */
    menu = make_menu (fe);
    gtk_box_pack_start (GTK_BOX (box), menu, FALSE, FALSE, 3);

    /* Build and connect the entry window */
    entry = gtk_entry_new ();
    if (fi->value)
        gtk_entry_set_text (GTK_ENTRY (entry), fi->value);
    g_signal_connect (G_OBJECT (entry), "changed", G_CALLBACK (entry_changed), fe);
    gtk_box_pack_start (GTK_BOX (box), entry, FALSE, FALSE, 3);
    priv->entry = entry;

    /* Build and connect the case-sensitive check button; defaults to off */
    toggle = gtk_check_button_new_with_label (_("Match case"));
    g_signal_connect (G_OBJECT(toggle), "toggled", G_CALLBACK (toggle_changed), fe);
    gtk_box_pack_start (GTK_BOX (box), toggle, FALSE, FALSE, 3);

    /* And return the box */
    return box;
}

static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe)
{
    GNCSearchString *ss = (GNCSearchString *)fe;
    QofQueryCompare how;
    QofStringMatch options = QOF_STRING_MATCH_NORMAL;
    gboolean is_regex = FALSE;

    g_return_val_if_fail (ss, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_STRING (ss), NULL);

    switch (ss->how)
    {
    case SEARCH_STRING_MATCHES_REGEX:
        is_regex = TRUE;
        /* FALL THROUGH */
    case SEARCH_STRING_CONTAINS:
        how = QOF_COMPARE_CONTAINS;
        break;
    case SEARCH_STRING_EQUAL:
        how = QOF_COMPARE_EQUAL;
        break;
    case SEARCH_STRING_NOT_MATCHES_REGEX:
        is_regex = TRUE;
        /* FALL THROUGH */
    case SEARCH_STRING_NOT_CONTAINS:
        how = QOF_COMPARE_NCONTAINS;
        break;
    case SEARCH_STRING_NOT_EQUAL:
        how = QOF_COMPARE_NEQ;
        break;
    default:
        g_warning ("invalid string choice: %d", ss->how);
        return NULL;
    }

    if (ss->ign_case)
        options = QOF_STRING_MATCH_CASEINSENSITIVE;

    return qof_query_string_predicate (how, ss->value, options, is_regex);
}

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe)
{
    GNCSearchString *se, *fse = (GNCSearchString *)fe;

    g_return_val_if_fail (fse, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_STRING (fse), NULL);

    se = gnc_search_string_new ();
    gnc_search_string_set_value (se, fse->value);
    gnc_search_string_set_how (se, fse->how);
    gnc_search_string_set_case (se, fse->ign_case);

    return (GNCSearchCoreType *)se;
}

/*
 * Currency selection for search criteria.
 * Copyright (C) 2026 Étienne Beaulé
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <https://www.gnu.org/licenses/>.
 */

#include <config.h>
#include <string>
#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-currency-edit.h"
#include "gnc-gui-query.h"
#include "search-currency.h"
extern "C"
{
#include "search-core-utils.h"
}

struct CurrencySearchState
{
    guint m_how{QOF_COMPARE_EQUAL};
    std::string m_mnemonic;
    GtkWidget *m_picker{};
    GtkWindow *m_parent{};
};

struct _GNCSearchCurrency
{
    GNCSearchCoreType parent_instance;
    CurrencySearchState *m_state;
};

G_DEFINE_TYPE (GNCSearchCurrency, gnc_search_currency, GNC_TYPE_SEARCH_CORE_TYPE)

static void
currency_changed (GtkComboBox *picker, GNCSearchCurrency *self)
{
    self->m_state->m_mnemonic.clear ();
    if (gtk_combo_box_get_active (picker) != -1)
    {
        auto currency{gnc_currency_edit_get_currency (GNC_CURRENCY_EDIT (picker))};
        self->m_state->m_mnemonic = gnc_commodity_get_mnemonic (currency);
    }
}

static GtkWidget *
get_widget (GNCSearchCoreType *fe)
{
    auto& state{*GNC_SEARCH_CURRENCY (fe)->m_state};
    auto box{gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 3)};
    auto combo{GTK_COMBO_BOX (gnc_combo_box_new_search ())};

    gnc_combo_box_search_add (combo, _("is"), QOF_COMPARE_EQUAL);
    gnc_combo_box_search_add (combo, _("is not"), QOF_COMPARE_NEQ);
    gnc_combo_box_search_set_active (combo, state.m_how);
    gnc_combo_box_search_changed (combo, &state.m_how);
    gtk_box_pack_start (GTK_BOX (box), GTK_WIDGET (combo), false, false, 3);

    state.m_picker = gnc_currency_edit_new ();
    if (!state.m_mnemonic.empty ())
        g_object_set (state.m_picker, "mnemonic", state.m_mnemonic.c_str (), nullptr);
    g_signal_connect (state.m_picker, "changed", G_CALLBACK (currency_changed), GNC_SEARCH_CURRENCY (fe));
    gtk_box_pack_start (GTK_BOX (box), state.m_picker, false, false, 3);
    return box;
}

static void
pass_parent (GNCSearchCoreType *fe, gpointer parent)
{
    GNC_SEARCH_CURRENCY (fe)->m_state->m_parent = GTK_WINDOW (parent);
}

static void
grab_focus (GNCSearchCoreType *fe)
{
    auto& state{*GNC_SEARCH_CURRENCY (fe)->m_state};
    if (state.m_picker)
        gtk_widget_grab_focus (gtk_bin_get_child (GTK_BIN (state.m_picker)));
}

static void
editable_enters (GNCSearchCoreType *fe)
{
    auto& state{*GNC_SEARCH_CURRENCY (fe)->m_state};
    if (state.m_picker)
        gtk_entry_set_activates_default (GTK_ENTRY (gtk_bin_get_child (GTK_BIN (state.m_picker))), true);
}

static gboolean
validate (GNCSearchCoreType *fe)
{
    auto& state{*GNC_SEARCH_CURRENCY (fe)->m_state};
    if (!state.m_mnemonic.empty ())
        return true;
    gnc_error_dialog (state.m_parent, "%s", _("You need to select a currency."));
    return false;
}

static QofQueryPredData *
get_predicate (GNCSearchCoreType *fe)
{
    auto& state{*GNC_SEARCH_CURRENCY (fe)->m_state};
    if (state.m_mnemonic.empty ())
        return nullptr;
    return qof_query_string_predicate (static_cast<QofQueryCompare> (state.m_how), state.m_mnemonic.c_str (),
                                       QOF_STRING_MATCH_NORMAL, false);
}

static GNCSearchCoreType *
clone_criterion (GNCSearchCoreType *fe)
{
    auto& state{*GNC_SEARCH_CURRENCY (fe)->m_state};
    auto copy{gnc_search_currency_new ()};
    copy->m_state->m_how = state.m_how;
    copy->m_state->m_mnemonic = state.m_mnemonic;
    return GNC_SEARCH_CORE_TYPE (copy);
}

static void
finalize (GObject *obj)
{
    delete GNC_SEARCH_CURRENCY (obj)->m_state;
    G_OBJECT_CLASS (gnc_search_currency_parent_class)->finalize (obj);
}

static void
gnc_search_currency_class_init (GNCSearchCurrencyClass *klass)
{
    auto core{GNC_SEARCH_CORE_TYPE_CLASS (klass)};
    G_OBJECT_CLASS (klass)->finalize = finalize;
    core->get_widget = get_widget;
    core->pass_parent = pass_parent;
    core->grab_focus = grab_focus;
    core->editable_enters = editable_enters;
    core->validate = validate;
    core->get_predicate = get_predicate;
    core->clone = clone_criterion;
}

static void
gnc_search_currency_init (GNCSearchCurrency *self)
{
    self->m_state = new CurrencySearchState;
}

GNCSearchCurrency *
gnc_search_currency_new (void)
{
    return GNC_SEARCH_CURRENCY (g_object_new (GNC_TYPE_SEARCH_CURRENCY, nullptr));
}

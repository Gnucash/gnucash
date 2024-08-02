/********************************************************************\
 * dialog-options.cpp -- option handling                      *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
 * Copyright (c) 2011 Robert Fewell                                 *
 * Copyright 2020 John Ralls                                        *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include <config.h> // Need this to include Account.h

#include <Account.h> // To include as C++ overriding later indirect includes
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <glib/gi18n.h>
#include <gnc-optiondb.hpp>
#include <gnc-optiondb-impl.hpp>
#include "dialog-options.hpp"
#include <libguile.h>

#include <qofbookslots.h> // for OPTION_SECTION_ACCOUNTS

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include <gnc-prefs.h> // for GNC_PREFS_NUM_SOURCE
#include "gnc-session.h" // for gnc_get_current_session
#include "gnc-ui.h" // for DF_MANUAL

#include <any>
#include <iostream>
#include <sstream>

#include "gnc-option-gtk-ui.hpp"

#define GNC_PREF_CLOCK_24H "clock-24h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static constexpr const char* DIALOG_OPTIONS_CM_CLASS{"dialog-options"};
static constexpr const char* GNC_PREFS_GROUP{"dialogs.options"};

/*
 * Point where preferences switch control method from a set of
 * notebook tabs to a list.
 */
#define MAX_TAB_COUNT 6



enum page_tree
{
    PAGE_INDEX = 0,
    PAGE_NAME,
    NUM_COLUMNS
};


static void dialog_reset_cb(GtkWidget * w, gpointer data);
static void dialog_list_select_cb (GtkTreeSelection *selection, gpointer data);
static void component_close_handler (gpointer data);

static inline GtkWidget* const
option_get_gtk_widget (const GncOption* option)
{
    if (!option) return nullptr;
    auto ui_item{dynamic_cast<const GncOptionGtkUIItem*>(option->get_ui_item())};
    if (ui_item)
        return ui_item->get_widget();

    return nullptr;
}

static void
dialog_changed_internal (GtkWidget *widget, bool sensitive)
{
    g_return_if_fail(widget);

    auto toplevel{gtk_widget_get_toplevel(widget)};
    if (toplevel == widget && !GTK_IS_WINDOW(toplevel))
        return;
    g_assert(toplevel && GTK_IS_WINDOW(toplevel));

    auto option_win =
        static_cast<GncOptionsDialog*>(g_object_get_data(G_OBJECT(toplevel),
                                                     "optionwin"));

    if (option_win) // this null when part of assistant
        option_win->set_sensitive(sensitive);
}

void
GncOptionsDialog::set_sensitive(bool sensitive) noexcept
{
    gtk_widget_set_sensitive (GTK_WIDGET(m_apply_button), sensitive);
    gtk_widget_set_sensitive (GTK_WIDGET(m_ok_button), sensitive);
    gtk_button_set_label (m_cancel_button,
                          sensitive ? _("_Cancel") : _("_Close"));
}

void
GncOptionsDialog::changed() noexcept
{
    set_sensitive(true);
}

struct SCMDeleter {
  void operator()(SCM cb) { scm_gc_unprotect_object(cb); }
};

class SCMCallbackWrapper
{
    std::unique_ptr<scm_unused_struct, SCMDeleter> m_callback;
public:
    SCMCallbackWrapper(SCM cb) : m_callback{scm_gc_protect_object(cb)} {}
    SCMCallbackWrapper(const SCMCallbackWrapper& cbw) : m_callback{scm_gc_protect_object(cbw.get())} {}
    SCM get() const { return m_callback.get(); }
};

void
gnc_option_changed_widget_cb(GtkWidget *widget, GncOption* option)
{
    if (!option || option->is_internal()) return;
    auto ui_item{option->get_ui_item()};
    g_return_if_fail(ui_item);
    auto& widget_changed_cb{option->get_widget_changed()};
    auto gtk_ui_item{dynamic_cast<GncOptionGtkUIItem*>(ui_item)};
    if (widget_changed_cb.has_value() && gtk_ui_item)
    {
        try
        {
            auto cb{std::any_cast<SCMCallbackWrapper>(widget_changed_cb)};
            SCM widget_value{gtk_ui_item->get_widget_scm_value(*option)};
            scm_call_1(cb.get(), widget_value);
        }
        catch(std::bad_any_cast& err)
        {
            PERR("Bad widget changed callback type %s", err.what());
        }
    }
    const_cast<GncOptionUIItem*>(ui_item)->set_dirty(true);
    dialog_changed_internal(widget, true);
}

void
gnc_option_changed_option_cb(GtkWidget *dummy, GncOption* option)
{
    if (!option) return;
    auto widget{option_get_gtk_widget(option)};
    gnc_option_changed_widget_cb(widget, option);
}


// This do-nothing template is specialized for each GncOptionUIType.
template<GncOptionUIType type> GtkWidget*
create_option_widget(GncOption& option, GtkGrid*, GtkLabel*, char*, GtkWidget**,
                     bool*)
{
    return nullptr;
}

static void
gnc_option_set_ui_widget(GncOption& option, GtkGrid *page_box, gint grid_row)
{
    ENTER("option %p(%s), box %p",
          &option, option.get_name().c_str(), page_box);
    auto type = option.get_ui_type();
    if (type == GncOptionUIType::INTERNAL)
    {
        LEAVE("internal type");
        return;
    }

    GncOptionUIFactory::create(option, page_box, grid_row);

    LEAVE(" ");
}

static GtkBox*
create_content_box()
{
    auto content_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 2);
    gtk_widget_set_name (content_box, "page-content-box");
    gtk_box_set_homogeneous (GTK_BOX (content_box), FALSE);

    gtk_container_set_border_width(GTK_CONTAINER(content_box), 12);
    return GTK_BOX(content_box);
}

static GtkGrid*
create_options_box(GtkBox* content_box)
{
    auto options_scrolled_win = gtk_scrolled_window_new(NULL, NULL);
    gtk_box_pack_start(GTK_BOX(content_box), options_scrolled_win,
                       TRUE, TRUE, 0);

    /* Build space for the content - the options box */
    auto options_box = gtk_grid_new(); // this will have two columns
    gtk_widget_set_name (options_box, "options-box");
    gtk_grid_set_row_homogeneous (GTK_GRID(options_box), FALSE);
    gtk_grid_set_column_homogeneous (GTK_GRID(options_box), FALSE);
    gtk_grid_set_row_spacing (GTK_GRID(options_box), 6);
    gtk_grid_set_column_spacing (GTK_GRID(options_box), 6);
    gtk_widget_set_halign (GTK_WIDGET(options_box), GTK_ALIGN_START);
    
    gtk_container_set_border_width(GTK_CONTAINER(options_box), 0);
    gtk_container_add (GTK_CONTAINER(options_scrolled_win),
                       GTK_WIDGET(options_box));
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(options_scrolled_win),
                                   GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
    return GTK_GRID(options_box);
}

static GtkButtonBox*
create_reset_button_box(GtkBox* page_content_box)
{
    auto buttonbox = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
    gtk_button_box_set_layout (GTK_BUTTON_BOX (buttonbox),
                               GTK_BUTTONBOX_EDGE);
    gtk_container_set_border_width(GTK_CONTAINER (buttonbox), 5);
    gtk_box_pack_end(GTK_BOX(page_content_box), buttonbox, FALSE, FALSE, 0);
    return GTK_BUTTON_BOX(buttonbox);
}

static int
setup_notebook_pages(GncOptionsDialog* dlg, GtkBox* page_content_box,
                     const char* name)
{
    auto notebook{dlg->get_notebook()};
    auto page_count = gtk_notebook_page_num(GTK_NOTEBOOK(notebook),
                                            GTK_WIDGET(page_content_box));

    if (dlg->get_page_list_view())
    {
        /* Build the matching list item for selecting from large page sets */
        auto view = GTK_TREE_VIEW(dlg->get_page_list_view());
        auto list = GTK_LIST_STORE(gtk_tree_view_get_model(view));

        PINFO("Page name is %s and page_count is %d", name, page_count);
        GtkTreeIter iter;
        gtk_list_store_append(list, &iter);
        gtk_list_store_set(list, &iter,
                           PAGE_NAME, _(name),
                           PAGE_INDEX, page_count,
                           -1);

        if (page_count > MAX_TAB_COUNT - 1)   /* Convert 1-based -> 0-based */
        {
            gtk_widget_show(dlg->get_page_list());
            gtk_notebook_set_show_tabs(GTK_NOTEBOOK(notebook), FALSE);
            gtk_notebook_set_show_border(GTK_NOTEBOOK(notebook), FALSE);
        }
        else
            gtk_widget_hide(dlg->get_page_list());

    }
    return page_count;
}

static int
dialog_append_page(GncOptionsDialog* dlg, GncOptionSectionPtr& section)
{
    auto name = section->get_name().c_str();
    if (!name || *name == '\0')
        return -1;

    if (strncmp(name, "__", 2) == 0)
        return -1;

    auto page_label = gtk_label_new(_(name));
    PINFO("Page_label is %s", _(name));
    gtk_widget_show(page_label);

    /* Build this options page */
    auto page_content_box = create_content_box();
    auto options_box = create_options_box(page_content_box);

    /* Create all the options */
    size_t row = 0;
    section->foreach_option(
        [options_box, &row](GncOption& option) {
            g_object_set_data (G_OBJECT(options_box), "options-grid-row",
                               GINT_TO_POINTER(row));
            gnc_option_set_ui_widget(option, GTK_GRID(options_box), row);
            ++row;
        });

    /* Add a button box at the bottom of the page */
    auto buttonbox = create_reset_button_box(page_content_box);
    /* The reset button on each option page */
    auto reset_button = gtk_button_new_with_label (_("Reset defaults"));
    gtk_widget_set_tooltip_text(reset_button,
                                _("Reset all values to their defaults."));

    g_signal_connect(G_OBJECT(reset_button), "clicked",
                     G_CALLBACK(dialog_reset_cb), dlg);
    g_object_set_data(G_OBJECT(reset_button), "section",
                      static_cast<void*>(section.get()));
    gtk_box_pack_end(GTK_BOX(buttonbox), reset_button, FALSE, FALSE, 0);
    gtk_widget_show_all(GTK_WIDGET(page_content_box));
    gtk_notebook_append_page(GTK_NOTEBOOK(dlg->get_notebook()),
                             GTK_WIDGET(page_content_box), page_label);

    /* Switch to selection from a list if the page count threshold is reached */
    /* Run any callbacks on the default widget values. */
    section->foreach_option(
        [](GncOption& option) {
            gnc_option_changed_option_cb(nullptr, &option);
        });
    return setup_notebook_pages(dlg, page_content_box, name);
}

/**
 * Populate the dialog's notebook with the contents of odb.
 *
 * @param odb         - option database to use                       *
 * @param show_dialog - should dialog be made visible or not         *
 */
void
GncOptionsDialog::build_contents(GncOptionDB  *odb, bool show_dialog)
{
    gint default_page = -1;

    g_return_if_fail (odb != NULL);

    m_option_db = odb;

    auto default_section = odb->get_default_section();

    PINFO("Default Section name is %s",
          default_section ? default_section->get_name().c_str() : "NULL");

    odb->foreach_section(
        [this, default_section, &default_page]
        (GncOptionSectionPtr& section) {
            auto page = dialog_append_page(this, section);
            if (default_section && section.get() == default_section)
                default_page = page;
        });

    gtk_notebook_popup_enable(GTK_NOTEBOOK(m_notebook));
    if (default_page >= 0)
    {
        /* Find the page list and set the selection to the default page */
        auto selection{gtk_tree_view_get_selection(GTK_TREE_VIEW(m_page_list_view))};
        GtkTreeIter iter;

        auto model{gtk_tree_view_get_model(GTK_TREE_VIEW(m_page_list_view))};
        gtk_tree_model_iter_nth_child(model, &iter, NULL, default_page);
        gtk_tree_selection_select_iter (selection, &iter);
        gtk_notebook_set_current_page(GTK_NOTEBOOK(m_notebook), default_page);
    }
    dialog_changed_internal(m_window, FALSE);
    if (show_dialog)
        gtk_widget_show(m_window);
}

void GncOptionsDialog::call_apply_cb() noexcept
{
    auto close_cb = m_close_cb;

    m_close_cb = nullptr;
    if (m_apply_cb)
        (m_apply_cb)(this, m_apply_cb_data);
    m_close_cb = close_cb;
    set_sensitive(false);
}

void GncOptionsDialog::call_help_cb() noexcept
{
    if (m_help_cb)
        (m_help_cb)(this, m_help_cb_data);
}

void GncOptionsDialog::call_close_cb() noexcept
{
    if (m_close_cb)
    {
        gtk_window_close(GTK_WINDOW(m_window));
        (m_close_cb)(this, m_close_cb_data);
    }
    else
    {
        gtk_widget_hide(m_window);
    }
}

void GncOptionsDialog::call_book_help_cb() noexcept
{
/*    if (m_book_options_help_cb)
        (m_book_options_help_cb)(this, m_book_options_help_cb_data);
*/
}

void GncOptionsDialog::call_style_sheet_help_cb() noexcept
{
/*
    if (m_style_sheet_help_cb)
        (m_style_shet_help_cb)(this, m_style_sheet_help_cb_data);
*/
}

// Help button signal handler
static void
dialog_help_button_cb(GtkWidget * widget, GncOptionsDialog *win)
{
    win->call_help_cb();
}

// Cancel/close button clicked signal handler
static void
dialog_cancel_button_cb(GtkWidget * widget, GncOptionsDialog *win)
{
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(win->get_widget()));
    win->call_close_cb();
}

// Apply button clicked signal handler
static void
dialog_apply_button_cb(GtkWidget * widget, GncOptionsDialog *win)
{
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(win->get_widget()));
    win->call_apply_cb();
}

// OK Button clicked signal handler
static void
dialog_ok_button_cb(GtkWidget * widget, GncOptionsDialog *win)
{
    win->call_apply_cb();
    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(win->get_widget()));
    win->call_close_cb();
}

// "destroy" signal handler
static void
dialog_destroy_cb (GtkWidget *object, GncOptionsDialog *win)
{
    win->call_close_cb();
}

// "key_press_event" signal handler
static int
dialog_window_key_press_cb(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
    GncOptionsDialog *win = static_cast<decltype(win)>(data);

    if (event->keyval == GDK_KEY_Escape)
    {
        component_close_handler (win);
        return TRUE;
    }
    else
        return FALSE;
}

static void
dialog_reset_cb(GtkWidget * w, gpointer data)
{
    GncOptionsDialog *win = static_cast<decltype(win)>(data);
    gpointer val;
    bool dialog_changed = false;

    val = g_object_get_data(G_OBJECT(w), "section");
    g_return_if_fail (val);
    g_return_if_fail (win);

    auto section = static_cast<GncOptionSection*>(val);
    section->foreach_option(
        [&dialog_changed](GncOption& option) {
            if (option.is_changed())
            {
                option.reset_default_value();
                option.get_ui_item()->set_dirty(true);
                dialog_changed = true;
            }
            option.set_ui_item_from_option();
        });

    dialog_changed_internal (win->get_widget(), dialog_changed);
}

// changed signal handler
static void
dialog_list_select_cb (GtkTreeSelection *selection, gpointer data)
{
    GncOptionsDialog * win = static_cast<decltype(win)>(data);
    GtkTreeModel *list;
    GtkTreeIter iter;
    gint index = 0;

    if (!gtk_tree_selection_get_selected(selection, &list, &iter))
        return;
    gtk_tree_model_get(list, &iter,
                       PAGE_INDEX, &index,
                       -1);
    PINFO("Index is %d", index);
    gtk_notebook_set_current_page(GTK_NOTEBOOK(win->get_notebook()), index);
}

static void
component_close_handler (gpointer data)
{
    GncOptionsDialog *win = static_cast<decltype(win)>(data);
    dialog_cancel_button_cb (NULL, win);
}

/** Constructs a GncOptionsDialog
 *
 * Based on the description in the GtkBuilder file. Initializes signals.
 * Two component classes might be used, DIALOG_BOOK_OPTIONS_CM_CLASS or DIALOG_OPTIONS_CM_CLASS of which the latter is the default.
 *
 * @param modal: If true the "Apply" button is hidden. It doesn't make the dialog run in its own event loop so it's not truly modal.
 * @param title: The title that will appear in the dialog's title bar.
 * @param component_class: For registering the dialog in the component manager.
 * @param parent: The widget for which the dialog will be transient-for.
 */
GncOptionsDialog::GncOptionsDialog(bool modal, const char* title,
                             const char* component_class,
                           GtkWindow *parent) :
    m_component_class{component_class ? component_class : DIALOG_OPTIONS_CM_CLASS}
{
    auto builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-options.glade", "gnucash_options_window");
    m_window = GTK_WIDGET(gtk_builder_get_object (builder, "gnucash_options_window"));
    g_object_ref(m_window);
    m_page_list = GTK_WIDGET(gtk_builder_get_object (builder, "page_list_scroll"));
    g_object_set_data(G_OBJECT(m_window), "optionwin", this);

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(m_window), "gnc-id-options");

    /* Page List */

    m_page_list_view = GTK_WIDGET(gtk_builder_get_object (builder, "page_list_treeview"));

    auto view = GTK_TREE_VIEW(m_page_list_view);

    auto store = gtk_list_store_new(NUM_COLUMNS, G_TYPE_INT, G_TYPE_STRING);
    gtk_tree_view_set_model(view, GTK_TREE_MODEL(store));
    g_object_unref(store);

    auto renderer = gtk_cell_renderer_text_new();
    auto column =
        gtk_tree_view_column_new_with_attributes(_("Page"), renderer,
                                                 "text", PAGE_NAME,
                                                 nullptr);
    gtk_tree_view_append_column(view, column);

    gtk_tree_view_column_set_alignment(column, 0.5);

    auto selection = gtk_tree_view_get_selection(view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_BROWSE);
    g_signal_connect (selection, "changed",
                      G_CALLBACK (dialog_list_select_cb), this);

    m_help_button = GTK_BUTTON(gtk_builder_get_object (builder, "helpbutton"));
    g_signal_connect(m_help_button, "clicked",
                     G_CALLBACK(dialog_help_button_cb), this);
    m_cancel_button = GTK_BUTTON(gtk_builder_get_object (builder, "cancelbutton"));
    g_signal_connect(m_cancel_button, "clicked",
                     G_CALLBACK(dialog_cancel_button_cb), this);
    m_apply_button = GTK_BUTTON(gtk_builder_get_object (builder, "applybutton"));
    g_signal_connect(m_apply_button, "clicked",
                     G_CALLBACK(dialog_apply_button_cb), this);
    m_ok_button = GTK_BUTTON(gtk_builder_get_object (builder, "okbutton"));
    g_signal_connect(m_ok_button, "clicked",
                     G_CALLBACK(dialog_ok_button_cb), this);

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func,
                                      this);

    // when added to a page of the hierarchy assistant there will be no parent
    if (parent)
        gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(m_window),
                                 parent);

    if (title)
        gtk_window_set_title(GTK_WINDOW(m_window), title);

    /* modal */
    if (modal)
        gtk_widget_hide (GTK_WIDGET(m_apply_button));

    /* glade doesn't support a notebook with zero pages */
    auto hbox = GTK_WIDGET(gtk_builder_get_object (builder,
                                                   "notebook_placeholder"));
    m_notebook = gtk_notebook_new();

    gtk_widget_set_vexpand (m_notebook, TRUE);

    gtk_widget_show(m_notebook);
    gtk_box_pack_start(GTK_BOX(hbox), m_notebook, TRUE, TRUE, 5);

    auto component_id = gnc_register_gui_component (m_component_class,
                                                    nullptr,
                                                    component_close_handler,
                                                    this);
    gnc_gui_component_set_session (component_id, gnc_get_current_session());

    g_signal_connect (m_window, "destroy", G_CALLBACK(dialog_destroy_cb), this);

    g_signal_connect (m_window, "key_press_event",
                      G_CALLBACK(dialog_window_key_press_cb), this);

    g_object_unref(G_OBJECT(builder));
}

GncOptionsDialog::~GncOptionsDialog()
{
    if (m_destroying)
        return;
    m_destroying = true;
    gnc_unregister_gui_component_by_data(m_component_class, this);
    g_signal_handlers_disconnect_by_func(m_window, (gpointer)dialog_destroy_cb, this);
    g_signal_handlers_disconnect_by_func(m_window, (gpointer)dialog_window_key_press_cb, this);
    m_option_db->foreach_section([](GncOptionSectionPtr& section)
    {
        section->foreach_option([](GncOption& option)
        {
            option.set_ui_item(std::unique_ptr<GncOptionUIItem>(nullptr));
        });
    });
    g_object_unref(m_window);
}

void
GncOptionsDialog::set_apply_cb(GncOptionsDialogCallback cb, gpointer data) noexcept
{
    m_apply_cb = cb;
    m_apply_cb_data = data;
}

void
GncOptionsDialog::set_help_cb(GncOptionsDialogCallback cb, gpointer data) noexcept
{
    m_help_cb = cb;
    m_help_cb_data = data;
}

void
GncOptionsDialog::set_close_cb( GncOptionsDialogCallback cb, gpointer data) noexcept
{
    m_close_cb = cb;
    m_close_cb_data = data;
}


static void
gnc_book_options_help_cb (GncOptionsDialog *win, gpointer dat)
{
    gnc_gnome_help (GTK_WINDOW (win->get_widget()), DF_MANUAL, DL_BOOK_OPTIONS);
}

void
GncOptionsDialog::set_book_help_cb() noexcept
{
    set_help_cb((GncOptionsDialogCallback)gnc_book_options_help_cb, nullptr);
}

static void
gnc_style_sheet_options_help_cb (GncOptionsDialog *win, gpointer dat)
{
    gnc_gnome_help (GTK_WINDOW(win->get_widget()), DF_MANUAL, DL_STYLE_SHEET);
}

void
GncOptionsDialog::set_style_sheet_help_cb () noexcept
{
    set_help_cb ((GncOptionsDialogCallback)gnc_style_sheet_options_help_cb,
                 nullptr);
}



void
gnc_options_dialog_set_new_book_option_values (GncOptionDB *odb)
{
    if (!odb) return;
    auto num_split_action = gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL,
                                               GNC_PREF_NUM_SOURCE);
    if (num_split_action)
    {
        auto option{odb->find_option(OPTION_SECTION_ACCOUNTS,
                                    OPTION_NAME_NUM_FIELD_SOURCE)};
        auto num_source_button{option_get_gtk_widget(option)};
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (num_source_button),
                                      num_split_action);
    }
}


/********************************************************************\
 * dialog-options.hpp -- GNOME option handling                      *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
 * Copyright (c) 2011 Robert Fewell                                 *
 * Copyright 2019-2021 John Ralls <jralls@ceridwen.us>              *
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
/** @addtogroup GUI
    @{ */
/** @addtogroup GuiOptions Options Dialog
    @{ */

#ifndef GNC_DIALOG_OPTIONS_HPP_
#define GNC_DIALOG_OPTIONS_HPP_

#include <vector>

#include "gnc-option-uitype.hpp"
#include <gnc-option-ui.hpp>
#include <gnc-optiondb.hpp>
#include <gtk/gtk.h>

class GncOptionsDialog;

typedef void (* GncOptionsDialogCallback)(GncOptionsDialog*, void* data);

class GncOptionsDialog
{
    GtkWidget  * m_window;
    GtkWidget  * m_notebook;
    GtkWidget  * m_page_list_view;
    GtkWidget  * m_page_list;
    GtkButton  * m_help_button;
    GtkButton  * m_cancel_button;
    GtkButton  * m_apply_button;
    GtkButton  * m_ok_button;

    GncOptionsDialogCallback m_apply_cb;
    gpointer             m_apply_cb_data;

    GncOptionsDialogCallback m_help_cb;
    gpointer             m_help_cb_data;

    GncOptionsDialogCallback m_close_cb;
    gpointer             m_close_cb_data;

    /* Hold onto this for a complete reset */
    GncOptionDB* m_option_db;

    /* Hold on to this to unregister the right class */
    const char* m_component_class;

    /* widget being destroyed */
    bool m_destroying{false};

public:
    GncOptionsDialog(const char* title, GtkWindow* parent) :
        GncOptionsDialog(false, title, nullptr, parent) {}
    GncOptionsDialog(bool modal, const char* title, const char* component_class,
                 GtkWindow* parent);
    GncOptionsDialog(const GncOptionsDialog&) = default;
    GncOptionsDialog(GncOptionsDialog&&) = default;
    ~GncOptionsDialog();

    GtkWidget* get_widget() const noexcept { return m_window; }
    GtkWidget* get_page_list() const noexcept { return m_page_list; }
    GtkWidget* get_page_list_view() const noexcept { return m_page_list_view; }
    GtkWidget* get_notebook() const noexcept { return m_notebook; }
    GncOptionDB* get_option_db() noexcept { return m_option_db; }
    inline void build_contents(GncOptionDB* odb){
        build_contents(odb, true); }
    void build_contents(GncOptionDB* odb, bool show_dialog);
    void set_sensitive(bool sensitive) noexcept;
    void changed() noexcept;
    void set_apply_cb(GncOptionsDialogCallback, void* cb_data) noexcept;
    void call_apply_cb() noexcept;
    void set_help_cb(GncOptionsDialogCallback, void* cb_data) noexcept;
    void call_help_cb() noexcept;
    void set_close_cb(GncOptionsDialogCallback, void* cb_data) noexcept;
    void call_close_cb() noexcept;
    void set_book_help_cb() noexcept;
    void call_book_help_cb() noexcept;
    void set_style_sheet_help_cb() noexcept;
    void call_style_sheet_help_cb() noexcept;
};

/**
 * Set the initial values of new book options to values specified in user
 * preferences.

 * Nothing to do with GncOptionsDialog, but it depends on Gtk and s used in
 * both assistant-hierarchy and gnc-main-window.
 * @param odb: The book's options database.
 */
void gnc_options_dialog_set_new_book_option_values (GncOptionDB *odb);

#endif // GNC_DIALOG_OPTIONS_HPP_
/** @}
    @} */

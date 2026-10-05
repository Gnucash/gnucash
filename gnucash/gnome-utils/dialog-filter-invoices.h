/*
 * dialog-filter-invoices.h -- Filter dialog for invoices 
 * Copyright (C) 2026 Roy Hansen 
 * Author: Roy Hansen <roy@royhansen.no>
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

#ifndef GNC_FILTER_INVOICES_DIALOG_H_
#define GNC_FILTER_INVOICES_DIALOG_H_

#include <memory>
#include <functional>
#include "gnc-plugin.h"

class GncFilterInvoicesDialog {
    class FilterDialog;

    std::unique_ptr<FilterDialog> filter; 
    GncPluginPage                 *plugin_page = nullptr;
    GncOwnerType                  owner_type;

    using Callback = std::function<void()>;

public:
    GncFilterInvoicesDialog (GncPluginPage &page, GncOwnerType &owner,
                            Callback apply_cb);

    ~GncFilterInvoicesDialog ();

    Callback apply_filter = [](){
        g_warn_if_fail (true);
    };

    void create_dialog ();

    QofQuery *make_filter ();

    void
    free_filter (QofQuery *query)
    {
        qof_query_destroy (query);
    }
};

#endif /* GNC_FILTER_INVOICES_DIALOG_H_ */

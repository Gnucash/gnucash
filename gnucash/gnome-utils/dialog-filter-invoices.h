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
 * along with this program; if not, see
 * <https://www.gnu.org/licenses/>.
 */

#ifndef GNC_FILTER_INVOICES_DIALOG_H_
#define GNC_FILTER_INVOICES_DIALOG_H_

#include <memory>
#include <functional>
#include "gnc-plugin.h"

class GncFilterInvoicesDialogInternal;

class GncFilterInvoicesDialog {
    std::unique_ptr<GncFilterInvoicesDialogInternal> m_filter; 

public:
    enum OwnerType {
        TYPE_CUSTOMER,
        TYPE_VENDOR,
        TYPE_EMPLOYEE
    };

    GncFilterInvoicesDialog (GncPluginPage &plugin_page,
                             OwnerType owner_type,
                             std::function<void()> apply_cb);

    ~GncFilterInvoicesDialog ();

    void dialog_open ();

    QofQuery *create_qof_query (bool empty = false);
};

#endif /* GNC_FILTER_INVOICES_DIALOG_H_ */

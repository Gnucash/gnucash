/**      utils.h
*
*      This program is free software; you can redistribute it and/or modify
*      it under the terms of the GNU General Public License as published by
*      the Free Software Foundation; either version 2 of the License, or
*      (at your option) any later version.
*
*      This program is distributed in the hope that it will be useful,
*      but WITHOUT ANY WARRANTY; without even the implied warranty of
*      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*      GNU General Public License for more details.
*
*      You should have received a copy of the GNU General Public License
*      along with this program; if not, write to the Free Software
*      Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*      MA 02110-1301, USA.
*
* Developed from code written by Sebastian Held <sebastian.held@gmx.de>
* as part of his invoice importer module
* Mike Evans <mikee@saxicola.co.uk>
*
**********************************************************************/


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <glib/gi18n.h>
#include <regex.h>
#include <glib.h>
#include <glib/gstdio.h>

#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnome-utils/gnc-gui-query.h"
#include "gncAddress.h"
#include "gncCustomerP.h"
#include "gncCustomer.h"
#include "gncInvoice.h"
#include "gnc-exp-parser.h"

// query
#include "QueryCore.h"
#include "QueryNew.h"
#include "GNCId.h"



#ifndef GNC_PLUGIN_invoice_import_invoice_import_H
#define GNC_PLUGIN_invoice_import_invoice_import_H

GncCustomer * search_customer_on_id  (QofBook *book, const gchar *id);
GncInvoice  * search_invoice_on_id   (QofBook *book, const gchar *id);
GncInvoice  * search_bill_on_id   (QofBook *book, const gchar *id);

#endif

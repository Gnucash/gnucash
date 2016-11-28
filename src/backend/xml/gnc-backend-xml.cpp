/********************************************************************
 * gnc-backend-xml.c: load and save data to XML files               *
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
/** @file gnc-backend-xml.c
 *  @brief load and save data to XML files
 *  @author Copyright (c) 2000 Gnumatic Inc.
 *  @author Copyright (c) 2002 Derek Atkins <warlord@MIT.EDU>
 *  @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an ordinary Unix filesystem file.
 */
extern "C"
{
#include "config.h"


#include <glib.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <libintl.h>
#include <locale.h>
#include <fcntl.h>
#include <limits.h>
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#else
# ifdef _MSC_VER
    typedef int ssize_t;
# endif
#endif
#include <errno.h>
#include <string.h>
#ifdef HAVE_DIRENT_H
# include <dirent.h>
#endif
#include <time.h>
#ifdef G_OS_WIN32
# include <io.h>
# define close _close
# define mktemp _mktemp
# define read _read
# define write _write
#endif
#include "platform.h"
#if COMPILER(MSVC)
# define g_fopen fopen
# define g_open _open
#endif

#include "qof.h"
#include "gnc-engine.h"
#include <gnc-uri-utils.h>
#include "gnc-prefs.h"

#ifndef HAVE_STRPTIME
# include "strptime.h"
#endif
}

#include <gnc-backend-prov.hpp>
#include "gnc-backend-xml.h"
#include <qof-backend.hpp>
#include "gnc-xml-backend.hpp"
#include "gnc-xml-helper.h"
#include "io-gncxml-v2.h"
#include "io-gncxml.h"

#include "gnc-address-xml-v2.h"
#include "gnc-bill-term-xml-v2.h"
#include "gnc-customer-xml-v2.h"
#include "gnc-employee-xml-v2.h"
#include "gnc-entry-xml-v2.h"
#include "gnc-invoice-xml-v2.h"
#include "gnc-job-xml-v2.h"
#include "gnc-order-xml-v2.h"
#include "gnc-owner-xml-v2.h"
#include "gnc-tax-table-xml-v2.h"
#include "gnc-vendor-xml-v2.h"

static QofLogModule log_module = GNC_MOD_BACKEND;


struct QofXmlBackendProvider : public QofBackendProvider
{
    QofXmlBackendProvider (const char* name, const char* type) :
        QofBackendProvider {name, type} {}
    QofXmlBackendProvider(QofXmlBackendProvider&) = delete;
    QofXmlBackendProvider operator=(QofXmlBackendProvider&) = delete;
    QofXmlBackendProvider(QofXmlBackendProvider&&) = delete;
    QofXmlBackendProvider operator=(QofXmlBackendProvider&&) = delete;
    ~QofXmlBackendProvider () = default;
    QofBackend* create_backend(void) { return new GncXmlBackend; }
    bool type_check(const char* type);

};

bool
QofXmlBackendProvider::type_check (const char *uri)
{
    struct stat sbuf;
    int rc;
    FILE* t;
    gchar* filename;
    QofBookFileType xml_type;
    gboolean result;

    if (!uri)
    {
        return FALSE;
    }

    filename = gnc_uri_get_path (uri);
    if (0 == g_strcmp0 (filename, QOF_STDOUT))
    {
        result = FALSE;
        goto det_exit;
    }
    t = g_fopen (filename, "r");
    if (!t)
    {
        PINFO (" new file");
        result = TRUE;
        goto det_exit;
    }
    fclose (t);
    rc = g_stat (filename, &sbuf);
    if (rc < 0)
    {
        result = FALSE;
        goto det_exit;
    }
    if (sbuf.st_size == 0)
    {
        PINFO (" empty file");
        result = TRUE;
        goto det_exit;
    }
    xml_type = gnc_is_xml_data_file_v2 (filename, NULL);
    if ((xml_type == GNC_BOOK_XML2_FILE) ||
        (xml_type == GNC_BOOK_XML1_FILE) ||
        (xml_type == GNC_BOOK_POST_XML2_0_0_FILE))
    {
        result = TRUE;
        goto det_exit;
    }
    PINFO (" %s is not a gnc XML file", filename);
    result = FALSE;

det_exit:
    g_free (filename);
    return result;
}

/* ================================================================= */

static void
business_core_xml_init (void)
{
    /* Initialize our pointers into the backend subsystem */
    gnc_address_xml_initialize ();
    gnc_billterm_xml_initialize ();
    gnc_customer_xml_initialize ();
    gnc_employee_xml_initialize ();
    gnc_entry_xml_initialize ();
    gnc_invoice_xml_initialize ();
    gnc_job_xml_initialize ();
    gnc_order_xml_initialize ();
    gnc_owner_xml_initialize ();
    gnc_taxtable_xml_initialize ();
    gnc_vendor_xml_initialize ();
}

#ifndef GNC_NO_LOADABLE_MODULES
G_MODULE_EXPORT void
qof_backend_module_init (void)
{
    gnc_module_init_backend_xml ();
}
#endif

void
gnc_module_init_backend_xml (void)
{
    const char* name {"GnuCash File Backend Version 2"};
    auto prov = QofBackendProvider_ptr(new QofXmlBackendProvider{name, "xml"});

    qof_backend_register_provider(std::move(prov));
    prov = QofBackendProvider_ptr(new QofXmlBackendProvider{name, "file"});
    qof_backend_register_provider(std::move(prov));

    /* And the business objects */
    business_core_xml_init ();
}

/* ========================== END OF FILE ===================== */

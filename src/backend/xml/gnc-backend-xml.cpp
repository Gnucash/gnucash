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
#include <qofbackend-p.h>
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
    QofBackend* create_backend(void);
    bool type_check(const char* type);

};


static void
xml_session_begin (QofBackend* qof_be, QofSession* session,
                   const char* book_id, gboolean ignore_lock,
                   gboolean create, gboolean force)
{
    GncXmlBackend* xml_be = (GncXmlBackend*) qof_be;

    ENTER (" ");
    xml_be->session_begin(session, book_id, ignore_lock, create, force);
    LEAVE (" ");
    return;
}

/* ================================================================= */

static void
xml_session_end (QofBackend* qof_be)
{
    GncXmlBackend* xml_be = (GncXmlBackend*)qof_be;
    ENTER (" ");
    xml_be->session_end();
    LEAVE (" ");
}

static void
xml_destroy_backend (QofBackend* qof_be)
{
    delete reinterpret_cast<GncXmlBackend*>(qof_be);
}

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

static void
xml_sync_all (QofBackend* qof_be, QofBook* book)
{
    GncXmlBackend* xml_be = reinterpret_cast<decltype(xml_be)>(qof_be);
    xml_be->sync(book);
    ENTER ("book=%p, xml_be->m_book=%p", book, xml_be->get_book());

    LEAVE ("book=%p", book);
}


static void
xml_begin_edit (QofBackend* qof_be, QofInstance* inst)
{
    GncXmlBackend* xml_be = (GncXmlBackend*) qof_be;
    xml_be->begin(inst);
}

static void
xml_rollback_edit (QofBackend* qof_be, QofInstance* inst)
{

    GncXmlBackend* xml_be = (GncXmlBackend*) qof_be;
    xml_be->rollback(inst);
}

/* ---------------------------------------------------------------------- */


/* Load financial data from a file into the book, automatically
   detecting the format of the file, if possible.  Return FALSE on
   error, and set the error parameter to indicate what went wrong if
   it's not NULL.  This function does not manage file locks in any
   way. */

static void
gnc_xml_be_load_from_file (QofBackend* qof_be, QofBook* book,
                           QofBackendLoadType loadType)
{
    GncXmlBackend* xml_be = (GncXmlBackend*) qof_be;
    xml_be->load(book, loadType);
}

/* ---------------------------------------------------------------------- */



static void
gnc_xml_be_write_accounts_to_file (QofBackend* qof_be, QofBook* book)
{
    auto datafile = ((GncXmlBackend*)qof_be)->get_filename();
    gnc_book_write_accounts_to_xml_file_v2 (qof_be, book, datafile);
}

/* ================================================================= */

QofBackend*
QofXmlBackendProvider::create_backend(void)
{

    auto xml_be = new GncXmlBackend;
    auto qof_be = xml_be->get_qof_be();
    qof_be->session_begin = xml_session_begin;
    qof_be->session_end = xml_session_end;
    qof_be->destroy_backend = xml_destroy_backend;

    qof_be->load = gnc_xml_be_load_from_file;

    /* The file backend treats accounting periods transactionally. */
    qof_be->begin = xml_begin_edit;
    qof_be->commit = NULL;
    qof_be->rollback = xml_rollback_edit;

    qof_be->sync = xml_sync_all;

    qof_be->export_fn = gnc_xml_be_write_accounts_to_file;

    return qof_be;
}

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

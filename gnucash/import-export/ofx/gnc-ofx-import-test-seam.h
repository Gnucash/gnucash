/*
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 */

/********************************************************************\
 * gnc-ofx-import-test-seam.h -- internal production lifecycle seam *
 ********************************************************************/
#ifndef GNC_OFX_IMPORT_TEST_SEAM_H
#define GNC_OFX_IMPORT_TEST_SEAM_H

#include <gtk/gtk.h>

#include "gnc-ofx-import-teardown.h"

G_BEGIN_DECLS

typedef struct GncOfxImportTestSeam GncOfxImportTestSeam;

/** Construct the same ofx_info/lifecycle pair used by the real file-import
 * entry point, without opening a file chooser or invoking libofx. */
GncOfxImportTestSeam *gnc_ofx_import_test_seam_new (
    GApplication *application);
void gnc_ofx_import_test_seam_free (GncOfxImportTestSeam *seam);

/** Register the real OfxImportState used by the corresponding asynchronous
 * account or commodity callback path. */
gboolean gnc_ofx_import_test_begin_account_state (GncOfxImportTestSeam *seam);
gboolean gnc_ofx_import_test_begin_commodity_state (GncOfxImportTestSeam *seam);

/** Invoke the real parent-destroy and asynchronous cancel callbacks. */
void gnc_ofx_import_test_parent_destroy (GncOfxImportTestSeam *seam);
void gnc_ofx_import_test_complete_account_cancel (GncOfxImportTestSeam *seam);
void gnc_ofx_import_test_complete_commodity_cancel (GncOfxImportTestSeam *seam);

/** Populate the actual ofx_info matcher and raw-transaction ownership slots. */
gboolean gnc_ofx_import_test_create_matcher (GncOfxImportTestSeam *seam);
gboolean gnc_ofx_import_test_add_open_transaction (GncOfxImportTestSeam *seam);

/** Attach the real reconcile-destroy continuation to a returned owned window. */
GtkWindow *gnc_ofx_import_test_attach_reconcile (GncOfxImportTestSeam *seam);

guint gnc_ofx_import_test_metadata_cleanup_calls (
    const GncOfxImportTestSeam *seam);
guint gnc_ofx_import_test_payload_destroy_calls (
    const GncOfxImportTestSeam *seam);
guint gnc_ofx_import_test_reconcile_calls (
    const GncOfxImportTestSeam *seam);
GncImportOperationTeardownResult gnc_ofx_import_test_cleanup_result (
    const GncOfxImportTestSeam *seam);

G_END_DECLS

#endif

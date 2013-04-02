/*
 * test-qif.c -- Test the QIF Import routines.
 *
 * Created by:	Derek Atkins <derek@ihtfp.com>
 * Copyright (c) 2003 Derek Atkins <warlord@MIT.EDU>
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

#include <glib.h>
#include <libguile.h>

#include "gnc-module.h"
#include "qif-import.h"
#include "qif-import-p.h"	/* Let's test some internal stuff, too */

#include "test-stuff.h"

/* XXX */
extern void qif_object_init(void);

static QifContext
test_qif_load_file(QifContext ctx, const char *filename,
                   gint txn_count, gint acct_count, gboolean needs_acct)
{
    QifContext file;

    printf("qif loading \"%s\"...\n", filename);
    file = qif_file_new(ctx, filename);
    do_test(file != NULL, "failed to read file");
    if (!file) return NULL;

    do_test(qif_object_list_count(file, QIF_O_TXN) == txn_count,
            "Transaction count didn't match");
    do_test(qif_object_map_count(file, QIF_O_ACCOUNT) == acct_count,
            "Account count didn't match");
    do_test(qif_file_needs_account(file) == needs_acct,
            "Needs account flad didn't match");

    return file;
}

static void
test_qif(void)
{
    QifContext ctx, file;
    char *filename;
    const char *location = g_getenv("GNC_TEST_FILES");
    int i;

    ctx = qif_context_new();
    do_test(ctx != NULL, "failed to create the qif context");
    if (!ctx) return;

    if (!location)
        location = "test-files";

    for (i = 0; i < 1; i++)
    {
        filename = g_strdup_printf("%s/%s", location, "test-1-bank-txn.qif");
        file = test_qif_load_file(ctx, filename, 1, 0, TRUE);
        g_free(filename);
        if (!file) continue;

        if (qif_file_needs_account(file))
            qif_file_set_default_account(file, "test-1-bank-txn");

        do_test(qif_file_needs_account(file) == FALSE,
                "'Needs account' flag not cleared properly");

        do_test(qif_file_parse(file, NULL) == QIF_E_OK,
                "file failed to parse.");
    }

    qif_context_destroy(ctx);

    success("QIF test successful");
}

static void
main_helper(void *closure, int argc, char **argv)
{
    qif_object_init();		/* XXX:FIXME */
    test_qif();
    print_test_results();
    exit(get_rv());
}

int
main(int argc, char **argv)
{
    scm_boot_guile(argc, argv, main_helper, NULL);
    return 0;
}


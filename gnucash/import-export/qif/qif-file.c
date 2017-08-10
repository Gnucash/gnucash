/*
 * qif-file.c -- parse a QIF File into its pieces
 *
 * Written by:  Derek Atkins  <derek@@ihtfp.com>
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


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <glib.h>
#include <glib/gstdio.h>
#include <string.h>

#include "gnc-engine.h"

#include "qif-import-p.h"
#include "qif-objects-p.h"

static QofLogModule log_module = GNC_MOD_IMPORT;


static QifLine
qif_make_line(const char* buf, gint lineno)
{
    QifLine line;
    g_return_val_if_fail(buf && *buf, NULL);

    line = g_new0(struct _QifLine, 1);
    line->type = *buf;
    line->lineno = lineno;
    line->line = g_strdup(buf + 1);

    return line;
}

void
qif_record_destroy(GList *record)
{
    GList *node;
    QifLine line;

    for (node = record; node; node = node->next)
    {
        line = node->data;
        g_free(line->line);
        g_free(line);
    }

    g_list_free(record);
}

/* This returns a record, which is a bunch of QifLines, ending
 * with a line with just a '^'.  If it finds a line that begins
 * with a !, then destroy the current record state, set the "found_bangtype",
 * and return NULL.
 */
static GList *
qif_make_record(QifContext ctx, char *buf, size_t bufsiz, gboolean *found_bangtype)
{
    GList *record = NULL;
    QifLine line;

    g_return_val_if_fail(ctx, NULL);
    g_return_val_if_fail(buf, NULL);
    g_return_val_if_fail(found_bangtype, NULL);

    *found_bangtype = FALSE;

    while (fgets(buf, bufsiz, ctx->fp) != NULL)
    {

        /* increment the line number */
        ctx->lineno++;

        /* strip start/end whitespace */
        g_strstrip(buf);

        /* if there is nothing left in the string, ignore it */
        if (strlen(buf) == 0)
            continue;

        /* If this is a bangline, then set the flag, clear our state, and return NULL */
        if (*buf == '!')
        {
            *found_bangtype = TRUE;
            break;
        }

        /* See if this is an End of Record marker */
        if (*buf == '^')
        {
            /* Yep.  If we've got a record then break and return ... */
            if (record)
                break;
            /* ... otherwise just continue reading (i.e. ignore empty records) */
            else
                continue;
        }

        /* otherwise, add the line to the list */
        line = qif_make_line(buf, ctx->lineno);
        if (line)
            record = g_list_prepend(record, line);

        /* and continue... */
    }

    /* If we found a bangtype, destroy anything we've collected */
    if (*found_bangtype)
    {
        if (record)
            PERR("error loading file: incomplete record at line %d", ctx->lineno);

        qif_record_destroy(record);
        record = NULL;
    }

    return g_list_reverse(record);
}

/* read a qif file and parse it, line by line
 * return QIF_E_OK on success or some other QIF Error.
 */
static QifError
qif_read_file(QifContext ctx, FILE *f)
{
    char buf[BUFSIZ];
    GList *record;
    gboolean found_bang;
    QifError err = QIF_E_OK;

    g_return_val_if_fail(ctx, QIF_E_BADARGS);
    g_return_val_if_fail(f, QIF_E_BADARGS);

    ctx->fp = f;
    ctx->lineno = -1;

    do
    {
        found_bang = FALSE;
        record = qif_make_record(ctx, buf, sizeof(buf), &found_bang);

        /* If we got a record, process it */
        if (record)
        {
            if (!ctx->handler || !ctx->handler->parse_record)
            {
                PERR("Trying to process QIF record without a handler at %d", ctx->lineno);
            }
            else
            {
                err = ctx->handler->parse_record(ctx, record);
            }

            /* Now destroy it; we don't need it anymore */
            qif_record_destroy(record);
        }

        /* if we found a bangtype, process that */
        if (found_bang)
        {
            g_assert(*buf == '!');

            /* First, process the end of the last handler.  This could possibly
             * merge items into the context or perform some other operation
             */
            if (ctx->handler && ctx->handler->end)
            {
                err = ctx->handler->end(ctx);
                if (err != QIF_E_OK)
                    break;
            }

            /* Now process the bangtype (stored in buf) to set the new handler */
            qif_parse_bangtype(ctx, buf);
        }

    }
    while ((record || found_bang) && err == QIF_E_OK);

    /* Make sure to run any end processor */
    if (err == QIF_E_OK && ctx->handler && ctx->handler->end)
        err = ctx->handler->end(ctx);

    if (err == QIF_E_OK)
        qif_object_list_reverse(ctx, QIF_O_TXN);

    return err;
}

static QifError
qif_import_file(QifContext ctx, const char *filename)
{
    QifError err;
    FILE *fp;

    g_return_val_if_fail(ctx, QIF_E_BADARGS);
    g_return_val_if_fail(filename, QIF_E_BADARGS);
    g_return_val_if_fail(*filename, QIF_E_BADARGS);

    /* Open the file */
    fp = g_fopen(filename, "r");
    if (fp == NULL)
        return QIF_E_NOFILE;

    ctx->filename = g_strdup(filename);

    /* read the file */
    err = qif_read_file(ctx, fp);

    /* close the file */
    fclose(fp);

    return err;
}


QifContext
qif_file_new(QifContext ctx, const char *filename)
{
    QifContext fctx;

    g_return_val_if_fail(ctx, NULL);
    g_return_val_if_fail(filename, NULL);

    fctx = qif_context_new();

    /* we should assume that we've got a bank account... just in case.. */
    qif_parse_bangtype(fctx, "!type:bank");

    /* Open the file */
    if (qif_import_file(fctx, filename) != QIF_E_OK)
    {
        qif_context_destroy(fctx);
        fctx = NULL;
    }

    /* Return the new context */
    if (fctx)
    {
        ctx->files = g_list_prepend(ctx->files, fctx);
        fctx->parent = ctx;

        /* Make sure the file gets merged into the parent */
        ctx->parsed = FALSE;
    }

    return fctx;
}

QifError
qif_file_parse(QifContext ctx, gpointer ui_args)
{
    g_return_val_if_fail(ctx, QIF_E_BADARGS);
    g_return_val_if_fail(!qif_file_needs_account(ctx), QIF_E_BADSTATE);

    qif_parse_all(ctx, ui_args);
    ctx->parsed = TRUE;

    return QIF_E_OK;
}

gboolean
qif_file_needs_account(QifContext ctx)
{
    g_return_val_if_fail(ctx, FALSE);

    return ((ctx->parse_flags & QIF_F_TXN_NEEDS_ACCT) ||
            (ctx->parse_flags & QIF_F_ITXN_NEEDS_ACCT));
}

const char *
qif_file_filename(QifContext ctx)
{
    g_return_val_if_fail(ctx, NULL);
    return ctx->filename;
}

static void
set_txn_acct(gpointer obj, gpointer arg)
{
    QifTxn txn = obj;
    QifAccount acct = arg;

    if (!txn->from_acct)
        txn->from_acct = acct;
}

void
qif_file_set_default_account(QifContext ctx, const char *acct_name)
{
    QifAccount acct;

    g_return_if_fail(ctx);
    g_return_if_fail(acct_name);

    if (! qif_file_needs_account(ctx)) return;

    acct = find_or_make_acct(ctx, g_strdup(acct_name),
                             qif_parse_acct_type_guess(ctx->parse_type));

    qif_object_list_foreach(ctx, QIF_O_TXN, set_txn_acct, acct);

    qif_clear_flag(ctx->parse_flags, QIF_F_TXN_NEEDS_ACCT);
    qif_clear_flag(ctx->parse_flags, QIF_F_ITXN_NEEDS_ACCT);
}

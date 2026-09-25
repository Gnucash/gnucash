/********************************************************************\
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
 *                                                                  *
\********************************************************************/
/*
 * split-register.c
 * author Copyright (c) 1998-2000 Linas Vepstas <linas@linas.org>
 * author Copyright (c) 2000-2001 Dave Peticolas <dave@krondo.com>
 * author Copyright (c) 2017 Aaron Laws
 */
#include <config.h>

#include <glib.h>
#include <glib/gi18n.h>

#include "combocell.h"
#include "completioncell.h"
#include "datecell.h"
#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "split-register-p.h"
#include "gnc-date.h"
#include <gnc-hooks.h>
#include "gnc-ledger-display.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-warnings.h"
#include "split-register-copy-ops.h"
#include "numcell.h"
#include "pricecell.h"
#include "quickfillcell.h"
#include "recncell.h"
#include "split-register.h"
#include "split-register-control.h"
#include "split-register-layout.h"
#include "split-register-model.h"
#include "split-register-model-save.h"
#include "table-allgui.h"
#include "dialog-account.h"
#include "dialog-dup-trans.h"
#include "engine-helpers.h"
#include "qofbookslots.h"


/** static variables ******************************************************/

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_LEDGER;

/* The copied split or transaction, if any */
typedef struct
{
    GType ftype;
    union
    {
        FloatingSplit *fs;
        FloatingTxn *ft;
    };
    CursorClass cursor_class;
    GncGUID leader_guid;
    gint anchor_split_index;
} ft_fs_store;

static ft_fs_store copied_item = { 0, { NULL } };
/* Invalidates deferred paste requests when the process clipboard changes. */
static guint64 copied_item_generation;

typedef struct _SplitRegisterSaveRequest SplitRegisterSaveRequest;

typedef enum
{
    GNC_SPLIT_REGISTER_SAVE_SAVED,
    GNC_SPLIT_REGISTER_SAVE_FAILED,
    GNC_SPLIT_REGISTER_SAVE_DEFERRED
} GncSplitRegisterSaveResult;

/** static prototypes *****************************************************/

static gboolean gnc_split_register_save_to_copy_buffer (SplitRegister *reg,
                                                        FloatingTxn *ft,
                                                        FloatingSplit *fs,
                                                        gboolean use_cut_semantics);
static GncSplitRegisterSaveResult gnc_split_register_auto_calc
    (SplitRegister *reg, Split *split, SplitRegisterSaveRequest *request);
static void split_register_save_exchange_finished (SplitRegister *reg,
                                                   gboolean accepted,
                                                   gpointer user_data);
static void recalculate_shares (Split* split, SplitRegister* reg,
                                gnc_numeric value, gnc_numeric price,
                                gboolean value_changed);
static void recalculate_price (Split* split, SplitRegister* reg,
                               gnc_numeric value, gnc_numeric amount);
static void recalculate_value (Split* split, SplitRegister* reg,
                               gnc_numeric price, gnc_numeric amount,
                               gboolean shares_changed);


/** implementations *******************************************************/

struct _SplitRegisterSaveRequest
{
    GncSplitRegisterAsyncRequest base;
    gatomicrefcount ref_count;
    QofBook *book;
    gboolean do_commit;
    gboolean completed;
    GncSplitRegisterSaveCallback callback;
    gpointer user_data;
};

static void split_register_save_request_continue (SplitRegisterSaveRequest *request);

static SplitRegisterSaveRequest *
split_register_save_request_ref (SplitRegisterSaveRequest *request)
{
    g_atomic_ref_count_inc (&request->ref_count);
    return request;
}

static void
split_register_save_request_unref (SplitRegisterSaveRequest *request)
{
    if (g_atomic_ref_count_dec (&request->ref_count))
        g_free (request);
}

void
gnc_split_register_async_request_track (SplitRegister *reg,
                                        GncSplitRegisterAsyncRequest *request,
                                        GncSplitRegisterAsyncCancel cancel)
{
    SRInfo *info;

    g_return_if_fail (reg != NULL);
    g_return_if_fail (request != NULL);

    info = gnc_split_register_get_info (reg);
    g_return_if_fail (info != NULL);
    request->reg = reg;
    request->cancel = cancel;
    info->async_requests = g_list_prepend (info->async_requests, request);
}

void
gnc_split_register_async_request_untrack (GncSplitRegisterAsyncRequest *request)
{
    SRInfo *info;

    if (!request || !request->reg)
        return;

    info = gnc_split_register_get_info (request->reg);
    if (info)
        info->async_requests = g_list_remove (info->async_requests, request);
    request->reg = NULL;
    request->cancel = NULL;
}

static void
split_register_save_request_complete (SplitRegisterSaveRequest *request,
                                      gboolean saved)
{
    SplitRegister *reg;
    GncSplitRegisterSaveCallback callback;
    gpointer user_data;
    SRInfo *info;

    if (!request || request->completed)
        return;

    request->completed = TRUE;
    reg = request->base.reg;
    info = reg ? gnc_split_register_get_info (reg) : NULL;
    if (info && info->active_save_request == &request->base)
        info->active_save_request = NULL;
    if (reg && reg->table && reg->table->control)
        gnc_table_control_set_input_suspended (reg->table->control, FALSE);
    gnc_split_register_async_request_untrack (&request->base);

    callback = request->callback;
    user_data = request->user_data;
    request->callback = NULL;
    request->user_data = NULL;
    if (callback)
        callback (reg, saved, user_data);
    split_register_save_request_unref (request);
}

static void
split_register_save_request_cancel (GncSplitRegisterAsyncRequest *base)
{
    split_register_save_request_complete ((SplitRegisterSaveRequest *)base, FALSE);
}

static void
gnc_split_register_cancel_async_requests (SplitRegister *reg)
{
    SRInfo *info = gnc_split_register_get_info (reg);

    while (info && info->async_requests)
    {
        GncSplitRegisterAsyncRequest *request = info->async_requests->data;

        if (request && request->cancel)
            request->cancel (request);
        else
            info->async_requests = g_list_delete_link (info->async_requests,
                                                        info->async_requests);
    }
}

gboolean
gnc_split_register_save_pending (SplitRegister *reg)
{
    SRInfo *info = reg ? gnc_split_register_get_info (reg) : NULL;
    return info && info->active_save_request != NULL;
}

static void
clear_copied_item()
{
    if (copied_item.ftype == GNC_TYPE_SPLIT)
        gnc_float_split_free (copied_item.fs);
    if (copied_item.ftype == GNC_TYPE_TRANSACTION)
        gnc_float_txn_free (copied_item.ft);
    copied_item.ftype = 0;
    copied_item.fs = NULL;
    copied_item.ft = NULL;
    copied_item.cursor_class = CURSOR_CLASS_NONE;
    copied_item.leader_guid = *guid_null();
    copied_item.anchor_split_index = 0;
    copied_item_generation++;
}

gboolean
gnc_split_register_has_copied_item (void)
{
    return copied_item.ft || copied_item.fs;
}

static void
gnc_copy_split_onto_split (Split* from, Split* to,
                           Account *template_account,
                           gboolean use_cut_semantics)
{
    FloatingSplit *fs;
    gboolean is_template = FALSE;

    if ((from == NULL) || (to == NULL))
        return;

    if (template_account)
        is_template = TRUE;

    fs = gnc_split_to_float_split (from, is_template);
    if (!fs)
        return;

    gnc_float_split_to_split (fs, to, template_account);
    gnc_float_split_free (fs);
}

void
gnc_copy_trans_onto_trans (Transaction* from, Transaction* to,
                           gboolean use_cut_semantics,
                           Account *template_account,
                           gboolean do_commit)
{
    FloatingTxn *ft;
    gboolean is_template = FALSE;

    if ((from == NULL) || (to == NULL))
        return;

    if (template_account)
        is_template = TRUE;

    ft = gnc_txn_to_float_txn (from, use_cut_semantics, is_template);
    if (!ft)
        return;

    if (is_template)
        gnc_float_txn_to_template_txn (ft, to, template_account, do_commit);
    else
        gnc_float_txn_to_txn (ft, to, do_commit);

    gnc_float_txn_free (ft);
}

static int
gnc_split_get_value_denom (Split* split)
{
    gnc_commodity* currency;
    int denom;

    currency = xaccTransGetCurrency (xaccSplitGetParent (split));
    denom = gnc_commodity_get_fraction (currency);
    if (denom == 0)
    {
        gnc_commodity* commodity = gnc_default_currency ();
        denom = gnc_commodity_get_fraction (commodity);
        if (denom == 0)
            denom = 100;
    }

    return denom;
}

static int
gnc_split_get_amount_denom (Split* split)
{
    int denom;

    denom = xaccAccountGetCommoditySCU (xaccSplitGetAccount (split));
    if (denom == 0)
    {
        gnc_commodity* commodity = gnc_default_currency ();
        denom = gnc_commodity_get_fraction (commodity);
        if (denom == 0)
            denom = 100;
    }

    return denom;
}

/* returns TRUE if begin_edit was aborted */
gboolean
gnc_split_register_begin_edit_or_warn (SRInfo* info, Transaction* trans)
{
    ENTER ("info=%p, trans=%p", info, trans);

    if (!xaccTransIsOpen (trans))
    {
        xaccTransBeginEdit (trans);
        /* This is now the pending transaction */
        info->pending_trans_guid = *xaccTransGetGUID (trans);
        LEAVE ("opened and marked pending");
        return FALSE;
    }
    else
    {
        Split*       blank_split = xaccSplitLookup (&info->blank_split_guid,
                                                    gnc_get_current_book ());
        Transaction* blank_trans = xaccSplitGetParent (blank_split);

        if (trans == blank_trans)
        {
            /* This is a brand-new transaction. It is already
             * open, so just mark it as pending. */
            info->pending_trans_guid = *xaccTransGetGUID (trans);
            LEAVE ("already open, now pending.");
            return FALSE;
        }
        else
        {
            GtkWindow* parent = NULL;
            if (info->get_parent)
                parent = GTK_WINDOW (info->get_parent (info->user_data));
            gnc_error_dialog (parent, "%s",
                              _ ("This transaction is already being edited in another register. Please finish editing it there first."));
            LEAVE ("already editing");
            return TRUE;
        }
    }
    LEAVE (" ");
    return FALSE;  /* to satisfy static code analysis */
}

void
gnc_split_register_expand_current_trans (SplitRegister* reg, gboolean expand)
{
    SRInfo* info = gnc_split_register_get_info (reg);
    VirtualLocation virt_loc;

    if (!reg)
        return;

    if (reg->style == REG_STYLE_AUTO_LEDGER ||
        reg->style == REG_STYLE_JOURNAL)
        return;

    /* ok, so I just wanted an excuse to use exclusive-or */
    if (! (expand ^ info->trans_expanded))
        return;

    if (!expand)
    {
        virt_loc = reg->table->current_cursor_loc;
        gnc_split_register_get_trans_split (reg, virt_loc.vcell_loc,
                                            &virt_loc.vcell_loc);

        if (gnc_table_find_close_valid_cell (reg->table, &virt_loc, FALSE))
            gnc_table_move_cursor_gui (reg->table, virt_loc);
        else
        {
            PERR ("Can't find place to go!");
            return;
        }
    }

    info->trans_expanded = expand;

    gnc_table_set_virt_cell_cursor (reg->table,
                                    reg->table->current_cursor_loc.vcell_loc,
                                    gnc_split_register_get_active_cursor (reg));

    gnc_split_register_set_trans_visible (
        reg, reg->table->current_cursor_loc.vcell_loc, expand, FALSE);

    virt_loc = reg->table->current_cursor_loc;
    if (!expand || !gnc_table_virtual_loc_valid (reg->table, virt_loc, FALSE))
    {
        if (gnc_table_find_close_valid_cell (reg->table, &virt_loc, FALSE))
            gnc_table_move_cursor_gui (reg->table, virt_loc);
        else
        {
            PERR ("Can't find place to go!");
            return;
        }
    }

    gnc_table_refresh_gui (reg->table, TRUE);

    if (expand)
        gnc_split_register_show_trans (reg,
                                       reg->table->current_cursor_loc.vcell_loc);
}

gboolean
gnc_split_register_current_trans_expanded (SplitRegister* reg)
{
    SRInfo* info = gnc_split_register_get_info (reg);

    if (!reg)
        return FALSE;

    if (reg->style == REG_STYLE_AUTO_LEDGER ||
        reg->style == REG_STYLE_JOURNAL)
        return TRUE;

    return info->trans_expanded;
}

Transaction*
gnc_split_register_get_current_trans (SplitRegister* reg)
{
    Split* split;
    VirtualCellLocation vcell_loc;

    if (reg == NULL)
        return NULL;

    split = gnc_split_register_get_current_split (reg);
    if (split != NULL)
        return xaccSplitGetParent (split);

    /* Split is blank. Assume it is the blank split of a multi-line
     * transaction. Go back one row to find a split in the transaction. */
    vcell_loc = reg->table->current_cursor_loc.vcell_loc;

    vcell_loc.virt_row--;

    split = gnc_split_register_get_split (reg, vcell_loc);

    return xaccSplitGetParent (split);
}

Split*
gnc_split_register_get_current_split (SplitRegister* reg)
{
    if (reg == NULL)
        return NULL;

    return gnc_split_register_get_split (
        reg, reg->table->current_cursor_loc.vcell_loc);
}

Split*
gnc_split_register_get_blank_split (SplitRegister* reg)
{
    SRInfo* info = gnc_split_register_get_info (reg);

    if (!reg) return NULL;

    return xaccSplitLookup (&info->blank_split_guid, gnc_get_current_book ());
}

gboolean
gnc_split_register_get_split_virt_loc (SplitRegister* reg, Split* split,
                                       VirtualCellLocation* vcell_loc)
{
    Table* table;
    int v_row;
    int v_col;

    if (!reg || !split) return FALSE;

    table = reg->table;

    /* go backwards because typically you search for splits at the end
     * and because we find split rows before transaction rows. */

    for (v_row = table->num_virt_rows - 1; v_row > 0; v_row--)
        for (v_col = 0; v_col < table->num_virt_cols; v_col++)
        {
            VirtualCellLocation vc_loc = { v_row, v_col };
            VirtualCell* vcell;
            Split* s;

            vcell = gnc_table_get_virtual_cell (table, vc_loc);
            if (!vcell || !vcell->visible)
                continue;

            s = xaccSplitLookup (vcell->vcell_data, gnc_get_current_book ());

            if (s == split)
            {
                if (vcell_loc)
                    *vcell_loc = vc_loc;

                return TRUE;
            }
        }

    return FALSE;
}

gboolean
gnc_split_register_get_split_amount_virt_loc (SplitRegister* reg, Split* split,
                                              VirtualLocation* virt_loc)
{
    VirtualLocation v_loc;
    CursorClass cursor_class;
    const char* cell_name;
    gnc_numeric value;

    if (!gnc_split_register_get_split_virt_loc (reg, split, &v_loc.vcell_loc))
        return FALSE;

    cursor_class = gnc_split_register_get_cursor_class (reg, v_loc.vcell_loc);

    value = xaccSplitGetValue (split);

    switch (cursor_class)
    {
        case CURSOR_CLASS_SPLIT:
        case CURSOR_CLASS_TRANS:
            cell_name = (gnc_numeric_negative_p (value)) ? CRED_CELL : DEBT_CELL;
            break;
        default:
            return FALSE;
    }

    if (!gnc_table_get_cell_location (reg->table, cell_name,
                                      v_loc.vcell_loc, &v_loc))
        return FALSE;

    if (virt_loc == NULL)
        return TRUE;

    *virt_loc = v_loc;

    return TRUE;
}

typedef struct
{
    GncSplitRegisterAsyncRequest base;
    GWeakRef owner;
    QofBook *book;
    GncGUID transaction_guid;
    GncGUID source_split_guid;
    GncGUID trans_split_guid;
    GncGUID cursor_split_guid;
    CursorClass cursor_class;
    gboolean has_source_split;
    gboolean has_trans_split;
    gboolean has_cursor_split;
    gboolean cancelled;
} SplitRegisterDuplicateRequest;

static void
split_register_duplicate_request_free (SplitRegisterDuplicateRequest *request)
{
    gnc_split_register_async_request_untrack (&request->base);
    g_weak_ref_clear (&request->owner);
    g_free (request);
}

static void
split_register_duplicate_request_cancel (GncSplitRegisterAsyncRequest *base)
{
    SplitRegisterDuplicateRequest *request = (SplitRegisterDuplicateRequest *)base;

    request->cancelled = TRUE;
    gnc_split_register_async_request_untrack (&request->base);
}

static void
split_register_duplicate_capture_cursor (SplitRegisterDuplicateRequest *request,
                                         SplitRegister *reg)
{
    Split *split = gnc_split_register_get_current_split (reg);

    request->has_cursor_split = split != NULL;
    if (split)
        request->cursor_split_guid = *xaccSplitGetGUID (split);
}

static gboolean
split_register_duplicate_context (SplitRegisterDuplicateRequest *request,
                                  GObject **owner_out, SRInfo **info_out,
                                  Transaction **transaction_out,
                                  Split **split_out, Split **trans_split_out)
{
    GObject *owner = g_weak_ref_get (&request->owner);
    Transaction *current;
    Split *current_split;
    Split *split;
    Split *trans_split;

    if (request->cancelled || !request->base.reg ||
        !owner || request->book != gnc_get_current_book ())
    {
        g_clear_object (&owner);
        return FALSE;
    }
    current = gnc_split_register_get_current_trans (request->base.reg);
    current_split = gnc_split_register_get_current_split (request->base.reg);
    if (!current || gnc_split_register_get_current_cursor_class (request->base.reg) != request->cursor_class ||
        !guid_equal (xaccTransGetGUID (current), &request->transaction_guid) ||
        (request->has_cursor_split && (!current_split ||
         !guid_equal (xaccSplitGetGUID (current_split), &request->cursor_split_guid))) ||
        (!request->has_cursor_split && current_split))
    {
        g_object_unref (owner);
        return FALSE;
    }

    split = request->has_source_split
        ? xaccSplitLookup (&request->source_split_guid, request->book) : NULL;
    trans_split = request->has_trans_split
        ? xaccSplitLookup (&request->trans_split_guid, request->book) : NULL;
    if ((request->has_source_split && (!split || xaccSplitGetParent (split) != current)) ||
        (request->has_trans_split && (!trans_split || xaccSplitGetParent (trans_split) != current)))
    {
        g_object_unref (owner);
        return FALSE;
    }

    *owner_out = owner;
    *info_out = gnc_split_register_get_info (request->base.reg);
    *transaction_out = current;
    *split_out = split;
    *trans_split_out = trans_split;
    return TRUE;
}

static void split_register_duplicate_apply (SplitRegisterDuplicateRequest *request,
                                            const GncDupTransResult *result);

static void
split_register_duplicate_dialog_finished (GncDupTransResult *result,
                                          gpointer user_data)
{
    SplitRegisterDuplicateRequest *request = user_data;

    if (result)
        split_register_duplicate_apply (request, result);
    else
        split_register_duplicate_request_free (request);
    gnc_dup_trans_result_free (result);
}

static void
split_register_duplicate_show_dialog (SplitRegisterDuplicateRequest *request)
{
    GObject *owner;
    SRInfo *info;
    Transaction *transaction;
    Split *split;
    Split *trans_split;
    GtkWindow *parent;

    if (!split_register_duplicate_context (request, &owner, &info, &transaction,
                                           &split, &trans_split))
    {
        split_register_duplicate_request_free (request);
        return;
    }
    parent = GTK_WINDOW (gnc_split_register_get_parent (request->base.reg));
    if (!GTK_IS_WINDOW (parent))
    {
        g_object_unref (owner);
        split_register_duplicate_request_free (request);
        return;
    }

    if (request->cursor_class == CURSOR_CLASS_SPLIT)
    {
        const char *number = gnc_get_num_action (NULL, split);
        if (!request->base.reg->use_tran_num_for_num_field && gnc_strisnum (number))
        {
            Account *account = xaccSplitGetAccount (split);
            const char *initial = account ? xaccAccountGetLastNum (account) : number;
            gnc_dup_trans_dialog_async (parent, NULL, _("New Split Information"), FALSE,
                                        info->last_date_entered, initial ? initial : "",
                                        NULL, NULL,
                                        split_register_duplicate_dialog_finished, request);
            g_object_unref (owner);
            return;
        }
        g_object_unref (owner);
        split_register_duplicate_apply (request, NULL);
        return;
    }

    {
        Account *account = gnc_split_register_get_default_account (request->base.reg);
        const char *number = (account && gnc_strisnum (gnc_get_num_action (transaction, trans_split)))
            ? xaccAccountGetLastNum (account) : gnc_get_num_action (transaction, trans_split);
        const char *tnum = request->base.reg->use_tran_num_for_num_field
            ? NULL : gnc_get_num_action (transaction, NULL);
        gnc_dup_trans_dialog_async (parent, NULL, NULL, !request->base.reg->is_template,
                                    info->last_date_entered, number ? number : "", tnum,
                                    xaccTransGetDocLink (transaction),
                                    split_register_duplicate_dialog_finished, request);
    }
    g_object_unref (owner);
}

static void
split_register_duplicate_apply (SplitRegisterDuplicateRequest *request,
                                const GncDupTransResult *result)
{
    GObject *owner;
    SRInfo *info;
    Transaction *transaction;
    Split *split;
    Split *trans_split;

    if (!split_register_duplicate_context (request, &owner, &info, &transaction,
                                           &split, &trans_split))
    {
        split_register_duplicate_request_free (request);
        return;
    }

    gnc_suspend_gui_refresh ();
    if (request->cursor_class == CURSOR_CLASS_SPLIT)
    {
        gboolean new_number = result && result->num;
        Split *new_split = xaccMallocSplit (request->book);
        Account *template_account = xaccAccountLookup (&info->template_account, request->book);

        xaccTransBeginEdit (transaction);
        xaccSplitSetParent (new_split, transaction);
        gnc_copy_split_onto_split (split, new_split, template_account, FALSE);
        if (new_number)
            gnc_set_num_action (NULL, new_split, result->num, NULL);
        xaccTransCommitEdit (transaction);

        if (new_number && gnc_strisnum (result->num))
        {
            Account *account = xaccSplitGetAccount (new_split);
            if (account && xaccAccountEqual (account,
                                             gnc_split_register_get_default_account (request->base.reg), TRUE))
            {
                NumCell *num_cell = (NumCell *)gnc_table_layout_get_cell (
                    request->base.reg->table->layout, NUM_CELL);
                if (gnc_num_cell_set_last_num (num_cell, result->num))
                    gnc_split_register_set_last_num (request->base.reg, result->num);
            }
            else if (account)
                xaccAccountSetLastNum (account, result->num);
        }
        info->cursor_hint_split = new_split;
        info->cursor_hint_cursor_class = CURSOR_CLASS_SPLIT;
    }
    else if (result)
    {
        GDate entered_date;
        GDate *readonly_threshold = NULL;
        int split_index;
        int trans_split_index;
        Transaction *new_transaction;
        Account *template_account;
        NumCell *num_cell;

        if (qof_book_uses_autoreadonly (request->book))
        {
            readonly_threshold = qof_book_get_autoreadonly_gdate (request->book);
            gnc_gdate_set_time64 (&entered_date, result->date);
            if (readonly_threshold && g_date_compare (&entered_date, readonly_threshold) < 0)
            {
                gnc_error_dialog (GTK_WINDOW (gnc_split_register_get_parent (request->base.reg)), "%s",
                                  _("Cannot store a transaction at this date"));
                g_date_free (readonly_threshold);
                gnc_resume_gui_refresh ();
                g_object_unref (owner);
                split_register_duplicate_request_free (request);
                return;
            }
            g_date_free (readonly_threshold);
        }

        split_index = xaccTransGetSplitIndex (transaction, split);
        trans_split_index = xaccTransGetSplitIndex (transaction, trans_split);
        if (split_index < 0 || trans_split_index < 0)
        {
            gnc_resume_gui_refresh ();
            g_object_unref (owner);
            split_register_duplicate_request_free (request);
            return;
        }

        new_transaction = xaccMallocTransaction (request->book);
        xaccTransBeginEdit (new_transaction);
        template_account = xaccAccountLookup (&info->template_account, request->book);
        gnc_copy_trans_onto_trans (transaction, new_transaction, FALSE, template_account, FALSE);
        xaccTransSetDatePostedSecsNormalized (new_transaction, result->date);
        xaccTransSetDateEnteredSecs (new_transaction, gnc_time (NULL));
        xaccTransSetDocLink (new_transaction, result->doclink ? result->doclink : "");
        gnc_set_num_action (new_transaction, NULL, result->num, result->tnum);
        if (!request->base.reg->use_tran_num_for_num_field)
            gnc_set_num_action (NULL, xaccTransGetSplit (new_transaction, trans_split_index),
                                result->num, NULL);
        xaccTransCommitEdit (new_transaction);

        num_cell = (NumCell *)gnc_table_layout_get_cell (request->base.reg->table->layout, NUM_CELL);
        if (gnc_num_cell_set_last_num (num_cell, result->num))
            gnc_split_register_set_last_num (request->base.reg, result->num);
        if (split_index >= xaccTransCountSplits (new_transaction))
            split_index = 0;
        info->cursor_hint_trans = new_transaction;
        info->cursor_hint_split = xaccTransGetSplit (new_transaction, split_index);
        info->cursor_hint_trans_split = xaccTransGetSplit (new_transaction, trans_split_index);
        info->cursor_hint_cursor_class = CURSOR_CLASS_TRANS;
        info->trans_expanded = FALSE;
    }
    gnc_resume_gui_refresh ();
    g_object_unref (owner);
    split_register_duplicate_request_free (request);
}

static void
split_register_duplicate_save_completed (SplitRegister *reg, gboolean saved,
                                         gpointer user_data)
{
    SplitRegisterDuplicateRequest *request = user_data;
    GObject *owner;
    SRInfo *info;
    Transaction *transaction;
    Split *split;
    Split *trans_split;

    if (!saved || !reg || !split_register_duplicate_context
        (request, &owner, &info, &transaction, &split, &trans_split))
    {
        split_register_duplicate_request_free (request);
        return;
    }

    transaction = xaccTransLookup (&request->transaction_guid, request->book);
    if (!transaction)
    {
        g_object_unref (owner);
        split_register_duplicate_request_free (request);
        return;
    }
    split = gnc_split_register_get_current_split (reg);
    if (!split && request->cursor_class == CURSOR_CLASS_SPLIT)
        split = xaccTransGetSplit (transaction, xaccTransCountSplits (transaction) - 1);
    request->has_source_split = split != NULL;
    if (split)
        request->source_split_guid = *xaccSplitGetGUID (split);
    trans_split = gnc_split_register_get_current_trans_split (reg, NULL);
    request->has_trans_split = trans_split != NULL;
    if (trans_split)
        request->trans_split_guid = *xaccSplitGetGUID (trans_split);
    split_register_duplicate_capture_cursor (request, reg);
    g_object_unref (owner);
    split_register_duplicate_show_dialog (request);
}

static void
split_register_duplicate_save_finished (gint response, gpointer user_data)
{
    SplitRegisterDuplicateRequest *request = user_data;
    GObject *owner;
    SRInfo *info;
    Transaction *transaction;
    Split *split;
    Split *trans_split;
    SplitRegister *reg;

    if (response != GTK_RESPONSE_ACCEPT ||
        !split_register_duplicate_context (request, &owner, &info, &transaction,
                                           &split, &trans_split))
    {
        split_register_duplicate_request_free (request);
        return;
    }

    reg = request->base.reg;
    g_object_unref (owner);
    gnc_split_register_save_async (reg, TRUE,
                                   split_register_duplicate_save_completed,
                                   request);
}

void
gnc_split_register_duplicate_current_async (SplitRegister *reg, GObject *owner)
{
    SRInfo *info;
    Transaction *transaction;
    Split *split;
    Split *trans_split;
    Split *blank_split;
    CursorClass cursor_class;
    gboolean changed;
    SplitRegisterDuplicateRequest *request;
    GtkWindow *parent;

    g_return_if_fail (reg != NULL);
    g_return_if_fail (G_IS_OBJECT (owner));
    info = gnc_split_register_get_info (reg);
    transaction = gnc_split_register_get_current_trans (reg);
    split = gnc_split_register_get_current_split (reg);
    trans_split = gnc_split_register_get_current_trans_split (reg, NULL);
    cursor_class = gnc_split_register_get_current_cursor_class (reg);
    if (!transaction || cursor_class == CURSOR_CLASS_NONE ||
        (!split && cursor_class == CURSOR_CLASS_TRANS))
        return;

    changed = gnc_table_current_cursor_changed (reg->table, FALSE);
    blank_split = xaccSplitLookup (&info->blank_split_guid, gnc_get_current_book ());
    if (!changed && (!split || split == blank_split))
        return;

    request = g_new0 (SplitRegisterDuplicateRequest, 1);
    request->book = gnc_get_current_book ();
    request->base.reg = reg;
    request->transaction_guid = *xaccTransGetGUID (transaction);
    request->cursor_class = cursor_class;
    request->has_source_split = split != NULL;
    if (split)
        request->source_split_guid = *xaccSplitGetGUID (split);
    request->has_trans_split = trans_split != NULL;
    if (trans_split)
        request->trans_split_guid = *xaccSplitGetGUID (trans_split);
    split_register_duplicate_capture_cursor (request, reg);
    g_weak_ref_init (&request->owner, owner);
    gnc_split_register_async_request_track (reg, &request->base,
                                             split_register_duplicate_request_cancel);

    if (!changed)
    {
        split_register_duplicate_show_dialog (request);
        return;
    }
    parent = GTK_WINDOW (gnc_split_register_get_parent (reg));
    if (!GTK_IS_WINDOW (parent))
    {
        split_register_duplicate_request_free (request);
        return;
    }
    gnc_warning_dialog_async (parent, GNC_PREF_WARN_REG_TRANS_DUP,
                              _("Save transaction before duplicating?"),
                              _("The current transaction has been changed. Would you like to record the changes before duplicating the transaction, or cancel the duplication?"),
                              _("_Record"), GTK_RESPONSE_ACCEPT, TRUE,
                              split_register_duplicate_save_finished, request);
}
static void
gnc_split_register_copy_current_internal (SplitRegister* reg,
                                          gboolean use_cut_semantics)
{
    SRInfo* info = gnc_split_register_get_info (reg);
    CursorClass cursor_class;
    Transaction* trans;
    Split* blank_split;
    gboolean changed;
    Split *split;
    FloatingSplit *new_fs = NULL;
    FloatingTxn *new_ft = NULL;

    g_return_if_fail (reg);
    ENTER ("reg=%p, use_cut_semantics=%s", reg,
           use_cut_semantics ? "TRUE" : "FALSE");

    blank_split = xaccSplitLookup (&info->blank_split_guid,
                                   gnc_get_current_book ());
    split = gnc_split_register_get_current_split (reg);
    trans = gnc_split_register_get_current_trans (reg);

    /* This shouldn't happen, but be paranoid. */
    if (trans == NULL)
    {
        LEAVE ("no trans");
        return;
    }

    cursor_class = gnc_split_register_get_current_cursor_class (reg);

    /* Can't do anything with this. */
    if (cursor_class == CURSOR_CLASS_NONE)
    {
        LEAVE ("no cursor class");
        return;
    }

    /* This shouldn't happen, but be paranoid. */
    if ((split == NULL) && (cursor_class == CURSOR_CLASS_TRANS))
    {
        g_warning ("BUG DETECTED: transaction cursor with no anchoring split!");
        LEAVE ("transaction cursor with no anchoring split");
        return;
    }

    changed = gnc_table_current_cursor_changed (reg->table, FALSE);

    /* See if we were asked to copy an unchanged blank split. Don't. */
    if (!changed && ((split == NULL) || (split == blank_split)))
    {
        /* We're either on an unedited, brand-new split or an unedited, brand-new
         * transaction (the transaction anchored by the blank split.) */
        /* FIXME: This doesn't work exactly right. When entering a new transaction,
         *        you can edit the description, move to a split row, then move
         *        back to the description, then ask for a copy, and this code will
         *        be reached. It forgets that you changed the row the first time
         *        you were there.  -Charles */
        LEAVE ("nothing to copy/cut");
        return;
    }

    /* unprotect the old object, if any */
    clear_copied_item();

    /* Ok, we are now ready to make the copy. */

    if (cursor_class == CURSOR_CLASS_SPLIT)
    {
        /* We are on a split in an expanded transaction. Just copy the split. */
        new_fs = gnc_split_to_float_split (split, reg->is_template);

        if (new_fs)
        {
            if (changed)
                gnc_split_register_save_to_copy_buffer (reg, NULL, new_fs,
                                                        use_cut_semantics);

            copied_item.leader_guid = *guid_null ();
        }
    }
    else
    {
        /* We are on a transaction row. Copy the whole transaction. */
        new_ft = gnc_txn_to_float_txn (trans, use_cut_semantics, reg->is_template);

        if (new_ft)
        {
            if (changed)
            {
                int split_index;
                FloatingSplit *fs;

                split_index = xaccTransGetSplitIndex (trans, split);
                if (split_index >= 0)
                    fs = gnc_float_txn_get_float_split (new_ft, split_index);
                else
                    fs = NULL;

                gnc_split_register_save_to_copy_buffer (reg, new_ft, fs,
                                                        use_cut_semantics);
            }

            copied_item.leader_guid = info->default_account;
            copied_item.anchor_split_index = xaccTransGetSplitIndex (trans, split);
        }
    }

    if (!new_fs && !new_ft)
    {
        g_warning ("BUG DETECTED: copy failed");
        LEAVE ("copy failed");
        return;
    }

    if (new_fs)
    {
        copied_item.fs = new_fs;
        copied_item.ftype = GNC_TYPE_SPLIT;
    }
    else if (new_ft)
    {
        copied_item.ft = new_ft;
        copied_item.ftype = GNC_TYPE_TRANSACTION;
    }

    copied_item.cursor_class = cursor_class;
    copied_item_generation++;
    gnc_hook_add_dangler (HOOK_BOOK_CLOSED, (GFunc)clear_copied_item, NULL, NULL);
    LEAVE ("%s %s", use_cut_semantics ? "cut" : "copied",
           cursor_class == CURSOR_CLASS_SPLIT ? "split" : "transaction");
}

void
gnc_split_register_copy_current (SplitRegister* reg)
{
    gnc_split_register_copy_current_internal (reg, FALSE);
}

void
gnc_split_register_cut_current (SplitRegister* reg)
{
    SRInfo* info = gnc_split_register_get_info (reg);
    CursorClass cursor_class;
    Transaction* trans;
    Split* blank_split;
    gboolean changed;
    Split* split;

    blank_split = xaccSplitLookup (&info->blank_split_guid,
                                   gnc_get_current_book ());
    split = gnc_split_register_get_current_split (reg);
    trans = gnc_split_register_get_current_trans (reg);

    /* This shouldn't happen, but be paranoid. */
    if (trans == NULL)
        return;

    cursor_class = gnc_split_register_get_current_cursor_class (reg);

    /* Can't do anything with this. */
    if (cursor_class == CURSOR_CLASS_NONE)
        return;

    /* This shouldn't happen, but be paranoid. */
    if ((split == NULL) && (cursor_class == CURSOR_CLASS_TRANS))
        return;

    changed = gnc_table_current_cursor_changed (reg->table, FALSE);

    /* See if we were asked to cut an unchanged blank split. Don't. */
    if (!changed && ((split == NULL) || (split == blank_split)))
        return;

    gnc_split_register_copy_current_internal (reg, TRUE);

    if (cursor_class == CURSOR_CLASS_SPLIT)
        gnc_split_register_delete_current_split (reg);
    else
        gnc_split_register_delete_current_trans (reg);
}

typedef struct
{
    GncSplitRegisterAsyncRequest base;
    GWeakRef parent;
    QofBook *book;
    GncGUID transaction_guid;
    GncGUID split_guid;
    GncGUID trans_split_guid;
    GncGUID blank_split_guid;
    VirtualLocation cursor_loc;
    CursorClass cursor_class;
    GType copied_type;
    CursorClass copied_cursor_class;
    guint64 copied_item_generation;
    gboolean has_split;
    gboolean has_trans_split;
    gboolean cancelled;
} SplitRegisterPasteRequest;

static gboolean
split_register_paste_request_context (SplitRegisterPasteRequest *request,
                                      SRInfo **info_out,
                                      Transaction **transaction_out,
                                      Split **split_out,
                                      Split **trans_split_out,
                                      Split **blank_split_out)
{
    SplitRegister *reg = request->base.reg;
    GtkWidget *parent = NULL;
    SRInfo *info;
    Transaction *transaction;
    Split *split;
    Split *trans_split;
    Split *blank_split;

    if (request->cancelled || !reg || !reg->table ||
        request->book != gnc_get_current_book () ||
        copied_item_generation != request->copied_item_generation ||
        copied_item.ftype != request->copied_type ||
        copied_item.cursor_class != request->copied_cursor_class ||
        !virt_loc_equal (reg->table->current_cursor_loc, request->cursor_loc) ||
        gnc_split_register_get_current_cursor_class (reg) != request->cursor_class)
        return FALSE;

    parent = GTK_WIDGET (g_weak_ref_get (&request->parent));
    if (!parent || !GTK_IS_WINDOW (parent) ||
        gnc_split_register_get_parent (reg) != parent)
        goto out;

    info = gnc_split_register_get_info (reg);
    transaction = gnc_split_register_get_current_trans (reg);
    split = gnc_split_register_get_current_split (reg);
    trans_split = gnc_split_register_get_current_trans_split (reg, NULL);
    blank_split = xaccSplitLookup (&request->blank_split_guid, request->book);
    if (!info || !transaction || !blank_split ||
        !guid_equal (&info->blank_split_guid, &request->blank_split_guid) ||
        !guid_equal (xaccTransGetGUID (transaction), &request->transaction_guid) ||
        request->has_split != (split != NULL) ||
        (split && (!guid_equal (xaccSplitGetGUID (split), &request->split_guid) ||
                   xaccSplitGetParent (split) != transaction)) ||
        request->has_trans_split != (trans_split != NULL) ||
        (trans_split &&
         (!guid_equal (xaccSplitGetGUID (trans_split), &request->trans_split_guid) ||
          xaccSplitGetParent (trans_split) != transaction)))
        goto out;

    *info_out = info;
    *transaction_out = transaction;
    *split_out = split;
    *trans_split_out = trans_split;
    *blank_split_out = blank_split;
    g_object_unref (parent);
    return TRUE;

out:
    g_clear_object (&parent);
    return FALSE;
}

static void
split_register_paste_request_free (SplitRegisterPasteRequest *request)
{
    gnc_split_register_async_request_untrack (&request->base);
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static void
split_register_paste_request_cancel (GncSplitRegisterAsyncRequest *base)
{
    SplitRegisterPasteRequest *request = (SplitRegisterPasteRequest *)base;
    SplitRegister *reg = request->base.reg;

    request->cancelled = TRUE;
    if (reg && reg->table && reg->table->control)
        gnc_table_control_set_input_suspended (reg->table->control, FALSE);
    /* GtkAlertDialog owns the completion callback. Keep this carrier alive
     * until it observes cancellation, but detach it from the closing register. */
    gnc_split_register_async_request_untrack (&request->base);
}

static void
split_register_paste_request_apply (SplitRegisterPasteRequest *request)
{
    SplitRegister *reg = request->base.reg;
    SRInfo *info;
    Transaction *transaction;
    Transaction *blank_transaction;
    Split *split;
    Split *trans_split;
    Split *blank_split;
    Account *template_account;
    gboolean refresh_suspended = FALSE;

    if (!split_register_paste_request_context (request, &info, &transaction,
                                               &split, &trans_split, &blank_split))
        return;

    template_account = xaccAccountLookup (&info->template_account, request->book);
    blank_transaction = xaccSplitGetParent (blank_split);

    if (request->cursor_class == CURSOR_CLASS_SPLIT)
    {
        if (gnc_split_register_begin_edit_or_warn (info, transaction))
            return;

        gnc_suspend_gui_refresh ();
        refresh_suspended = TRUE;
        if (!split)
        {
            /* We are on a null split in an expanded transaction. */
            split = xaccMallocSplit (request->book);
            xaccSplitSetParent (split, transaction);
        }
        gnc_float_split_to_split (copied_item.fs, split, template_account);
    }
    else
    {
        Account *copied_leader;
        Account *default_account;
        int trans_split_index;
        int split_index;
        int num_splits;

        split_index = xaccTransGetSplitIndex (transaction, split);
        trans_split_index = xaccTransGetSplitIndex (transaction, trans_split);
        if (split_index < 0 || trans_split_index < 0)
            return;

        if (gnc_split_register_begin_edit_or_warn (info, transaction))
            return;

        gnc_suspend_gui_refresh ();
        refresh_suspended = TRUE;

        DEBUG ("Pasting txn, trans=%p, split=%p, blank_trans=%p, blank_split=%p",
               transaction, split, blank_transaction, blank_split);

        copied_leader = xaccAccountLookup (&copied_item.leader_guid, request->book);
        default_account = gnc_split_register_get_default_account (reg);
        if (copied_leader && default_account)
            gnc_float_txn_to_txn_swap_accounts (copied_item.ft, transaction,
                                                 copied_leader, default_account,
                                                 FALSE);
        else if (reg->is_template)
            gnc_float_txn_to_template_txn (copied_item.ft, transaction,
                                            template_account, FALSE);
        else
            gnc_float_txn_to_txn (copied_item.ft, transaction, FALSE);

        num_splits = xaccTransCountSplits (transaction);
        if (split_index >= num_splits)
            split_index = 0;

        if (transaction == blank_transaction)
        {
            /* Pasting deletes the former blank split; choose the replacement
             * from the original clipboard target. */
            gint anchor_split_index = copied_item.anchor_split_index;

            if (anchor_split_index >= num_splits)
                anchor_split_index = 0;

            blank_split = xaccTransGetSplit (transaction, anchor_split_index);
            if (!blank_split)
                goto out;
            info->blank_split_guid = *xaccSplitGetGUID (blank_split);
            info->blank_split_edited = TRUE;
            info->auto_complete = FALSE;
            DEBUG ("replacement blank_split=%p", blank_split);

            /* NOTE: At this point, the blank transaction virtual cell is still
             *       anchored by the old, deleted blank split. The register will
             *       have to be reloaded (redrawn) to correct this. */
        }

        info->cursor_hint_trans = transaction;
        info->cursor_hint_split = xaccTransGetSplit (transaction, split_index);
        info->cursor_hint_trans_split = xaccTransGetSplit (transaction,
                                                            trans_split_index);
        info->cursor_hint_cursor_class = CURSOR_CLASS_TRANS;
    }

out:
    if (refresh_suspended)
        gnc_resume_gui_refresh ();
}

static void
split_register_paste_request_finished (G_GNUC_UNUSED GtkWindow *parent,
                                       gint response, gpointer user_data)
{
    SplitRegisterPasteRequest *request = user_data;
    SplitRegister *reg = request->base.reg;

    if (response == GTK_RESPONSE_YES)
        split_register_paste_request_apply (request);
    if (reg && reg->table && reg->table->control)
        gnc_table_control_set_input_suspended (reg->table->control, FALSE);
    split_register_paste_request_free (request);
}

static SplitRegisterPasteRequest *
split_register_paste_request_new (SplitRegister *reg, gboolean *confirm_out)
{
    SRInfo *info;
    Transaction *transaction;
    Split *split;
    Split *trans_split;
    Split *blank_split;
    GtkWidget *parent;
    CursorClass cursor_class;

    *confirm_out = FALSE;
    if (!reg || !reg->table ||
        (reg->table->control &&
         gnc_table_control_input_suspended (reg->table->control)))
        return NULL;

    info = gnc_split_register_get_info (reg);
    transaction = gnc_split_register_get_current_trans (reg);
    split = gnc_split_register_get_current_split (reg);
    trans_split = gnc_split_register_get_current_trans_split (reg, NULL);
    blank_split = info ? xaccSplitLookup (&info->blank_split_guid,
                                          gnc_get_current_book ()) : NULL;
    parent = gnc_split_register_get_parent (reg);
    cursor_class = gnc_split_register_get_current_cursor_class (reg);
    if (!info || !transaction || !blank_split || !GTK_IS_WINDOW (parent) ||
        cursor_class == CURSOR_CLASS_NONE ||
        (cursor_class == CURSOR_CLASS_TRANS && !split) ||
        copied_item.cursor_class == CURSOR_CLASS_NONE)
        return NULL;

    if (cursor_class == CURSOR_CLASS_SPLIT)
    {
        const char *anchor_message =
            _("This is the split anchoring this transaction to the register. "
              "You may not overwrite it from this register window. You may "
              "overwrite it if you navigate to a register that shows another "
              "side of this same transaction.");

        if (copied_item.cursor_class != CURSOR_CLASS_SPLIT ||
            copied_item.ftype != GNC_TYPE_SPLIT)
            return NULL;

        /* The General Journal does not have any anchoring splits. */
        if (split && reg->type != GENERAL_JOURNAL && split == trans_split)
        {
            gnc_warning_dialog (GTK_WINDOW (parent), "%s", anchor_message);
            return NULL;
        }
        *confirm_out = split != NULL;
    }
    else if (cursor_class == CURSOR_CLASS_TRANS)
    {
        if (copied_item.cursor_class != CURSOR_CLASS_TRANS ||
            copied_item.ftype != GNC_TYPE_TRANSACTION)
            return NULL;

        if (reg->type != SEARCH_LEDGER && reg->type != GENERAL_JOURNAL &&
            gnc_float_txn_has_template (copied_item.ft))
        {
            gnc_warning_dialog (GTK_WINDOW (parent), "%s",
                                _("Scheduled transactions can only be pasted "
                                  "to the General Journal"));
            return NULL;
        }
        *confirm_out = split != blank_split;
    }
    else
        return NULL;

    SplitRegisterPasteRequest *request = g_new0 (SplitRegisterPasteRequest, 1);
    request->base.reg = reg;
    request->book = gnc_get_current_book ();
    request->transaction_guid = *xaccTransGetGUID (transaction);
    request->cursor_loc = reg->table->current_cursor_loc;
    request->cursor_class = cursor_class;
    request->copied_type = copied_item.ftype;
    request->copied_cursor_class = copied_item.cursor_class;
    request->copied_item_generation = copied_item_generation;
    request->has_split = split != NULL;
    if (split)
        request->split_guid = *xaccSplitGetGUID (split);
    request->has_trans_split = trans_split != NULL;
    if (trans_split)
        request->trans_split_guid = *xaccSplitGetGUID (trans_split);
    request->blank_split_guid = *xaccSplitGetGUID (blank_split);
    g_weak_ref_init (&request->parent, G_OBJECT (parent));
    return request;
}

void
gnc_split_register_paste_current (SplitRegister* reg)
{
    SplitRegisterPasteRequest *request;
    GtkWidget *parent;
    gboolean confirm;

    ENTER ("reg=%p", reg);
    request = split_register_paste_request_new (reg, &confirm);
    if (!request)
    {
        LEAVE ("paste request rejected");
        return;
    }

    if (!confirm)
    {
        split_register_paste_request_apply (request);
        split_register_paste_request_free (request);
        LEAVE ("pasted without confirmation");
        return;
    }

    parent = gnc_split_register_get_parent (reg);
    if (!GTK_IS_WINDOW (parent))
    {
        split_register_paste_request_free (request);
        LEAVE ("no parent window");
        return;
    }

    gnc_split_register_async_request_track (reg, &request->base,
                                             split_register_paste_request_cancel);
    if (reg->table->control)
        gnc_table_control_set_input_suspended (reg->table->control, TRUE);

    if (request->cursor_class == CURSOR_CLASS_SPLIT)
        gnc_verify_dialog_async (GTK_WINDOW (parent), FALSE,
                                 split_register_paste_request_finished, request,
                                 "%s", _("You are about to overwrite an existing split. "
                                         "Are you sure you want to do that?"));
    else
        gnc_verify_dialog_async (GTK_WINDOW (parent), FALSE,
                                 split_register_paste_request_finished, request,
                                 "%s", _("You are about to overwrite an existing "
                                         "transaction. Are you sure you want to do that?"));
    LEAVE ("paste confirmation pending");
}

gboolean
gnc_split_register_is_blank_split (SplitRegister* reg, Split* split)
{
    SRInfo* info = gnc_split_register_get_info (reg);
    Split* current_blank_split = xaccSplitLookup (&info->blank_split_guid,
                                                  gnc_get_current_book ());

    if (split == current_blank_split)
        return TRUE;

    return FALSE;
}

void
gnc_split_register_change_blank_split_ref (SplitRegister* reg, Split* split)
{
    SRInfo* info = gnc_split_register_get_info (reg);
    Split* current_blank_split = xaccSplitLookup (&info->blank_split_guid,
                                                  gnc_get_current_book ());
    Split* pref_split = NULL; // has the same account as incoming split
    Split* other_split = NULL; // other split
    Account* blank_split_account = xaccSplitGetAccount (current_blank_split);
    Transaction* trans = xaccSplitGetParent (split);

    // loop through splitlist looking for splits other than the blank_split
    for (GList *n = xaccTransGetSplitList (trans); n; n = n->next)
    {
        Split *s = n->data;
        if (s != current_blank_split && xaccTransStillHasSplit (trans, s))
        {
            if (blank_split_account == xaccSplitGetAccount (s))
                pref_split = s;  // prefer same account
            else
                other_split = s; // any other split
        }
    }
    // now change the saved blank split reference
    if (pref_split != NULL)
        info->blank_split_guid = *xaccSplitGetGUID (pref_split);
    else if (other_split != NULL)
        info->blank_split_guid = *xaccSplitGetGUID (other_split);
    else
      info->blank_split_guid = *guid_null();
}

void
gnc_split_register_delete_current_split (SplitRegister* reg)
{
    SRInfo* info = gnc_split_register_get_info (reg);
    Transaction* pending_trans;
    Transaction* trans;
    Split* blank_split;
    Split* split;

    if (!reg) return;

    blank_split = xaccSplitLookup (&info->blank_split_guid,
                                   gnc_get_current_book ());

    pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                     gnc_get_current_book ());

    /* get the current split based on cursor position */
    split = gnc_split_register_get_current_split (reg);
    if (split == NULL)
        return;

    /* If we are deleting the blank split, just cancel. The user is
     * allowed to delete the blank split as a method for discarding
     * any edits they may have made to it. */
    if (split == blank_split)
    {
        gnc_split_register_cancel_cursor_split_changes (reg);
        return;
    }

    gnc_suspend_gui_refresh ();

    trans = xaccSplitGetParent (split);

    /* Check pending transaction */
    if (trans == pending_trans)
    {
        g_assert (xaccTransIsOpen (trans));
    }
    else
    {
        g_assert (!pending_trans);
        if (gnc_split_register_begin_edit_or_warn (info, trans))
        {
            gnc_resume_gui_refresh ();
            return;
        }
    }
    xaccSplitDestroy (split);

    gnc_resume_gui_refresh ();
    gnc_split_register_redraw (reg);
}

void
gnc_split_register_delete_current_trans (SplitRegister* reg)
{
    SRInfo* info = gnc_split_register_get_info (reg);
    Transaction* pending_trans;
    Transaction* trans;
    Split* blank_split;
    Split* split;
    gboolean was_open;

    ENTER ("reg=%p", reg);
    if (!reg)
    {
        LEAVE ("no register");
        return;
    }

    blank_split = xaccSplitLookup (&info->blank_split_guid,
                                   gnc_get_current_book ());
    pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                     gnc_get_current_book ());

    /* get the current split based on cursor position */
    split = gnc_split_register_get_current_split (reg);
    if (split == NULL)
    {
        LEAVE ("no split");
        return;
    }

    gnc_suspend_gui_refresh ();
    trans = xaccSplitGetParent (split);

    /* If we just deleted the blank split, clean up. The user is
     * allowed to delete the blank split as a method for discarding
     * any edits they may have made to it. */
    if (split == blank_split)
    {
        DEBUG ("deleting blank split");
        info->blank_split_guid = *guid_null ();
        info->auto_complete = FALSE;
    }
    else
    {
        info->trans_expanded = FALSE;
    }

    /* Check pending transaction */
    if (trans == pending_trans)
    {
        DEBUG ("clearing pending trans");
        info->pending_trans_guid = *guid_null ();
        pending_trans = NULL;
    }

    was_open = xaccTransIsOpen (trans);
    xaccTransDestroy (trans);
    if (was_open)
    {
        DEBUG ("committing");
        xaccTransCommitEdit (trans);
    }
    gnc_resume_gui_refresh ();
    gnc_split_register_redraw (reg);
    LEAVE (" ");
}

void
gnc_split_register_void_current_trans (SplitRegister* reg, const char* reason)
{
    SRInfo* info = gnc_split_register_get_info (reg);
    Transaction* pending_trans;
    Transaction* trans;
    Split* blank_split;
    Split* split;

    if (!reg) return;

    blank_split = xaccSplitLookup (&info->blank_split_guid,
                                   gnc_get_current_book ());
    pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                     gnc_get_current_book ());

    /* get the current split based on cursor position */
    split = gnc_split_register_get_current_split (reg);
    if (split == NULL)
        return;

    /* Bail if trying to void the blank split. */
    if (split == blank_split)
        return;

    /* already voided. */
    if (xaccSplitGetReconcile (split) == VREC)
        return;

    info->trans_expanded = FALSE;

    gnc_suspend_gui_refresh ();

    trans = xaccSplitGetParent (split);
    xaccTransVoid (trans, reason);

    /* Check pending transaction */
    if (trans == pending_trans)
    {
        info->pending_trans_guid = *guid_null ();
        pending_trans = NULL;
    }
    if (xaccTransIsOpen (trans))
    {
        PERR ("We should not be voiding an open transaction.");
        xaccTransCommitEdit (trans);
    }
    gnc_resume_gui_refresh ();
}

void
gnc_split_register_unvoid_current_trans (SplitRegister* reg)
{
    SRInfo* info = gnc_split_register_get_info (reg);
    Transaction* pending_trans;
    Transaction* trans;
    Split* blank_split;
    Split* split;

    if (!reg) return;

    blank_split = xaccSplitLookup (&info->blank_split_guid,
                                   gnc_get_current_book ());
    pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                     gnc_get_current_book ());

    /* get the current split based on cursor position */
    split = gnc_split_register_get_current_split (reg);
    if (split == NULL)
        return;

    /* Bail if trying to unvoid the blank split. */
    if (split == blank_split)
        return;

    /* not voided. */
    if (xaccSplitGetReconcile (split) != VREC)
        return;

    info->trans_expanded = FALSE;

    gnc_suspend_gui_refresh ();

    trans = xaccSplitGetParent (split);

    xaccTransUnvoid (trans);

    /* Check pending transaction */
    if (trans == pending_trans)
    {
        info->pending_trans_guid = *guid_null ();
        pending_trans = NULL;
    }

    gnc_resume_gui_refresh ();
}

void
gnc_split_register_empty_current_trans_except_split (SplitRegister* reg,
                                                     Split* split)
{
    SRInfo* info;
    Transaction* trans;
    Transaction* pending;
    int i = 0;
    Split* s;

    if ((reg == NULL)  || (split == NULL))
        return;

    gnc_suspend_gui_refresh ();
    info = gnc_split_register_get_info (reg);
    pending = xaccTransLookup (&info->pending_trans_guid, gnc_get_current_book ());

    trans = xaccSplitGetParent (split);
    if (!pending)
    {
        if (gnc_split_register_begin_edit_or_warn (info, trans))
        {
            gnc_resume_gui_refresh ();
            return;
        }
    }
    else if (pending == trans)
    {
        g_assert (xaccTransIsOpen (trans));
    }
    else g_assert_not_reached ();

    while ((s = xaccTransGetSplit (trans, i)) != NULL)
    {
        if (s != split)
            xaccSplitDestroy (s);
        else i++;
    }

    gnc_resume_gui_refresh ();
    gnc_split_register_redraw (reg);
}

void
gnc_split_register_empty_current_trans (SplitRegister* reg)
{
    Split* split;

    /* get the current split based on cursor position */
    split = gnc_split_register_get_current_split (reg);
    gnc_split_register_empty_current_trans_except_split (reg, split);
}

void
gnc_split_register_cancel_cursor_split_changes (SplitRegister* reg)
{
    VirtualLocation virt_loc;

    if (reg == NULL)
        return;

    virt_loc = reg->table->current_cursor_loc;

    if (!gnc_table_current_cursor_changed (reg->table, FALSE))
        return;

    /* We're just cancelling the current split here, not the transaction.
     * When cancelling edits, reload the cursor from the transaction. */
    gnc_table_clear_current_cursor_changes (reg->table);

    if (gnc_table_find_close_valid_cell (reg->table, &virt_loc, FALSE))
        gnc_table_move_cursor_gui (reg->table, virt_loc);

    gnc_table_refresh_gui (reg->table, TRUE);
}

void
gnc_split_register_cancel_cursor_trans_changes (SplitRegister* reg)
{
    SRInfo* info = gnc_split_register_get_info (reg);
    Transaction* pending_trans, *blank_trans;
    gboolean refresh_all = FALSE;

    pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                     gnc_get_current_book ());

    blank_trans = xaccSplitGetParent (gnc_split_register_get_blank_split (reg));

    if (pending_trans == blank_trans)
        refresh_all = TRUE;

    /* Get the currently open transaction, rollback the edits on it, and
     * then repaint everything. To repaint everything, make a note of
     * all of the accounts that will be affected by this rollback. */
    if (!xaccTransIsOpen (pending_trans))
    {
        gnc_split_register_cancel_cursor_split_changes (reg);
        return;
    }

    if (!pending_trans)
        return;

    gnc_suspend_gui_refresh ();

    xaccTransRollbackEdit (pending_trans);

    info->pending_trans_guid = *guid_null ();

    gnc_resume_gui_refresh ();

    if (refresh_all)
        gnc_gui_refresh_all ();  // force a refresh of all registers
    else
        gnc_split_register_redraw (reg);
}

void
gnc_split_register_redraw (SplitRegister* reg)
{
    gnc_ledger_display_refresh_by_split_register (reg);
}

/* Copy from the register object to scheme. This needs to be
 * in sync with gnc_split_register_save and xaccSRSaveChangedCells. */
static gboolean
gnc_split_register_save_to_copy_buffer (SplitRegister *reg,
                                        FloatingTxn *ft, FloatingSplit *fs,
                                        gboolean use_cut_semantics)
{
    FloatingSplit *other_fs = NULL;
    Transaction *trans;

    /* use the changed flag to avoid heavy-weight updates
     * of the split & transaction fields. This will help
     * cut down on unnecessary register redraws. */
    if (!gnc_table_current_cursor_changed (reg->table, FALSE))
        return FALSE;

    /* get the handle to the current split and transaction */
    trans = gnc_split_register_get_current_trans (reg);
    if (trans == NULL)
        return FALSE;

    /* copy the contents from the cursor to the split */
    if (gnc_table_layout_get_cell_changed (reg->table->layout, DATE_CELL, TRUE))
    {
        BasicCell* cell;
        time64 time;
        cell = gnc_table_layout_get_cell (reg->table->layout, DATE_CELL);
        gnc_date_cell_get_date ((DateCell*) cell, &time, TRUE);
        xaccTransSetDatePostedSecsNormalized (trans, time);
    }

    if (gnc_table_layout_get_cell_changed (reg->table->layout, NUM_CELL, TRUE))
    {
        const char* value;

        value = gnc_table_layout_get_cell_value (reg->table->layout, NUM_CELL);
        if (reg->use_tran_num_for_num_field)
            xaccTransSetNum (trans, value);
        /* else this contains the same as ACTN_CELL which is already handled below *
         * and the TNUM_CELL contains transaction number which is handled in next  *
         * if statement. */
    }

    if (gnc_table_layout_get_cell_changed (reg->table->layout, TNUM_CELL, TRUE))
    {
        const char* value;

        value = gnc_table_layout_get_cell_value (reg->table->layout, TNUM_CELL);
        if (!reg->use_tran_num_for_num_field)
            xaccTransSetNum (trans, value);
        /* else this cell is not used */
    }

    if (gnc_table_layout_get_cell_changed (reg->table->layout, DESC_CELL, TRUE))
    {
        const char* value;

        value = gnc_table_layout_get_cell_value (reg->table->layout, DESC_CELL);
        xaccTransSetDescription (trans, value);
    }

    if (gnc_table_layout_get_cell_changed (reg->table->layout, NOTES_CELL, TRUE))
    {
        const char* value;

        value = gnc_table_layout_get_cell_value (reg->table->layout, NOTES_CELL);
        xaccTransSetNotes (trans, value);
    }

    if (gnc_table_layout_get_cell_changed (reg->table->layout, RECN_CELL, TRUE))
    {
        BasicCell* cell;
        char flag;

        cell = gnc_table_layout_get_cell (reg->table->layout, RECN_CELL);
        flag = gnc_recn_cell_get_flag ((RecnCell*) cell);

        gnc_float_split_set_reconcile_state (fs, flag);
    }

    if (gnc_table_layout_get_cell_changed (reg->table->layout, ACTN_CELL, TRUE))
    {
        const char* value;

        value = gnc_table_layout_get_cell_value (reg->table->layout, ACTN_CELL);
        gnc_float_split_set_action (fs, value);
    }

    if (gnc_table_layout_get_cell_changed (reg->table->layout, MEMO_CELL, TRUE))
    {
        const char* value;

        value = gnc_table_layout_get_cell_value (reg->table->layout, MEMO_CELL);
        gnc_float_split_set_memo (fs, value);
    }

    if (gnc_table_layout_get_cell_changed (reg->table->layout, XFRM_CELL, TRUE))
    {
        Account* new_account;

        new_account = gnc_split_register_get_account (reg, XFRM_CELL);

        if (new_account != NULL)
            gnc_float_split_set_account (fs, new_account);
    }

    if (reg->style == REG_STYLE_LEDGER)
        other_fs = gnc_float_txn_get_other_float_split (ft, fs);

    if (gnc_table_layout_get_cell_changed (reg->table->layout, MXFRM_CELL, TRUE))
    {
        other_fs = gnc_float_txn_get_other_float_split (ft, fs);

        if (!other_fs)
        {
            if (ft && g_list_length (ft->m_splits) == 1)
            {
                Split* temp_split;

                temp_split = xaccMallocSplit (gnc_get_current_book ());
                other_fs = gnc_split_to_float_split (temp_split, reg->is_template);
                xaccSplitDestroy (temp_split);

                gnc_float_txn_append_float_split (ft, other_fs);
            }
        }

        if (other_fs)
        {
            Account* new_account;

            new_account = gnc_split_register_get_account (reg, MXFRM_CELL);

            if (new_account != NULL)
                gnc_float_split_set_account (other_fs, new_account);
        }
    }

    if (gnc_table_layout_get_cell_changed (reg->table->layout,
                                           DEBT_CELL, TRUE) ||
        gnc_table_layout_get_cell_changed (reg->table->layout,
                                           CRED_CELL, TRUE))
    {
        BasicCell* cell;
        gnc_numeric new_value;
        gnc_numeric credit;
        gnc_numeric debit;

        cell = gnc_table_layout_get_cell (reg->table->layout, CRED_CELL);
        credit = gnc_price_cell_get_value ((PriceCell*) cell);

        cell = gnc_table_layout_get_cell (reg->table->layout, DEBT_CELL);
        debit = gnc_price_cell_get_value ((PriceCell*) cell);

        new_value = gnc_numeric_sub_fixed (debit, credit);

        gnc_float_split_set_value (fs, new_value);
    }

    if (gnc_table_layout_get_cell_changed (reg->table->layout, PRIC_CELL, TRUE))
    {
        /* do nothing for now */
    }

    if (gnc_table_layout_get_cell_changed (reg->table->layout, SHRS_CELL, TRUE))
    {
        BasicCell* cell;
        gnc_numeric shares;

        cell = gnc_table_layout_get_cell (reg->table->layout, SHRS_CELL);

        shares = gnc_price_cell_get_value ((PriceCell*) cell);

        gnc_float_split_set_amount (fs, shares);
    }

    if (gnc_table_layout_get_cell_changed (reg->table->layout,
                                           DEBT_CELL, TRUE) ||
        gnc_table_layout_get_cell_changed (reg->table->layout,
                                           CRED_CELL, TRUE) ||
        gnc_table_layout_get_cell_changed (reg->table->layout,
                                           PRIC_CELL, TRUE) ||
        gnc_table_layout_get_cell_changed (reg->table->layout,
                                           SHRS_CELL, TRUE))
    {
        if (other_fs)
        {
            gnc_numeric num;

            num = gnc_float_split_get_amount (fs);
            gnc_float_split_set_amount (other_fs, gnc_numeric_neg (num));

            num = gnc_float_split_get_value (fs);
            gnc_float_split_set_value (other_fs, gnc_numeric_neg (num));
        }
    }

    return TRUE;
}
static void
unreconcile_splits (SplitRegister* reg)
{
    if (reg->unrecn_splits == NULL)
        return; //Nothing to do.
    PINFO ("Unreconcile %d splits of reconciled transaction",
           g_list_length (reg->unrecn_splits));

    for (GList* node = reg->unrecn_splits; node; node = node->next)
    {
        Split* split = node->data;
        Transaction* txn = xaccSplitGetParent (split);
        if (!xaccTransIsOpen (txn))
            PWARN ("Unreconcile of split failed because its parent transaction wasn't open for editing");
        else if (xaccSplitGetReconcile (split) == YREC)
            xaccSplitSetReconcile (split, NREC);
    }
    g_list_free (reg->unrecn_splits);
    reg->unrecn_splits = NULL;
}

static GncSplitRegisterSaveResult
split_register_save_now (SplitRegister* reg, gboolean do_commit,
                         SplitRegisterSaveRequest *request)
{
    SRInfo* info;
    GncSplitRegisterSaveResult save_result;
    GncSplitRegisterExchangeResult exchange_result;
    Transaction* pending_trans;
    Transaction* blank_trans;
    Transaction* trans;
    Account* account;
    Split* blank_split;
    const char* memo;
    const char* desc;
    Split* split;

    ENTER ("reg=%p, do_commit=%s", reg, do_commit ? "TRUE" : "FALSE");

    if (!reg)
    {
        LEAVE ("no register");
        return GNC_SPLIT_REGISTER_SAVE_FAILED;
    }

    info = gnc_split_register_get_info (reg);

    blank_split = xaccSplitLookup (&info->blank_split_guid,
                                   gnc_get_current_book ());

    pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                     gnc_get_current_book ());

    blank_trans = xaccSplitGetParent (blank_split);

    /* get the handle to the current split and transaction */
    split = gnc_split_register_get_current_split (reg);
    trans = gnc_split_register_get_current_trans (reg);
    if (trans == NULL)
    {
        LEAVE ("no transaction");
        return GNC_SPLIT_REGISTER_SAVE_FAILED;
    }

    /* use the changed flag to avoid heavy-weight updates
     * of the split & transaction fields. This will help
     * cut down on unnecessary register redraws. */
    if (!gnc_table_current_cursor_changed (reg->table, FALSE))
    {
        if (!do_commit)
        {
            LEAVE ("commit unnecessary");
            return GNC_SPLIT_REGISTER_SAVE_FAILED;
        }

        if (!xaccTransIsOpen (trans))
        {
            LEAVE ("transaction not open");
            return GNC_SPLIT_REGISTER_SAVE_FAILED;
        }

        if (trans == pending_trans ||
            (trans == blank_trans && info->blank_split_edited))
        {
            /* We are going to commit. */

            gnc_suspend_gui_refresh ();

            if (trans == blank_trans)
            {
                /* We have to clear the blank split before the
                 * refresh or a new one won't be created. */
                info->last_date_entered = xaccTransGetDate (trans);
                info->blank_split_guid = *guid_null ();
                info->blank_split_edited = FALSE;
                info->auto_complete = FALSE;
            }

            /* We have to clear the pending guid *before* committing the
             * trans, because the event handler will find it otherwise. */
            if (trans == pending_trans)
                info->pending_trans_guid = *guid_null ();

            PINFO ("committing trans (%p)", trans);
            unreconcile_splits (reg);
            xaccTransCommitEdit (trans);
            xaccTransRecordPrice (trans, PRICE_SOURCE_SPLIT_REG);

            gnc_resume_gui_refresh ();
        }
        else
            DEBUG ("leaving trans (%p) open", trans);

        LEAVE ("unchanged cursor");
        return GNC_SPLIT_REGISTER_SAVE_SAVED;
    }

    DEBUG ("save split=%p", split);
    DEBUG ("blank_split=%p, blank_trans=%p, pending_trans=%p, trans=%p",
           blank_split, blank_trans, pending_trans, trans);

    /* Act on any changes to the current cell before the save. */
    if (!gnc_split_register_check_cell (reg,
                                        gnc_table_get_current_cell_name (reg->table)))
    {
        LEAVE ("need another go at changing cell");
        return GNC_SPLIT_REGISTER_SAVE_FAILED;
    }

    save_result = gnc_split_register_auto_calc (reg, split, request);
    if (save_result != GNC_SPLIT_REGISTER_SAVE_SAVED)
    {
        LEAVE ("auto calc pending or failed");
        return save_result;
    }

    /* Validate the transfer account names */
    (void)gnc_split_register_get_account (reg, MXFRM_CELL);
    (void)gnc_split_register_get_account (reg, XFRM_CELL);

    /* Maybe deal with exchange-rate transfers. A deferred dialog retains the
     * same SaveRequest and resumes this function from its completion callback. */
    exchange_result = gnc_split_register_handle_exchange_async
        (reg, FALSE, split_register_save_exchange_finished, request);
    if (exchange_result == GNC_SPLIT_REGISTER_EXCHANGE_DEFERRED)
    {
        LEAVE ("exchange request pending");
        return GNC_SPLIT_REGISTER_SAVE_DEFERRED;
    }
    if (exchange_result == GNC_SPLIT_REGISTER_EXCHANGE_REJECTED)
    {
        LEAVE ("exchange rate rejected");
        return GNC_SPLIT_REGISTER_SAVE_FAILED;
    }

    gnc_suspend_gui_refresh ();

    /* determine whether we should commit the pending transaction */
    if (pending_trans != trans)
    {
        // FIXME: How could the pending transaction not be open?
        // FIXME: For that matter, how could an open pending
        // transaction ever not be the current trans?
        if (xaccTransIsOpen (pending_trans))
        {
            g_warning ("Impossible? committing pending %p", pending_trans);
            unreconcile_splits (reg);
            xaccTransCommitEdit (pending_trans);
            xaccTransRecordPrice (trans, PRICE_SOURCE_SPLIT_REG);
        }
        else if (pending_trans)
        {
            g_critical ("BUG DETECTED! pending transaction (%p) not open",
                        pending_trans);
            g_assert_not_reached ();
        }

        if (trans == blank_trans)
        {
            /* Don't begin editing the blank trans, because it's
               already open, but mark it pending now. */
            g_assert (xaccTransIsOpen (blank_trans));
            /* This is now the pending transaction */
            info->pending_trans_guid = *xaccTransGetGUID (blank_trans);
        }
        else
        {
            PINFO ("beginning edit of trans %p", trans);
            if (gnc_split_register_begin_edit_or_warn (info, trans))
            {
                gnc_resume_gui_refresh ();
                LEAVE ("transaction opened elsewhere");
                return GNC_SPLIT_REGISTER_SAVE_FAILED;
            }
        }
        pending_trans = trans;
    }
    g_assert (xaccTransIsOpen (trans));

    /* If we are saving a brand new transaction and the blank split hasn't
     * been edited, then we need to give it a default account. */
    /* Q: Why check 'split == blank_split'? Isn't 'trans == blank_trans'
     *    even better? What if there were some way that we could be on
     *    a row other than the transaction row or blank split row, but
     *    the blank split still hasn't been edited? It seems to be assumed
     *    that it isn't possible, but... -Charles, Jan 2009 */
    if (split == blank_split && !info->blank_split_edited)
    {
        /* If we've reached this point, it means that the blank split is
         * anchoring the transaction - see gnc_split_register_add_transaction ()
         * for an explanation - and the transaction has been edited (as evidenced
         * by the earlier check for a changed cursor.) Since the blank split
         * itself has not been edited, we'll have to assign a default account. */
        account = gnc_split_register_get_default_account (reg);
        if (account)
            xaccSplitSetAccount (blank_split, account);
        xaccTransSetDateEnteredSecs (trans, gnc_time (NULL));
    }

    if (split == NULL)
    {
        /* If we were asked to save data for a row for which there is no
         * associated split, then assume that this was an "empty" row - see
         * gnc_split_register_add_transaction () for an explanation. This row
         * is used to add splits to an existing transaction, or to add the
         * 2nd through nth split rows to a brand new transaction.
         * xaccSRGetCurrent will handle this case, too. We will create
         * a new split, copy the row contents to that split, and append
         * the split to the pre-existing transaction. */
        Split* trans_split;

        split = xaccMallocSplit (gnc_get_current_book ());
        xaccTransAppendSplit (trans, split);

        gnc_table_set_virt_cell_data (reg->table,
                                      reg->table->current_cursor_loc.vcell_loc,
                                      xaccSplitGetGUID (split));
        DEBUG ("assigned cell to new split=%p", split);

        trans_split = gnc_split_register_get_current_trans_split (reg, NULL);
        if ((info->cursor_hint_trans == trans) &&
            (info->cursor_hint_trans_split == trans_split) &&
            (info->cursor_hint_split == NULL))
        {
            info->cursor_hint_split = split;
            info->cursor_hint_cursor_class = CURSOR_CLASS_SPLIT;
        }
    }

    DEBUG ("updating trans=%p", trans);

    {
        SRSaveData* sd;

        sd = gnc_split_register_save_data_new (
            trans, split, (info->trans_expanded ||
                           reg->style == REG_STYLE_AUTO_LEDGER ||
                           reg->style == REG_STYLE_JOURNAL));
        gnc_table_save_cells (reg->table, sd);
        gnc_split_register_save_data_destroy (sd);
    }

    memo = xaccSplitGetMemo (split);
    memo = memo ? memo : "(null)";
    desc = xaccTransGetDescription (trans);
    desc = desc ? desc : "(null)";
    PINFO ("finished saving split \"%s\" of trans \"%s\"", memo, desc);

    /* If the modified split is the "blank split", then it is now an
     * official part of the account. Set the blank split to NULL, so we
     * can be sure of getting a new blank split. Also, save the date
     * for the new blank split. */
    if (trans == blank_trans)
    {
        if (do_commit)
        {
            info->blank_split_guid = *guid_null ();
            info->auto_complete = FALSE;
            blank_split = NULL;
            info->last_date_entered = xaccTransGetDate (trans);
        }
        else
            info->blank_split_edited = TRUE;
    }

    /* If requested, commit the current transaction and set the pending
     * transaction to NULL. */
    if (do_commit)
    {
        g_assert (trans == blank_trans || trans == pending_trans);
        if (pending_trans == trans)
        {
            pending_trans = NULL;
            info->pending_trans_guid = *guid_null ();
        }
        unreconcile_splits (reg);
        xaccTransCommitEdit (trans);
        xaccTransRecordPrice (trans, PRICE_SOURCE_SPLIT_REG);
    }

    gnc_table_clear_current_cursor_changes (reg->table);

    gnc_resume_gui_refresh ();

    LEAVE (" ");
    return GNC_SPLIT_REGISTER_SAVE_SAVED;
}


static void
split_register_save_exchange_finished (SplitRegister *reg, gboolean accepted,
                                       gpointer user_data)
{
    SplitRegisterSaveRequest *request = user_data;

    if (!request || request->completed)
        return;
    if (!accepted || !reg || request->book != gnc_get_current_book ())
    {
        split_register_save_request_complete (request, FALSE);
        return;
    }
    split_register_save_request_continue (request);
}

static void
split_register_save_request_continue (SplitRegisterSaveRequest *request)
{
    GncSplitRegisterSaveResult result;
    SplitRegister *reg;

    if (!request || request->completed)
        return;

    reg = request->base.reg;
    if (!reg || !reg->table || request->book != gnc_get_current_book ())
    {
        split_register_save_request_complete (request, FALSE);
        return;
    }

    result = split_register_save_now (reg, request->do_commit, request);
    if (result == GNC_SPLIT_REGISTER_SAVE_DEFERRED)
        return;
    split_register_save_request_complete
        (request, result == GNC_SPLIT_REGISTER_SAVE_SAVED);
}

void
gnc_split_register_save_async (SplitRegister *reg, gboolean do_commit,
                               GncSplitRegisterSaveCallback callback,
                               gpointer user_data)
{
    SRInfo *info;
    SplitRegisterSaveRequest *request;

    if (!reg || !reg->table)
    {
        if (callback)
            callback (NULL, FALSE, user_data);
        return;
    }

    info = gnc_split_register_get_info (reg);
    if (!info || info->active_save_request)
    {
        if (callback)
            callback (reg, FALSE, user_data);
        return;
    }

    request = g_new0 (SplitRegisterSaveRequest, 1);
    g_atomic_ref_count_init (&request->ref_count);
    request->book = gnc_get_current_book ();
    request->do_commit = do_commit;
    request->callback = callback;
    request->user_data = user_data;
    info->active_save_request = &request->base;
    gnc_split_register_async_request_track (reg, &request->base,
                                            split_register_save_request_cancel);
    gnc_table_control_set_input_suspended (reg->table->control, TRUE);
    split_register_save_request_continue (request);
}

typedef struct
{
    gboolean completed;
    gboolean saved;
} SplitRegisterSaveSyncResult;

static void
split_register_save_sync_finished (G_GNUC_UNUSED SplitRegister *reg,
                                   gboolean saved, gpointer user_data)
{
    SplitRegisterSaveSyncResult *result = user_data;
    result->completed = TRUE;
    result->saved = saved;
}

gboolean
gnc_split_register_save (SplitRegister *reg, gboolean do_commit)
{
    SplitRegisterSaveSyncResult result = { FALSE, FALSE };

    gnc_split_register_save_async (reg, do_commit,
                                   split_register_save_sync_finished, &result);
    /* No caller may use this compatibility result to advance a continuation:
     * an unresolved request deliberately reports FALSE rather than success. */
    return result.completed && result.saved;
}
#define SPLIT_REGISTER_ACCOUNT_CREATE_REQUEST "gnc-split-register-account-create-request"

typedef struct
{
    GWeakRef parent;
    QofBook *book;
    gchar *name;
} SplitRegisterAccountCreateRequest;

static void
split_register_account_create_request_free (SplitRegisterAccountCreateRequest *request)
{
    GtkWidget *parent = GTK_WIDGET (g_weak_ref_get (&request->parent));

    if (parent && g_object_get_data (G_OBJECT (parent),
                                     SPLIT_REGISTER_ACCOUNT_CREATE_REQUEST) == request)
        g_object_steal_data (G_OBJECT (parent), SPLIT_REGISTER_ACCOUNT_CREATE_REQUEST);
    g_clear_object (&parent);
    g_weak_ref_clear (&request->parent);
    g_free (request->name);
    g_free (request);
}

static void
split_register_account_create_finished (Account *account, gboolean accepted,
                                        gpointer user_data)
{
    SplitRegisterAccountCreateRequest *request = user_data;
    GtkWidget *parent = GTK_WIDGET (g_weak_ref_get (&request->parent));

    if (accepted && account && parent && request->book == gnc_get_current_book () &&
        gnc_account_get_book (account) == request->book)
        gnc_gui_refresh_all ();
    g_clear_object (&parent);
    split_register_account_create_request_free (request);
}

static void
split_register_account_create_confirmed (GtkWindow *parent, gint response,
                                         gpointer user_data)
{
    SplitRegisterAccountCreateRequest *request = user_data;
    GtkWidget *owner = GTK_WIDGET (g_weak_ref_get (&request->parent));

    if (response == GTK_RESPONSE_YES && owner && GTK_IS_WINDOW (owner) &&
        owner == GTK_WIDGET (parent) && request->book == gnc_get_current_book ())
    {
        gnc_ui_new_accounts_from_name_with_defaults_async (
            GTK_WINDOW (owner), request->name, NULL, NULL, NULL,
            split_register_account_create_finished, request);
        g_clear_object (&owner);
        return;
    }
    g_clear_object (&owner);
    split_register_account_create_request_free (request);
}

Account*
gnc_split_register_get_account_by_name (SplitRegister* reg, BasicCell* bcell,
                                         const char* name)
{
    const char* placeholder = _ ("The account %s does not allow transactions.");
    const char* missing = _ ("The account %s does not exist. "
                             "Would you like to create it?");
    char* account_name;
    ComboCell* cell = (ComboCell*) bcell;
    Account* account;
    GtkWidget *owner = gnc_split_register_get_parent (reg);
    GtkWindow *parent = GTK_IS_WINDOW (owner) ? GTK_WINDOW (owner) : NULL;

    if (!name || (strlen (name) == 0))
        return NULL;

    /* Find the account */
    account = gnc_account_lookup_for_register (gnc_get_current_root_account (),
                                               name);
    if (!account)
        account = gnc_account_lookup_by_code (gnc_get_current_root_account (), name);

    if (!account)
    {
        SplitRegisterAccountCreateRequest *request;

        if (!parent || g_object_get_data (G_OBJECT (parent),
                                          SPLIT_REGISTER_ACCOUNT_CREATE_REQUEST))
            return NULL;
        request = g_new0 (SplitRegisterAccountCreateRequest, 1);
        request->book = gnc_get_current_book ();
        request->name = g_strdup (name);
        g_weak_ref_init (&request->parent, G_OBJECT (parent));
        g_object_set_data (G_OBJECT (parent), SPLIT_REGISTER_ACCOUNT_CREATE_REQUEST,
                           request);
        gnc_verify_dialog_async (parent, TRUE, split_register_account_create_confirmed,
                                 request, missing, name);
        return NULL;
    }

    if (account)
    {
        /* Now have the account. */
        account_name = gnc_get_account_name_for_split_register (account,
                                                                reg->show_leaf_accounts);
        if (g_strcmp0 (account_name, gnc_basic_cell_get_value (bcell)))
        {
            /* The name has changed. Update the cell. */
            gnc_combo_cell_set_value (cell, account_name);
            gnc_basic_cell_set_changed (&cell->cell, TRUE);
        }
        g_free (account_name);

        /* See if the account (either old or new) is a placeholder. */
        if (account && xaccAccountGetPlaceholder (account))
        {
            gchar* fullname = gnc_account_get_full_name (account);
            gnc_error_dialog (GTK_WINDOW (gnc_split_register_get_parent (reg)),
                              placeholder, fullname);
            g_free (fullname);
            return NULL;
        }
    }

    /* Be seeing you. */
    return account;
}

Account*
gnc_split_register_get_account (SplitRegister* reg, const char* cell_name)
{
    BasicCell* cell;
    const char* name;

    if (!gnc_table_layout_get_cell_changed (reg->table->layout, cell_name, TRUE))
        return NULL;

    cell = gnc_table_layout_get_cell (reg->table->layout, cell_name);
    if (!cell)
        return NULL;
    name = gnc_basic_cell_get_value (cell);
    return gnc_split_register_get_account_by_name (reg, cell, name);
}

static gnc_numeric
calculate_value (SplitRegister* reg)
{
    gnc_numeric credit;
    gnc_numeric debit;

    PriceCell* cell = (PriceCell*)gnc_table_layout_get_cell (reg->table->layout,
                                                             CRED_CELL);
    credit = gnc_price_cell_get_value (cell);

    cell = (PriceCell*)gnc_table_layout_get_cell (reg->table->layout,
                                                  DEBT_CELL);
    debit = gnc_price_cell_get_value (cell);

    return gnc_numeric_sub_fixed (debit, credit);
}


typedef struct
{
    GncSplitRegisterAsyncRequest base;
    SplitRegisterSaveRequest *save_request;
    QofBook *book;
    GncGUID split_guid;
    VirtualLocation virt_loc;
    gnc_numeric value;
    gnc_numeric price;
    gnc_numeric amount;
    gboolean shares_changed;
    gboolean value_changed;
    gboolean cancelled;
} SplitRegisterRecalcRequest;

static gboolean
split_register_recalc_request_is_current (SplitRegisterRecalcRequest *request,
                                          Split **split_out)
{
    SplitRegister *reg = request->base.reg;
    Split *split;

    if (!reg || !reg->table || request->book != gnc_get_current_book () ||
        !virt_loc_equal (reg->table->current_cursor_loc, request->virt_loc))
        return FALSE;

    split = gnc_split_register_get_current_split (reg);
    if (!split || !guid_equal (xaccSplitGetGUID (split), &request->split_guid))
        return FALSE;

    if (split_out)
        *split_out = split;
    return TRUE;
}

static void
split_register_recalc_request_cancel (GncSplitRegisterAsyncRequest *base)
{
    SplitRegisterRecalcRequest *request = (SplitRegisterRecalcRequest *)base;

    /* GtkAlertDialog still owns the completion callback.  Detach from the
     * register now, but keep the request and its SaveRequest reference until
     * that callback has observed cancellation and released both safely. */
    request->cancelled = TRUE;
    gnc_split_register_async_request_untrack (&request->base);
    if (request->save_request)
        split_register_save_request_complete (request->save_request, FALSE);
}

static void
split_register_recalc_choice_finished (G_GNUC_UNUSED GtkWindow *dialog,
                                       gint choice, gpointer user_data)
{
    SplitRegisterRecalcRequest *request = user_data;
    SplitRegisterSaveRequest *save_request = request->save_request;
    SplitRegister *reg = request->base.reg;
    Split *split = NULL;
    gboolean accepted = FALSE;

    if (!request->cancelled && choice >= 0 && save_request &&
        split_register_recalc_request_is_current (request, &split))
    {
        switch (choice)
        {
        case 0:
            if (!gnc_numeric_zero_p (request->price))
            {
                recalculate_shares (split, reg, request->value, request->price,
                                    request->value_changed);
                accepted = TRUE;
            }
            break;
        case 1:
            if (!gnc_numeric_zero_p (request->amount))
            {
                recalculate_price (split, reg, request->value, request->amount);
                accepted = TRUE;
            }
            break;
        case 2:
            recalculate_value (split, reg, request->price, request->amount,
                               request->shares_changed);
            accepted = TRUE;
            break;
        default:
            break;
        }
    }

    request->save_request = NULL;
    gnc_split_register_async_request_untrack (&request->base);
    g_free (request);
    if (accepted)
        split_register_save_request_continue (save_request);
    else
        split_register_save_request_complete (save_request, FALSE);
    if (save_request)
        split_register_save_request_unref (save_request);
}

static void
recalculate_shares (Split* split, SplitRegister* reg,
                    gnc_numeric value, gnc_numeric price, gboolean value_changed)
{
    gint64 denom = gnc_split_get_amount_denom (split);
    gnc_numeric amount = gnc_numeric_div (value, price, denom,
                                          GNC_HOW_RND_ROUND_HALF_UP);

    BasicCell* cell = gnc_table_layout_get_cell (reg->table->layout, SHRS_CELL);
    gnc_price_cell_set_value ((PriceCell*) cell, amount);
    gnc_basic_cell_set_changed (cell, TRUE);

    if (value_changed)
    {
        cell = gnc_table_layout_get_cell (reg->table->layout, PRIC_CELL);
        gnc_basic_cell_set_changed (cell, FALSE);
    }
}

static void
recalculate_price (Split* split, SplitRegister* reg,
                   gnc_numeric value, gnc_numeric amount)
{
    BasicCell* price_cell;
    gint64 denom = gnc_split_get_value_denom (split);
    gnc_numeric price = gnc_numeric_div (value, amount, denom,
                                         GNC_HOW_RND_ROUND_HALF_UP);

    price_cell = gnc_table_layout_get_cell (reg->table->layout, PRIC_CELL);
    gnc_price_cell_set_value ((PriceCell*) price_cell, price);
    gnc_basic_cell_set_changed (price_cell, TRUE);
}

static void
recalculate_value (Split* split, SplitRegister* reg,
                   gnc_numeric price, gnc_numeric amount, gboolean shares_changed)
{
    BasicCell* debit_cell = gnc_table_layout_get_cell (reg->table->layout,
                                                       DEBT_CELL);
    BasicCell* credit_cell = gnc_table_layout_get_cell (reg->table->layout,
                                                        CRED_CELL);
    gint64 denom = gnc_split_get_value_denom (split);
    gnc_numeric value = gnc_numeric_mul (price, amount, denom,
                                         GNC_HOW_RND_ROUND_HALF_UP);

    gnc_price_cell_set_debt_credit_value ((PriceCell*) debit_cell,
                                          (PriceCell*) credit_cell, value);

    gnc_basic_cell_set_changed (debit_cell, TRUE);
    gnc_basic_cell_set_changed (credit_cell, TRUE);

    if (shares_changed)
    {
        BasicCell* cell = gnc_table_layout_get_cell (reg->table->layout,
                                                     PRIC_CELL);
        gnc_basic_cell_set_changed (cell, FALSE);
    }
}

static GncSplitRegisterSaveResult
 gnc_split_register_auto_calc (SplitRegister* reg, Split* split,
                               SplitRegisterSaveRequest *save_request)
{
    PriceCell* cell = NULL;
    gboolean recalc_shares = FALSE;
    gboolean recalc_price = FALSE;
    gboolean recalc_value = FALSE;
    gboolean price_changed;
    gboolean value_changed;
    gboolean shares_changed;
    gnc_numeric calc_value;
    gnc_numeric value;
    gnc_numeric price;
    gnc_numeric amount;
    Account* account;
    int denom;

    if (STOCK_REGISTER != reg->type && CURRENCY_REGISTER != reg->type &&
        PORTFOLIO_LEDGER != reg->type)
        return GNC_SPLIT_REGISTER_SAVE_SAVED;

    account = gnc_split_register_get_account (reg, XFRM_CELL);
    if (!account)
        account = xaccSplitGetAccount (split);
    if (!account)
        account = gnc_split_register_get_default_account (reg);
    if (!xaccAccountIsPriced (account))
        return GNC_SPLIT_REGISTER_SAVE_SAVED;

    price_changed = gnc_table_layout_get_cell_changed (reg->table->layout,
                                                       PRIC_CELL, TRUE);
    value_changed = (gnc_table_layout_get_cell_changed (reg->table->layout,
                                                        DEBT_CELL, TRUE) ||
                     gnc_table_layout_get_cell_changed (reg->table->layout,
                                                        CRED_CELL, TRUE));
    shares_changed = gnc_table_layout_get_cell_changed (reg->table->layout,
                                                        SHRS_CELL, TRUE);
    if (!price_changed && !value_changed && !shares_changed)
        return GNC_SPLIT_REGISTER_SAVE_SAVED;

    if (xaccTransUseTradingAccounts (xaccSplitGetParent (split)))
    {
        gnc_commodity* acc_commodity = xaccAccountGetCommodity (account);
        if (!(xaccAccountIsPriced (account) || !gnc_commodity_is_iso (acc_commodity)))
            return GNC_SPLIT_REGISTER_SAVE_SAVED;
    }

    if (shares_changed)
    {
        cell = (PriceCell*) gnc_table_layout_get_cell (reg->table->layout,
                                                       SHRS_CELL);
        amount = gnc_price_cell_get_value (cell);
    }
    else
        amount = xaccSplitGetAmount (split);

    if (price_changed)
    {
        cell = (PriceCell*) gnc_table_layout_get_cell (reg->table->layout,
                                                       PRIC_CELL);
        price = gnc_price_cell_get_value (cell);
    }
    else
        price = xaccSplitGetSharePrice (split);

    value = value_changed ? calculate_value (reg) : xaccSplitGetValue (split);
    if (gnc_numeric_zero_p (amount) && gnc_numeric_zero_p (price) &&
        !gnc_numeric_zero_p (value))
        return GNC_SPLIT_REGISTER_SAVE_SAVED;

    if (!gnc_numeric_zero_p (amount))
    {
        if (gnc_numeric_zero_p (price))
        {
            if (!gnc_numeric_zero_p (value))
                recalc_price = TRUE;
        }
        else if (gnc_numeric_zero_p (value))
            recalc_value = TRUE;
    }
    else if (!gnc_numeric_zero_p (price) && !gnc_numeric_zero_p (value))
        recalc_shares = TRUE;

    if (!recalc_shares && !recalc_price && !recalc_value)
    {
        if (price_changed && value_changed && !shares_changed)
            recalc_shares = TRUE;
        else if (value_changed && shares_changed)
            recalc_price = TRUE;
        else if (price_changed && shares_changed)
            recalc_value = TRUE;
    }

    calc_value = gnc_numeric_mul (price, amount, GNC_DENOM_AUTO,
                                  GNC_HOW_DENOM_LCD);
    denom = gnc_split_get_value_denom (split);
    if (!recalc_shares && !recalc_price && !recalc_value &&
        !gnc_numeric_same (value, calc_value, denom, GNC_HOW_RND_ROUND_HALF_UP))
    {
        GList *choices = NULL;
        SplitRegisterRecalcRequest *request;
        gint default_choice = price_changed ? 2 : 1;

        choices = g_list_append (choices, g_strdup_printf
                                 (shares_changed ? "%s (%s)" : "%s",
                                  _("_Shares"), shares_changed ? _("Changed") : ""));
        choices = g_list_append (choices, g_strdup_printf
                                 (price_changed ? "%s (%s)" : "%s",
                                  _("_Price"), price_changed ? _("Changed") : ""));
        choices = g_list_append (choices, g_strdup_printf
                                 (value_changed ? "%s (%s)" : "%s",
                                  _("_Value"), value_changed ? _("Changed") : ""));
        request = g_new0 (SplitRegisterRecalcRequest, 1);
        request->save_request = split_register_save_request_ref (save_request);
        request->book = gnc_get_current_book ();
        request->split_guid = *xaccSplitGetGUID (split);
        request->virt_loc = reg->table->current_cursor_loc;
        request->value = value;
        request->price = price;
        request->amount = amount;
        request->shares_changed = shares_changed;
        request->value_changed = value_changed;
        gnc_split_register_async_request_track (reg, &request->base,
                                                split_register_recalc_request_cancel);
        GtkWidget *owner = gnc_split_register_get_parent (reg);
        gnc_choose_option_dialog_async
            (GTK_IS_WINDOW (owner) ? GTK_WINDOW (owner) : NULL, _("Recalculate Transaction"),
             _("The values entered for this transaction are inconsistent. Which "
               "value would you like to have recalculated?"), choices,
             default_choice, split_register_recalc_choice_finished, request);
        g_list_free_full (choices, g_free);
        return GNC_SPLIT_REGISTER_SAVE_DEFERRED;
    }

    if (recalc_shares && !gnc_numeric_zero_p (price))
        recalculate_shares (split, reg, value, price, value_changed);
    if (recalc_price && !gnc_numeric_zero_p (amount))
        recalculate_price (split, reg, value, amount);
    if (recalc_value)
        recalculate_value (split, reg, price, amount, shares_changed);

    return GNC_SPLIT_REGISTER_SAVE_SAVED;
}
static GNCAccountType
gnc_split_register_type_to_account_type (SplitRegisterType sr_type)
{
    switch (sr_type)
    {
        case BANK_REGISTER:
            return ACCT_TYPE_BANK;
        case CASH_REGISTER:
            return ACCT_TYPE_CASH;
        case ASSET_REGISTER:
            return ACCT_TYPE_ASSET;
        case CREDIT_REGISTER:
            return ACCT_TYPE_CREDIT;
        case LIABILITY_REGISTER:
            return ACCT_TYPE_LIABILITY;
        case PAYABLE_REGISTER:
            return ACCT_TYPE_PAYABLE;
        case RECEIVABLE_REGISTER:
            return ACCT_TYPE_RECEIVABLE;
        case INCOME_LEDGER:
        case INCOME_REGISTER:
            return ACCT_TYPE_INCOME;
        case EXPENSE_REGISTER:
            return ACCT_TYPE_EXPENSE;
        case STOCK_REGISTER:
        case PORTFOLIO_LEDGER:
            return ACCT_TYPE_STOCK;
        case CURRENCY_REGISTER:
            return ACCT_TYPE_CURRENCY;
        case TRADING_REGISTER:
            return ACCT_TYPE_TRADING;
        case GENERAL_JOURNAL:
            return ACCT_TYPE_NONE;
        case EQUITY_REGISTER:
            return ACCT_TYPE_EQUITY;
        case SEARCH_LEDGER:
            return ACCT_TYPE_NONE;
        default:
            return ACCT_TYPE_NONE;
    }
}

const char*
gnc_split_register_get_debit_string (SplitRegister* reg)
{
    SRInfo* info = gnc_split_register_get_info (reg);

    if (!reg)
        return NULL;

    if (info->debit_str)
        return info->debit_str;

    info->debit_str =
        gnc_account_get_debit_string
        (gnc_split_register_type_to_account_type (reg->type));

    if (info->debit_str)
        return info->debit_str;

    info->debit_str = g_strdup (_ ("Debit"));

    return info->debit_str;
}

const char*
gnc_split_register_get_credit_string (SplitRegister* reg)
{
    SRInfo* info = gnc_split_register_get_info (reg);

    if (!reg)
        return NULL;

    if (info->credit_str)
        return info->credit_str;

    info->credit_str =
        gnc_account_get_credit_string
        (gnc_split_register_type_to_account_type (reg->type));

    if (info->credit_str)
        return info->credit_str;

    info->credit_str = g_strdup (_ ("Credit"));

    return info->credit_str;
}

gboolean
gnc_split_register_changed (SplitRegister* reg)
{
    SRInfo* info = gnc_split_register_get_info (reg);
    Transaction* pending_trans;

    ENTER ("reg=%p", reg);

    if (reg == NULL)
    {
        LEAVE ("no register");
        return FALSE;
    }

    if (gnc_table_current_cursor_changed (reg->table, FALSE))
    {
        LEAVE ("cursor changed");
        return TRUE;
    }

    pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                     gnc_get_current_book ());
    if (xaccTransIsOpen (pending_trans))
    {
        LEAVE ("open and pending txn");
        return TRUE;
    }

    LEAVE ("register unchanged");
    return FALSE;
}

void
gnc_split_register_show_present_divider (SplitRegister* reg,
                                         gboolean show_present)
{
    SRInfo* info = gnc_split_register_get_info (reg);

    if (reg == NULL)
        return;

    info->show_present_divider = show_present;
}

gboolean
gnc_split_register_full_refresh_ok (SplitRegister* reg)
{
    SRInfo* info = gnc_split_register_get_info (reg);

    if (!info)
        return FALSE;

    return info->full_refresh;
}

/* configAction strings into the action cell */
/* hack alert -- this stuff really, really should be in a config file ... */
static void
gnc_split_register_config_action (SplitRegister* reg)
{
    ComboCell* cell;

    cell = (ComboCell*) gnc_table_layout_get_cell (reg->table->layout,
                                                   ACTN_CELL);

    /* setup strings in the action pull-down */
    switch (reg->type)
    {
        case BANK_REGISTER:
            /* broken ! FIXME bg */
        case SEARCH_LEDGER:
            gnc_combo_cell_add_menu_item (cell, C_ ("Action Column", "Deposit"));
            gnc_combo_cell_add_menu_item (cell, _ ("Withdraw"));
            gnc_combo_cell_add_menu_item (cell, _ ("Check"));
            gnc_combo_cell_add_menu_item (cell, _ ("Interest"));
            gnc_combo_cell_add_menu_item (cell, _ ("ATM Deposit"));
            gnc_combo_cell_add_menu_item (cell, _ ("ATM Draw"));
            gnc_combo_cell_add_menu_item (cell, _ ("Teller"));
            gnc_combo_cell_add_menu_item (cell, _ ("Charge"));
            gnc_combo_cell_add_menu_item (cell, _ ("Payment"));
            gnc_combo_cell_add_menu_item (cell, _ ("Receipt"));
            gnc_combo_cell_add_menu_item (cell, _ ("Increase"));
            gnc_combo_cell_add_menu_item (cell, _ ("Decrease"));
            /* Action: Point Of Sale */
            gnc_combo_cell_add_menu_item (cell, _ ("POS"));
            gnc_combo_cell_add_menu_item (cell, _ ("Phone"));
            gnc_combo_cell_add_menu_item (cell, _ ("Online"));
            /* Action: Automatic Deposit ?!? */
            gnc_combo_cell_add_menu_item (cell, _ ("AutoDep"));
            gnc_combo_cell_add_menu_item (cell, _ ("Wire"));
            gnc_combo_cell_add_menu_item (cell, _ ("Credit"));
            gnc_combo_cell_add_menu_item (cell, _ ("Direct Debit"));
            gnc_combo_cell_add_menu_item (cell, _ ("Transfer"));
            break;
        case CASH_REGISTER:
            gnc_combo_cell_add_menu_item (cell, _ ("Increase"));
            gnc_combo_cell_add_menu_item (cell, _ ("Decrease"));
            gnc_combo_cell_add_menu_item (cell, _ ("Buy"));
            gnc_combo_cell_add_menu_item (cell, _ ("Sell"));
            break;
        case ASSET_REGISTER:
            gnc_combo_cell_add_menu_item (cell, _ ("Buy"));
            gnc_combo_cell_add_menu_item (cell, _ ("Sell"));
            gnc_combo_cell_add_menu_item (cell, _ ("Fee"));
            break;
        case CREDIT_REGISTER:
            gnc_combo_cell_add_menu_item (cell, _ ("ATM Deposit"));
            gnc_combo_cell_add_menu_item (cell, _ ("ATM Draw"));
            gnc_combo_cell_add_menu_item (cell, _ ("Buy"));
            gnc_combo_cell_add_menu_item (cell, _ ("Credit"));
            gnc_combo_cell_add_menu_item (cell, _ ("Fee"));
            gnc_combo_cell_add_menu_item (cell, _ ("Interest"));
            gnc_combo_cell_add_menu_item (cell, _ ("Online"));
            gnc_combo_cell_add_menu_item (cell, _ ("Sell"));
            break;
        case LIABILITY_REGISTER:
            gnc_combo_cell_add_menu_item (cell, _ ("Buy"));
            gnc_combo_cell_add_menu_item (cell, _ ("Sell"));
            gnc_combo_cell_add_menu_item (cell, _ ("Loan"));
            gnc_combo_cell_add_menu_item (cell, _ ("Interest"));
            gnc_combo_cell_add_menu_item (cell, _ ("Payment"));
            break;
        case RECEIVABLE_REGISTER:
        case PAYABLE_REGISTER:
            gnc_combo_cell_add_menu_item (cell, _ ("Invoice"));
            gnc_combo_cell_add_menu_item (cell, _ ("Payment"));
            gnc_combo_cell_add_menu_item (cell, _ ("Interest"));
            gnc_combo_cell_add_menu_item (cell, _ ("Credit"));
            break;
        case INCOME_LEDGER:
        case INCOME_REGISTER:
            gnc_combo_cell_add_menu_item (cell, _ ("Increase"));
            gnc_combo_cell_add_menu_item (cell, _ ("Decrease"));
            gnc_combo_cell_add_menu_item (cell, _ ("Buy"));
            gnc_combo_cell_add_menu_item (cell, _ ("Sell"));
            gnc_combo_cell_add_menu_item (cell, _ ("Interest"));
            gnc_combo_cell_add_menu_item (cell, _ ("Payment"));
            gnc_combo_cell_add_menu_item (cell, _ ("Rebate"));
            gnc_combo_cell_add_menu_item (cell, _ ("Paycheck"));
            break;
        case EXPENSE_REGISTER:
        case TRADING_REGISTER:
            gnc_combo_cell_add_menu_item (cell, _ ("Increase"));
            gnc_combo_cell_add_menu_item (cell, _ ("Decrease"));
            gnc_combo_cell_add_menu_item (cell, _ ("Buy"));
            gnc_combo_cell_add_menu_item (cell, _ ("Sell"));
            break;
        case GENERAL_JOURNAL:
        case EQUITY_REGISTER:
            gnc_combo_cell_add_menu_item (cell, _ ("Buy"));
            gnc_combo_cell_add_menu_item (cell, _ ("Sell"));
            gnc_combo_cell_add_menu_item (cell, _ ("Equity"));
            break;
        case STOCK_REGISTER:
        case PORTFOLIO_LEDGER:
        case CURRENCY_REGISTER:
            gnc_combo_cell_add_menu_item (cell, ACTION_BUY_STR);
            gnc_combo_cell_add_menu_item (cell, ACTION_SELL_STR);
            gnc_combo_cell_add_menu_item (cell, _ ("Price"));
            gnc_combo_cell_add_menu_item (cell, _ ("Fee"));
            /* Action: Dividend */
            gnc_combo_cell_add_menu_item (cell, _ ("Dividend"));
            gnc_combo_cell_add_menu_item (cell, _ ("Interest"));
            /* Action: Long Term Capital Gains */
            gnc_combo_cell_add_menu_item (cell, _ ("LTCG"));
            /* Action: Short Term Capital Gains */
            gnc_combo_cell_add_menu_item (cell, _ ("STCG"));
            gnc_combo_cell_add_menu_item (cell, _ ("Income"));
            /* Action: Distribution */
            gnc_combo_cell_add_menu_item (cell, _ ("Dist"));
            gnc_combo_cell_add_menu_item (cell, C_ ("Action Column", "Split"));
            break;

        default:
            gnc_combo_cell_add_menu_item (cell, _ ("Increase"));
            gnc_combo_cell_add_menu_item (cell, _ ("Decrease"));
            gnc_combo_cell_add_menu_item (cell, _ ("Buy"));
            gnc_combo_cell_add_menu_item (cell, _ ("Sell"));
            break;
    }
}

static void
gnc_split_register_config_cells (SplitRegister* reg)
{
    gnc_combo_cell_add_ignore_string
        ((ComboCell*)
         gnc_table_layout_get_cell (reg->table->layout, MXFRM_CELL),
         SPLIT_TRANS_STR);

    gnc_combo_cell_add_ignore_string
        ((ComboCell*)
         gnc_table_layout_get_cell (reg->table->layout, MXFRM_CELL),
         STOCK_SPLIT_STR);

    /* the action cell */
    gnc_combo_cell_set_autosize
        ((ComboCell*)
         gnc_table_layout_get_cell (reg->table->layout, ACTN_CELL), TRUE);

     /* the description cell */
    gnc_completion_cell_set_autosize
        ((CompletionCell*)
          gnc_table_layout_get_cell (reg->table->layout, DESC_CELL), TRUE);

    /* Use GNC_COMMODITY_MAX_FRACTION for prices and "exchange rates"  */
    gnc_price_cell_set_fraction
        ((PriceCell*)
         gnc_table_layout_get_cell (reg->table->layout, PRIC_CELL),
         GNC_COMMODITY_MAX_FRACTION);

    /* Initialize shares and share balance cells */
    gnc_price_cell_set_print_info
        ((PriceCell*) gnc_table_layout_get_cell (reg->table->layout, SHRS_CELL),
         gnc_default_share_print_info ());

    gnc_price_cell_set_print_info
        ((PriceCell*) gnc_table_layout_get_cell (reg->table->layout, TSHRS_CELL),
         gnc_default_share_print_info ());

    /* Initialize the rate cell
     * use a share_print_info to make sure we don't have rounding errors
     */
    gnc_price_cell_set_print_info
        ((PriceCell*) gnc_table_layout_get_cell (reg->table->layout, RATE_CELL),
         gnc_default_share_print_info ());

    /* The action cell should accept strings not in the list */
    gnc_combo_cell_set_strict
        ((ComboCell*)
         gnc_table_layout_get_cell (reg->table->layout, ACTN_CELL), FALSE);

     /* The description cell should accept strings not in the list */
    gnc_completion_cell_set_strict
        ((CompletionCell*)
          gnc_table_layout_get_cell (reg->table->layout, DESC_CELL), FALSE);

    /* number format for share quantities in stock ledgers */
    switch (reg->type)
    {
        case CURRENCY_REGISTER:
        case STOCK_REGISTER:
        case PORTFOLIO_LEDGER:
            gnc_price_cell_set_print_info
                ((PriceCell*)
                 gnc_table_layout_get_cell (reg->table->layout, PRIC_CELL),
                 gnc_default_price_print_info (gnc_default_currency ()));
            break;

        default:
            break;
    }

    /* add menu items for the action cell */
    gnc_split_register_config_action (reg);
}

static void
split_register_pref_changed (gpointer prefs, gchar* pref, gpointer user_data)
{
    SplitRegister* reg = user_data;
    SRInfo* info;

    g_return_if_fail (pref);
    if (reg == NULL)
        return;

    info = reg->sr_info;
    if (!info)
        return;

    if (g_str_has_suffix (pref, GNC_PREF_ACCOUNTING_LABELS))
    {
        /* Release current strings. Will be reloaded at next reference. */
        g_free (info->tdebit_str);
        g_free (info->tcredit_str);

        info->debit_str = NULL;
        info->tdebit_str = NULL;
        info->credit_str = NULL;
        info->tcredit_str = NULL;

    }
    else if (g_str_has_suffix (pref, GNC_PREF_ACCOUNT_SEPARATOR))
    {
        info->separator_changed = TRUE;
    }
    else if (g_str_has_suffix (pref, GNC_PREF_SHOW_LEAF_ACCT_NAMES))
    {
        reg->show_leaf_accounts = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                                      GNC_PREF_SHOW_LEAF_ACCT_NAMES);
    }
    else if (g_str_has_suffix (pref, GNC_PREF_ALT_COLOR_BY_TRANS))
    {
        reg->double_alt_color = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                                    GNC_PREF_ALT_COLOR_BY_TRANS);
    }
    else
    {
        g_warning ("split_register_pref_changed: Unknown preference %s", pref);
    }
}

static void
split_register_book_option_changed (gpointer new_val, gpointer user_data)
{
    SplitRegister* reg = user_data;
    gboolean* new_data = (gboolean*)new_val;

    if (reg == NULL)
        return;

    reg->use_tran_num_for_num_field = (*new_data ? FALSE : TRUE);
}

static void
gnc_split_register_init (SplitRegister* reg,
                         SplitRegisterType type,
                         SplitRegisterStyle style,
                         gboolean use_double_line,
                         gboolean do_auto_complete,
                         gboolean is_template,
                         gboolean mismatched_commodities)
{
    TableLayout* layout;
    TableModel* model;
    TableControl* control;

    /* Register 'destroy' callback */
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                           GNC_PREF_ACCOUNTING_LABELS,
                           split_register_pref_changed,
                           reg);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                           GNC_PREF_ACCOUNT_SEPARATOR,
                           split_register_pref_changed,
                           reg);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL_REGISTER,
                           GNC_PREF_SHOW_LEAF_ACCT_NAMES,
                           split_register_pref_changed,
                           reg);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL_REGISTER,
                           GNC_PREF_ALT_COLOR_BY_TRANS,
                           split_register_pref_changed,
                           reg);
    gnc_book_option_register_cb (OPTION_NAME_NUM_FIELD_SOURCE,
                                 split_register_book_option_changed,
                                 reg);

    reg->sr_info = NULL;

    reg->unrecn_splits = NULL;

    reg->show_leaf_accounts = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                                  GNC_PREF_SHOW_LEAF_ACCT_NAMES);
    reg->double_alt_color = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                                GNC_PREF_ALT_COLOR_BY_TRANS);

    reg->type = type;
    reg->style = style;
    reg->use_double_line = use_double_line;
    reg->do_auto_complete = do_auto_complete;
    reg->is_template = is_template;
    reg->mismatched_commodities = mismatched_commodities;
    reg->use_tran_num_for_num_field =
        (qof_book_use_split_action_for_num_field (gnc_get_current_book ())
         ? FALSE : TRUE);

    layout = gnc_split_register_layout_new (reg);

    if (is_template)
        model = gnc_template_register_model_new ();
    else
        model = gnc_split_register_model_new ();
    model->handler_user_data = reg;

    control = gnc_split_register_control_new ();
    control->user_data = reg;

    reg->table = gnc_table_new (layout, model, control);

    gnc_split_register_config_cells (reg);

    /* Set up header */
    {
        VirtualCellLocation vcell_loc = { 0, 0 };
        CellBlock* header;

        header = gnc_table_layout_get_cursor (reg->table->layout, CURSOR_HEADER);

        gnc_table_set_vcell (reg->table, header, NULL, TRUE, TRUE, vcell_loc);
    }

    /* Set up first and only initial row */
    {
        VirtualLocation vloc;
        CellBlock* cursor;

        vloc.vcell_loc.virt_row = 1;
        vloc.vcell_loc.virt_col = 0;
        vloc.phys_row_offset = 0;
        vloc.phys_col_offset = 0;

        cursor = gnc_table_layout_get_cursor (reg->table->layout,
                                              CURSOR_SINGLE_LEDGER);

        gnc_table_set_vcell (reg->table, cursor, NULL, TRUE, TRUE, vloc.vcell_loc);

        if (gnc_table_find_close_valid_cell (reg->table, &vloc, FALSE))
            gnc_table_move_cursor (reg->table, vloc);
        else
        {
            PERR ("Can't find valid initial location");
        }
    }
}

SplitRegister*
gnc_split_register_new (SplitRegisterType type,
                        SplitRegisterStyle style,
                        gboolean use_double_line,
                        gboolean is_template,
                        gboolean mismatched_commodities)
{
    SplitRegister* reg;
    gboolean default_do_auto_complete = TRUE;

    reg = g_new0 (SplitRegister, 1);

    if (type >= NUM_SINGLE_REGISTER_TYPES)
        style = REG_STYLE_JOURNAL;

    gnc_split_register_init (reg,
                             type,
                             style,
                             use_double_line,
                             default_do_auto_complete,
                             is_template,
                             mismatched_commodities);

    return reg;
}

void
gnc_split_register_config (SplitRegister* reg,
                           SplitRegisterType newtype,
                           SplitRegisterStyle newstyle,
                           gboolean use_double_line)
{
    if (!reg) return;

    /* If shrinking the transaction split, put the cursor on the first row of the trans */
    if (reg->use_double_line && !use_double_line)
    {
        VirtualLocation virt_loc = reg->table->current_cursor_loc;
        if (gnc_table_find_close_valid_cell (reg->table, &virt_loc, FALSE))
        {
            if (virt_loc.phys_row_offset)
            {
                gnc_table_move_vertical_position (reg->table, &virt_loc,
                                                  -virt_loc.phys_row_offset);
                gnc_table_move_cursor_gui (reg->table, virt_loc);
            }
        }
        else
        {
            /* WTF?  Go to a known safe location. */
            virt_loc.vcell_loc.virt_row = 1;
            virt_loc.vcell_loc.virt_col = 0;
            virt_loc.phys_row_offset = 0;
            virt_loc.phys_col_offset = 0;
            gnc_table_move_cursor_gui (reg->table, virt_loc);
        }
    }

    reg->type = newtype;

    if (reg->type >= NUM_SINGLE_REGISTER_TYPES)
        newstyle = REG_STYLE_JOURNAL;

    reg->style = newstyle;
    reg->use_double_line = use_double_line;

    gnc_table_realize_gui (reg->table);
}

void
gnc_split_register_set_reverse_sort (SplitRegister* reg, gboolean reverse_sort)
{
    g_return_if_fail (reg);
    gnc_table_model_set_reverse_sort (reg->table->model, reverse_sort);
}

void
gnc_split_register_set_auto_complete (SplitRegister* reg,
                                      gboolean do_auto_complete)
{
    g_return_if_fail (reg);
    reg->do_auto_complete = do_auto_complete;
}

static void
gnc_split_register_destroy_info (SplitRegister* reg)
{
    SRInfo* info;

    if (reg == NULL)
        return;

    if (reg->unrecn_splits != NULL)
    {
        g_list_free (reg->unrecn_splits);
        reg->unrecn_splits =  NULL;
    }

    info = reg->sr_info;
    if (!info)
        return;

    gnc_split_register_cancel_async_requests (reg);
    g_assert (info->async_requests == NULL);
    info->active_save_request = NULL;

    g_free (info->tdebit_str);
    g_free (info->tcredit_str);

    info->debit_str = NULL;
    info->tdebit_str = NULL;
    info->credit_str = NULL;
    info->tcredit_str = NULL;

    g_free (reg->sr_info);

    reg->sr_info = NULL;
}

void
gnc_split_register_set_data (SplitRegister* reg, void* user_data,
                             SRGetParentCallback get_parent)
{
    SRInfo* info = gnc_split_register_get_info (reg);

    g_return_if_fail (reg != NULL);

    info->user_data = user_data;
    info->get_parent = get_parent;
}

static void
gnc_split_register_cleanup (SplitRegister* reg)
{
    SRInfo* info = gnc_split_register_get_info (reg);
    Transaction* pending_trans;
    Transaction* blank_trans = NULL;
    Split* blank_split;

    ENTER ("reg=%p", reg);

    blank_split = xaccSplitLookup (&info->blank_split_guid,
                                   gnc_get_current_book ());

    pending_trans = xaccTransLookup (&info->pending_trans_guid,
                                     gnc_get_current_book ());

    gnc_suspend_gui_refresh ();

    /* Destroy the transaction containing the "blank split", which was only
     * created to support the area for entering a new transaction. Since the
     * register is closing, this transaction is no longer needed. */
    if (blank_split != NULL)
    {
        gboolean was_open;

        blank_trans = xaccSplitGetParent (blank_split);

        DEBUG ("blank_split=%p, blank_trans=%p, pending_trans=%p",
               blank_split, blank_trans, pending_trans);

        /* Destroying the transaction will automatically remove its splits. */
        was_open = xaccTransIsOpen (blank_trans);
        xaccTransDestroy (blank_trans);
        if (was_open)
            xaccTransCommitEdit (blank_trans);

        /* Update the register info. */
        if (blank_trans == pending_trans)
        {
            info->pending_trans_guid = *guid_null ();
            pending_trans = NULL;
        }
        info->blank_split_guid = *guid_null ();
        info->auto_complete = FALSE;
        blank_split = NULL;
    }

    /* be sure to take care of any open transactions */
    if (pending_trans != NULL)
    {
        g_critical ("BUG DETECTED: pending_trans=%p, blank_split=%p, blank_trans=%p",
                    pending_trans, blank_split, blank_trans);
        g_assert_not_reached ();
        info->pending_trans_guid = *guid_null ();
        /* CAS: It's not clear to me that we'd really want to commit
           here, rather than rollback. But, maybe this is just dead
           code. */
        if (xaccTransIsOpen (pending_trans))
            xaccTransCommitEdit (pending_trans);
        else g_assert_not_reached ();

        pending_trans = NULL;
    }

    gnc_split_register_destroy_info (reg);

    gnc_resume_gui_refresh ();

    LEAVE (" ");
}

void
gnc_split_register_destroy (SplitRegister* reg)
{
    g_return_if_fail (reg);

    ENTER ("reg=%p", reg);

    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_ACCOUNTING_LABELS,
                                 split_register_pref_changed,
                                 reg);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_ACCOUNT_SEPARATOR,
                                 split_register_pref_changed,
                                 reg);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                 GNC_PREF_SHOW_LEAF_ACCT_NAMES,
                                 split_register_pref_changed,
                                 reg);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL_REGISTER,
                                 GNC_PREF_ALT_COLOR_BY_TRANS,
                                 split_register_pref_changed,
                                 reg);
    gnc_book_option_remove_cb (OPTION_NAME_NUM_FIELD_SOURCE,
                               split_register_book_option_changed,
                               reg);

    gnc_split_register_cleanup (reg);

    gnc_table_destroy (reg->table);
    reg->table = NULL;

    /* free the memory itself */
    g_free (reg);
    LEAVE (" ");
}

void
gnc_split_register_set_read_only (SplitRegister* reg, gboolean read_only)
{
    gnc_table_model_set_read_only (reg->table->model, read_only);
}

SplitRegisterTypeGroup
gnc_split_register_get_register_group (SplitRegister *reg)
{
    switch (reg->type)
    {
        case BANK_REGISTER:
        case CASH_REGISTER:
        case ASSET_REGISTER:
        case CREDIT_REGISTER:
        case LIABILITY_REGISTER:
        case INCOME_REGISTER:
        case EXPENSE_REGISTER:
        case EQUITY_REGISTER:
        case TRADING_REGISTER:
        {
            return REG_TYPE_GROUP_CURRENCY;
            break;
        }
        case PAYABLE_REGISTER:
        case RECEIVABLE_REGISTER:
        {
            return REG_TYPE_GROUP_APAR;
            break;
        }
        case INCOME_LEDGER:
        case GENERAL_JOURNAL:
        case SEARCH_LEDGER:
        {
            return REG_TYPE_GROUP_JOURNAL;
            break;
        }
        case STOCK_REGISTER:
        case CURRENCY_REGISTER:
        {
            return REG_TYPE_GROUP_STOCK;
            break;
        }
        case PORTFOLIO_LEDGER:
        {
            return REG_TYPE_GROUP_PORTFOLIO;
            break;
        }
        default:
            return REG_TYPE_GROUP_UNKNOWN;
            PERR ("unknown register type %d\n", reg->type);
        break;
    }
}

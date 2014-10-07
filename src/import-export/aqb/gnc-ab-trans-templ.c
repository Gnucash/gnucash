/*
 * gnc-ab-trans-templ.c --
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

/**
 * @internal
 * @file gnc-ab-trans-templ.c
 * @brief Templates for AqBanking transactions
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#include "config.h"

#include "gnc-ab-trans-templ.h"

/* This static indicates the debugging module that this .o belongs to.  */
G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

/* KvpFrame slot names */
#define TT_NAME "name"
#define TT_RNAME "rnam"
#define TT_RACC "racc"
#define TT_RBCODE "rbcd"
#define TT_PURPOS "purp"
#define TT_PURPOSCT "purc"
#define TT_AMOUNT "amou"

struct _GncABTransTempl
{
    /* Name of this Template */
    gchar *name;
    gchar *name_key; /* Collation key */

    /* Recipient */
    gchar *recp_name;
    gchar *recp_account;
    gchar *recp_bankcode;

    /* Amount */
    gnc_numeric amount;

    /* Purpose, description */
    gchar *purpose;
    gchar *purpose_cont;
};


GncABTransTempl *
gnc_ab_trans_templ_new(void)
{
    return gnc_ab_trans_templ_new_full(NULL, NULL, NULL, NULL,
                                       gnc_numeric_zero(), NULL, NULL);
}

GncABTransTempl *
gnc_ab_trans_templ_new_full(const char *name, const char *recp_name,
                            const char *recp_account, const char *recp_bankcode,
                            gnc_numeric amount, const char *purpose,
                            const char *purpose_cont)
{
    GncABTransTempl *r = g_new(GncABTransTempl, 1);
    r->name = g_strdup(name);
    r->name_key = g_utf8_collate_key(name, -1);
    r->recp_name = g_strdup(recp_name);
    r->recp_account = g_strdup(recp_account);
    r->recp_bankcode = g_strdup(recp_bankcode);
    r->amount = amount;
    r->purpose = g_strdup(purpose);
    r->purpose_cont = g_strdup(purpose_cont);

    return r;
}

GncABTransTempl *
gnc_ab_trans_templ_new_from_kvp(const KvpFrame *k)
{
    g_return_val_if_fail(k, NULL);

    return gnc_ab_trans_templ_new_full(
               kvp_value_get_string(kvp_frame_get_slot(k, TT_NAME)),
               kvp_value_get_string(kvp_frame_get_slot(k, TT_RNAME)),
               kvp_value_get_string(kvp_frame_get_slot(k, TT_RACC)),
               kvp_value_get_string(kvp_frame_get_slot(k, TT_RBCODE)),
               kvp_value_get_numeric(kvp_frame_get_slot(k, TT_AMOUNT)),
               kvp_value_get_string(kvp_frame_get_slot(k, TT_PURPOS)),
               kvp_value_get_string(kvp_frame_get_slot(k, TT_PURPOSCT)));
}

GList *
gnc_ab_trans_templ_list_new_from_kvp_list(GList *v)
{
    GList *res = NULL;
    GList *iter;

    for (iter = v; iter; iter = iter->next)
    {
        KvpFrame *frame = kvp_value_get_frame((KvpValue*) iter->data);
        res = g_list_prepend(res, gnc_ab_trans_templ_new_from_kvp(frame));
    }
    res = g_list_reverse(res);

    return res;
}

void
gnc_ab_trans_templ_free(GncABTransTempl *t)
{
    if (!t) return;
    g_free(t->name);
    g_free(t->name_key);
    g_free(t->recp_name);
    g_free(t->recp_account);
    g_free(t->recp_bankcode);
    g_free(t->purpose);
    g_free(t->purpose_cont);
    g_free(t);
}

void
gnc_ab_trans_templ_list_free(GList *l)
{
    GList *iter;
    for (iter = l; iter; iter = iter->next)
        gnc_ab_trans_templ_free((GncABTransTempl*) iter->data);
    g_list_free(l);
}

KvpFrame *
gnc_ab_trans_templ_to_kvp(const GncABTransTempl *t)
{
    KvpFrame *k;

    g_return_val_if_fail(t, NULL);

    k = kvp_frame_new();
    kvp_frame_set_slot(k, TT_NAME, kvp_value_new_string(t->name));
    kvp_frame_set_slot(k, TT_RNAME, kvp_value_new_string(t->recp_name));
    kvp_frame_set_slot(k, TT_RACC, kvp_value_new_string(t->recp_account));
    kvp_frame_set_slot(k, TT_RBCODE, kvp_value_new_string(t->recp_bankcode));
    kvp_frame_set_slot(k, TT_AMOUNT, kvp_value_new_gnc_numeric(t->amount));
    kvp_frame_set_slot(k, TT_PURPOS, kvp_value_new_string(t->purpose));
    kvp_frame_set_slot(k, TT_PURPOSCT, kvp_value_new_string(t->purpose_cont));

    return k;
}

GList *
gnc_ab_trans_templ_list_to_kvp_list(GList *k)
{
    GList *res = NULL;
    GList *iter;

    for (iter = k; iter; iter = iter->next)
    {
        GncABTransTempl *t = (GncABTransTempl*) iter->data;
        KvpValue *value = kvp_value_new_frame_nc(gnc_ab_trans_templ_to_kvp(t));
        res = g_list_prepend(res, value);
    }
    res = g_list_reverse(res);

    return res;
}

const gchar *
gnc_ab_trans_templ_get_name(const GncABTransTempl *t)
{
    g_return_val_if_fail(t, NULL);
    return t->name;
}

const gchar *
gnc_ab_trans_templ_get_recp_name(const GncABTransTempl *t)
{
    g_return_val_if_fail(t, NULL);
    return t->recp_name;
}

const gchar *
gnc_ab_trans_templ_get_recp_account(const GncABTransTempl *t)
{
    g_return_val_if_fail(t, NULL);
    return t->recp_account;
}

const gchar *
gnc_ab_trans_templ_get_recp_bankcode(const GncABTransTempl *t)
{
    g_return_val_if_fail(t, NULL);
    return t->recp_bankcode;
}

gnc_numeric
gnc_ab_trans_templ_get_amount(const GncABTransTempl *t)
{
    g_return_val_if_fail(t, gnc_numeric_zero());
    return t->amount;
}

const gchar *
gnc_ab_trans_templ_get_purpose(const GncABTransTempl *t)
{
    g_return_val_if_fail(t, NULL);
    return t->purpose;
}

const gchar *
gnc_ab_trans_templ_get_purpose_cont(const GncABTransTempl *t)
{
    g_return_val_if_fail(t, NULL);
    return t->purpose_cont;
}

void
gnc_ab_trans_templ_set_name(GncABTransTempl *t, const gchar *name)
{
    g_return_if_fail(t);
    g_free(t->name);
    t->name = g_strdup(name);
}

void
gnc_ab_trans_templ_set_recp_name(GncABTransTempl *t, const gchar *recp_name)
{
    g_return_if_fail(t);
    g_free(t->recp_name);
    t->recp_name = g_strdup(recp_name);
}

void
gnc_ab_trans_templ_set_recp_account(GncABTransTempl *t,
                                    const gchar *recp_account)
{
    g_return_if_fail(t);
    g_free(t->recp_account);
    t->recp_account = g_strdup(recp_account);
}

void
gnc_ab_trans_templ_set_recp_bankcode(GncABTransTempl *t,
                                     const gchar *recp_bankcode)
{
    g_return_if_fail(t);
    g_free(t->recp_bankcode);
    t->recp_bankcode = g_strdup(recp_bankcode);
}

void
gnc_ab_trans_templ_set_amount(GncABTransTempl *t, gnc_numeric amount)
{
    g_return_if_fail(t);
    t->amount = amount;
}

void
gnc_ab_trans_templ_set_purpose(GncABTransTempl *t, const gchar *purpose)
{
    g_return_if_fail(t);
    g_free(t->purpose);
    t->purpose = g_strdup(purpose);
}

void
gnc_ab_trans_templ_set_purpose_cont(GncABTransTempl *t,
                                    const gchar *purpose_cont)
{
    g_return_if_fail(t);
    g_free(t->purpose_cont);
    t->purpose_cont = g_strdup(purpose_cont);
}

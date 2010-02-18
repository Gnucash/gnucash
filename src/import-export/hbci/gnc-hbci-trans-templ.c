/********************************************************************\
 * gnc-hbci-trans-template.c -- Templates for HBCI transactions     *
 * Copyright (C) 2003 Christian Stimming                            *
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

#include "config.h"

#include "gnc-hbci-trans-templ.h"
#include <gtk/gtk.h>
#include <glib/gi18n.h>

struct _trans_data
{
    /* Name of this Template */
    gchar *name;
    gchar *name_key;	/* Collation key */

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


GNCTransTempl *gnc_trans_templ_new()
{
    GNCTransTempl *r = g_new0(GNCTransTempl, 1);
    r->amount = gnc_numeric_zero();
    return r;
}
GNCTransTempl *gnc_trans_templ_new_full(const char *name,
                                        const char *recp_name,
                                        const char *recp_account,
                                        const char *recp_bankcode,
                                        gnc_numeric amount,
                                        const char *purpose,
                                        const char *purpose_cont)
{
    GNCTransTempl *r = g_new0(GNCTransTempl, 1);
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

void gnc_trans_templ_delete(GNCTransTempl *t)
{
    if (!t) return;
    if (t->name) g_free(t->name);
    if (t->name_key) g_free(t->name_key);
    if (t->recp_name) g_free(t->recp_name);
    if (t->recp_account) g_free(t->recp_account);
    if (t->recp_bankcode) g_free(t->recp_bankcode);
    if (t->purpose) g_free(t->purpose);
    if (t->purpose_cont) g_free(t->purpose_cont);
    g_free(t);
}
static void delete_glist_func(gpointer data, gpointer user_data)
{
    GNCTransTempl *t = data;
    gnc_trans_templ_delete(t);
}
void gnc_trans_templ_delete_glist(GList *l)
{
    g_list_foreach(l, delete_glist_func, NULL);
    g_list_free(l);
}


/* kvp_frame slot names */
#define TT_NAME "name"
#define TT_RNAME "rnam"
#define TT_RACC "racc"
#define TT_RBCODE "rbcd"
#define TT_PURPOS "purp"
#define TT_PURPOSCT "purc"
#define TT_AMOUNT "amou"

/** Constructor from a kvp_frame */
GNCTransTempl *gnc_trans_templ_from_kvp(kvp_frame *k)
{
    GNCTransTempl *res = gnc_trans_templ_new();
    g_assert(k);

    res->name = g_strdup(kvp_value_get_string
                         (kvp_frame_get_slot(k, TT_NAME)));
    res->name_key = g_utf8_collate_key(res->name, -1);
    res->recp_name = g_strdup(kvp_value_get_string
                              (kvp_frame_get_slot(k, TT_RNAME)));
    res->recp_account = g_strdup(kvp_value_get_string
                                 (kvp_frame_get_slot(k, TT_RACC)));
    res->recp_bankcode = g_strdup(kvp_value_get_string
                                  (kvp_frame_get_slot(k, TT_RBCODE)));
    res->purpose = g_strdup(kvp_value_get_string
                            (kvp_frame_get_slot(k, TT_PURPOS)));
    res->purpose_cont = g_strdup(kvp_value_get_string
                                 (kvp_frame_get_slot(k, TT_PURPOSCT)));
    res->amount = kvp_value_get_numeric(kvp_frame_get_slot(k, TT_AMOUNT));
    return res;
}

/** Creates a kvp_frame from this TransTempl */
kvp_frame *gnc_trans_templ_to_kvp(const GNCTransTempl *t)
{
    kvp_frame *k = kvp_frame_new();
    g_assert(t);

    kvp_frame_set_slot(k, TT_NAME, kvp_value_new_string(t->name));
    kvp_frame_set_slot(k, TT_RNAME, kvp_value_new_string(t->recp_name));
    kvp_frame_set_slot(k, TT_RACC, kvp_value_new_string(t->recp_account));
    kvp_frame_set_slot(k, TT_RBCODE, kvp_value_new_string(t->recp_bankcode));
    kvp_frame_set_slot(k, TT_PURPOS, kvp_value_new_string(t->purpose));
    kvp_frame_set_slot(k, TT_PURPOSCT, kvp_value_new_string(t->purpose_cont));
    kvp_frame_set_slot(k, TT_AMOUNT, kvp_value_new_gnc_numeric(t->amount));
    return k;
}

/** Creates a GList of GNCTransTempl from a GList of kvp_values which
    in turn contain a kvp_frame. */
static void glist_from_kvp_func(gpointer data, gpointer user_data)
{
    GList **tmp = user_data;
    GList *res = *tmp;
    kvp_value *k = data;
    *tmp = g_list_append(res, gnc_trans_templ_from_kvp(kvp_value_get_frame(k)));
}
/** Creates a GList of GNCTransTempl from a GList of kvp_values which
    in turn contain a kvp_frame. */
GList *gnc_trans_templ_glist_from_kvp_glist(GList *v)
{
    GList *res = NULL;
    if (!v) return NULL;

    g_list_foreach (v, glist_from_kvp_func, &res);
    return res;
}


/** Creates a GList of kvp_value (which in turn contain a kvp_frame)
    from a GList of GNCTransTempl. */
static void glist_to_kvp_func(gpointer data, gpointer user_data)
{
    GList **tmp = user_data;
    GList *res = *tmp;
    GNCTransTempl *g = data;
    *tmp = g_list_append(res,
                         kvp_value_new_frame_nc(gnc_trans_templ_to_kvp(g)));
}
/** Creates a GList of kvp_value (which in turn contain a kvp_frame)
    from a GList of GNCTransTempl. */
GList *gnc_trans_templ_kvp_glist_from_glist(GList *k)
{
    GList *res = NULL;
    if (!k) return NULL;

    g_list_foreach (k, glist_to_kvp_func, &res);
    return res;
}



/* Value accessors.
 *
 * Gee, how I *HATE* OO programming in C! This STINKS! barf barf barf */
const char *gnc_trans_templ_get_name(const GNCTransTempl *t)
{
    g_assert(t);
    return t->name;
}
const char *gnc_trans_templ_get_name_key(const GNCTransTempl *t)
{
    g_assert(t);
    return t->name_key;
}
const char *gnc_trans_templ_get_recp_name(const GNCTransTempl *t)
{
    g_assert(t);
    return t->recp_name;
}
const char *gnc_trans_templ_get_recp_account(const GNCTransTempl *t)
{
    g_assert(t);
    return t->recp_account;
}
const char *gnc_trans_templ_get_recp_bankcode(const GNCTransTempl *t)
{
    g_assert(t);
    return t->recp_bankcode;
}
gnc_numeric gnc_trans_templ_get_amount(const GNCTransTempl *t)
{
    g_assert(t);
    return t->amount;
}
const char *gnc_trans_templ_get_purpose(const GNCTransTempl *t)
{
    g_assert(t);
    return t->purpose;
}
const char *gnc_trans_templ_get_purpose_cont(const GNCTransTempl *t)
{
    g_assert(t);
    return t->purpose_cont;
}
/** value storing. This sucks even more. barf barf barf again */
void gnc_trans_templ_set_amount(GNCTransTempl *t, gnc_numeric n)
{
    g_assert(t);
    t->amount = n;
}
void gnc_trans_templ_set_name(GNCTransTempl *t, const char *c)
{
    g_assert(t);
    if (t->name)
        g_free(t->name);
    if (t->name_key)
        g_free(t->name_key);
    t->name = g_strdup(c);
    t->name_key = g_utf8_collate_key(c, -1);
}
void gnc_trans_templ_set_recp_name(GNCTransTempl *t, const char *c)
{
    g_assert(t);
    if (t->recp_name)
        g_free(t->recp_name);
    t->recp_name = g_strdup(c);
}
void gnc_trans_templ_set_recp_account(GNCTransTempl *t, const char *c)
{
    g_assert(t);
    if (t->recp_account)
        g_free(t->recp_account);
    t->recp_account = g_strdup(c);
}
void gnc_trans_templ_set_recp_bankcode(GNCTransTempl *t, const char *c)
{
    g_assert(t);
    if (t->recp_bankcode)
        g_free(t->recp_bankcode);
    t->recp_bankcode = g_strdup(c);
}
void gnc_trans_templ_set_purpose(GNCTransTempl *t, const char *c)
{
    g_assert(t);
    if (t->purpose)
        g_free(t->purpose);
    t->purpose = g_strdup(c);
}
void gnc_trans_templ_set_purpose_cont(GNCTransTempl *t, const char *c)
{
    g_assert(t);
    if (t->purpose_cont)
        g_free(t->purpose_cont);
    t->purpose_cont = g_strdup(c);
}



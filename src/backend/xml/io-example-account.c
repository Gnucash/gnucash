/********************************************************************\
 * io-example-account.c -- implementation for example accounts      *
 *                                                                  *
 * Copyright (C) 2001 James LewisMoss <dres@debian.org>             *
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
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <sys/types.h>
#include <ctype.h>
#include <dirent.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include "sixtp.h"

#include "gnc-engine.h"
#include "gnc-xml.h"
#include "io-example-account.h"
#include "io-gncxml-gen.h"
#include "io-utils.h"
#include "sixtp-dom-generators.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-parsers.h"

#include "Scrub.h"
#include "TransLog.h"

static QofLogModule log_module = GNC_MOD_IO;

#define GNC_ACCOUNT_STRING "gnc-account-example"
#define GNC_ACCOUNT_SHORT "gnc-act:short-description"
#define GNC_ACCOUNT_LONG "gnc-act:long-description"
#define GNC_ACCOUNT_TITLE "gnc-act:title"
#define GNC_ACCOUNT_EXCLUDEP "gnc-act:exclude-from-select-all"
#define GNC_ACCOUNT_SELECTED "gnc-act:start-selected"

void
gnc_destroy_example_account(GncExampleAccount *gea)
{
    if(gea->title != NULL)
    {
        g_free(gea->title);
        gea->title = NULL;
    }
    if(gea->filename != NULL)
    {
        g_free(gea->filename);
        gea->filename = NULL;
    }
    if(gea->root != NULL)
    {
        xaccAccountBeginEdit (gea->root);
        xaccAccountDestroy(gea->root);
        gea->root = NULL;
    }
    if(gea->short_description != NULL)
    {
        g_free(gea->short_description);
        gea->short_description = NULL;
    }
    if(gea->long_description != NULL)
    {
        g_free(gea->long_description);
        gea->long_description = NULL;
    }
    if(gea->book != NULL)
    {
        qof_book_destroy(gea->book);
        gea->book = NULL;
    }
    g_free(gea);
}

static void
clear_up_account_commodity(
    gnc_commodity_table *tbl, Account *act,
    gnc_commodity * (*getter) (const Account *account),
    void (*setter) (Account *account, gnc_commodity *comm))
{
    gnc_commodity *gcom;
    gnc_commodity *com = getter(act);

    if(!com)
    {
        return;
    }

    g_return_if_fail (tbl != NULL);

    gcom = gnc_commodity_table_lookup(tbl,
                                      gnc_commodity_get_namespace(com),
                                      gnc_commodity_get_mnemonic(com));

    if(gcom == com)
    {
        return;
    }
    else if(!gcom)
    {
        PWARN("unable to find global commodity for %s adding new",
                  gnc_commodity_get_unique_name(com));
        gnc_commodity_table_insert(tbl, com);
    }
    else
    {
        setter(act, gcom);
        gnc_commodity_destroy(com);
    }
}

static void
add_account_local(GncExampleAccount *gea, Account *act)
{
    gnc_commodity_table *table;

    table = gnc_book_get_commodity_table (gea->book);

    clear_up_account_commodity(table, act,
                               xaccAccountGetCommodity,
                               xaccAccountSetCommodity);

    xaccAccountScrubCommodity (act);

    if (xaccAccountGetType(act) == ACCT_TYPE_ROOT)
    {
        gea->root = act;
    }
    else if (!gnc_account_get_parent(act))
    {
        if (!gea->root) {
            g_warning("The example account file should declared a ROOT "
		      "account before declaring any other accounts.");
            gea->root = gnc_book_get_root_account(gea->book);
        }
        gnc_account_append_child(gea->root, act);
    }
}

static gboolean
generic_callback(const char *tag, gpointer globaldata, gpointer data)
{
    GncExampleAccount *gea = (GncExampleAccount*)globaldata;

    if(safe_strcmp(tag, "gnc:account") == 0)
    {
        add_account_local(gea, (Account*)data);
    }
    return TRUE;
}

static char*
squash_extra_whitespace(char *text)
{
    int spot;
    int length = strlen(text);
    
    for(spot = 1; spot < length; spot++)
    {
        if(isspace(text[spot]) && isspace(text[spot - 1]))
        {
            memmove(text + spot, text + spot + 1, length - spot + 1);
            length--;
        }
        else
        {
            spot++;
        }
    }
    return text;
}

static char*
grab_clean_string(xmlNodePtr tree)
{
    return squash_extra_whitespace(g_strstrip(dom_tree_to_text(tree)));
}

static gboolean
gnc_short_descrip_end_handler(gpointer data_for_children,
                              GSList* data_from_children, GSList* sibling_data,
                              gpointer parent_data, gpointer global_data,
                              gpointer *result, const gchar *tag)
{
    GncExampleAccount *gea =
    (GncExampleAccount*)((gxpf_data*)global_data)->parsedata;

    gea->short_description = grab_clean_string((xmlNodePtr)data_for_children);
    
    return TRUE;
}

static sixtp*
gnc_short_descrip_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new(gnc_short_descrip_end_handler, NULL, NULL);
}

static gboolean
gnc_long_descrip_end_handler(gpointer data_for_children,
                             GSList* data_from_children, GSList* sibling_data,
                             gpointer parent_data, gpointer global_data,
                             gpointer *result, const gchar *tag)
{
    GncExampleAccount *gea =
    (GncExampleAccount*)((gxpf_data*)global_data)->parsedata;

    gea->long_description = grab_clean_string((xmlNodePtr)data_for_children);
    
    return TRUE;
}

static sixtp*
gnc_long_descrip_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new(gnc_long_descrip_end_handler, NULL, NULL);
}

static gboolean
gnc_excludep_end_handler(gpointer data_for_children,
                             GSList* data_from_children, GSList* sibling_data,
                             gpointer parent_data, gpointer global_data,
                             gpointer *result, const gchar *tag)
{
    GncExampleAccount *gea =
    (GncExampleAccount*)((gxpf_data*)global_data)->parsedata;
    gint64 val = 0;

    dom_tree_to_integer ((xmlNodePtr)data_for_children, &val);
    gea->exclude_from_select_all = (val ? TRUE : FALSE);
    
    return TRUE;
}

static sixtp*
gnc_excludep_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new(gnc_excludep_end_handler, NULL, NULL);
}

static gboolean
gnc_selected_end_handler(gpointer data_for_children,
                             GSList* data_from_children, GSList* sibling_data,
                             gpointer parent_data, gpointer global_data,
                             gpointer *result, const gchar *tag)
{
    GncExampleAccount *gea =
    (GncExampleAccount*)((gxpf_data*)global_data)->parsedata;
    gint64 val = 0;

    dom_tree_to_integer ((xmlNodePtr)data_for_children, &val);
    gea->start_selected = (val ? TRUE : FALSE);
    
    return TRUE;
}

static sixtp*
gnc_selected_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new(gnc_selected_end_handler, NULL, NULL);
}

static gboolean
gnc_title_end_handler(gpointer data_for_children,
                             GSList* data_from_children, GSList* sibling_data,
                             gpointer parent_data, gpointer global_data,
                             gpointer *result, const gchar *tag)
{
    GncExampleAccount *gea =
    (GncExampleAccount*)((gxpf_data*)global_data)->parsedata;

    gea->title = grab_clean_string((xmlNodePtr)data_for_children);
    
    return TRUE;
}

static sixtp*
gnc_titse_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new(gnc_title_end_handler, NULL, NULL);
}


GncExampleAccount*
gnc_read_example_account(const gchar *filename)
{
    GncExampleAccount *gea;
    sixtp *top_parser;
    sixtp *main_parser;

    g_return_val_if_fail (filename != NULL, NULL);

    gea = g_new0(GncExampleAccount, 1);

    gea->book = qof_book_new();
    gea->filename = g_strdup(filename);

    top_parser = sixtp_new();
    main_parser = sixtp_new();

    if(!sixtp_add_some_sub_parsers(
        top_parser, TRUE,
        GNC_ACCOUNT_STRING, main_parser,
        NULL, NULL))
    {
        return FALSE;
    }

    if(!sixtp_add_some_sub_parsers(
           main_parser, TRUE,
           GNC_ACCOUNT_TITLE, gnc_titse_sixtp_parser_create(),
           GNC_ACCOUNT_SHORT, gnc_short_descrip_sixtp_parser_create(),
           GNC_ACCOUNT_LONG, gnc_long_descrip_sixtp_parser_create(),
	   GNC_ACCOUNT_EXCLUDEP, gnc_excludep_sixtp_parser_create(),
	   GNC_ACCOUNT_SELECTED, gnc_selected_sixtp_parser_create(),
           "gnc:account", gnc_account_sixtp_parser_create(),
           NULL, NULL))
    {
        return FALSE;
    }

    if(!gnc_xml_parse_file(top_parser, filename,
                           generic_callback, gea, gea->book))
    {
        sixtp_destroy(top_parser);
        xaccLogEnable ();
        return FALSE;
    }

    return gea;
}

static void
write_string_part(FILE *out, const char *tag, const char *data)
{
    xmlNodePtr node;

    node = text_to_dom_tree(tag, data);

    xmlElemDump(out, NULL, node);
    fprintf(out, "\n");

    xmlFreeNode(node);
}

static void
write_bool_part(FILE *out, const char *tag, gboolean data)
{
    xmlNodePtr node;

    node = int_to_dom_tree(tag, data);

    xmlElemDump(out, NULL, node);
    fprintf(out, "\n");

    xmlFreeNode(node);
}

gboolean
gnc_write_example_account(GncExampleAccount *gea, const gchar *filename)
{
    FILE *out;
    sixtp_gdv2 data = { 0 };;

    out = g_fopen(filename, "w");
    if (out == NULL)
    {
        return FALSE;
    }

    fprintf(out, "<?xml version=\"1.0\"?>\n");
    fprintf(out, "<" GNC_ACCOUNT_STRING ">\n");

    write_string_part(out, GNC_ACCOUNT_TITLE, gea->title);
    
    write_string_part(out, GNC_ACCOUNT_SHORT, gea->short_description);

    write_string_part(out, GNC_ACCOUNT_LONG, gea->long_description);
    
    write_bool_part(out, GNC_ACCOUNT_EXCLUDEP, gea->exclude_from_select_all);

    write_account_tree(out, gea->root, &data);

    fprintf(out, "</" GNC_ACCOUNT_STRING ">\n\n");
    
    write_emacs_trailer(out);

    fclose(out);
    
    return TRUE;

}

/***********************************************************************/

static void
slist_destroy_example_account(gpointer data, gpointer user_data)
{
    if(data != NULL)
    {
        gnc_destroy_example_account((GncExampleAccount*)data);
    }
    else
    {
        PWARN("GncExampleAccount pointer in slist was NULL");
    }
}

void
gnc_free_example_account_list(GSList *list)
{
    g_slist_foreach(list, slist_destroy_example_account, NULL);
    g_slist_free(list);
}

GSList*
gnc_load_example_account_list(const char *dirname)
{
    GSList *ret;
    GDir *dir;
    const gchar *direntry;

    dir = g_dir_open(dirname, 0, NULL);

    if(dir == NULL)
    {
        return NULL;
    }
    
    ret = NULL;

    for(direntry = g_dir_read_name(dir); direntry != NULL;
        direntry = g_dir_read_name(dir))
    {
        gchar *filename;
        GncExampleAccount *gea;
        if (!g_str_has_suffix(direntry, "xea"))
          continue;

        filename = g_build_filename(dirname, direntry, (gchar*) NULL);

        if(!g_file_test(filename, G_FILE_TEST_IS_DIR))
        {
            gea = gnc_read_example_account(filename);

            if(gea == NULL)
            {
                g_free(filename);
                gnc_free_example_account_list(ret);
                g_dir_close(dir);
                return NULL;
            }

            ret = g_slist_append(ret, gea);
        }

        g_free(filename);
    }
    g_dir_close(dir);

    return ret;
}



/***********************************************************************/
gboolean
gnc_is_example_account_xml(const gchar *name)
{
    return gnc_is_our_xml_file(name, GNC_ACCOUNT_STRING, NULL);
}

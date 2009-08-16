/***************************************************************************
 *            test-file-stuff.c
 *
 *  Thu Sep 29 22:50:50 2005
 *  Copyright  2005  GnuCash team
 ****************************************************************************/
/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301, USA.
 */
 
#include "config.h"

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include <glib/gstdio.h>

#include "gnc-engine.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-parsers.h"
#include "test-file-stuff.h"
#include "test-stuff.h"
#include "io-gncxml-gen.h"
#include "sixtp-utils.h"

/***********************************************************************/

static int
files_return(int ret, const char* msg)
{
    printf("%s", msg);
    return ret;
}

int
files_compare(const gchar* f1, const gchar* f2)
{
    gchar buf1[512];
    gchar buf2[512];
    int fd1, fd2;
    int amount_read1, amount_read2;

    fd1 = g_open(f1, O_RDONLY, 0);
    fd2 = g_open(f2, O_RDONLY, 0);

    do
    {
        int memcmp_ret;

        amount_read1 = read(fd1, buf1, 512);
        amount_read2 = read(fd2, buf2, 512);

        if(amount_read1 > amount_read2)
            return files_return(1, "read1 > read2");
        if(amount_read1 < amount_read2)
            return files_return(-1, "read1 < read2");

        if((memcmp_ret = memcmp(buf1, buf2, amount_read1)) != 0)
        {
            return files_return(memcmp_ret, "memcmp return");
        }

    } while(amount_read1 == 512);

    return 0;
}

void
write_dom_node_to_file(xmlNodePtr node, int fd)
{
    FILE *out;

    out = fdopen(fd, "w");

    xmlElemDump(out, NULL, node);

    fclose(out);
}

gboolean
print_dom_tree(gpointer data_for_children, GSList* data_from_children,
               GSList* sibling_data, gpointer parent_data,
               gpointer global_data, gpointer *result, const gchar *tag)
{
    if (parent_data == NULL) 
    {
        xmlElemDump((FILE*)global_data, NULL, (xmlNodePtr)data_for_children);
        xmlFreeNode(data_for_children);
    }
    return TRUE;
}

gboolean
check_dom_tree_version(xmlNodePtr node, gchar *verstr)
{
    char *verteststr;

    g_return_val_if_fail(node, FALSE);
    g_return_val_if_fail(verstr, FALSE);
    g_return_val_if_fail(node->properties, FALSE);
    g_return_val_if_fail(node->properties->xmlAttrPropertyValue, FALSE);
    g_return_val_if_fail(node->properties->xmlAttrPropertyValue->content,
                         FALSE);

    verteststr = (char*) node->properties->xmlAttrPropertyValue->content;
    if(safe_strcmp(verstr, verteststr) == 0)
    {
        return TRUE;
    }
    else
    {
        return FALSE;
    }
}

gboolean
equals_node_val_vs_string(xmlNodePtr node, const gchar* str)
{
    gchar *cmp1;

    g_return_val_if_fail(node, FALSE);
    g_return_val_if_fail(str, FALSE);
    
    cmp1 = dom_tree_to_text(node);

    if(!cmp1)
    {
        return FALSE;
    }
    else if(safe_strcmp(cmp1, str) == 0)
    {
        g_free(cmp1);
        return TRUE;
    }
    else 
    {
        printf("Differing types: node:`%s' vs string:`%s'\n", cmp1, str);
        g_free(cmp1);
        return FALSE;
    }
}

gboolean
equals_node_val_vs_int(xmlNodePtr node, gint64 val)
{
    gchar *text;
    gint64 test_val;
    
    g_return_val_if_fail(node, FALSE);

    text = dom_tree_to_text(node);

    if(!string_to_gint64(text, &test_val))
    {
        g_free(text);
        return FALSE;
    }

    g_free(text);

    return val == test_val;
}

gboolean
equals_node_val_vs_boolean(xmlNodePtr node, gboolean val)
{
    return equals_node_val_vs_string(node, val ? "TRUE" : "FALSE");
}

gboolean
equals_node_val_vs_guid(xmlNodePtr node, const GUID *id)
{
    GUID *cmpid;

    g_return_val_if_fail(node, FALSE);
    g_return_val_if_fail(id, FALSE);

    cmpid = dom_tree_to_guid(node);

    g_return_val_if_fail(cmpid, FALSE);
    
    if(guid_compare(cmpid, id) == 0)
    {
        g_free(cmpid);
        return TRUE;
    }
    else
    {
        g_free(cmpid);
        return FALSE;
    }
}

gboolean
equals_node_val_vs_commodity(xmlNodePtr node, const gnc_commodity *com, 
                             QofBook *book)
{
    gnc_commodity *cmpcom;

    g_return_val_if_fail(node, FALSE);
    g_return_val_if_fail(com, FALSE);

    cmpcom = dom_tree_to_commodity_ref_no_engine(node, book);

    g_return_val_if_fail(cmpcom, FALSE);

    if(gnc_commodity_equiv(com, cmpcom))
    {
        gnc_commodity_destroy(cmpcom);
        return TRUE;
    }
    else
    {
        gnc_commodity_destroy(cmpcom);
        return FALSE;
    }
}

gboolean
equals_node_val_vs_kvp_frame(xmlNodePtr node, const kvp_frame *frm)
{
    kvp_frame *cmpfrm;

    g_return_val_if_fail(node, FALSE);
    g_return_val_if_fail(frm, FALSE);

    cmpfrm = dom_tree_to_kvp_frame(node);

    g_return_val_if_fail(cmpfrm, FALSE);

    if(kvp_frame_compare(frm, cmpfrm) == 0)
    {
        kvp_frame_delete(cmpfrm);
        return TRUE;
    }
    else
    {
        gchar *frm1str;
        gchar *frm2str;

        frm1str = kvp_frame_to_string(frm);
        frm2str = kvp_frame_to_string(cmpfrm);

        printf("%s vs %s\n", frm1str, frm2str);

        g_free(frm1str);
        g_free(frm2str);
        
        kvp_frame_delete(cmpfrm);
        return FALSE;
    }
}

gboolean
equals_node_val_vs_date(xmlNodePtr node, const Timespec tm)
{
    Timespec tm_test = dom_tree_to_timespec(node);

    if(tm_test.tv_sec == tm.tv_sec && tm_test.tv_nsec == tm.tv_nsec)
    {
        return TRUE;
    }
    else
    {
        return FALSE;
    }
}

/***********************************************************************/

static gboolean
just_dom_tree_end_handler(gpointer data_for_children,
                          GSList* data_from_children, GSList* sibling_data,
                          gpointer parent_data, gpointer global_data,
                          gpointer *result, const gchar *tag)
{
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    xmlNodePtr *globaldata = (xmlNodePtr*)global_data;

    if(parent_data)
    {
        return TRUE;
    }

    /* OK.  For some messed up reason this is getting called again with a
       NULL tag.  So we ignore those cases */
    if(!tag)
    {
        return TRUE;
    }
    
    g_return_val_if_fail(tree, FALSE);

    *globaldata = tree;

    return TRUE;
}

static xmlNodePtr
grab_file_doc(const char *filename)
{
    sixtp *parser;
    xmlNodePtr ret;
    gpointer parse_result = NULL;
    
    parser = sixtp_dom_parser_new(just_dom_tree_end_handler, NULL, NULL);

    sixtp_parse_file(parser, filename, NULL, &ret, &parse_result);

    return ret;
}
    
static void
test_load_file(const char *filename, gxpf_callback cb, sixtp *top_parser, 
               QofBook *book)
{
    xmlNodePtr node;
    
    node = grab_file_doc(filename);

    if (!node)
    {
        failure_args("failure of libxml to parse file", __FILE__, __LINE__,
                     "%s", filename);
        return;
    }

    if (!gnc_xml_parse_file(top_parser, filename, cb, node->children, book))
    {
        failure_args("failure to parse file", __FILE__, __LINE__,
                     "%s", filename);
    }

    xmlFreeNode(node);
}
    
void
test_files_in_dir(int argc, char **argv, gxpf_callback cb,
                  sixtp *parser, const char *parser_tag,
                  QofBook *book)
{
    int count;
    sixtp *main_parser;
    sixtp *top_parser;


    top_parser = sixtp_new();
    main_parser = sixtp_new();

    if (!sixtp_add_some_sub_parsers(top_parser, TRUE, "gnc-v2", main_parser,
                                    NULL, NULL))
        return;
    
    if (!sixtp_add_some_sub_parsers(main_parser, TRUE, parser_tag, parser,
                                    NULL, NULL))
        return;


    for(count = 1; count < argc; count++)
    {
        struct stat file_info;
        const char *to_open = argv[count];
        if(g_stat(to_open, &file_info) != 0)
        {
            printf("cannot stat %s.\n", to_open);
            failure("unable to stat file");
        }
        else
        {
            if(!S_ISDIR(file_info.st_mode))
            {
#if 0
                printf( "testing load of file \"%s\":\n", argv[count] );
#endif
                test_load_file(to_open, cb, top_parser, book);
            }
        }
    }
    
    sixtp_destroy(top_parser);
}

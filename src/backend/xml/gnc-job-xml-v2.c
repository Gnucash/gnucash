/********************************************************************\
 * gnc-job-xml-v2.c -- job xml i/o implementation         *
 *                                                                  *
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>                *
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

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-xml-helper.h"

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"

#include "gnc-xml.h"
#include "io-gncxml-gen.h"
#include "io-gncxml-v2.h"

#include "gncJobP.h"
#include "gnc-job-xml-v2.h"
#include "gnc-owner-xml-v2.h"
#include "xml-helpers.h"

#define _GNC_MOD_NAME	GNC_ID_JOB

static QofLogModule log_module = GNC_MOD_IO;

const gchar *job_version_string = "2.0.0";

/* ids */
#define gnc_job_string "gnc:GncJob"
#define job_guid_string "job:guid"
#define job_id_string "job:id"
#define job_name_string "job:name"
#define job_reference_string "job:reference"
#define job_owner_string "job:owner"
#define job_active_string "job:active"
#define job_slots_string "job:slots"

static xmlNodePtr
job_dom_tree_create (GncJob *job)
{
    xmlNodePtr ret;

    ret = xmlNewNode(NULL, BAD_CAST gnc_job_string);
    xmlSetProp(ret, BAD_CAST "version", BAD_CAST job_version_string);

    xmlAddChild(ret, guid_to_dom_tree(job_guid_string,
                                      qof_instance_get_guid (QOF_INSTANCE (job))));

    xmlAddChild(ret, text_to_dom_tree(job_id_string,
                                      gncJobGetID (job)));

    xmlAddChild(ret, text_to_dom_tree(job_name_string,
                                      gncJobGetName (job)));

    maybe_add_string (ret, job_reference_string, gncJobGetReference (job));

    xmlAddChild(ret, gnc_owner_to_dom_tree (job_owner_string,
                                            gncJobGetOwner (job)));

    xmlAddChild(ret, int_to_dom_tree(job_active_string,
                                     gncJobGetActive (job)));

    return ret;
}

/***********************************************************************/

struct job_pdata
{
    GncJob *job;
    QofBook *book;
};

static gboolean
set_string(xmlNodePtr node, GncJob* job,
           void (*func)(GncJob *job, const char *txt))
{
    char* txt = dom_tree_to_text(node);
    g_return_val_if_fail(txt, FALSE);

    func(job, txt);

    g_free(txt);

    return TRUE;
}

static gboolean
job_name_handler (xmlNodePtr node, gpointer job_pdata)
{
    struct job_pdata *pdata = job_pdata;

    return set_string(node, pdata->job, gncJobSetName);
}

static gboolean
job_guid_handler (xmlNodePtr node, gpointer job_pdata)
{
    struct job_pdata *pdata = job_pdata;
    GncGUID *guid;
    GncJob *job;

    guid = dom_tree_to_guid(node);
    g_return_val_if_fail(guid, FALSE);
    job = gncJobLookup (pdata->book, guid);
    if (job)
    {
        gncJobDestroy (pdata->job);
        pdata->job = job;
        gncJobBeginEdit (job);
    }
    else
    {
        gncJobSetGUID(pdata->job, guid);
    }

    g_free(guid);

    return TRUE;
}

static gboolean
job_id_handler (xmlNodePtr node, gpointer job_pdata)
{
    struct job_pdata *pdata = job_pdata;

    return set_string(node, pdata->job, gncJobSetID);
}

static gboolean
job_reference_handler (xmlNodePtr node, gpointer job_pdata)
{
    struct job_pdata *pdata = job_pdata;

    return set_string(node, pdata->job, gncJobSetReference);
}

static gboolean
job_owner_handler (xmlNodePtr node, gpointer job_pdata)
{
    struct job_pdata *pdata = job_pdata;
    GncOwner owner;
    gboolean ret;

    ret = gnc_dom_tree_to_owner (node, &owner, pdata->book);
    if (ret)
        gncJobSetOwner (pdata->job, &owner);

    return ret;
}

static gboolean
job_active_handler (xmlNodePtr node, gpointer job_pdata)
{
    struct job_pdata *pdata = job_pdata;
    gint64 val;
    gboolean ret;

    ret = dom_tree_to_integer(node, &val);
    if (ret)
        gncJobSetActive(pdata->job, (gboolean)val);

    return ret;
}

static gboolean
job_slots_handler (xmlNodePtr node, gpointer job_pdata)
{
    return TRUE;
}

static struct dom_tree_handler job_handlers_v2[] =
{
    { job_guid_string, job_guid_handler, 1, 0 },
    { job_id_string, job_id_handler, 1, 0 },
    { job_name_string, job_name_handler, 1, 0 },
    { job_reference_string, job_reference_handler, 0, 0 },
    { job_owner_string, job_owner_handler, 1, 0 },
    { job_active_string, job_active_handler, 1, 0 },
    { job_slots_string, job_slots_handler, 0, 0 },
    { NULL, 0, 0, 0 }
};

static GncJob*
dom_tree_to_job (xmlNodePtr node, QofBook *book)
{
    struct job_pdata job_pdata;
    gboolean successful;

    job_pdata.job = gncJobCreate(book);
    job_pdata.book = book;
    gncJobBeginEdit (job_pdata.job);

    successful = dom_tree_generic_parse (node, job_handlers_v2,
                                         &job_pdata);

    if (successful)
        gncJobCommitEdit (job_pdata.job);
    else
    {
        PERR ("failed to parse job tree");
        gncJobDestroy (job_pdata.job);
        job_pdata.job = NULL;
    }

    return job_pdata.job;
}

static gboolean
gnc_job_end_handler(gpointer data_for_children,
                    GSList* data_from_children, GSList* sibling_data,
                    gpointer parent_data, gpointer global_data,
                    gpointer *result, const gchar *tag)
{
    int successful;
    GncJob *job;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    gxpf_data *gdata = (gxpf_data*)global_data;
    QofBook *book = gdata->bookdata;

    successful = TRUE;

    if (parent_data)
    {
        return TRUE;
    }

    /* OK.  For some messed up reason this is getting called again with a
       NULL tag.  So we ignore those cases */
    if (!tag)
    {
        return TRUE;
    }

    g_return_val_if_fail(tree, FALSE);

    job = dom_tree_to_job(tree, book);
    if (job != NULL)
    {
        gdata->cb(tag, gdata->parsedata, job);
    }

    xmlFreeNode(tree);

    return job != NULL;
}

static sixtp *
job_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new(gnc_job_end_handler, NULL, NULL);
}

static gboolean
job_should_be_saved (GncJob *job)
{
    const char *id;

    /* make sure this is a valid job before we save it -- should have an ID */
    id = gncJobGetID (job);
    if (id == NULL || *id == '\0')
        return FALSE;

    return TRUE;
}

static void
do_count (QofInstance * job_p, gpointer count_p)
{
    int *count = count_p;
    if (job_should_be_saved ((GncJob *)job_p))
        (*count)++;
}

static int
job_get_count (QofBook *book)
{
    int count = 0;
    qof_object_foreach (_GNC_MOD_NAME, book, do_count, (gpointer) &count);
    return count;
}

static void
xml_add_job (QofInstance * job_p, gpointer out_p)
{
    xmlNodePtr node;
    GncJob *job = (GncJob *) job_p;
    FILE *out = out_p;

    if (ferror(out))
        return;
    if (!job_should_be_saved (job))
        return;

    node = job_dom_tree_create (job);
    xmlElemDump(out, NULL, node);
    xmlFreeNode (node);
    if (ferror(out) || fprintf(out, "\n") < 0)
        return;
}

static gboolean
job_write (FILE *out, QofBook *book)
{
    qof_object_foreach (_GNC_MOD_NAME, book, xml_add_job, (gpointer) out);
    return ferror(out) == 0;
}

static gboolean
job_ns(FILE *out)
{
    g_return_val_if_fail(out, FALSE);
    return gnc_xml2_write_namespace_decl(out, "job");
}

void
gnc_job_xml_initialize (void)
{
    static GncXmlDataType_t be_data =
    {
        GNC_FILE_BACKEND_VERS,
        gnc_job_string,
        job_sixtp_parser_create,
        NULL,			/* add_item */
        job_get_count,
        job_write,
        NULL,			/* scrub */
        job_ns,
    };

    qof_object_register_backend (_GNC_MOD_NAME,
                                 GNC_FILE_BACKEND,
                                 &be_data);
}

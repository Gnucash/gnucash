/***************************************************************************
 *            qsf-backend.c
 *
 *  Sat Jan  1 15:07:14 2005
 *  Copyright  2005, 2006  Neil Williams
 *  linux@codehelp.co.uk
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
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include "config.h"
#include <glib.h>
#include <glib/gstdio.h>
#include "qof.h"
#include "qofbackend-p.h"
#include "qof-backend-qsf.h"
#include <libxml/xmlmemory.h>
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlschemas.h>
#include "qsf-xml.h"
#include "qsf-dir.h"
#include <errno.h>
#include <sys/stat.h>

#ifndef HAVE_STRPTIME
#include "strptime.h"
#endif

#define QSF_TYPE_BINARY "binary"
#define QSF_TYPE_GLIST  "glist"
#define QSF_TYPE_FRAME  "frame"

static QofLogModule log_module = QOF_MOD_QSF;
static void qsf_object_commitCB(gpointer key, gpointer value, gpointer data);

struct QSFBackend_s
{
    QofBackend be;
    qsf_param *params;
    gchar *fullpath;
};

typedef struct QSFBackend_s QSFBackend;

static void option_cb (QofBackendOption *option, gpointer data)
{
    qsf_param *params;

    params = (qsf_param*)data;
    g_return_if_fail(params);
    if (0 == safe_strcmp(QSF_COMPRESS, option->option_name))
    {
        params->use_gz_level = (*(gint64*)option->value);
        DEBUG (" gz=%" G_GINT64_FORMAT, params->use_gz_level);
    }
    if (0 == safe_strcmp(QSF_MAP_FILES, option->option_name))
    {
        params->map_files = g_list_copy((GList*)option->value);
    }
    if (0 == safe_strcmp(QSF_ENCODING, option->option_name))
    {
        params->encoding = g_strdup(option->value);
        DEBUG (" encoding=%s", params->encoding);
    }
}

static void
qsf_load_config(QofBackend *be, KvpFrame *config)
{
    QSFBackend *qsf_be;
    qsf_param  *params;

    ENTER (" ");
    qsf_be = (QSFBackend*)be;
    g_return_if_fail(qsf_be->params);
    params = qsf_be->params;
    qof_backend_option_foreach(config, option_cb, params);
    LEAVE (" ");
}

static KvpFrame*
qsf_get_config(QofBackend *be)
{
    QofBackendOption *option;
    QSFBackend *qsf_be;
    qsf_param *params;

    if (!be)
    {
        return NULL;
    }
    ENTER (" ");
    qsf_be = (QSFBackend*)be;
    g_return_val_if_fail(qsf_be->params, NULL);
    params = qsf_be->params;
    qof_backend_prepare_frame(be);
    option = g_new0(QofBackendOption, 1);
    option->option_name = QSF_COMPRESS;
    option->description = _("Level of compression to use: 0 for none, 9 for highest.");
    option->tooltip = _("QOF can compress QSF XML files using gzip. "
                        "Note that compression is not used when outputting to STDOUT.");
    option->type = KVP_TYPE_GINT64;
    option->value = (gpointer) & params->use_gz_level;
    qof_backend_prepare_option(be, option);
    g_free(option);
    option = g_new0(QofBackendOption, 1);
    option->option_name = QSF_MAP_FILES;
    option->description = _("List of QSF map files to use for this session.");
    option->tooltip = _("QOF can convert objects within QSF XML files "
                        "using a map of the changes required.");
    option->type = KVP_TYPE_GLIST;
    option->value = (gpointer)params->map_files;
    qof_backend_prepare_option(be, option);
    g_free(option);
    option = g_new0(QofBackendOption, 1);
    option->option_name = QSF_ENCODING;
    option->description = _("String encoding to use when writing the XML file.");
    option->tooltip = _("QSF defaults to UTF-8. Other encodings are supported by "
                        "passing the string encoding in this option.");
    option->type = KVP_TYPE_STRING;
    option->value = (gpointer)params->encoding;
    qof_backend_prepare_option(be, option);
    g_free(option);
    LEAVE (" ");
    return qof_backend_complete_frame(be);
}

GList**
qsf_map_prepare_list(GList **maps)
{
    *maps = g_list_prepend(*maps, "pilot-qsf-GnuCashInvoice.xml");
    *maps = g_list_prepend(*maps, "pilot-qsf-gncCustomer.xml");
    return maps;
}

static void
qsf_param_init(qsf_param *params)
{
    Timespec *qsf_ts;
    gchar qsf_time_string[QSF_DATE_LENGTH];
    gchar qsf_enquiry_date[QSF_DATE_LENGTH];
    gchar qsf_time_match[QSF_DATE_LENGTH];
    gchar qsf_time_now[QSF_DATE_LENGTH];
    time_t qsf_time_now_t;
    gchar *qsf_time_precision;

    g_return_if_fail(params != NULL);
    params->count = 0;
    params->use_gz_level = 0;
    params->supported_types = NULL;
    params->file_type = QSF_UNDEF;
    params->qsf_ns = NULL;
    params->output_doc = NULL;
    params->output_node = NULL;
    params->lister = NULL;
    params->full_kvp_path = NULL;
    params->map_ns = NULL;
    params->map_files = NULL;
    params->map_path = NULL;
    params->encoding = "UTF-8";
    params->qsf_object_list = NULL;
    params->qsf_parameter_hash = g_hash_table_new(g_str_hash, g_str_equal);
    params->qsf_default_hash = g_hash_table_new(g_str_hash, g_str_equal);
    params->qsf_define_hash = g_hash_table_new(g_str_hash, g_str_equal);
    params->qsf_calculate_hash = g_hash_table_new(g_str_hash, g_str_equal);
    params->referenceList = NULL;
    params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_STRING);
    params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_GUID);
    params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_BOOLEAN);
    params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_NUMERIC);
    params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_DATE);
    params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_INT32);
    params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_INT64);
    params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_DOUBLE);
    params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_CHAR);
    params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_KVP);
    params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_COLLECT);
    params->supported_types = g_slist_append(params->supported_types, QOF_TYPE_CHOICE);
    qsf_time_precision = "%j";
    qsf_time_now_t = time(NULL);
    qsf_ts = g_new(Timespec, 1);
    timespecFromTime_t(qsf_ts, qsf_time_now_t);
    qof_strftime(qsf_enquiry_date, QSF_DATE_LENGTH, QSF_XSD_TIME, gmtime(&qsf_time_now_t));
    qof_strftime(qsf_time_match, QSF_DATE_LENGTH, qsf_time_precision, gmtime(&qsf_time_now_t));
    qof_strftime(qsf_time_string, QSF_DATE_LENGTH, "%F", gmtime(&qsf_time_now_t));
    qof_strftime(qsf_time_now, QSF_DATE_LENGTH, QSF_XSD_TIME, gmtime(&qsf_time_now_t));
    g_hash_table_insert(params->qsf_default_hash, "qsf_enquiry_date", qsf_enquiry_date);
    g_hash_table_insert(params->qsf_default_hash, "qsf_time_now", &qsf_time_now_t);
    g_hash_table_insert(params->qsf_default_hash, "qsf_time_string", qsf_time_string);
    /* default map files */
    params->map_files = *qsf_map_prepare_list(&params->map_files);
}

static gboolean
qsf_determine_file_type(const gchar *path)
{
    struct stat sbuf;

    if (!path)
    {
        return TRUE;
    }
    if (0 == safe_strcmp(path, QOF_STDOUT))
    {
        return TRUE;
    }
    if (g_stat(path, &sbuf) < 0)
    {
        return FALSE;
    }
    if (sbuf.st_size == 0)
    {
        return TRUE;
    }
    if (is_our_qsf_object(path))
    {
        return TRUE;
    }
    else if (is_qsf_object(path))
    {
        return TRUE;
    }
    else if (is_qsf_map(path))
    {
        return TRUE;
    }
    return FALSE;
}

/* GnuCash does LOTS of filesystem work, QSF is going to leave most of it to libxml2. :-)
Just strip the file: from the start of the book_path URL. Locks are not implemented.
*/
static void
qsf_session_begin(QofBackend *be, QofSession *session, const gchar *book_path,
                  gboolean ignore_lock, gboolean create_if_nonexistent)
{
    QSFBackend *qsf_be;
    gchar *p, *path;

    PINFO (" ignore_lock=%d create_if_nonexistent=%d", ignore_lock, create_if_nonexistent);
    g_return_if_fail(be != NULL);
    qsf_be = (QSFBackend*)be;
    g_return_if_fail(qsf_be->params != NULL);
    qsf_be->fullpath = NULL;
    if (book_path == NULL)
    {
        /* use stdout */
        qof_backend_set_error(be, ERR_BACKEND_NO_ERR);
        return;
    }
    if (g_str_has_prefix (book_path, "file:"))
    {
        qsf_be->fullpath = g_strdup (book_path + 5);
    }
    else if (g_str_has_prefix (book_path, "qsf:"))
    {
        qsf_be->fullpath = g_strdup (book_path + 4);
    }
    else
    {
        qsf_be->fullpath = g_strdup (book_path);
    }
    if (create_if_nonexistent)
    {
        FILE *f;

        f = g_fopen(qsf_be->fullpath, "a+");
        if (f)
        {
            fclose(f);
        }
        else
        {
            qof_backend_set_error(be, ERR_BACKEND_READONLY);
            return;
        }
    }
    qof_backend_set_error(be, ERR_BACKEND_NO_ERR);
}

static void
qsf_free_params(qsf_param *params)
{
    g_hash_table_destroy(params->qsf_calculate_hash);
    g_hash_table_destroy(params->qsf_default_hash);
    if (params->referenceList)
    {
        g_list_free(params->referenceList);
    }
    g_slist_free(params->supported_types);
    if (params->map_ns)
    {
        xmlFreeNs(params->map_ns);
    }
}

static void
qsf_session_end( QofBackend *be)
{
    QSFBackend *qsf_be;

    qsf_be = (QSFBackend*)be;
    g_return_if_fail(qsf_be != NULL);
    qsf_free_params(qsf_be->params);
    g_free(qsf_be->fullpath);
    qsf_be->fullpath = NULL;
    xmlCleanupParser();
}

static void
qsf_destroy_backend (QofBackend *be)
{
    qof_backend_destroy(be);
    g_free(be);
}

static void
ent_ref_cb (QofInstance* ent, gpointer user_data)
{
    qsf_param *params;
    QofInstanceReference *ref;
    void (*reference_setter) (QofInstance*, QofInstance*);
    QofInstance *reference;
    QofCollection *coll;
    QofIdType type;

    params = (qsf_param*)user_data;
    g_return_if_fail(params);
    while (params->referenceList)
    {
        ref = (QofInstanceReference*)params->referenceList->data;
        if (qof_object_is_choice(ent->e_type))
        {
            type = ref->choice_type;
        }
        else
        {
            type = ref->type;
        }
        coll = qof_book_get_collection(params->book, type);
        reference = qof_collection_lookup_entity(coll, ref->ref_guid);
        reference_setter = (void(*)(QofInstance*, QofInstance*))ref->param->param_setfcn;
        if (reference_setter != NULL)
        {
            qof_begin_edit((QofInstance*)ent);
            qof_begin_edit((QofInstance*)reference);
            reference_setter(ent, reference);
            qof_commit_edit((QofInstance*)ent);
            qof_commit_edit((QofInstance*)reference);
        }
        params->referenceList = g_list_next(params->referenceList);
    }
}

static void
insert_ref_cb(QofObject *obj, gpointer user_data)
{
    qsf_param *params;

    params = (qsf_param*)user_data;
    g_return_if_fail(params);
    qof_object_foreach(obj->e_type, params->book, ent_ref_cb, params);
}

/*================================================
	Load QofInstance into QofBook from XML in memory
==================================================*/

static gboolean
qsfdoc_to_qofbook(xmlDocPtr doc, qsf_param *params)
{
    QofInstance *inst;
    struct qsf_node_iterate iter;
    QofBook *book;
    GList *object_list;
    xmlNodePtr qsf_root;
    xmlNsPtr qsf_ns;

    g_return_val_if_fail(params != NULL, FALSE);
    g_return_val_if_fail(params->input_doc != NULL, FALSE);
    g_return_val_if_fail(params->book != NULL, FALSE);
    g_return_val_if_fail(params->file_type == OUR_QSF_OBJ, FALSE);
    qsf_root = xmlDocGetRootElement(params->input_doc);
    if (!qsf_root)
    {
        return FALSE;
    }
    qsf_ns = qsf_root->ns;
    iter.ns = qsf_ns;
    book = params->book;
    params->referenceList = (GList*)qof_book_get_data(book, ENTITYREFERENCE);
    qsf_node_foreach(qsf_root, qsf_book_node_handler, &iter, params);
    object_list = g_list_copy(params->qsf_object_list);
    while (object_list != NULL)
    {
        params->object_set = object_list->data;
        object_list = g_list_next(object_list);
        params->qsf_parameter_hash = params->object_set->parameters;
        if (!qof_class_is_registered(params->object_set->object_type))
        {
            continue;
        }
        inst = (QofInstance*)qof_object_new_instance(params->object_set->object_type, book);
        g_return_val_if_fail(inst != NULL, FALSE);
        params->qsf_ent = inst;
        qof_begin_edit(inst);
        g_hash_table_foreach(params->qsf_parameter_hash, qsf_object_commitCB, params);
        qof_commit_edit(inst);
    }
    qof_object_foreach_type(insert_ref_cb, params);
    qof_book_set_data(book, ENTITYREFERENCE, params->referenceList);
    return TRUE;
}

/* QofBackend routine to load from file - needs a map.
*/
static gboolean
load_qsf_object(QofBook *book, const gchar *fullpath, qsf_param *params)
{
    xmlNodePtr qsf_root, map_root;
    xmlDocPtr mapDoc, foreign_doc;
    gchar *map_path, *map_file;

    map_file = params->map_path;
    mapDoc = NULL;
    /* use selected map */
    if (!map_file)
    {
        qof_backend_set_error(params->be, ERR_QSF_NO_MAP);
        return FALSE;
    }
    foreign_doc = xmlParseFile(fullpath);
    if (foreign_doc == NULL)
    {
        qof_backend_set_error(params->be, ERR_FILEIO_PARSE_ERROR);
        return FALSE;
    }
    qsf_root = NULL;
    qsf_root = xmlDocGetRootElement(foreign_doc);
    params->qsf_ns = qsf_root->ns;
    params->book = book;
    map_path = g_strdup_printf("%s/%s", QSF_SCHEMA_DIR, map_file);
    if (!map_path)
    {
        qof_backend_set_error(params->be, ERR_QSF_NO_MAP);
        return FALSE;
    }
    mapDoc = xmlParseFile(map_path);
    if (!mapDoc)
    {
        qof_backend_set_error(params->be, ERR_QSF_NO_MAP);
        return FALSE;
    }
    map_root = xmlDocGetRootElement(mapDoc);
    params->map_ns = map_root->ns;
    params->input_doc = qsf_object_convert(mapDoc, qsf_root, params);
    qsfdoc_to_qofbook(params->input_doc, params);
    return TRUE;
}

static gboolean
load_our_qsf_object(QofBook *book, const gchar *fullpath, qsf_param *params)
{
    xmlNodePtr qsf_root;

    params->input_doc = xmlParseFile(fullpath);
    if (params->input_doc == NULL)
    {
        qof_backend_set_error(params->be, ERR_FILEIO_PARSE_ERROR);
        return FALSE;
    }
    qsf_root = NULL;
    qsf_root = xmlDocGetRootElement(params->input_doc);
    params->qsf_ns = qsf_root->ns;
    return qsfdoc_to_qofbook(params->input_doc, params);
}

/* Determine the type of QSF and load it into the QofBook

- is_our_qsf_object, OUR_QSF_OBJ, QSF object file using only QOF objects known
	to the calling process.	No map is required.
- is_qsf_object, IS_QSF_OBJ, QSF object file that may or may not have a QSF map
	to convert external objects. This temporary type will be set to HAVE_QSF_MAP
	if a suitable map exists, or an error value returned: ERR_QSF_NO_MAP,
	ERR_QSF_BAD_MAP or ERR_QSF_WRONG_MAP. This allows the calling process to inform
	the user that the QSF itself is valid but a suitable map cannot be found.
- is_qsf_map, IS_QSF_MAP, QSF map file. In the backend, this generates
	ERR_QSF_MAP_NOT_OBJ but	it can be used internally when processing maps to
	match a QSF object.

returns NULL on error, otherwise a pointer to the QofBook. Use
	the qof_book_merge API to merge the new data into the current
	QofBook.
*/
static void
qsf_file_type(QofBackend *be, QofBook *book, QofBackendLoadType loadType)
{
    QSFBackend *qsf_be;
    qsf_param *params;
    FILE *f;
    gchar *path;
    gboolean result;

    g_return_if_fail(be != NULL);
    g_return_if_fail(book != NULL);
    qsf_be = (QSFBackend*) be;
    g_return_if_fail(qsf_be != NULL);
    g_return_if_fail(qsf_be->fullpath != NULL);
    g_return_if_fail(qsf_be->params != NULL);
    params = qsf_be->params;
    params->book = book;
    path = g_strdup(qsf_be->fullpath);
    f = g_fopen(path, "r");
    if (!f)
    {
        qof_backend_set_error(be, ERR_FILEIO_READ_ERROR);
    }
    fclose(f);
    params->filepath = g_strdup(path);
    qof_backend_get_error(be);
    result = is_our_qsf_object_be(params);
    if (result)
    {
        params->file_type = OUR_QSF_OBJ;
        result = load_our_qsf_object(book, path, params);
        if (!result)
        {
            qof_backend_set_error(be, ERR_FILEIO_PARSE_ERROR);
        }
        return;
    }
    else if (is_qsf_object_be(params))
    {
        params->file_type = IS_QSF_OBJ;
        result = load_qsf_object(book, path, params);
        if (!result)
        {
            qof_backend_set_error(be, ERR_FILEIO_PARSE_ERROR);
        }
        return;
    }
    if (result == FALSE)
    {
        if (is_qsf_map_be(params))
        {
            params->file_type = IS_QSF_MAP;
            qof_backend_set_error(be, ERR_QSF_MAP_NOT_OBJ);
        }
    }
}

static void
qsf_object_sequence(QofParam *qof_param, gpointer data)
{
    qsf_param *params;
    GSList *checklist, *result;

    g_return_if_fail(data != NULL);
    params = (qsf_param*) data;
    result = NULL;
    checklist = NULL;
    params->knowntype = FALSE;
    checklist = g_slist_copy(params->supported_types);
    for (result = checklist; result != NULL; result = result->next)
    {
        if (0 == safe_strcmp((QofIdType)result->data, qof_param->param_type))
        {
            params->knowntype = TRUE;
        }
    }
    g_slist_free(checklist);
    if (0 == safe_strcmp(qof_param->param_type, params->qof_type))
    {
        params->qsf_sequence = g_slist_append(params->qsf_sequence, qof_param);
        params->knowntype = TRUE;
    }
    /* handle params->qof_type = QOF_TYPE_GUID and qof_param->param_type != known type */
    if (0 == safe_strcmp(params->qof_type, QOF_TYPE_GUID)
            && (params->knowntype == FALSE))
    {
        params->qsf_sequence = g_slist_append(params->qsf_sequence, qof_param);
        params->knowntype = TRUE;
    }
}

/* receives each entry from supported_types in sequence
	type = qof data type from supported list
	user_data = params. Holds object type
*/
static void
qsf_supported_parameters(gpointer type, gpointer user_data)
{
    qsf_param *params;

    g_return_if_fail(user_data != NULL);
    params = (qsf_param*) user_data;
    params->qof_type = (QofIdType)type;
    params->knowntype = FALSE;
    qof_class_param_foreach(params->qof_obj_type, qsf_object_sequence, params);
}

static KvpValueType
qsf_to_kvp_helper(const char *type_string)
{
    if (0 == safe_strcmp(QOF_TYPE_INT64,   type_string))
    {
        return KVP_TYPE_GINT64;
    }
    if (0 == safe_strcmp(QOF_TYPE_DOUBLE,  type_string))
    {
        return KVP_TYPE_DOUBLE;
    }
    if (0 == safe_strcmp(QOF_TYPE_NUMERIC, type_string))
    {
        return KVP_TYPE_NUMERIC;
    }
    if (0 == safe_strcmp(QOF_TYPE_STRING,  type_string))
    {
        return KVP_TYPE_STRING;
    }
    if (0 == safe_strcmp(QOF_TYPE_GUID,    type_string))
    {
        return KVP_TYPE_GUID;
    }
    if (0 == safe_strcmp(QOF_TYPE_DATE,    type_string))
    {
        return KVP_TYPE_TIMESPEC;
    }
    if (0 == safe_strcmp(QSF_TYPE_BINARY,  type_string))
    {
        return KVP_TYPE_BINARY;
    }
    if (0 == safe_strcmp(QSF_TYPE_GLIST,   type_string))
    {
        return KVP_TYPE_GLIST;
    }
    if (0 == safe_strcmp(QSF_TYPE_FRAME,   type_string))
    {
        return KVP_TYPE_FRAME;
    }
    return 0;
}

static QofIdTypeConst
kvp_value_to_qof_type_helper(KvpValueType n)
{
    switch (n)
    {
    case KVP_TYPE_GINT64   :
    {
        return QOF_TYPE_INT64;
        break;
    }
    case KVP_TYPE_DOUBLE   :
    {
        return QOF_TYPE_DOUBLE;
        break;
    }
    case KVP_TYPE_NUMERIC  :
    {
        return QOF_TYPE_NUMERIC;
        break;
    }
    case KVP_TYPE_STRING   :
    {
        return QOF_TYPE_STRING;
        break;
    }
    case KVP_TYPE_GUID     :
    {
        return QOF_TYPE_GUID;
        break;
    }
    case KVP_TYPE_TIMESPEC :
    {
        return QOF_TYPE_DATE;
        break;
    }
    case KVP_TYPE_BINARY   :
    {
        return QSF_TYPE_BINARY;
        break;
    }
    case KVP_TYPE_GLIST    :
    {
        return QSF_TYPE_GLIST;
        break;
    }
    case KVP_TYPE_FRAME    :
    {
        return QSF_TYPE_FRAME;
        break;
    }
    default :
    {
        return NULL;
    }
    }
}


static void
qsf_from_kvp_helper(const gchar *path, KvpValue *content, gpointer data)
{
    qsf_param *params;
    QofParam *qof_param;
    xmlNodePtr node;
    KvpValueType n;
    gchar *full_path;

    params = (qsf_param*)data;
    qof_param = params->qof_param;
    full_path = NULL;
    g_return_if_fail(params && path && content);
    ENTER (" ");
    n = kvp_value_get_type(content);
    switch (n)
    {
    case KVP_TYPE_GINT64   :
    case KVP_TYPE_DOUBLE   :
    case KVP_TYPE_NUMERIC  :
    case KVP_TYPE_STRING   :
    case KVP_TYPE_GUID     :
    case KVP_TYPE_TIMESPEC :
    case KVP_TYPE_BINARY   :
    case KVP_TYPE_GLIST    :
    {
        node = xmlAddChild(params->output_node, xmlNewNode(params->qsf_ns,
                           BAD_CAST qof_param->param_type));
        xmlNodeAddContent(node, BAD_CAST kvp_value_to_bare_string(content));
        xmlNewProp(node, BAD_CAST QSF_OBJECT_TYPE, BAD_CAST qof_param->param_name);
        full_path = g_strconcat(params->full_kvp_path, "/", path, NULL);
        xmlNewProp(node, BAD_CAST QSF_OBJECT_KVP, BAD_CAST full_path);
        xmlNewProp(node, BAD_CAST QSF_OBJECT_VALUE,
                   BAD_CAST kvp_value_to_qof_type_helper(n));
        PINFO (" set %s", kvp_value_to_qof_type_helper(n));
        break;
    }
    case KVP_TYPE_FRAME:
    {
        if (!params->full_kvp_path)
        {
            params->full_kvp_path = g_strdup(path);
        }
        else
        {
            params->full_kvp_path = g_strconcat(params->full_kvp_path,
                                                "/", path, NULL);
        }
        PINFO (" full=%s, path=%s ", params->full_kvp_path, path);
        kvp_frame_for_each_slot(kvp_value_get_frame(content),
                                qsf_from_kvp_helper, params);
        g_free(params->full_kvp_path);
        params->full_kvp_path = NULL;
        break;
    }
    default:
    {
        PERR (" unsupported value = %d", kvp_value_get_type(content));
        break;
    }
    }
    LEAVE (" ");
}

static void
qsf_from_coll_cb (QofInstance *ent, gpointer user_data)
{
    qsf_param *params;
    QofParam *qof_param;
    xmlNodePtr node;
    gchar qsf_guid[GUID_ENCODING_LENGTH + 1];

    params = (qsf_param*)user_data;
    if (!ent || !params)
    {
        return;
    }
    qof_param = params->qof_param;
    guid_to_string_buff(qof_instance_get_guid(ent), qsf_guid);
    node = xmlAddChild(params->output_node, xmlNewNode(params->qsf_ns,
                       BAD_CAST qof_param->param_type));
    xmlNodeAddContent(node, BAD_CAST qsf_guid);
    xmlNewProp(node, BAD_CAST QSF_OBJECT_TYPE, BAD_CAST qof_param->param_name);
}

/******* reference handling ***********/

static gint
qof_reference_list_cb(gconstpointer a, gconstpointer b)
{
    const QofInstanceReference *aa;
    const QofInstanceReference *bb;

    aa = (QofInstanceReference*) a;
    bb = (QofInstanceReference*) b;
    if (aa == NULL)
    {
        return 1;
    }
    g_return_val_if_fail((bb != NULL), 1);
    g_return_val_if_fail((aa->type != NULL), 1);
    if ((0 == guid_compare(bb->ent_guid, aa->ent_guid))
            && (0 == safe_strcmp(bb->type, aa->type))
            && (0 == safe_strcmp(bb->param->param_name, aa->param->param_name)))
    {
        return 0;
    }
    return 1;
}

static QofInstanceReference*
qof_reference_lookup(GList *referenceList, QofInstanceReference *find)
{
    GList *single_ref;
    QofInstanceReference *ent_ref;

    if (referenceList == NULL)
    {
        return NULL;
    }
    g_return_val_if_fail(find != NULL, NULL);
    single_ref = NULL;
    ent_ref = NULL;
    single_ref = g_list_find_custom(referenceList, find, qof_reference_list_cb);
    if (single_ref == NULL)
    {
        return ent_ref;
    }
    ent_ref = (QofInstanceReference*)single_ref->data;
    g_list_free(single_ref);
    return ent_ref;
}

static void
reference_list_lookup(gpointer data, gpointer user_data)
{
    QofInstance *ent;
    QofParam *ref_param;
    QofInstanceReference *reference, *starter;
    qsf_param  *params;
    const GncGUID *guid;
    xmlNodePtr node, object_node;
    xmlNsPtr ns;
    GList *copy_list;
    gchar qsf_guid[GUID_ENCODING_LENGTH + 1], *ref_name;

    params = (qsf_param*)user_data;
    ref_param = (QofParam*)data;
    object_node = params->output_node;
    ent = params->qsf_ent;
    g_return_if_fail(ent);
    ns = params->qsf_ns;
    starter = g_new(QofInstanceReference, 1);
    starter->ent_guid = qof_instance_get_guid(ent);
    starter->type = g_strdup(ent->e_type);
    starter->param = ref_param;
    starter->ref_guid = NULL;
    copy_list = g_list_copy(params->referenceList);
    reference = qof_reference_lookup(copy_list, starter);
    g_free(starter);
    if (reference != NULL)
    {
        if ((ref_param->param_getfcn == NULL) || (ref_param->param_setfcn == NULL))
        {
            return;
        }
        ref_name = g_strdup(reference->param->param_name);
        node = xmlAddChild(object_node, xmlNewNode(ns, BAD_CAST QOF_TYPE_GUID));
        guid_to_string_buff(reference->ref_guid, qsf_guid);
        xmlNodeAddContent(node, BAD_CAST qsf_guid);
        xmlNewProp(node, BAD_CAST QSF_OBJECT_TYPE, BAD_CAST ref_name);
        g_free(ref_name);
    }
    else
    {
        ent = QOF_INSTANCE(ref_param->param_getfcn(ent, ref_param));
        if (!ent)
        {
            return;
        }
        if ((0 == safe_strcmp(ref_param->param_type, QOF_TYPE_COLLECT)) ||
                (0 == safe_strcmp(ref_param->param_type, QOF_TYPE_CHOICE)))
        {
            return;
        }
        node = xmlAddChild(object_node, xmlNewNode(ns, BAD_CAST QOF_TYPE_GUID));
        guid = qof_instance_get_guid(ent);
        guid_to_string_buff(guid, qsf_guid);
        xmlNodeAddContent(node, BAD_CAST qsf_guid);
        xmlNewProp(node, BAD_CAST QSF_OBJECT_TYPE, BAD_CAST ref_param->param_name);
    }
}

/*=====================================
	Convert QofInstance to QSF XML node
qof_param holds the parameter sequence.
=======================================*/
static void
qsf_entity_foreach(QofInstance *ent, gpointer data)
{
    qsf_param  *params;
    GSList     *param_list, *supported;
    GList      *ref;
    xmlNodePtr node, object_node;
    xmlNsPtr   ns;
    gchar      *string_buffer;
    QofParam   *qof_param;
    QofInstance  *choice_ent;
    KvpFrame   *qsf_kvp;
    QofCollection *qsf_coll;
    gint        param_count;
    gboolean   own_guid;
    const GncGUID *cm_guid;
    gchar       cm_sa[GUID_ENCODING_LENGTH + 1];

    g_return_if_fail(ent != NULL);
    g_return_if_fail(data != NULL);
    params = (qsf_param*)data;
    param_count = ++params->count;
    ns = params->qsf_ns;
    qsf_kvp = kvp_frame_new();
    own_guid = FALSE;
    choice_ent = NULL;
    object_node = xmlNewChild(params->book_node, params->qsf_ns,
                              BAD_CAST QSF_OBJECT_TAG, NULL);
    xmlNewProp(object_node, BAD_CAST QSF_OBJECT_TYPE, BAD_CAST ent->e_type);
    string_buffer = g_strdup_printf("%i", param_count);
    xmlNewProp(object_node, BAD_CAST QSF_OBJECT_COUNT, BAD_CAST string_buffer);
    g_free(string_buffer);
    param_list = g_slist_copy(params->qsf_sequence);
    while (param_list != NULL)
    {
        qof_param = (QofParam*)param_list->data;
        g_return_if_fail(qof_param != NULL);
        if (0 == safe_strcmp(qof_param->param_type, QOF_TYPE_GUID))
        {
            if (!own_guid)
            {
                cm_guid = qof_instance_get_guid(ent);
                node = xmlAddChild(object_node, xmlNewNode(ns, BAD_CAST QOF_TYPE_GUID));
                guid_to_string_buff(cm_guid, cm_sa);
                string_buffer = g_strdup(cm_sa);
                xmlNodeAddContent(node, BAD_CAST string_buffer);
                xmlNewProp(node, BAD_CAST QSF_OBJECT_TYPE , BAD_CAST QOF_PARAM_GUID);
                g_free(string_buffer);
                own_guid = TRUE;
            }
            params->qsf_ent = ent;
            params->output_node = object_node;
            ref = qof_class_get_referenceList(ent->e_type);
            if (ref != NULL)
            {
                g_list_foreach(ref, reference_list_lookup, params);
            }
        }
        if (0 == safe_strcmp(qof_param->param_type, QOF_TYPE_COLLECT))
        {
            qsf_coll = qof_param->param_getfcn(ent, qof_param);
            if (qsf_coll)
            {
                params->qof_param = qof_param;
                params->output_node = object_node;
                if (qof_collection_count(qsf_coll) > 0)
                {
                    qof_collection_foreach(qsf_coll, qsf_from_coll_cb, params);
                }
            }
            param_list = g_slist_next(param_list);
            continue;
        }
        if (0 == safe_strcmp(qof_param->param_type, QOF_TYPE_CHOICE))
        {
            /** \todo use the reference list here. */
            choice_ent = QOF_INSTANCE(qof_param->param_getfcn(ent, qof_param));
            if (!choice_ent)
            {
                param_list = g_slist_next(param_list);
                continue;
            }
            node = xmlAddChild(object_node, xmlNewNode(ns, BAD_CAST qof_param->param_type));
            cm_guid = qof_instance_get_guid(choice_ent);
            guid_to_string_buff(cm_guid, cm_sa);
            string_buffer = g_strdup(cm_sa);
            xmlNodeAddContent(node, BAD_CAST string_buffer);
            xmlNewProp(node, BAD_CAST QSF_OBJECT_TYPE, BAD_CAST qof_param->param_name);
            xmlNewProp(node, BAD_CAST "name", BAD_CAST choice_ent->e_type);
            g_free(string_buffer);
            param_list = g_slist_next(param_list);
            continue;
        }
        if (0 == safe_strcmp(qof_param->param_type, QOF_TYPE_KVP))
        {
            qsf_kvp = (KvpFrame*)qof_param->param_getfcn(ent, qof_param);
            if (kvp_frame_is_empty(qsf_kvp))
            {
                LEAVE(" ");
                return;
            }
            params->qof_param = qof_param;
            params->output_node = object_node;
            kvp_frame_for_each_slot(qsf_kvp, qsf_from_kvp_helper, params);
        }
        if ((qof_param->param_setfcn != NULL) && (qof_param->param_getfcn != NULL))
        {
            for ( supported = g_slist_copy(params->supported_types);
                    supported != NULL; supported = g_slist_next(supported))
            {
                if (0 == safe_strcmp((const gchar*)supported->data, (const gchar*)qof_param->param_type))
                {
                    node = xmlAddChild(object_node, xmlNewNode(ns, BAD_CAST qof_param->param_type));
                    string_buffer = g_strdup(qof_book_merge_param_as_string(qof_param, ent));
                    xmlNodeAddContent(node, BAD_CAST string_buffer);
                    xmlNewProp(node, BAD_CAST QSF_OBJECT_TYPE, BAD_CAST qof_param->param_name);
                    g_free(string_buffer);
                }
            }
        }
        param_list = g_slist_next(param_list);
    }
}

static void
qsf_foreach_obj_type(QofObject *qsf_obj, gpointer data)
{
    qsf_param *params;
    QofBook *book;
    GSList *support;

    g_return_if_fail(data != NULL);
    params = (qsf_param*) data;
    /* Skip unsupported objects */
    if ((qsf_obj->create == NULL) || (qsf_obj->foreach == NULL))
    {
        PINFO (" qsf_obj QOF support failed %s", qsf_obj->e_type);
        return;
    }
    params->qof_obj_type = qsf_obj->e_type;
    params->qsf_sequence = NULL;
    book = params->book;
    support = g_slist_copy(params->supported_types);
    g_slist_foreach(support, qsf_supported_parameters, params);
    qof_object_foreach(qsf_obj->e_type, book, qsf_entity_foreach, params);
}

/*=====================================================
	Take a QofBook and prepare a QSF XML doc in memory
=======================================================*/
/*	QSF only uses one QofBook per file - count may be removed later. */
static xmlDocPtr
qofbook_to_qsf(QofBook *book, qsf_param *params)
{
    xmlNodePtr top_node, node;
    xmlDocPtr doc;
    gchar buffer[GUID_ENCODING_LENGTH + 1];
    const GncGUID *book_guid;

    g_return_val_if_fail(book != NULL, NULL);
    params->book = book;
    params->referenceList =
        g_list_copy((GList*)qof_book_get_data(book, ENTITYREFERENCE));
    doc = xmlNewDoc(BAD_CAST QSF_XML_VERSION);
    top_node = xmlNewNode(NULL, BAD_CAST QSF_ROOT_TAG);
    xmlDocSetRootElement(doc, top_node);
    xmlSetNs(top_node, xmlNewNs(top_node, BAD_CAST QSF_DEFAULT_NS, NULL));
    params->qsf_ns = top_node->ns;
    node = xmlNewChild(top_node, params->qsf_ns, BAD_CAST QSF_BOOK_TAG, NULL);
    params->book_node = node;
    xmlNewProp(node, BAD_CAST QSF_BOOK_COUNT, BAD_CAST "1");
    book_guid = qof_book_get_guid(book);
    guid_to_string_buff(book_guid, buffer);
    xmlNewChild(params->book_node, params->qsf_ns,
                BAD_CAST QSF_BOOK_GUID, BAD_CAST buffer);
    params->output_doc = doc;
    params->book_node = node;
    qof_object_foreach_type(qsf_foreach_obj_type, params);
    return params->output_doc;
}

static void
write_qsf_from_book(const char *path, QofBook *book, qsf_param *params)
{
    xmlDocPtr qsf_doc;
    gint write_result;
    QofBackend *be;

    be = qof_book_get_backend(book);
    qsf_doc = qofbook_to_qsf(book, params);
    write_result = 0;
    DEBUG (" use_gz_level=%" G_GINT64_FORMAT " encoding=%s",
           params->use_gz_level, params->encoding);
    if ((params->use_gz_level > 0) && (params->use_gz_level <= 9))
    {
        xmlSetDocCompressMode(qsf_doc, params->use_gz_level);
    }
    g_return_if_fail(qsf_is_valid(QSF_SCHEMA_DIR, QSF_OBJECT_SCHEMA, qsf_doc) == TRUE);
    write_result = xmlSaveFormatFileEnc(path, qsf_doc, params->encoding, 1);
    if (write_result < 0)
    {
        qof_backend_set_error(be, ERR_FILEIO_WRITE_ERROR);
        return;
    }
    xmlFreeDoc(qsf_doc);
}

static void
write_qsf_to_stdout(QofBook *book, qsf_param *params)
{
    xmlDocPtr qsf_doc;

    qsf_doc = qofbook_to_qsf(book, params);
    g_return_if_fail(qsf_is_valid(QSF_SCHEMA_DIR, QSF_OBJECT_SCHEMA, qsf_doc) == TRUE);
    DEBUG (" use_gz_level=%" G_GINT64_FORMAT " encoding=%s",
           params->use_gz_level, params->encoding);
    xmlSaveFormatFileEnc("-", qsf_doc, params->encoding, 1);
    fprintf(stdout, "\n");
    xmlFreeDoc(qsf_doc);
}

static void
qsf_write_file(QofBackend *be, QofBook *book)
{
    QSFBackend *qsf_be;
    qsf_param *params;
    char *path;

    qsf_be = (QSFBackend*)be;
    params = qsf_be->params;
    /* if fullpath is blank, book_id was set to QOF_STDOUT */
    if (!qsf_be->fullpath || (*qsf_be->fullpath == '\0'))
    {
        write_qsf_to_stdout(book, params);
        return;
    }
    path = strdup(qsf_be->fullpath);
    write_qsf_from_book(path, book, params);
    g_free(path);
}

KvpValue*
string_to_kvp_value(const gchar *content, KvpValueType type)
{
    gchar        *tail;
    gint64      cm_i64;
    double      cm_double;
    gnc_numeric cm_numeric;
    GncGUID        *cm_guid;
    struct tm   kvp_time;
    time_t      kvp_time_t;
    Timespec    cm_date;

    switch (type)
    {
    case KVP_TYPE_GINT64:
        errno = 0;
        cm_i64 = strtoll(content, &tail, 0);
        if (errno == 0)
        {
            return kvp_value_new_gint64(cm_i64);
        }
        break;
    case KVP_TYPE_DOUBLE:
        errno = 0;
        cm_double = strtod(content, &tail);
        if (errno == 0)
        {
            return kvp_value_new_double(cm_double);
        }
        break;
    case KVP_TYPE_NUMERIC:
        string_to_gnc_numeric(content, &cm_numeric);
        return kvp_value_new_gnc_numeric(cm_numeric);
        break;
    case KVP_TYPE_STRING:
        return kvp_value_new_string(content);
        break;
    case KVP_TYPE_GUID:
        cm_guid = g_new(GncGUID, 1);
        if (TRUE == string_to_guid(content, cm_guid))
        {
            return kvp_value_new_guid(cm_guid);
        }
        break;
    case KVP_TYPE_TIMESPEC:
        strptime(content, QSF_XSD_TIME, &kvp_time);
        kvp_time_t = mktime(&kvp_time);
        timespecFromTime_t(&cm_date, kvp_time_t);
        return kvp_value_new_timespec(cm_date);
        break;
    case KVP_TYPE_BINARY:
//		return kvp_value_new_binary(value->value.binary.data,
//									value->value.binary.datasize);
        break;
    case KVP_TYPE_GLIST:
//		return kvp_value_new_glist(value->value.list);
        break;
    case KVP_TYPE_FRAME:
//		return kvp_value_new_frame(value->value.frame);
        break;
    case KVP_TYPE_GDATE:
    {
        GDate date;
        g_date_clear(&date, 1);
        g_date_set_parse(&date, content);
        return kvp_value_new_gdate(date);
    }
    }
    return NULL;
}

/* ======================================================
	Commit XML data from file to QofInstance in a QofBook
========================================================= */
void
qsf_object_commitCB(gpointer key, gpointer value, gpointer data)
{
    qsf_param          *params;
    qsf_objects        *object_set;
    xmlNodePtr         node;
    QofInstanceReference *reference;
    QofInstance          *qsf_ent;
    QofBook            *targetBook;
    const char         *qof_type, *parameter_name, *timechk;
    QofIdType          obj_type, reference_type;
    struct tm          qsf_time;
    time_t             qsf_time_t;
    gchar              *tail;
    /* cm_ prefix used for variables that hold the data to commit */
    gnc_numeric    cm_numeric;
    double         cm_double;
    gboolean       cm_boolean;
    gint32         cm_i32;
    gint64         cm_i64;
    Timespec       cm_date;
    gchar          *cm_char,  *(*char_getter)  (xmlNodePtr);
    GncGUID           *cm_guid;
    KvpFrame       *cm_kvp;
    KvpValue       *cm_value;
    KvpValueType   cm_type;
    QofSetterFunc  cm_setter;
    const QofParam *cm_param;
    void (*string_setter)    (QofInstance*, const gchar*);
    void (*date_setter)      (QofInstance*, Timespec);
    void (*numeric_setter)   (QofInstance*, gnc_numeric);
    void (*double_setter)    (QofInstance*, double);
    void (*boolean_setter)   (QofInstance*, gboolean);
    void (*i32_setter)       (QofInstance*, gint32);
    void (*i64_setter)       (QofInstance*, gint64);
    void (*char_setter)      (QofInstance*, gchar);

    g_return_if_fail(data && value && key);
    params = (qsf_param*)data;
    node = (xmlNodePtr)value;
    parameter_name = (const gchar*)key;
    qof_type = (gchar*)node->name;
    qsf_ent = params->qsf_ent;
    targetBook = params->book;
    memset (&qsf_time, '\0', sizeof(qsf_time));
    cm_date.tv_nsec = 0;
    cm_date.tv_sec = 0;
    obj_type = (gchar*)xmlGetProp(node->parent, BAD_CAST QSF_OBJECT_TYPE);
    if (0 == safe_strcasecmp(obj_type, parameter_name))
    {
        return;
    }
    cm_setter = qof_class_get_parameter_setter(obj_type, parameter_name);
    cm_param = qof_class_get_parameter(obj_type, parameter_name);
    object_set = params->object_set;
    if (safe_strcmp(qof_type, QOF_TYPE_STRING) == 0)
    {
        string_setter = (void(*)(QofInstance*, const gchar*))cm_setter;
        if (string_setter != NULL)
        {
            string_setter(qsf_ent, (gchar*)xmlNodeGetContent(node));
        }
    }
    if (safe_strcmp(qof_type, QOF_TYPE_DATE) == 0)
    {
        date_setter = (void(*)(QofInstance*, Timespec))cm_setter;
        timechk = NULL;
        timechk = strptime((char*)xmlNodeGetContent(node), QSF_XSD_TIME, &qsf_time);
        g_return_if_fail(timechk != NULL);
        qsf_time_t = mktime(&qsf_time);
        if (qsf_time_t != -3600)
        {
            timespecFromTime_t(&cm_date, qsf_time_t);
            if (date_setter != NULL)
            {
                date_setter(qsf_ent, cm_date);
            }
        }
    }
    if ((safe_strcmp(qof_type, QOF_TYPE_NUMERIC) == 0)  ||
            (safe_strcmp(qof_type, QOF_TYPE_DEBCRED) == 0))
    {
        numeric_setter = (void(*)(QofInstance*, gnc_numeric))cm_setter;
        string_to_gnc_numeric((char*)xmlNodeGetContent(node), &cm_numeric);
        if (numeric_setter != NULL)
        {
            numeric_setter(qsf_ent, cm_numeric);
        }
    }
    if (safe_strcmp(qof_type, QOF_TYPE_GUID) == 0)
    {
        cm_guid = g_new(GncGUID, 1);
        if (TRUE != string_to_guid((char*)xmlNodeGetContent(node), cm_guid))
        {
            qof_backend_set_error(params->be, ERR_QSF_BAD_OBJ_GUID);
            PINFO (" string to guid conversion failed for %s:%s:%s",
                   xmlNodeGetContent(node), obj_type, qof_type);
            return;
        }
        reference_type = (char*)xmlGetProp(node, BAD_CAST QSF_OBJECT_TYPE);
        if (0 == safe_strcmp(QOF_PARAM_GUID, reference_type))
        {
            qof_instance_set_guid(qsf_ent, cm_guid);
        }
        else
        {
            reference = qof_instance_get_reference_from(qsf_ent, cm_param);
            if (reference)
            {
                params->referenceList = g_list_append(params->referenceList, reference);
            }
        }
    }
    if (safe_strcmp(qof_type, QOF_TYPE_INT32) == 0)
    {
        errno = 0;
        cm_i32 = (gint32)strtol ((char*)xmlNodeGetContent(node), &tail, 0);
        if (errno == 0)
        {
            i32_setter = (void(*)(QofInstance*, gint32))cm_setter;
            if (i32_setter != NULL)
            {
                i32_setter(qsf_ent, cm_i32);
            }
        }
        else
        {
            qof_backend_set_error(params->be, ERR_QSF_OVERFLOW);
        }
    }
    if (safe_strcmp(qof_type, QOF_TYPE_INT64) == 0)
    {
        errno = 0;
        cm_i64 = strtoll((gchar*)xmlNodeGetContent(node), &tail, 0);
        if (errno == 0)
        {
            i64_setter = (void(*)(QofInstance*, gint64))cm_setter;
            if (i64_setter != NULL)
            {
                i64_setter(qsf_ent, cm_i64);
            }
        }
        else
        {
            qof_backend_set_error(params->be, ERR_QSF_OVERFLOW);
        }
    }
    if (safe_strcmp(qof_type, QOF_TYPE_DOUBLE) == 0)
    {
        errno = 0;
        cm_double = strtod((gchar*)xmlNodeGetContent(node), &tail);
        if (errno == 0)
        {
            double_setter = (void(*)(QofInstance*, double))cm_setter;
            if (double_setter != NULL)
            {
                double_setter(qsf_ent, cm_double);
            }
        }
    }
    if (safe_strcmp(qof_type, QOF_TYPE_BOOLEAN) == 0)
    {
        if (0 == safe_strcasecmp((gchar*)xmlNodeGetContent(node),
                                 QSF_XML_BOOLEAN_TEST))
        {
            cm_boolean = TRUE;
        }
        else
        {
            cm_boolean = FALSE;
        }
        boolean_setter = (void(*)(QofInstance*, gboolean))cm_setter;
        if (boolean_setter != NULL)
        {
            boolean_setter(qsf_ent, cm_boolean);
        }
    }
    if (safe_strcmp(qof_type, QOF_TYPE_KVP) == 0)
    {
        cm_type = qsf_to_kvp_helper((gchar*)xmlGetProp(node, BAD_CAST QSF_OBJECT_VALUE));
        if (!cm_type)
        {
            return;
        }
        cm_value = string_to_kvp_value((gchar*)xmlNodeGetContent(node), cm_type);
        cm_kvp = (KvpFrame*)cm_param->param_getfcn(qsf_ent, cm_param);
        cm_kvp = kvp_frame_set_value(cm_kvp, (gchar*)xmlGetProp(node,
                                     BAD_CAST QSF_OBJECT_KVP), cm_value);
    }
    if (safe_strcmp(qof_type, QOF_TYPE_COLLECT) == 0)
    {
        QofCollection *qsf_coll;
        QofIdType type;
        QofInstanceReference *reference;
        QofParam *copy_param;
        /* retrieve the *type* of the collection, ignore any contents. */
        qsf_coll = cm_param->param_getfcn(qsf_ent, cm_param);
        type = qof_collection_get_type(qsf_coll);
        cm_guid = g_new(GncGUID, 1);
        if (TRUE != string_to_guid((gchar*)xmlNodeGetContent(node), cm_guid))
        {
            qof_backend_set_error(params->be, ERR_QSF_BAD_OBJ_GUID);
            PINFO (" string to guid collect failed for %s", xmlNodeGetContent(node));
            return;
        }
        /* create a QofInstanceReference with this type and GncGUID.
         there is only one entity each time.
         cm_guid contains the GncGUID of the reference.
         type is the type of the reference. */
        reference = g_new0(QofInstanceReference, 1);
        reference->type = g_strdup(qsf_ent->e_type);
        reference->ref_guid = cm_guid;
        reference->ent_guid = qof_instance_get_guid(qsf_ent);
        copy_param = g_new0(QofParam, 1);
        copy_param->param_name = g_strdup(cm_param->param_name);
        copy_param->param_type = g_strdup(cm_param->param_type);
        reference->param = copy_param;
        params->referenceList = g_list_append(params->referenceList, reference);
    }
    if (safe_strcmp(qof_type, QOF_TYPE_CHAR) == 0)
    {
        char_getter = (gchar * (*)(xmlNodePtr))xmlNodeGetContent;
        cm_char = char_getter(node);
        char_setter = (void(*)(QofInstance*, gchar))cm_setter;
        if (char_setter != NULL)
        {
            char_setter(qsf_ent, *cm_char);
        }
        xmlFree(cm_char);
    }
}

static QofBackend*
qsf_backend_new(void)
{
    QSFBackend *qsf_be;
    QofBackend *be;

    qsf_be = g_new0(QSFBackend, 1);
    be = (QofBackend*) qsf_be;
    qof_backend_init(be);
    qsf_be->params = g_new(qsf_param, 1);
    qsf_be->params->be = be;
    qsf_param_init(qsf_be->params);
    qsf_be->be.session_begin = qsf_session_begin;

    be->session_end = qsf_session_end;
    be->destroy_backend = qsf_destroy_backend;
    be->load = qsf_file_type;
    be->save_may_clobber_data = NULL;
    /* The QSF backend will always load and save the entire QSF XML file. */
    be->begin = NULL;
    be->commit = NULL;
    be->rollback = NULL;
    /* QSF uses the built-in SQL, not a dedicated SQL server. */
    be->compile_query = NULL;
    be->free_query = NULL;
    be->run_query = NULL;
    be->counter = NULL;
    /* The QSF backend is not multi-user. */
    be->events_pending = NULL;
    be->process_events = NULL;

    be->sync = qsf_write_file;
    /* use for maps, later. */
    be->load_config = qsf_load_config;
    be->get_config = qsf_get_config;

    qsf_be->fullpath = NULL;
    return be;
}

/* The QOF method of loading each backend.
QSF is loaded as a GModule using the QOF method - QofBackendProvider.
*/
static void
qsf_provider_free (QofBackendProvider *prov)
{
    prov->provider_name = NULL;
    prov->access_method = NULL;
    g_free (prov);
}

G_MODULE_EXPORT void
qof_backend_module_init (void)
{
    QofBackendProvider *prov;

    prov = g_new0 (QofBackendProvider, 1);
    prov->provider_name = "QSF Backend Version 0.2";
    prov->access_method = "file";
    prov->partial_book_supported = TRUE;
    prov->backend_new = qsf_backend_new;
    prov->check_data_type = qsf_determine_file_type;
    prov->provider_free = qsf_provider_free;
    qof_backend_register_provider (prov);

    prov = g_new0 (QofBackendProvider, 1);
    prov->provider_name = "QSF Backend Version 0.2";
    prov->access_method = "qsf";
    prov->partial_book_supported = TRUE;
    prov->backend_new = qsf_backend_new;
    prov->check_data_type = qsf_determine_file_type;
    prov->provider_free = qsf_provider_free;
    qof_backend_register_provider (prov);
}

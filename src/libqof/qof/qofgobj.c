/********************************************************************\
 * qofgobj.c -- QOF to GLib GObject mapping                         *
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
#include "qof.h"
#include "qofgobj.h"

static QofLogModule log_module = QOF_MOD_QUERY;

static gboolean initialized = FALSE;
static GSList *paramList = NULL;
static GSList *classList = NULL;

/* =================================================================== */

#if 0
static gboolean
clear_table (gpointer key, gpointer value, gpointer user_data)
{
    g_slist_free (value);
    return TRUE;
}
#endif

void
qof_gobject_init(void)
{
    if (initialized) return;
    initialized = TRUE;

    // gobjectClassTable = g_hash_table_new (g_str_hash, g_str_equal);

    /* Init the other subsystems that we need */
    qof_object_initialize();
    qof_query_init ();
}

void
qof_gobject_shutdown (void)
{
    GSList *n;

    if (!initialized) return;
    initialized = FALSE;

//  GSList *n;
    for (n = paramList; n; n = n->next) g_free(n->data);
    g_slist_free (paramList);

    for (n = classList; n; n = n->next) g_free(n->data);
    g_slist_free (classList);

#if 0
    // XXX also need to walk over books, and collection and delete
    // the collection get_data instance lists !!
    // without this we have a memory leak !!
    g_hash_table_foreach_remove (gobjectParamTable, clear_table, NULL);
    g_hash_table_destroy (gobjectParamTable);
#endif
}

/* =================================================================== */

#define GOBJECT_TABLE  "GobjectTable"

void
qof_gobject_register_instance (QofBook *book, QofType type, GObject *gob)
{
    QofCollection *coll;
    GSList *instance_list;

    if (!book || !type) return;

    coll = qof_book_get_collection (book, type);

    instance_list = qof_collection_get_data (coll);
    instance_list = g_slist_prepend (instance_list, gob);
    qof_collection_set_data (coll, instance_list);
}

/* =================================================================== */

static gpointer
qof_gobject_getter (gpointer data, QofParam *getter)
{
    GObject *gob = data;
    const char *str;

    GParamSpec *gps = getter->param_userdata;

    /* Note that the return type must actually be of type
     * getter->param_type but we just follow the hard-coded
     * mapping below ... */
    if (G_IS_PARAM_SPEC_STRING(gps))
    {
        GValue gval = {G_TYPE_INVALID};
        g_value_init (&gval, G_TYPE_STRING);
        g_object_get_property (gob, getter->param_name, &gval);

        str = g_value_get_string (&gval);
        return (gpointer) str;
    }
    else if (G_IS_PARAM_SPEC_INT(gps))
    {
        long ival;

        GValue gval = {G_TYPE_INVALID};
        g_value_init (&gval, G_TYPE_INT);
        g_object_get_property (gob, getter->param_name, &gval);

        ival = g_value_get_int (&gval);
        return (gpointer) ival;
    }
    else if (G_IS_PARAM_SPEC_UINT(gps))
    {
        long ival;
        GValue gval = {G_TYPE_INVALID};
        g_value_init (&gval, G_TYPE_UINT);
        g_object_get_property (gob, getter->param_name, &gval);

        ival = g_value_get_uint (&gval);
        return (gpointer) ival;
    }
    else if (G_IS_PARAM_SPEC_BOOLEAN(gps))
    {
        gboolean ival;

        GValue gval = {G_TYPE_INVALID};
        g_value_init (&gval, G_TYPE_BOOLEAN);
        g_object_get_property (gob, getter->param_name, &gval);

        ival = g_value_get_boolean (&gval);
        return GINT_TO_POINTER( ival);
    }

    PWARN ("unhandled parameter type %s for paramter %s",
           G_PARAM_SPEC_TYPE_NAME(gps), getter->param_name);
    return NULL;
}

static double
qof_gobject_double_getter (gpointer data, QofParam *getter)
{
    GObject *gob = data;
    double fval;

    GParamSpec *gps = getter->param_userdata;

    /* Note that the return type must actually be of type
     * getter->param_type but we just follow the hard-coded
     * mapping below ... */
    if (G_IS_PARAM_SPEC_FLOAT(gps))
    {
        GValue gval = {G_TYPE_INVALID};
        g_value_init (&gval, G_TYPE_FLOAT);
        g_object_get_property (gob, getter->param_name, &gval);

        fval = g_value_get_float (&gval);
        return fval;
    }
    else if (G_IS_PARAM_SPEC_DOUBLE(gps))
    {
        GValue gval = {G_TYPE_INVALID};
        g_value_init (&gval, G_TYPE_DOUBLE);
        g_object_get_property (gob, getter->param_name, &gval);

        fval = g_value_get_double (&gval);
        return fval;
    }

    PWARN ("unhandled parameter type %s for paramter %s",
           G_PARAM_SPEC_TYPE_NAME(gps), getter->param_name);
    return 0.0;
}

/* =================================================================== */
/* Loop over every instance of the given type in the collection
 * of instances that we have on hand.
 */
static void
qof_gobject_foreach (QofCollection *coll, QofInstanceForeachCB cb, gpointer ud)
{
    GSList *n;
    n = qof_collection_get_data (coll);
    for (; n; n = n->next)
    {
        cb (n->data, ud);
    }
}

/* =================================================================== */

void
qof_gobject_register (QofType e_type, GObjectClass *obclass)
{
    int i;
    int j;
    QofParam *qof_param_list, *qpar;
    QofObject *class_def;
    GParamSpec **prop_list, *gparam;
    guint n_props;

    /* Get the GObject properties, convert to QOF properties */
    prop_list = g_object_class_list_properties (obclass, &n_props);

    qof_param_list = g_new0 (QofParam, n_props);
    paramList = g_slist_prepend (paramList, qof_param_list);

    PINFO ("object %s has %d props", e_type, n_props);
    j = 0;
    for (i = 0; i < n_props; i++)
    {
        gparam = prop_list[i];
        qpar = &qof_param_list[j];

        PINFO ("param %d %s is type %s",
               i, gparam->name, G_PARAM_SPEC_TYPE_NAME(gparam));

        qpar->param_name = g_param_spec_get_name (gparam);
        qpar->param_getfcn = (QofAccessFunc)qof_gobject_getter;
        qpar->param_setfcn = NULL;
        qpar->param_userdata = gparam;
        if ((G_IS_PARAM_SPEC_INT(gparam))  ||
                (G_IS_PARAM_SPEC_UINT(gparam)) ||
                (G_IS_PARAM_SPEC_ENUM(gparam)) ||
                (G_IS_PARAM_SPEC_FLAGS(gparam)))
        {
            qpar->param_type = QOF_TYPE_INT32;
            j++;
        }
        else if ((G_IS_PARAM_SPEC_INT64(gparam)) ||
                 (G_IS_PARAM_SPEC_UINT64(gparam)))
        {
            qpar->param_type = QOF_TYPE_INT64;
            j++;
        }
        else if (G_IS_PARAM_SPEC_BOOLEAN(gparam))
        {
            qpar->param_type = QOF_TYPE_BOOLEAN;
            j++;
        }
        else if (G_IS_PARAM_SPEC_STRING(gparam))
        {
            qpar->param_type = QOF_TYPE_STRING;
            j++;
        }
        else if ((G_IS_PARAM_SPEC_POINTER(gparam)) ||
                 (G_IS_PARAM_SPEC_OBJECT(gparam)))
        {
            /* No-op, silently ignore.  Someday we should handle this ...  */
        }
        else if ((G_IS_PARAM_SPEC_FLOAT(gparam)) ||
                 (G_IS_PARAM_SPEC_DOUBLE(gparam)))
        {
            qpar->param_getfcn = (QofAccessFunc) qof_gobject_double_getter;
            qpar->param_type = QOF_TYPE_DOUBLE;
            j++;
        }
        else if (G_IS_PARAM_SPEC_CHAR(gparam))
        {
            qpar->param_type = QOF_TYPE_CHAR;
            j++;
        }
        else
        {
            PWARN ("Unknown/unhandled parameter type %s on %s:%s\n",
                   G_PARAM_SPEC_TYPE_NAME(gparam), e_type, qpar->param_name);
        }
    }

    /* NULL-terminated list! */
    qof_param_list[j].param_type = NULL;

    qof_class_register (e_type, NULL, qof_param_list);

    /* ------------------------------------------------------ */
    /* Now do the class itself */
    class_def = g_new0 (QofObject, 1);
    classList = g_slist_prepend (classList, class_def);

    class_def->interface_version = QOF_OBJECT_VERSION;
    class_def->e_type = e_type;
    /* We could let the user specify a "nick" here, but
     * the actual class name seems reasonable, e.g. for debugging. */
    class_def->type_label = G_OBJECT_CLASS_NAME (obclass);
    class_def->create = NULL;
    class_def->book_begin = NULL;
    class_def->book_end = NULL;
    class_def->is_dirty = NULL;
    class_def->mark_clean = NULL;
    class_def->foreach = qof_gobject_foreach;
    class_def->printable = NULL;
    class_def->version_cmp = NULL;

    qof_object_register (class_def);
}

/* ======================= END OF FILE ================================ */

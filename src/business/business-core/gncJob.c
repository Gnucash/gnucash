/********************************************************************\
 * gncJob.c -- the Core Job Interface                               *
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

/*
 * Copyright (C) 2001, 2002 Derek Atkins
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>
#include <string.h>

#include "gncInvoice.h"
#include "gncJob.h"
#include "gncJobP.h"
#include "gncOwnerP.h"

struct _gncJob
{
    QofInstance inst;
    char *        id;
    char *        name;
    char *        desc;
    GncOwner      owner;
    gboolean      active;
};

struct _gncJobClass
{
    QofInstanceClass parent_class;
};

static QofLogModule log_module = GNC_MOD_BUSINESS;

#define _GNC_MOD_NAME        GNC_ID_JOB

/* ================================================================== */
/* misc inline functions */

G_INLINE_FUNC void mark_job (GncJob *job);
void mark_job (GncJob *job)
{
    qof_instance_set_dirty(&job->inst);
    qof_event_gen (&job->inst, QOF_EVENT_MODIFY, NULL);
}

/* ================================================================== */

enum
{
    PROP_0,
    PROP_NAME
};

/* GObject Initialization */
G_DEFINE_TYPE(GncJob, gnc_job, QOF_TYPE_INSTANCE);

static void
gnc_job_init(GncJob* job)
{
}

static void
gnc_job_dispose(GObject *jobp)
{
    G_OBJECT_CLASS(gnc_job_parent_class)->dispose(jobp);
}

static void
gnc_job_finalize(GObject* jobp)
{
    G_OBJECT_CLASS(gnc_job_parent_class)->finalize(jobp);
}

static void
gnc_job_get_property (GObject         *object,
                      guint            prop_id,
                      GValue          *value,
                      GParamSpec      *pspec)
{
    GncJob *job;

    g_return_if_fail(GNC_IS_JOB(object));

    job = GNC_JOB(object);
    switch (prop_id)
    {
    case PROP_NAME:
        g_value_set_string(value, job->name);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_job_set_property (GObject         *object,
                      guint            prop_id,
                      const GValue          *value,
                      GParamSpec      *pspec)
{
    GncJob *job;

    g_return_if_fail(GNC_IS_JOB(object));

    job = GNC_JOB(object);
    switch (prop_id)
    {
    case PROP_NAME:
        gncJobSetName(job, g_value_get_string(value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
gnc_job_class_init (GncJobClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

    gobject_class->dispose = gnc_job_dispose;
    gobject_class->finalize = gnc_job_finalize;
    gobject_class->set_property = gnc_job_set_property;
    gobject_class->get_property = gnc_job_get_property;

    g_object_class_install_property
    (gobject_class,
     PROP_NAME,
     g_param_spec_string ("name",
                          "Job Name",
                          "The job name is an arbitrary string "
                          "assigned by the user.  It is intended to "
                          "a short character string that is displayed "
                          "by the GUI as the job mnemonic.",
                          NULL,
                          G_PARAM_READWRITE));
}

/* Create/Destroy Functions */
GncJob *gncJobCreate (QofBook *book)
{
    GncJob *job;

    if (!book) return NULL;

    job = g_object_new (GNC_TYPE_JOB, NULL);
    qof_instance_init_data (&job->inst, _GNC_MOD_NAME, book);

    job->id = CACHE_INSERT ("");
    job->name = CACHE_INSERT ("");
    job->desc = CACHE_INSERT ("");
    job->active = TRUE;

    /* GncOwner not initialized */
    qof_event_gen (&job->inst, QOF_EVENT_CREATE, NULL);

    return job;
}

GncJob *
gncCloneJob (GncJob *from, QofBook *book)
{
    GncJob *job;

    if (!book) return NULL;

    job = g_object_new (GNC_TYPE_JOB, NULL);
    qof_instance_init_data (&job->inst, _GNC_MOD_NAME, book);
    qof_instance_gemini (&job->inst, &from->inst);

    job->id = CACHE_INSERT (from->id);
    job->name = CACHE_INSERT (from->name);
    job->desc = CACHE_INSERT (from->desc);
    job->active = from->active;

    job->owner = gncCloneOwner(&from->owner, book);

    qof_event_gen (&job->inst, QOF_EVENT_CREATE, NULL);

    return job;
}

void gncJobDestroy (GncJob *job)
{
    if (!job) return;
    qof_instance_set_destroying(job, TRUE);
    gncJobCommitEdit (job);
}

static void gncJobFree (GncJob *job)
{
    if (!job) return;

    qof_event_gen (&job->inst, QOF_EVENT_DESTROY, NULL);

    CACHE_REMOVE (job->id);
    CACHE_REMOVE (job->name);
    CACHE_REMOVE (job->desc);

    switch (gncOwnerGetType (&(job->owner)))
    {
    case GNC_OWNER_CUSTOMER:
        gncCustomerRemoveJob (gncOwnerGetCustomer(&job->owner), job);
        break;
    case GNC_OWNER_VENDOR:
        gncVendorRemoveJob (gncOwnerGetVendor(&job->owner), job);
        break;
    default:
        break;
    }

    /* qof_instance_release (&job->inst); */
    g_object_unref (job);
}

GncJob *
gncJobObtainTwin (GncJob *from, QofBook *book)
{
    GncJob *job;
    if (!from) return NULL;

    job = (GncJob *) qof_instance_lookup_twin (QOF_INSTANCE(from), book);
    if (!job)
    {
        job = gncCloneJob (from, book);
    }
    return job;
}

/* ================================================================== */
/* Set Functions */

#define SET_STR(obj, member, str) { \
        char * tmp; \
        \
        if (!safe_strcmp (member, str)) return; \
        gncJobBeginEdit (obj); \
        tmp = CACHE_INSERT (str); \
        CACHE_REMOVE (member); \
        member = tmp; \
        }

void gncJobSetID (GncJob *job, const char *id)
{
    if (!job) return;
    if (!id) return;
    SET_STR(job, job->id, id);
    mark_job (job);
    gncJobCommitEdit (job);
}

void gncJobSetName (GncJob *job, const char *name)
{
    if (!job) return;
    if (!name) return;
    SET_STR(job, job->name, name);
    mark_job (job);
    gncJobCommitEdit (job);
}

void gncJobSetReference (GncJob *job, const char *desc)
{
    if (!job) return;
    if (!desc) return;
    SET_STR(job, job->desc, desc);
    mark_job (job);
    gncJobCommitEdit (job);
}

void gncJobSetOwner (GncJob *job, GncOwner *owner)
{
    if (!job) return;
    if (!owner) return;
    if (gncOwnerEqual (owner, &(job->owner))) return;

    switch (gncOwnerGetType (owner))
    {
    case GNC_OWNER_CUSTOMER:
    case GNC_OWNER_VENDOR:
        break;
    default:
        PERR("Unsupported Owner type: %d", gncOwnerGetType(owner));
        return;
    }

    gncJobBeginEdit (job);

    switch (gncOwnerGetType (&(job->owner)))
    {
    case GNC_OWNER_CUSTOMER:
        gncCustomerRemoveJob (gncOwnerGetCustomer(&job->owner), job);
        break;
    case GNC_OWNER_VENDOR:
        gncVendorRemoveJob (gncOwnerGetVendor(&job->owner), job);
        break;
    default:
        break;
    }

    gncOwnerCopy (owner, &(job->owner));

    switch (gncOwnerGetType (&(job->owner)))
    {
    case GNC_OWNER_CUSTOMER:
        gncCustomerAddJob (gncOwnerGetCustomer(&job->owner), job);
        break;
    case GNC_OWNER_VENDOR:
        gncVendorAddJob (gncOwnerGetVendor(&job->owner), job);
        break;
    default:
        break;
    }

    mark_job (job);
    gncJobCommitEdit (job);
}

void gncJobSetActive (GncJob *job, gboolean active)
{
    if (!job) return;
    if (active == job->active) return;
    gncJobBeginEdit (job);
    job->active = active;
    mark_job (job);
    gncJobCommitEdit (job);
}

static void
qofJobSetOwner (GncJob *job, QofInstance *ent)
{
    if (!job || !ent)
    {
        return;
    }
    qof_begin_edit(&job->inst);
    qofOwnerSetEntity(&job->owner, ent);
    mark_job (job);
    qof_commit_edit(&job->inst);
}

void gncJobBeginEdit (GncJob *job)
{
    qof_begin_edit(&job->inst);
}

static void gncJobOnError (QofInstance *inst, QofBackendError errcode)
{
    PERR("Job QofBackend Failure: %d", errcode);
    gnc_engine_signal_commit_error( errcode );
}

static void job_free (QofInstance *inst)
{
    GncJob *job = (GncJob *)inst;
    gncJobFree (job);
}

static void gncJobOnDone (QofInstance *qof) { }

void gncJobCommitEdit (GncJob *job)
{
    if (!qof_commit_edit (QOF_INSTANCE(job))) return;
    qof_commit_edit_part2 (&job->inst, gncJobOnError,
                           gncJobOnDone, job_free);
}

/* ================================================================== */
/* Get Functions */

const char * gncJobGetID (const GncJob *job)
{
    if (!job) return NULL;
    return job->id;
}

const char * gncJobGetName (const GncJob *job)
{
    if (!job) return NULL;
    return job->name;
}

const char * gncJobGetReference (const GncJob *job)
{
    if (!job) return NULL;
    return job->desc;
}

GncOwner * gncJobGetOwner (GncJob *job)
{
    if (!job) return NULL;
    return &(job->owner);
}

gboolean gncJobGetActive (const GncJob *job)
{
    if (!job) return FALSE;
    return job->active;
}

static QofInstance*
qofJobGetOwner (GncJob *job)
{
    if (!job)
    {
        return NULL;
    }
    return QOF_INSTANCE(qofOwnerGetOwner(&job->owner));
}

/* Other functions */

int gncJobCompare (const GncJob * a, const GncJob *b)
{
    if (!a && !b) return 0;
    if (!a && b) return 1;
    if (a && !b) return -1;

    return (safe_strcmp(a->id, b->id));
}

/* ================================================================== */
/* Package-Private functions */

static const char * _gncJobPrintable (gpointer item)
{
    GncJob *c;
    if (!item) return NULL;
    c = item;
    return c->name;
}

static QofObject gncJobDesc =
{
    .interface_version = QOF_OBJECT_VERSION,
    .e_type            = _GNC_MOD_NAME,
    .type_label        = "Job",
    .create            = (gpointer)gncJobCreate,
    .book_begin        = NULL,
    .book_end          = NULL,
    .is_dirty          = qof_collection_is_dirty,
    .mark_clean        = qof_collection_mark_clean,
    .foreach           = qof_collection_foreach,
    .printable         = _gncJobPrintable,
    .version_cmp       = (int (*)(gpointer, gpointer)) qof_instance_version_cmp,
};

gboolean gncJobRegister (void)
{
    static QofParam params[] =
    {
        { JOB_ID, QOF_TYPE_STRING, (QofAccessFunc)gncJobGetID, (QofSetterFunc)gncJobSetID },
        { JOB_NAME, QOF_TYPE_STRING, (QofAccessFunc)gncJobGetName, (QofSetterFunc)gncJobSetName },
        { JOB_ACTIVE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncJobGetActive, (QofSetterFunc)gncJobSetActive },
        { JOB_REFERENCE, QOF_TYPE_STRING, (QofAccessFunc)gncJobGetReference, (QofSetterFunc)gncJobSetReference },
#ifdef GNUCASH_MAJOR_VERSION
        { JOB_OWNER, GNC_ID_OWNER, (QofAccessFunc)gncJobGetOwner, NULL },
#else
        { JOB_OWNER, QOF_TYPE_CHOICE, (QofAccessFunc)qofJobGetOwner, (QofSetterFunc)qofJobSetOwner },
#endif
        { QOF_PARAM_ACTIVE, QOF_TYPE_BOOLEAN, (QofAccessFunc)gncJobGetActive, NULL },
        { QOF_PARAM_BOOK, QOF_ID_BOOK, (QofAccessFunc)qof_instance_get_book, NULL },
        { QOF_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_instance_get_guid, NULL },
        { NULL },
    };

    if (!qof_choice_create(GNC_ID_JOB))
    {
        return FALSE;
    }
    if (!qof_choice_add_class(GNC_ID_INVOICE, GNC_ID_JOB, INVOICE_OWNER))
    {
        return FALSE;
    }

    qof_class_register (_GNC_MOD_NAME, (QofSortFunc)gncJobCompare, params);
#ifdef GNUCASH_MAJOR_VERSION
    qofJobGetOwner(NULL);
    qofJobSetOwner(NULL, NULL);
#endif
    return qof_object_register (&gncJobDesc);
}

gint64 gncJobNextID (QofBook *book)
{
    return qof_book_get_counter (book, _GNC_MOD_NAME);
}

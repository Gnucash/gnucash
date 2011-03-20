

#include "config.h"

#include <gnome.h>
#include <glib/gi18n.h>

#include "gnc-druid-gnome-ui.h"
#include "gnc-druid-provider-file-gnome.h"
#include "gnc-druid-provider-desc-file.h"

#include "gnc-basic-gobject.h"

#include "gnc-ui.h"
#include "gnc-gui-query.h"

static void gnc_druid_provider_file_gnome_class_init	(GNCDruidProviderFileGnomeClass *class);
static void gnc_druid_provider_file_gnome_finalize		(GObject *obj);

static GNCDruidPage* gnc_dp_file_gnome_first_page(GNCDruidProvider*);
static GNCDruidPage* gnc_dp_file_gnome_last_page(GNCDruidProvider*);
static GNCDruidPage* gnc_dp_file_gnome_next_page(GNCDruidProvider*);
static GNCDruidPage* gnc_dp_file_gnome_prev_page(GNCDruidProvider*);

static GNCDruidProviderClass *parent_class;

GNC_BASIC_GOBJECT_TYPE(GNCDruidProviderFileGnome, GNCDruidProviderFileGnomeClass,
                       G_TYPE_GNC_DRUID_PROVIDER,
                       gnc_druid_provider_file_gnome_class_init, NULL,
                       gnc_druid_provider_file_gnome_get_type)

static void
gnc_druid_provider_file_gnome_class_init (GNCDruidProviderFileGnomeClass *klass)
{
    GObjectClass *object_class;
    GNCDruidProviderClass *gdp_class = (GNCDruidProviderClass*)klass;

    object_class = G_OBJECT_CLASS (klass);
    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_druid_provider_file_gnome_finalize;

    /* override methods */
    gdp_class->first_page = gnc_dp_file_gnome_first_page;
    gdp_class->last_page = gnc_dp_file_gnome_last_page;
    gdp_class->next_page = gnc_dp_file_gnome_next_page;
    gdp_class->prev_page = gnc_dp_file_gnome_prev_page;
}

static void
gnc_dpfg_end_glob(GNCDruidProviderFileGnome *prov)
{
    if (prov->globbed)
    {
#ifdef HAVE_GLOB_H
        globfree(&prov->glob);
#endif
        prov->globbed = FALSE;
        prov->count = 0;
    }
}

static void
gnc_druid_provider_file_gnome_finalize (GObject *obj)
{
    GNCDruidProviderFileGnome *prov_f = (GNCDruidProviderFileGnome *)obj;

    gnc_dpfg_end_glob(prov_f);
    g_object_unref(prov_f->cb);

    G_OBJECT_CLASS (parent_class)->finalize(obj);
}

/* If we've got a file, remove it from the list.. */
static void
gnc_dpfg_del_file(GNCDruidProvider* prov)
{
    GNCDruidProviderFileGnome *prov_f = GNC_DRUID_PROVIDER_FILE_GNOME(prov);
    GNCDruidProviderDescFile *desc_f = GNC_DRUID_PROVIDER_DESC_FILE(prov->desc);

    if (prov_f->cb->this_file)
        desc_f->remove_file(prov->druid->be_ctx, prov_f->cb->this_file);

    prov_f->cb->this_file = NULL;
    gnc_dpfg_end_glob(prov_f);
}

/* Return TRUE if we have a valid file... */
static gboolean
gnc_dpfg_next_file(GNCDruidProvider *prov, const gchar *filename)
{
    GNCDruidProviderFileGnome *prov_f = GNC_DRUID_PROVIDER_FILE_GNOME(prov);
    gboolean res;

    prov_f->cb->filename = filename;
    res = prov->desc->next_cb(&(prov_f->cb->parent));

    if (!res)
        gnc_error_dialog(GTK_WIDGET(prov_f->page),
                         _("Failed to process file: %s"), filename);

    return res;
}

/* Return TRUE if we have a valid file... */
static gboolean
gnc_dpfg_next_glob(GNCDruidProvider* prov)
{
    GNCDruidProviderFileGnome *prov_f = GNC_DRUID_PROVIDER_FILE_GNOME(prov);
    gboolean res = FALSE;

    if (!prov_f->globbed)
        return FALSE;

    while (prov_f->count < prov_f->glob.gl_pathc)
    {
        res = gnc_dpfg_next_file(prov, prov_f->glob.gl_pathv[prov_f->count++]);
        if (res)
            return res;
    }

    /* At this point we've reached the end of the glob */
    gnc_dpfg_end_glob(prov_f);
    return res;
}

#ifdef HAVE_GLOB_H
static int
gnc_dpfg_file_err(const char *path, int err)
{
    int res;

    res = gnc_ok_cancel_dialog(NULL, GTK_RESPONSE_OK,
                               _("Failed to open file: %s: %s"), path, strerror(err));

    return (res == GTK_RESPONSE_OK ? 0 : 1);
}
#endif

/* Return TRUE if we have a valid file... */
static gboolean
gnc_dpfg_start_glob(GNCDruidProvider* prov, const char* filename)
{
    GNCDruidProviderFileGnome *prov_f = GNC_DRUID_PROVIDER_FILE_GNOME(prov);
    int err;

    g_assert(prov_f->globbed == FALSE);
#ifdef HAVE_GLOB_H
    err = glob(filename, GLOB_NOCHECK, gnc_dpfg_file_err, &prov_f->glob);
#else
    /* glob(3) was not available. */
    err = -1;
#endif
    prov_f->count = 0;

    if (!err)
        prov_f->globbed = TRUE;

    return gnc_dpfg_next_glob(prov);
}

static GNCDruidPage*
gnc_dp_file_gnome_first_page(GNCDruidProvider* prov)
{
    GNCDruidProviderDescFile *desc_f = GNC_DRUID_PROVIDER_DESC_FILE(prov->desc);
    gboolean res = FALSE;

    if (desc_f->glob)
        res = gnc_dpfg_next_glob(prov);
    if (res)
        return NULL;

    return prov->pages->data;
}

static GNCDruidPage*
gnc_dp_file_gnome_next_page(GNCDruidProvider* prov)
{
    GNCDruidProviderFileGnome *prov_f = GNC_DRUID_PROVIDER_FILE_GNOME(prov);
    GNCDruidProviderDescFile *desc_f = GNC_DRUID_PROVIDER_DESC_FILE(prov->desc);
    gchar *filename;
    gboolean res;

    filename = gtk_file_chooser_get_filename(prov_f->file_entry);

    if (desc_f->glob)
        res = gnc_dpfg_start_glob(prov, filename);
    else
        res = gnc_dpfg_next_file(prov, filename);

    g_free(filename);

    if (res)
        return NULL;

    return prov->pages->data;
}

static GNCDruidPage*
gnc_dpfg_back_to_multifile(GNCDruidProvider* prov, GNCDruidPage *default_page)
{
    GNCDruidProviderDescFile *desc_f = GNC_DRUID_PROVIDER_DESC_FILE(prov->desc);

    gnc_dpfg_del_file(prov);

    if (desc_f->glob && desc_f->multifile_provider &&
            desc_f->multifile_provider->get_files(prov->druid->be_ctx))
    {
        gnc_druid_jump_to_provider(prov->druid,
                                   desc_f->multifile_provider->parent.provider);
        return NULL;
    }

    return default_page;
}

static GNCDruidPage*
gnc_dp_file_gnome_last_page(GNCDruidProvider* prov)
{
    return gnc_dpfg_back_to_multifile(prov, prov->pages->data);
}

static GNCDruidPage*
gnc_dp_file_gnome_prev_page(GNCDruidProvider* prov)
{
    return gnc_dpfg_back_to_multifile(prov, NULL);
}

static GNCDruidProvider*
gnc_druid_pf_gnome_build(GNCDruid* druid, GNCDruidProviderDesc* desc)
{
    GNCDruidProvider *prov_base;
    GNCDruidProviderFileGnome *prov;
    GNCDruidProviderDescFile *desc_f;
    GNCDruidProviderFileCB *cb;
    GnomeDruidPageStandard *page;
    GtkFileChooser *file_entry;
    GtkWidget *label;

    /* verify that this is the correct provider descriptor */
    g_return_val_if_fail(IS_GNC_DRUID_PROVIDER_DESC_FILE(desc), NULL);
    desc_f = GNC_DRUID_PROVIDER_DESC_FILE(desc);

    g_return_val_if_fail(desc->next_cb, NULL);
    g_return_val_if_fail(desc_f->remove_file, NULL);

    /* Build the provider */
    prov = GNC_DRUID_PROVIDER_FILE_GNOME(g_object_new(G_TYPE_GNC_DRUID_PROVIDER_FILE_GNOME, NULL));
    g_assert(prov);
    prov_base = GNC_DRUID_PROVIDER(prov);

    /* Build the callback object. */
    cb = gnc_druid_provider_file_cb_new();
    g_assert(cb);
    cb->parent.prov_ctx = prov_base;
    cb->parent.druid_ctx = druid;
    prov->cb = cb;

    /* Build the Druid Page */
    page = GNOME_DRUID_PAGE_STANDARD(gnome_druid_page_standard_new());

    /* Remember this page for later */
    g_assert(page);
    prov->page = GNOME_DRUID_PAGE(page);
    prov_base->pages = g_list_prepend(NULL, page);

    /* Build the label */
    label = gtk_label_new(desc_f->text);
    gtk_box_pack_start(GTK_BOX(page->vbox), label, FALSE, FALSE, 0);

    /* Build the file entry */
    file_entry = GTK_FILE_CHOOSER(gtk_file_chooser_widget_new(GTK_FILE_CHOOSER_ACTION_OPEN));
    g_assert(file_entry);
    prov->file_entry = file_entry;
    gtk_file_chooser_set_current_folder(file_entry, desc_f->last_dir);
    gtk_box_pack_start(GTK_BOX(page->vbox), GTK_WIDGET(file_entry), TRUE, TRUE, 0);

    /* Set the page properties */
    if (desc->title)
        gnome_druid_page_standard_set_title(page, desc->title);

    /* Show the page */
    gtk_widget_show_all(GTK_WIDGET(page));

    /* Return the provider instance */
    return prov_base;
}

void
gnc_druid_provider_file_gnome_register(void)
{
    gnc_druid_provider_register(GNC_DRUID_GNOME_UI, GNC_DRUID_PROVIDER_TYPE_FILE,
                                gnc_druid_pf_gnome_build);
}

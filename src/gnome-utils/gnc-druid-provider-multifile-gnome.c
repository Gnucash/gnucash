

#include "config.h"
#include "gnc-druid-gnome-ui.h"
#include "gnc-druid-provider-file-gnome.h"
#include "gnc-druid-provider-multifile-gnome.h"
#include "gnc-druid-provider-desc-multifile.h"

#include "gnc-basic-gobject.h"
#include "dialog-utils.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"

enum file_cols
{
    FILE_COL_FILENAME = 0,
    FILE_COL_POINTER,
    NUM_FILE_COLS
};

static void gnc_druid_provider_multifile_gnome_class_init	(GNCDruidProviderMultifileGnomeClass *class);
static void gnc_druid_provider_multifile_gnome_finalize		(GObject *obj);

static GNCDruidPage* gnc_dp_multifile_gnome_first_page(GNCDruidProvider*);
static GNCDruidPage* gnc_dp_multifile_gnome_last_page(GNCDruidProvider*);
static GNCDruidPage* gnc_dp_multifile_gnome_next_page(GNCDruidProvider*);

static GNCDruidProviderClass *parent_class;

GNC_BASIC_GOBJECT_TYPE(GNCDruidProviderMultifileGnome, GNCDruidProviderMultifileGnomeClass,
                       G_TYPE_GNC_DRUID_PROVIDER,
                       gnc_druid_provider_multifile_gnome_class_init, NULL,
                       gnc_druid_provider_multifile_gnome_get_type)

static void
gnc_druid_provider_multifile_gnome_class_init (GNCDruidProviderMultifileGnomeClass *klass)
{
    GObjectClass *object_class;
    GNCDruidProviderClass *gdp_class = (GNCDruidProviderClass*)klass;

    object_class = G_OBJECT_CLASS (klass);
    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_druid_provider_multifile_gnome_finalize;

    /* override methods */
    gdp_class->first_page = gnc_dp_multifile_gnome_first_page;
    gdp_class->last_page = gnc_dp_multifile_gnome_last_page;
    gdp_class->next_page = gnc_dp_multifile_gnome_next_page;
    gdp_class->prev_page = gnc_dp_multifile_gnome_last_page;
}

static void
gnc_druid_provider_multifile_gnome_finalize (GObject *obj)
{
    //GNCDruidProviderMultifileGnome *prov_mf = (GNCDruidProviderMultifileGnome *)obj;

    G_OBJECT_CLASS (parent_class)->finalize(obj);
}

static void
gnc_dpmfg_select_file_cb(GtkTreeSelection *selection,
                         GNCDruidProviderMultifileGnome *prov_mf)
{
    GtkTreeModel *model;
    GtkTreeIter iter;

    if (gtk_tree_selection_get_selected(selection, &model, &iter))
    {
        gtk_tree_model_get(model, &iter,
                           FILE_COL_POINTER, &prov_mf->selected_file,
                           -1);
    }
    else
    {
        prov_mf->selected_file = NULL;
    }
}

static void
gnc_dpmfg_refresh_list(GNCDruidProviderMultifileGnome *prov_mf)
{
    GNCDruidProviderDescMultifile *desc_mf =
        GNC_DRUID_PROVIDER_DESC_MULTIFILE(prov_mf->parent.desc);
    GtkTreeView *view = GTK_TREE_VIEW(prov_mf->file_view);
    GtkListStore *store;
    GtkTreeIter iter;
    GtkTreePath *path;
    GtkTreeSelection *selection;
    GtkTreeRowReference *reference = NULL;
    GList *list;
    const gchar* filename;
    gpointer be_ctx = prov_mf->parent.druid->be_ctx;

    store = GTK_LIST_STORE(gtk_tree_view_get_model(view));
    gtk_list_store_clear(store);

    for (list = desc_mf->get_files(be_ctx); list; list = list->next)
    {
        filename = desc_mf->get_filename(be_ctx, list->data);

        gtk_list_store_prepend(store, &iter);
        gtk_list_store_set(store, &iter,
                           FILE_COL_FILENAME, filename,
                           FILE_COL_POINTER, list->data,
                           -1);
        if (prov_mf->selected_file == list->data)
        {
            path = gtk_tree_model_get_path(GTK_TREE_MODEL(store), &iter);
            reference = gtk_tree_row_reference_new(GTK_TREE_MODEL(store), path);
            gtk_tree_path_free(path);
        }
    }

    if (reference)
    {
        path = gtk_tree_row_reference_get_path(reference);
        gtk_tree_row_reference_free(reference);
        if (path)
        {
            selection = gtk_tree_view_get_selection(view);
            gtk_tree_selection_select_path(selection, path);
            gtk_tree_view_scroll_to_cell(view, path, NULL, TRUE, 0.5, 0.0);
            gtk_tree_path_free(path);
        }
    }
    else
    {
        prov_mf->selected_file = NULL;
    }
}

static GNCDruidPage*
gnc_dpmfg_jump_to_file(GNCDruidProvider *prov)
{
    GNCDruidProviderDescMultifile *desc_mf =
        GNC_DRUID_PROVIDER_DESC_MULTIFILE(prov->desc);

    gnc_druid_jump_to_provider(prov->druid, desc_mf->file_provider->parent.provider);
    return NULL;
}

static void
gnc_dpmfg_load_another_cb(GtkButton *button, gpointer user_data)
{
    GNCDruidProvider *prov = user_data;

    gnc_dpmfg_jump_to_file(prov);
}

static void
gnc_dpmfg_remove_file_cb(GtkButton *button, gpointer user_data)
{
    GNCDruidProviderMultifileGnome *prov_mf = user_data;
    GNCDruidProviderDescMultifile *desc_mf =
        GNC_DRUID_PROVIDER_DESC_MULTIFILE(prov_mf->parent.desc);
    GNCDruidProviderDescFile *desc_f = desc_mf->file_provider;
    gpointer be_ctx = prov_mf->parent.druid->be_ctx;

    if (!prov_mf->selected_file)
        return;

    desc_f->remove_file(be_ctx, prov_mf->selected_file);

    /* If we have no files, jump to the file provider.  Otherwise refresh the page */
    if (! desc_mf->get_files(be_ctx))
        gnc_dpmfg_jump_to_file(&prov_mf->parent);
    else
        gnc_dpmfg_refresh_list(prov_mf);
}

static GNCDruidPage*
gnc_dp_multifile_gnome_first_page(GNCDruidProvider* prov)
{
    GNCDruidProviderDescMultifile *desc_mf =
        GNC_DRUID_PROVIDER_DESC_MULTIFILE(prov->desc);
    GNCDruidProviderFileGnome *prov_f =
        GNC_DRUID_PROVIDER_FILE_GNOME(desc_mf->file_provider->parent.provider);

    /* If we're globbing and have more data to read, jump back to the file provider */
    if (prov_f->globbed && prov_f->count < prov_f->glob.gl_pathc)
        return gnc_dpmfg_jump_to_file(prov);

    /* Otherwise, clear out this_file, refresh the list and show the page */
    prov_f->cb->this_file = NULL;
    gnc_dpmfg_refresh_list(GNC_DRUID_PROVIDER_MULTIFILE_GNOME(prov));
    return prov->pages->data;
}

static GNCDruidPage*
gnc_dp_multifile_gnome_last_page(GNCDruidProvider* prov)
{
    GNCDruidProviderDescMultifile *desc_mf =
        GNC_DRUID_PROVIDER_DESC_MULTIFILE(prov->desc);

    /* If we have no files, just back up */
    if (! desc_mf->get_files(prov->druid->be_ctx))
        return NULL;

    /* Otherwise, refresh the list and show the page.  XXX: Do we need to refresh? */
    gnc_dpmfg_refresh_list(GNC_DRUID_PROVIDER_MULTIFILE_GNOME(prov));
    return prov->pages->data;
}

static GNCDruidPage*
gnc_dp_multifile_gnome_next_page(GNCDruidProvider* prov)
{
    GNCDruidProviderMultifileGnome *prov_mf =
        GNC_DRUID_PROVIDER_MULTIFILE_GNOME(prov);

    /* Ignore the return -- we don't care what they say */
    prov->desc->next_cb(prov_mf->cb);

    return NULL;
}

static GNCDruidProvider*
gnc_druid_pf_gnome_build(GNCDruid* druid, GNCDruidProviderDesc* desc)
{
    GNCDruidProvider *prov_base;
    GNCDruidProviderMultifileGnome *prov;
    GNCDruidProviderDescMultifile *desc_mf;
    GNCDruidCB *cb;
    GtkWidget *window, *page, *view, *button1, *button2, *label;
    GladeXML *xml;
    GtkTreeViewColumn *column;
    GtkCellRenderer *renderer;
    GtkListStore *store;
    GtkTreeSelection *selection;

    /* verify that this is the correct provider descriptor */
    g_return_val_if_fail(IS_GNC_DRUID_PROVIDER_DESC_MULTIFILE(desc), NULL);
    desc_mf = GNC_DRUID_PROVIDER_DESC_MULTIFILE(desc);

    g_return_val_if_fail(desc->next_cb, NULL);
    g_return_val_if_fail(desc_mf->file_provider, NULL);
    g_return_val_if_fail(desc_mf->get_files, NULL);
    g_return_val_if_fail(desc_mf->get_filename, NULL);

    /* Build the provider */
    prov = GNC_DRUID_PROVIDER_MULTIFILE_GNOME(g_object_new(G_TYPE_GNC_DRUID_PROVIDER_MULTIFILE_GNOME, NULL));
    g_assert(prov);
    prov_base = GNC_DRUID_PROVIDER(prov);

    /* Build the callback object. */
    cb = gnc_druid_cb_new();
    g_assert(cb);
    cb->prov_ctx = prov_base;
    cb->druid_ctx = druid;
    prov->cb = cb;

    /* Build the Druid Page */
    xml = gnc_glade_xml_new("druid-provider-multifile.glade",
                            "Multifile Provider Window");
    g_assert(xml);
    window = glade_xml_get_widget(xml, "Multifile Provider Window");
    page = glade_xml_get_widget(xml, "Multifile Provider Page");
    view = glade_xml_get_widget(xml, "file_view");
    button1 = glade_xml_get_widget(xml, "load_button");
    button2 = glade_xml_get_widget(xml, "unload_button");
    label = glade_xml_get_widget(xml, "instruction_label");

    g_object_ref(page);
    gtk_container_remove(GTK_CONTAINER(window), page);
    gtk_widget_destroy(window);

    /* XXX: Am I going to have to unref this later? */

    /* Remember this page for later */
    g_assert(page);
    prov->page = GNOME_DRUID_PAGE(page);
    prov_base->pages = g_list_prepend(NULL, page);
    prov->file_view = view;

    /* Set up the file view */
    store = gtk_list_store_new (NUM_FILE_COLS, G_TYPE_STRING, G_TYPE_POINTER);
    gtk_tree_view_set_model(GTK_TREE_VIEW(view), GTK_TREE_MODEL(store));
    g_object_unref(store);

    renderer = gtk_cell_renderer_text_new();
    column = gtk_tree_view_column_new_with_attributes("", renderer,
             "text", FILE_COL_FILENAME,
             NULL);
    gtk_tree_view_append_column(GTK_TREE_VIEW(view), column);

    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
    g_signal_connect(selection, "changed",
                     (GCallback)gnc_dpmfg_select_file_cb, prov);

    /* Set the page properties */
    g_signal_connect(G_OBJECT(button1), "clicked",
                     (GCallback)gnc_dpmfg_load_another_cb, prov);
    g_signal_connect(G_OBJECT(button2), "clicked",
                     (GCallback)gnc_dpmfg_remove_file_cb, prov);

    if (desc->title)
        gnome_druid_page_standard_set_title(GNOME_DRUID_PAGE_STANDARD(page),
                                            desc->title);

    if (desc_mf->text)
        gtk_label_set_text(GTK_LABEL(label), desc_mf->text);

    /* Show the page */
    gtk_widget_show_all(GTK_WIDGET(page));

    /* Return the provider instance */
    return prov_base;
}

void
gnc_druid_provider_multifile_gnome_register(void)
{
    gnc_druid_provider_register(GNC_DRUID_GNOME_UI, GNC_DRUID_PROVIDER_TYPE_MULTIFILE,
                                gnc_druid_pf_gnome_build);
}

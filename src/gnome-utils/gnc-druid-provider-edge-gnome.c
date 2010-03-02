

#include "config.h"
#include "gnc-druid-gnome-ui.h"
#include "gnc-druid-provider-edge-gnome.h"
#include "gnc-druid-provider-desc-edge.h"

#include "gnc-basic-gobject.h"

static void gnc_druid_provider_edge_gnome_class_init	(GNCDruidProviderEdgeGnomeClass *class);
static void gnc_druid_provider_edge_gnome_finalize		(GObject *obj);

static GNCDruidPage* gnc_dp_edge_gnome_first_page(GNCDruidProvider*);
static GNCDruidPage* gnc_dp_edge_gnome_last_page(GNCDruidProvider*);

static GNCDruidProviderClass *parent_class;

GNC_BASIC_GOBJECT_TYPE(GNCDruidProviderEdgeGnome, GNCDruidProviderEdgeGnomeClass,
                       G_TYPE_GNC_DRUID_PROVIDER,
                       gnc_druid_provider_edge_gnome_class_init, NULL,
                       gnc_druid_provider_edge_gnome_get_type)

static void
gnc_druid_provider_edge_gnome_class_init (GNCDruidProviderEdgeGnomeClass *klass)
{
    GObjectClass *object_class;
    GNCDruidProviderClass *gdp_class = (GNCDruidProviderClass*)klass;

    object_class = G_OBJECT_CLASS (klass);
    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_druid_provider_edge_gnome_finalize;

    /* override methods */
    gdp_class->first_page = gnc_dp_edge_gnome_first_page;
    gdp_class->last_page = gnc_dp_edge_gnome_last_page;
}

static void
gnc_druid_provider_edge_gnome_finalize (GObject *obj)
{
    //GNCDruidProviderEdgeGnome *provider = (GNCDruidProviderEdgeGnome *)obj;

    /* XXX: we don't need to do anything here.  The page will be destroyed
     * from the druid, and the list is destroyed by the provider superclass.
     */

    G_OBJECT_CLASS (parent_class)->finalize(obj);
}

static GNCDruidPage*
gnc_dp_edge_gnome_first_page(GNCDruidProvider* prov)
{
    return prov->pages->data;
}

static GNCDruidPage*
gnc_dp_edge_gnome_last_page(GNCDruidProvider* prov)
{
    return prov->pages->data;
}

static void
gnc_dp_edge_gnome_finish_cb(GnomeDruidPage* page, GtkWidget* arg1,
                            gpointer user_data)
{
    GNCDruidProvider *prov;

    g_return_if_fail(page);
    g_return_if_fail(user_data);
    g_return_if_fail(IS_GNC_DRUID_PROVIDER_EDGE_GNOME(user_data));

    prov = GNC_DRUID_PROVIDER(user_data);
    if (prov->druid->finish)
        prov->druid->finish(prov->druid->be_ctx);

    g_object_unref(G_OBJECT(prov->druid));
}

static GNCDruidProvider*
gnc_druid_pe_gnome_build(GNCDruid* druid, GNCDruidProviderDesc* desc)
{
    GNCDruidProvider *prov_base;
    GNCDruidProviderEdgeGnome *prov;
    GNCDruidProviderDescEdge *desc_e;
    GnomeDruidPageEdge *page;

    /* verify that this is the correct provider descriptor */
    g_return_val_if_fail(IS_GNC_DRUID_PROVIDER_DESC_EDGE(desc), NULL);
    desc_e = GNC_DRUID_PROVIDER_DESC_EDGE(desc);

    /* Build the provider */
    prov = GNC_DRUID_PROVIDER_EDGE_GNOME(g_object_new(G_TYPE_GNC_DRUID_PROVIDER_EDGE_GNOME, NULL));
    g_assert(prov);
    prov_base = GNC_DRUID_PROVIDER(prov);

    /* Build the Druid Page */
    page = GNOME_DRUID_PAGE_EDGE(gnome_druid_page_edge_new(desc_e->first_or_last ==
                                 GNC_DPE_FIRST ?
                                 GNOME_EDGE_START :
                                 GNOME_EDGE_FINISH));
    /* Remember this page for later */
    g_assert(page);
    prov->page = page;
    prov_base->pages = g_list_prepend(NULL, page);

    /* Set the page properties */
    if (desc->title)
        gnome_druid_page_edge_set_title(page, desc->title);
    if (desc_e->text)
        gnome_druid_page_edge_set_text(page, desc_e->text);

    /* Show the page */
    gtk_widget_show_all(GTK_WIDGET(page));

    if (desc_e->first_or_last == GNC_DPE_LAST)
        g_signal_connect(G_OBJECT(page), "finish",
                         (GCallback)gnc_dp_edge_gnome_finish_cb, prov);

    /* Return the provider instance */
    return prov_base;
}

void
gnc_druid_provider_edge_gnome_register(void)
{
    gnc_druid_provider_register(GNC_DRUID_GNOME_UI, GNC_DRUID_PROVIDER_TYPE_EDGE,
                                gnc_druid_pe_gnome_build);
}

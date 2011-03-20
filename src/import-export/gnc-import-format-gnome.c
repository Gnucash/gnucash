

#include "config.h"

#include <gnome.h>
#include <glib/gi18n.h>

#include "gnc-druid-gnome-ui.h"
#include "gnc-import-format-gnome.h"
#include "gnc-import-desc-format.h"

#include "gnc-basic-gobject.h"
#include "dialog-utils.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"

static void gnc_import_format_gnome_class_init	(GNCImportProvFormatGnomeClass *class);
static void gnc_import_format_gnome_finalize		(GObject *obj);

static GNCDruidPage* gnc_ip_format_gnome_first_page(GNCDruidProvider*);
static GNCDruidPage* gnc_ip_format_gnome_next_page(GNCDruidProvider*);

static GNCDruidProviderClass *parent_class;

GNC_BASIC_GOBJECT_TYPE(GNCImportProvFormatGnome, GNCImportProvFormatGnomeClass,
                       G_TYPE_GNC_DRUID_PROVIDER,
                       gnc_import_format_gnome_class_init, NULL,
                       gnc_import_format_gnome_get_type)

enum ifg_cols
{
    IFG_COL_TEXT = 0,
    IFG_COL_VALUE,
    NUM_IFG_COLS
};

static void
gnc_import_format_gnome_class_init (GNCImportProvFormatGnomeClass *klass)
{
    GObjectClass *object_class;
    GNCDruidProviderClass *gdp_class = (GNCDruidProviderClass*)klass;

    object_class = G_OBJECT_CLASS (klass);
    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_import_format_gnome_finalize;

    /* override methods */
    gdp_class->first_page = gnc_ip_format_gnome_first_page;
    gdp_class->last_page = gnc_ip_format_gnome_first_page;
    gdp_class->next_page = gnc_ip_format_gnome_next_page;
    // gdp_class->prev_page = gnc_ip_format_gnome_prev_page;
}

static void
gnc_import_format_gnome_finalize (GObject *obj)
{
    //GNCImportProvFormatGnome *prov_f = (GNCImportProvFormatGnome *)obj;

    G_OBJECT_CLASS(parent_class)->finalize(obj);
}

static void
gnc_ifg_option_changed (GtkComboBox *combo, GNCImportProvFormatGnome *prov_f)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    GncImportFormat value;

    g_return_if_fail(GTK_IS_COMBO_BOX(combo));

    model = gtk_combo_box_get_model(combo);
    if (!gtk_combo_box_get_active_iter(combo, &iter))
        return;

    gtk_tree_model_get(model, &iter, IFG_COL_VALUE, &value, -1);
    prov_f->choice = value;
}

#define ADD_MENU_ITEM(str,op) { \
  gtk_list_store_append(store, &iter); \
  gtk_list_store_set(store, &iter, \
		     IFG_COL_TEXT, str, \
		     IFG_COL_VALUE, op, \
		     -1); \
}

static void
make_menu (GNCImportProvFormatGnome *prov_f, GncImportFormat formats)
{
    GtkComboBox *combo = prov_f->format_combo;
    GtkListStore *store;
    GtkTreeIter iter;
    GtkCellRenderer *renderer;

    store = gtk_list_store_new(NUM_IFG_COLS, G_TYPE_STRING, G_TYPE_INT);
    gtk_combo_box_set_model(combo, GTK_TREE_MODEL(store));
    g_object_unref(store);

    renderer = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combo), renderer, TRUE);
    gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(combo), renderer,
                                  "text", IFG_COL_TEXT);

    /* Numeric formats */
    ADD_MENU_ITEM(_("Period-as-decimal (1,000.00)"), GNCIF_NUM_PERIOD);
    ADD_MENU_ITEM(_("Comma-as-decimal (1.000,00)"), GNCIF_NUM_COMMA);

    /* Date formats */
    ADD_MENU_ITEM(_("m-d-y"), GNCIF_DATE_MDY);
    ADD_MENU_ITEM(_("d-m-y"), GNCIF_DATE_DMY);
    ADD_MENU_ITEM(_("y-m-d"), GNCIF_DATE_YMD);
    ADD_MENU_ITEM(_("y-d-m"), GNCIF_DATE_YDM);

    g_signal_connect(G_OBJECT(prov_f->format_combo), "changed",
                     G_CALLBACK(gnc_ifg_option_changed), prov_f);
    gtk_combo_box_set_active(prov_f->format_combo, 0);
}

static GNCDruidPage*
gnc_ip_format_gnome_first_page(GNCDruidProvider* prov)
{
    GNCImportProvFormatGnome *prov_f = GNC_IMPORT_FORMAT_GNOME(prov);
    GNCImportDescFormat *desc_f = GNC_IMPORT_DESC_FORMAT(prov->desc);
    GncImportFormat formats;
    const gchar* sample;

    /* See if we have anything to do. */
    formats = desc_f->get_formats(prov_f->cb);

    /* If nothing to do, return NULL */
    if (formats == GNCIF_NONE)
        return NULL;

    /* Otherwise set up the menu and sample, then let the user at it */
    sample = desc_f->get_sample(prov_f->cb);

    if (sample)
        gtk_label_set_text(prov_f->sample_label, sample);

    make_menu(prov_f, formats);

    return prov->pages->data;
}

static GNCDruidPage*
gnc_ip_format_gnome_next_page(GNCDruidProvider* prov)
{
    GNCImportProvFormatGnome *prov_f = GNC_IMPORT_FORMAT_GNOME(prov);

    /* Grab the entry from the user and supply it to the backend */
    prov_f->cb->format = prov_f->choice;

    /* Figure out whether they want to move on or not */
    if (prov->desc->next_cb(&(prov_f->cb->parent)))
        return NULL;

    return prov->pages->data;
}

static GNCDruidProvider*
gnc_import_pf_gnome_build(GNCDruid* druid, GNCDruidProviderDesc* desc)
{
    GNCDruidProvider *prov_base;
    GNCImportProvFormatGnome *prov;
    GNCImportDescFormat *desc_f;
    GNCImportFormatCB *cb;
    GtkWidget *window, *page, *label, *format_combo, *sample_label;
    GladeXML *xml;

    /* verify that this is the correct provider descriptor */
    g_return_val_if_fail(IS_GNC_IMPORT_DESC_FORMAT(desc), NULL);
    desc_f = GNC_IMPORT_DESC_FORMAT(desc);

    g_return_val_if_fail(desc->next_cb, NULL);
    g_return_val_if_fail(desc_f->get_formats, NULL);
    g_return_val_if_fail(desc_f->get_sample, NULL);

    /* Build the provider */
    prov = GNC_IMPORT_FORMAT_GNOME(g_object_new(G_TYPE_GNC_IMPORT_FORMAT_GNOME, NULL));
    g_assert(prov);
    prov_base = GNC_DRUID_PROVIDER(prov);

    /* Build the callback object. */
    cb = gnc_import_format_cb_new();
    g_assert(cb);
    cb->parent.prov_ctx = prov_base;
    cb->parent.druid_ctx = druid;
    prov->cb = cb;

    /* Build the Druid Page */
    xml = gnc_glade_xml_new("import-provider-format.glade",
                            "Format Provider Window");
    g_assert(xml);
    window = glade_xml_get_widget(xml, "Format Provider Window");
    page = glade_xml_get_widget(xml, "Format Provider Page");
    label = glade_xml_get_widget(xml, "inst_label");
    format_combo = glade_xml_get_widget(xml, "import_format_combo");
    sample_label = glade_xml_get_widget(xml, "format_sample_label");

    prov->format_combo = GTK_COMBO_BOX(format_combo);
    prov->sample_label = GTK_LABEL(sample_label);

    g_object_ref(page);
    gtk_container_remove(GTK_CONTAINER(window), page);
    gtk_widget_destroy(window);

    /* XXX: Am I going to have to unref this later? */

    /* Remember this page for later */
    g_assert(page);
    prov->page = GNOME_DRUID_PAGE(page);
    prov_base->pages = g_list_prepend(NULL, page);

    if (desc->title)
        gnome_druid_page_standard_set_title(GNOME_DRUID_PAGE_STANDARD(page),
                                            desc->title);

    if (desc_f->text)
        gtk_label_set_text(GTK_LABEL(label), desc_f->text);

    /* Show the page */
    gtk_widget_show_all(GTK_WIDGET(page));

    /* Return the provider instance */
    return prov_base;
}

void
gnc_import_format_gnome_register(void)
{
    gnc_druid_provider_register(GNC_DRUID_GNOME_UI, GNC_IMPORT_DESC_TYPE_FORMAT,
                                gnc_import_pf_gnome_build);
}

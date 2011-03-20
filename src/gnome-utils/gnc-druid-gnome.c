

#include "config.h"
#include "gnc-druid-gnome.h"

static void gnc_druid_gnome_class_init	(GNCDruidGnomeClass *class);
static void gnc_druid_gnome_finalize		(GObject *obj);

static void gnc_druid_gnome_set_page(GNCDruid* druid, GNCDruidPage* page);
static void gnc_druid_gnome_append_provider(GNCDruid*, GNCDruidProvider*);

static gboolean gnc_druid_gnome_next_cb(GnomeDruidPage* page, GtkWidget *arg1,
                                        gpointer user_data);
static gboolean gnc_druid_gnome_prev_cb(GnomeDruidPage* page, GtkWidget *arg1,
                                        gpointer user_data);
static gboolean gnc_druid_gnome_cancel2_cb(GnomeDruidPage* page, GtkWidget *arg1,
        gpointer user_data);


static GNCDruidClass *parent_class;

GType
gnc_druid_gnome_get_type (void)
{
    static GType type = 0;

    if (type == 0)
    {
        GTypeInfo type_info =
        {
            sizeof (GNCDruidGnomeClass),
            NULL,
            NULL,
            (GClassInitFunc)gnc_druid_gnome_class_init,
            NULL,
            NULL,
            sizeof (GNCDruidGnome),
            0,
            NULL,
        };

        type = g_type_register_static (G_TYPE_GNC_DRUID, "GNCDruidGnome", &type_info, 0);
    }

    return type;
}

static void
gnc_druid_gnome_class_init (GNCDruidGnomeClass *klass)
{
    GObjectClass *object_class;
    GNCDruidClass *gdc_class = (GNCDruidClass*)klass;

    object_class = G_OBJECT_CLASS (klass);
    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_druid_gnome_finalize;

    /* override methods */
    gdc_class->set_page = gnc_druid_gnome_set_page;
    gdc_class->append_provider = gnc_druid_gnome_append_provider;
}

static void
gnc_druid_gnome_finalize (GObject *obj)
{
    GNCDruidGnome *druid = (GNCDruidGnome *)obj;

    gtk_widget_destroy(druid->window);

    G_OBJECT_CLASS (parent_class)->finalize(obj);
}

static void
gnc_druid_gnome_set_page(GNCDruid* druid_p, GNCDruidPage* page)
{
    GNCDruidGnome *druid;

    g_return_if_fail(druid_p);
    g_return_if_fail(IS_GNC_DRUID_GNOME(druid_p));

    druid = GNC_DRUID_GNOME(druid_p);
    gnome_druid_set_page(druid->druid, GNOME_DRUID_PAGE(page));
}

static void
gnc_druid_gnome_append_provider(GNCDruid* druid_p, GNCDruidProvider* provider)
{
    GNCDruidGnome *druid;
    GList *pages, *node;
    GnomeDruidPage *page;

    g_return_if_fail(druid_p);
    g_return_if_fail(IS_GNC_DRUID_GNOME(druid_p));
    g_return_if_fail(provider);
    g_return_if_fail(IS_GNC_DRUID_PROVIDER(provider));

    druid = GNC_DRUID_GNOME(druid_p);
    pages = gnc_druid_provider_get_pages(provider);
    for (node = pages; node; node = node->next)
    {
        page = GNOME_DRUID_PAGE(node->data);
        gnome_druid_append_page(druid->druid, page);
        g_signal_connect(G_OBJECT(page), "next",
                         (GCallback)gnc_druid_gnome_next_cb, druid);
        g_signal_connect(G_OBJECT(page), "back",
                         (GCallback)gnc_druid_gnome_prev_cb, druid);
        g_signal_connect(G_OBJECT(page), "cancel",
                         (GCallback)gnc_druid_gnome_cancel2_cb, druid);
    }
}

static gboolean
gnc_druid_gnome_next_cb(GnomeDruidPage* page, GtkWidget *arg1, gpointer user_data)
{
    GNCDruid *druid;

    g_return_val_if_fail(IS_GNC_DRUID_GNOME(user_data), FALSE);
    druid = GNC_DRUID(user_data);

    /* Move to the next druid page */
    gnc_druid_next_page(druid);

    return TRUE;
}

static gboolean
gnc_druid_gnome_prev_cb(GnomeDruidPage* page, GtkWidget *arg1, gpointer user_data)
{
    GNCDruid *druid;

    g_return_val_if_fail(IS_GNC_DRUID_GNOME(user_data), FALSE);
    druid = GNC_DRUID(user_data);

    /* move to the previous druid page */
    gnc_druid_prev_page(druid);

    return TRUE;
}

static gboolean
gnc_druid_gnome_cancel2_cb(GnomeDruidPage* page, GtkWidget *arg1,
                           gpointer user_data)
{
    GObject *obj = (GObject*)user_data;

    g_object_unref(obj);
    return TRUE;
}

static void
gnc_druid_gnome_cancel_cb(GnomeDruid* druid, gpointer user_data)
{
    GObject *obj = (GObject*)user_data;

    g_object_unref(obj);
}

static GNCDruid*
gnc_druid_gnome_build(const char* title)
{
    GNCDruidGnome *druid;
    GtkWidget *widget;
    GtkWidget *window = NULL;

    /* Build myself */
    druid = GNC_DRUID_GNOME(g_object_new(G_TYPE_GNC_DRUID_GNOME, NULL));
    druid->parent.ui_type = GNC_DRUID_GNOME_UI;

    /* Build the gnome druid */
    widget = gnome_druid_new_with_window(title, NULL, TRUE, &window);
    g_return_val_if_fail(widget, NULL);
    druid->druid = GNOME_DRUID(widget);
    druid->window = window;

    g_signal_connect(G_OBJECT(widget), "cancel",
                     (GCallback)gnc_druid_gnome_cancel_cb, druid);
    gtk_widget_show_all(widget);

    return GNC_DRUID(druid);
}

void
gnc_druid_gnome_register(void)
{
    gnc_druid_register_ui(GNC_DRUID_GNOME_UI, gnc_druid_gnome_build);
}

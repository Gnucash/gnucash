

#include "config.h"
#include "gnc-druid-gnome-ui.h"
#include "gnc-druid-provider-multifile-gnome.h"
#include "gnc-druid-provider-desc-multifile.h"
#include "gnc-druid-provider-file-gnome.h"

#include "gnc-basic-gobject.h"
#include "dialog-utils.h"
#include "messages.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"

static void gnc_druid_provider_multifile_gnome_class_init	(GNCDruidProviderMultifileGnomeClass *class);
static void gnc_druid_provider_multifile_gnome_finalize		(GObject *obj);

static GNCDruidPage* gnc_dp_multifile_gnome_first_page(GNCDruidProvider*);
static GNCDruidPage* gnc_dp_multifile_gnome_last_page(GNCDruidProvider*);
static GNCDruidPage* gnc_dp_multifile_gnome_next_page(GNCDruidProvider*);

static GNCDruidProviderClass *parent_class;

GNC_BASIC_GOBJECT_TYPE(GNCDruidProviderMultifileGnome,GNCDruidProviderMultifileGnomeClass,
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
gnc_dpmfg_select_file_cb(GtkCList *clist, int row, int column, GdkEvent *event,
			 gpointer user_data)
{
  GNCDruidProviderMultifileGnome *prov_mf = user_data;
  gpointer file = gtk_clist_get_row_data(clist, row);

  prov_mf->selected_file = file;
}

static void
gnc_dpmfg_refresh_list(GNCDruidProviderMultifileGnome *prov_mf)
{
  GNCDruidProviderDescMultifile *desc_mf =
    GNC_DRUID_PROVIDER_DESC_MULTIFILE(prov_mf->parent.desc);
  GtkCList *clist = GTK_CLIST(prov_mf->list);
  GList *list;
  const gchar* filename;
  gint row = 0;
  gint sel_row = -1;
  gpointer be_ctx = prov_mf->parent.druid->be_ctx;

  gtk_clist_freeze(clist);
  gtk_clist_clear(clist);

  for (list = desc_mf->get_files(be_ctx); list; list = list->next) {
    filename = desc_mf->get_filename(be_ctx, list->data);

    row = gtk_clist_append(clist, (char**)&filename);
    gtk_clist_set_row_data(clist, row, list->data);

    if (prov_mf->selected_file == list->data)
      sel_row = row;
  }

  gtk_clist_thaw(clist);

  if(sel_row >= 0)
    gtk_clist_select_row(clist, sel_row, 0);
  else
    prov_mf->selected_file = NULL;
  
  /* hopefully we don't need to queue the actual window */
  gtk_widget_queue_resize(GTK_WIDGET(prov_mf->page));
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
  GtkWidget *window, *page, *list, *button1, *button2, *label;
  GladeXML *xml;

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
  list = glade_xml_get_widget(xml, "file_list");
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
  prov->list = list;

  /* Set the page properties */
  g_signal_connect(G_OBJECT(list), "select-row",
		   (GCallback)gnc_dpmfg_select_file_cb, prov);
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

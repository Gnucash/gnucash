

#include "config.h"
#include "gnc-druid.h"

static void gnc_druid_class_init	(GNCDruidClass *class);
static void gnc_druid_finalize		(GObject *obj);
static void invalid_setpage(GNCDruid* druid, GNCDruidPage* page);

static GObjectClass *parent_class;
static GNCDruidNew new_druid_fcn = NULL;

GType
gnc_druid_get_type (void)
{
  static GType type = 0;

  if (type == 0) {
    GTypeInfo type_info = {
      sizeof (GNCDruidClass),
      NULL,
      NULL,
      (GClassInitFunc)gnc_druid_class_init,
      NULL,
      NULL,
      sizeof (GNCDruid),
      0,
      NULL,
    };
		
    type = g_type_register_static (G_TYPE_OBJECT, "GNCDruid", &type_info, 0);
  }
  
  return type;
}

static void
gnc_druid_class_init (GNCDruidClass *klass)
{
  GObjectClass *object_class;
	
  object_class = G_OBJECT_CLASS (klass);
  parent_class = g_type_class_peek_parent (klass);

  object_class->finalize = gnc_druid_finalize;

  /* override methods */
  klass->set_page = invalid_setpage;
}

static void
gnc_druid_finalize (GObject *obj)
{
  GNCDruid *druid = (GNCDruid *)obj;
  GList *node;

  /* Cancel the backend context */
  if (druid->cancel)
    (druid->cancel)(druid->be_ctx);

  /* Destroy list of providers */
  for (node = druid->providers; node; node = node->next)
    g_object_unref(G_OBJECT(node->data));
  g_list_free(druid->providers);

  G_OBJECT_CLASS (parent_class)->finalize(obj);
}

static void invalid_setpage(GNCDruid* druid, GNCDruidPage* page)
{
  g_warning("druid with invalid set-page");
  return;
}

void
gnc_druid_register_ui(const gchar* ui_type, GNCDruidNew new_druid)
{
  g_return_if_fail(ui_type);
  g_return_if_fail(new_druid);
  g_return_if_fail(!new_druid_fcn);

  new_druid_fcn = new_druid;
}

/* methods */

void
gnc_druid_set_page(GNCDruid* druid, GNCDruidPage* page)
{
  g_return_if_fail(druid);
  g_return_if_fail(IS_GNC_DRUID(druid));

  ((GNC_DRUID_GET_CLASS(druid))->set_page)(druid, page);
}

GNCDruidProvider*
gnc_druid_next_provider(GNCDruid* druid)
{
  GList *node;

  g_return_val_if_fail(druid, NULL);
  g_return_val_if_fail(IS_GNC_DRUID(druid), NULL);

  if (!druid->provider) {
    node = druid->providers;
  } else {
    node = g_list_find(druid->providers, druid->provider);
    node = node->next;
  }

  druid->provider = (node ? node->data : NULL);
  return druid->provider;
}

GNCDruidProvider*
gnc_druid_prev_provider(GNCDruid* druid)
{
  GList *node;

  g_return_val_if_fail(druid, NULL);
  g_return_val_if_fail(IS_GNC_DRUID(druid), NULL);

  if (!druid->provider) {
    node = g_list_last(druid->providers);
  } else {
    node = g_list_find(druid->providers, druid->provider);
    node = node->prev;
  }

  druid->provider = (node ? node->data : NULL);
  return druid->provider;
}

void
gnc_druid_next_page(GNCDruid* druid)
{
  GNCDruidProvider *prov;
  GNCDruidPage* page = NULL;

  g_return_if_fail(druid);
  g_return_if_fail(IS_GNC_DRUID(druid));

  for (prov = druid->provider; !page; ) {

    /* How we behave depends on whether we have a provider... 
     * if we do not have a provider, then get the next one and
     * try to get the first page.  If we DO have a provider, then
     * ask for the next page.  If neither gave us a page then we
     * should go to the next provider.  Once we get a page or if
     * we hit the next of the provider list, we're done.
     */

    if (!prov) {

      /* Nope, no provider */
      prov = gnc_druid_next_provider(druid);
      if (!prov)
	break;

      /* New provider -- get the first page */
      page = gnc_druid_provider_first_page(prov);

    } else {

      /* Yep, try to get the next page */
      page = gnc_druid_provider_next_page(prov);
    }

    /* If we didn't get a page, then we should go to the next provider */
    if (!page)
      prov = NULL;
  }

  if (page)
    gnc_druid_set_page(druid, page);

  g_return_if_fail(page);
}

void
gnc_druid_prev_page(GNCDruid* druid)
{
  GNCDruidProvider *prov;
  GNCDruidPage* page = NULL;

  g_return_if_fail(druid);
  g_return_if_fail(IS_GNC_DRUID(druid));

  for (prov = druid->provider; !page; ) {

    /* See gnc_druid_next_page for documentation */

    if (!prov) {

      /* Nope, no provider */
      prov = gnc_druid_prev_provider(druid);
      if (!prov)
	break;

      /* New provider -- get the first page */
      page = gnc_druid_provider_last_page(prov);

    } else {

      /* Yep, try to get the next page */
      page = gnc_druid_provider_prev_page(prov);
    }

    /* If we didn't get a page, then we should go to the next provider */
    if (!page)
      prov = NULL;
  }

  if (page)
    gnc_druid_set_page(druid, page);

  g_return_if_fail(page);
}

/* Other functions */

/**
 * gnc_druid_new -- create a druid based on the list of providers.  Hold
 *                  onto the backend context and the function to call if
 *                  the druid is cancelled.
 *
 * This will assume the "registered ui", or internally perform some
 * magic to figure out which "UI" to use..
 *
 * The provider list (and all the providerdesc objects) are owned by
 * the druid and will be freed by the druid.
 */
GNCDruid* gnc_druid_new(const gchar* title, GList *providers, gpointer backend_ctx,
			gboolean (*finish)(gpointer be_ctx),
			void (*cancel)(gpointer be_ctx))
{
  GNCDruid *druid;
  GList *prov_list = NULL;
  GList *node;
  GNCDruidProvider *prov;
  GNCDruidPage *page;
  GNCDruidClass *gdc;

  g_return_val_if_fail(title, NULL);
  g_return_val_if_fail(providers, NULL);
  g_return_val_if_fail(new_druid_fcn, NULL);

  /* Build the druid */
  druid = new_druid_fcn(title);
  g_return_val_if_fail(druid, NULL);
  g_return_val_if_fail(druid->ui_type, NULL);

  /* Fill in local data */
  druid->be_ctx = backend_ctx;
  druid->finish = finish;
  druid->cancel = cancel;

  gdc = GNC_DRUID_GET_CLASS(druid);
  g_return_val_if_fail(gdc->append_provider, NULL);

  /* Now build the set of providers */
  for (node = providers; node; node = node->next) {
    GNCDruidProviderDesc *prov_desc = node->data;
    
    /* create the provider */
    g_assert(prov_desc);
    prov = gnc_druid_provider_new(druid, prov_desc);
    g_assert(prov);

    /* add the provider to the druid */
    gdc->append_provider(druid, prov);
    prov_list = g_list_prepend(prov_list, prov);
  }
  druid->providers = g_list_reverse(prov_list);

  /* Free the list (the provider descriptions are in the providers) */
  g_list_free(providers);

  /* Set the first page of the druid */
  prov = gnc_druid_next_provider(druid);
  page = gnc_druid_provider_first_page(prov);
  gnc_druid_set_page(druid, page);

  /* And return the new druid. */
  return druid;
}

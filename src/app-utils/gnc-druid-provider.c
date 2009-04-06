/********************************************************************\
 * gnc-druid-provider.c                                             *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/


#include "config.h"
#include "gnc-druid-provider.h"
#include "gnc-basic-gobject.h"

static void gnc_druid_provider_class_init	(GNCDruidProviderClass *class);
static void gnc_druid_provider_finalize		(GObject *obj);
static GNCDruidPage* null_page(GNCDruidProvider* provider);
static GNCDruidPage* invalid_page(GNCDruidProvider* provider);

static GObjectClass *parent_class;
static GHashTable *typeTable = NULL;

GNC_BASIC_GOBJECT_TYPE(GNCDruidProvider, GNCDruidProviderClass,
		       G_TYPE_OBJECT, gnc_druid_provider_class_init, NULL,
		       gnc_druid_provider_get_type)

static void
gnc_druid_provider_class_init (GNCDruidProviderClass *klass)
{
  GObjectClass *object_class;
	
  object_class = G_OBJECT_CLASS (klass);
  parent_class = g_type_class_peek_parent (klass);

  object_class->finalize = gnc_druid_provider_finalize;

  /* override methods */
  klass->first_page = invalid_page;
  klass->last_page = invalid_page;
  klass->next_page = null_page;
  klass->prev_page = null_page;
}

static void
gnc_druid_provider_finalize (GObject *obj)
{
  GNCDruidProvider *provider = (GNCDruidProvider *)obj;

  /* Destroy the page list */
  g_list_free(provider->pages);

  /* Destroy the provider descriptor */
  g_object_unref(G_OBJECT(provider->desc));

  G_OBJECT_CLASS (parent_class)->finalize(obj);
}

static GNCDruidPage*
invalid_page(GNCDruidProvider* provider)
{
  g_warning("Provider without a valid first/last page method");
  return NULL;
}

static GNCDruidPage*
null_page(GNCDruidProvider* provider)
{
  return NULL;
}

static GHashTable*
find_or_make_table(GHashTable* table, const gchar *str)
{
  GHashTable *ret;

  g_return_val_if_fail(table, NULL);
  g_return_val_if_fail(str, NULL);

  ret = g_hash_table_lookup(table, str);
  if (!ret) {
    ret = g_hash_table_new(g_str_hash, g_str_equal);
    g_hash_table_insert(table, (gpointer)str, ret);
  }
  return ret;
}

void
gnc_druid_provider_register(const gchar* ui_type, const gchar* name,
			    GNCDruidProviderNew new_provider)
{
  GHashTable *table;

  g_return_if_fail(ui_type);
  g_return_if_fail(name);
  g_return_if_fail(new_provider);

  if (!typeTable)
    typeTable = g_hash_table_new(g_str_hash, g_str_equal);

  table = find_or_make_table(typeTable, ui_type);
  g_return_if_fail(table);

  g_hash_table_insert(table, (gpointer)name, new_provider);  
}

GNCDruidProvider* gnc_druid_provider_new(GNCDruid* druid_ctx,
					 GNCDruidProviderDesc* desc)
{
  GHashTable *table;
  GNCDruidProviderNew new_provider;
  GNCDruidProvider *provider;

  g_return_val_if_fail(desc, NULL);
  g_return_val_if_fail(IS_GNC_DRUID_PROVIDER_DESC(desc), NULL);
  g_return_val_if_fail(druid_ctx, NULL);
  g_return_val_if_fail(IS_GNC_DRUID(druid_ctx), NULL);
  g_return_val_if_fail(typeTable, NULL);
  g_return_val_if_fail(desc->name, NULL);
  g_return_val_if_fail(!desc->provider, NULL);

  /* Lookup the UI Type provider table */
  table = g_hash_table_lookup(typeTable, druid_ctx->ui_type);
  g_return_val_if_fail(table, NULL);

  /* Now look up the actual provider creator for this provider type */
  new_provider = g_hash_table_lookup(table, desc->name);
  g_return_val_if_fail(new_provider, NULL);

  /* Create the new provider, then set the local parameters */
  provider = new_provider(druid_ctx, desc);
  if (provider) {
    provider->druid = druid_ctx;
    provider->desc = desc;
    desc->provider = provider;
  }

  return provider;
}

/* methods */

GNCDruidPage*
gnc_druid_provider_first_page(GNCDruidProvider* provider)
{
  g_return_val_if_fail(provider, NULL);
  g_return_val_if_fail(IS_GNC_DRUID_PROVIDER(provider), NULL);

  return ((GNC_DRUID_PROVIDER_GET_CLASS(provider))->first_page)(provider);
}

GNCDruidPage*
gnc_druid_provider_last_page(GNCDruidProvider* provider)
{
  g_return_val_if_fail(provider, NULL);
  g_return_val_if_fail(IS_GNC_DRUID_PROVIDER(provider), NULL);

  return ((GNC_DRUID_PROVIDER_GET_CLASS(provider))->last_page)(provider);
}

GNCDruidPage*
gnc_druid_provider_next_page(GNCDruidProvider* provider)
{
  g_return_val_if_fail(provider, NULL);
  g_return_val_if_fail(IS_GNC_DRUID_PROVIDER(provider), NULL);

  return ((GNC_DRUID_PROVIDER_GET_CLASS(provider))->next_page)(provider);
}

GNCDruidPage*
gnc_druid_provider_prev_page(GNCDruidProvider* provider)
{
  g_return_val_if_fail(provider, NULL);
  g_return_val_if_fail(IS_GNC_DRUID_PROVIDER(provider), NULL);

  return ((GNC_DRUID_PROVIDER_GET_CLASS(provider))->prev_page)(provider);
}

GList*
gnc_druid_provider_get_pages(GNCDruidProvider* provider)
{
  g_return_val_if_fail(provider, NULL);
  g_return_val_if_fail(IS_GNC_DRUID_PROVIDER(provider), NULL);

  return provider->pages;
}

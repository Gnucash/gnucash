#include <gtk/gtkseparatormenuitem.h>
#include "eggseparatortoolitem.h"

#ifndef _
#  define _(s) (s)
#endif

static void egg_separator_tool_item_class_init (EggSeparatorToolItemClass*class);

static void egg_separator_tool_item_add (GtkContainer *container,
					 GtkWidget    *child);

static GObjectClass *parent_class = NULL;


GType
egg_separator_tool_item_get_type (void)
{
  static GType type = 0;

  if (!type)
    {
      static const GTypeInfo type_info =
	{
	  sizeof (EggSeparatorToolItemClass),
	  (GBaseInitFunc) 0,
	  (GBaseFinalizeFunc) 0,
	  (GClassInitFunc) egg_separator_tool_item_class_init,
	  (GClassFinalizeFunc) 0,
	  NULL,
	  sizeof (EggSeparatorToolItem),
	  0, /* n_preallocs */
	  (GInstanceInitFunc) NULL,
	};

      type = g_type_register_static (EGG_TYPE_TOOL_ITEM,
				     "EggSeparatorToolItem", &type_info, 0);
    }
  return type;
}


static void
egg_separator_tool_item_class_init (EggSeparatorToolItemClass *class)
{
  GtkContainerClass *container_class;
  EggToolItemClass *toolitem_class;

  parent_class = g_type_class_peek_parent (class);
  container_class = (GtkContainerClass *)class;
  toolitem_class = (EggToolItemClass *)class;

  container_class->add = egg_separator_tool_item_add;
}

static void
egg_separator_tool_item_add (GtkContainer *container,
			     GtkWidget    *child)
{
  g_warning("attempt to add a child to an EggSeparatorToolItem");
}

EggToolItem *
egg_separator_tool_item_new (void)
{
  EggToolItem *self;

  self = g_object_new (EGG_TYPE_SEPARATOR_TOOL_ITEM,
		       NULL);
  
  return self;
}

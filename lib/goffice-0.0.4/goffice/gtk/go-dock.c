/* File import from bonoboui to libgoffice by import-bonobo.  Do not edit.  */

/* go-dock.c

   Copyright (C) 1998 Free Software Foundation

   The Gnome Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
   Boston, MA  02110-1301 USA.

   Author: Ettore Perazzoli <ettore@comm2000.it>
*/
/*
  @NOTATION@
*/

#include <goffice/goffice-config.h>
#include <glib/gi18n.h>
#include <string.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include "go-dock.h"
#include "go-dock-band.h"
#include "go-dock-item.h"
#include <libgnome/gnome-macros.h>

GNOME_CLASS_BOILERPLATE (GoDock, go_dock,
			 GtkContainer, GTK_TYPE_CONTAINER);

#define noBONOBO_DOCK_DEBUG

/* FIXME: To be removed.  */
#if defined GO_DOCK_DEBUG && defined __GNUC__
#define DEBUG(x)                                        \
  do                                                    \
    {                                                   \
      printf ("%s.%d: ", __FUNCTION__, __LINE__);       \
      printf x;                                         \
      putchar ('\n');                                   \
    }                                                   \
  while (0)
#else
#define DEBUG(x)
#endif



struct _GoDockPrivate
{
	GdkDragContext *current_drag;
};

enum {
  LAYOUT_CHANGED,
  LAST_SIGNAL
};



static void     go_dock_size_request        (GtkWidget *widget,
                                                GtkRequisition *requisition);
static void     go_dock_size_allocate       (GtkWidget *widget,
                                                GtkAllocation *allocation);
static void     go_dock_map                 (GtkWidget *widget);
static void     go_dock_unmap               (GtkWidget *widget);
static void     go_dock_add                 (GtkContainer *container,
                                                GtkWidget *child);
static void     go_dock_remove              (GtkContainer *container,
                                                GtkWidget *widget);
static void     go_dock_forall              (GtkContainer *container,
                                                gboolean include_internals,
                                                GtkCallback callback,
                                                gpointer callback_data);
static void     go_dock_finalize            (GObject *object);

static void     size_request_v                 (GList *list,
                                                GtkRequisition *requisition);
static void     size_request_h                 (GList *list,
                                                GtkRequisition *requisition);
static gint     size_allocate_v                (GList *list,
                                                gint start_x, gint start_y,
                                                guint width, gint direction);
static gint     size_allocate_h                (GList *list,
                                                gint start_x, gint start_y,
                                                guint width, gint direction);
static void     map_widget                     (GtkWidget *w);
static void     map_widget_foreach             (gpointer data,
                                                gpointer user_data);
static void     map_band_list                  (GList *list);
static void     unmap_widget                     (GtkWidget *w);
static void     unmap_widget_foreach             (gpointer data,
                                                gpointer user_data);
static void     unmap_band_list                  (GList *list);
static gboolean remove_from_band_list          (GList **list,
                                                GoDockBand *child);
static void     forall_helper                  (GList *list,
                                                GtkCallback callback,
                                                gpointer callback_data);

static void     drag_begin                     (GtkWidget *widget,
                                                gpointer data);
static void     drag_end_bands                 (GList **list,
                                                GoDockItem *item);
static void     drag_end                       (GtkWidget *widget,
                                                gpointer data);
static gboolean drag_new                       (GoDock *dock,
                                                GoDockItem *item,
                                                GList **area,
                                                GList *where,
                                                gint x, gint y,
                                                gboolean is_vertical);
static gboolean drag_to                        (GoDock *dock,
                                                GoDockItem *item,
                                                GList *where,
                                                gint x, gint y,
                                                gboolean is_vertical);
static gboolean drag_floating                  (GoDock *dock,
                                                GoDockItem *item,
                                                gint x, gint y,
                                                gint rel_x, gint rel_y);
static gboolean drag_check                     (GoDock *dock,
                                                GoDockItem *item,
                                                GList **area,
                                                gint x, gint y,
                                                gboolean is_vertical);
static void     drag_snap                      (GoDock *dock,
                                                GtkWidget *widget,
                                                gint x, gint y);
static void     drag_motion                    (GtkWidget *widget,
                                                gint x, gint y,
                                                gpointer data);

static GoDockItem *get_docked_item_by_name  (GoDock *dock,
                                                const gchar *name,
                                                GoDockPlacement *placement_return,
                                                guint *num_band_return,
                                                guint *band_position_return,
                                                guint *offset_return);
static GoDockItem *get_floating_item_by_name (GoDock *dock,
                                                 const gchar *name);

static void           connect_drag_signals      (GoDock *dock,
                                                 GtkWidget *item);


static guint dock_signals[LAST_SIGNAL] = { 0 };



static void
go_dock_class_init (GoDockClass *class)
{
  GtkObjectClass *object_class;
  GObjectClass *gobject_class;
  GtkWidgetClass *widget_class;
  GtkContainerClass *container_class;

  object_class = (GtkObjectClass *) class;
  gobject_class = (GObjectClass *) class;
  widget_class = (GtkWidgetClass *) class;
  container_class = (GtkContainerClass *) class;

  gobject_class->finalize = go_dock_finalize;

  widget_class->size_request = go_dock_size_request;
  widget_class->size_allocate = go_dock_size_allocate;
  widget_class->map = go_dock_map;
  widget_class->unmap = go_dock_unmap;

  container_class->add = go_dock_add;
  container_class->remove = go_dock_remove;
  container_class->forall = go_dock_forall;

  dock_signals[LAYOUT_CHANGED] =
	  g_signal_new ("layout_changed",
			G_TYPE_FROM_CLASS (object_class),
			G_SIGNAL_RUN_LAST,
			G_STRUCT_OFFSET (GoDockClass,
					 layout_changed),
			NULL, NULL,
			g_cclosure_marshal_VOID__VOID,
			G_TYPE_NONE,
			0);
}

static void
go_dock_instance_init (GoDock *dock)
{
  GTK_WIDGET_SET_FLAGS (GTK_WIDGET (dock), GTK_NO_WINDOW);

  dock->_priv = NULL;
  /* XXX: when there is some private stuff enable this
  dock->_priv = g_new0(GoDockPrivate, 1);
  */

  dock->client_area = NULL;

  dock->top_bands = NULL;
  dock->bottom_bands = NULL;
  dock->right_bands = NULL;
  dock->left_bands = NULL;

  dock->floating_children = NULL;

  dock->floating_items_allowed = TRUE;
}



static void
size_request_v (GList *list, GtkRequisition *requisition)
{
  for (; list != NULL; list = list->next)
    {
      GtkWidget *w;
      GtkRequisition req;

      w = GTK_WIDGET (list->data);
      gtk_widget_size_request (w, &req);
      requisition->width += req.width;
      requisition->height = MAX (requisition->height, req.height);
    }
}

static void
size_request_h (GList *list, GtkRequisition *requisition)
{
  for (list = list; list != NULL; list = list->next)
    {
      GtkWidget *w;
      GtkRequisition req;

      w = GTK_WIDGET (list->data);
      gtk_widget_size_request (w, &req);
      requisition->height += req.height;
      requisition->width = MAX (requisition->width, req.width);
    }
}

static void
go_dock_size_request (GtkWidget *widget, GtkRequisition *requisition)
{
  GoDock *dock;
  GList *lp;

  dock = GO_DOCK (widget);

  if (dock->client_area != NULL && GTK_WIDGET_VISIBLE (dock->client_area))
    gtk_widget_size_request (dock->client_area, requisition);
  else
    {
      requisition->width = 0;
      requisition->height = 0;
    }

  size_request_v (dock->left_bands, requisition);
  size_request_v (dock->right_bands, requisition);
  size_request_h (dock->top_bands, requisition);
  size_request_h (dock->bottom_bands, requisition);

  lp = dock->floating_children;
  while (lp != NULL)
    {
      GtkWidget *w;
      GtkRequisition float_item_requisition;

      w = lp->data;
      lp = lp->next;
      gtk_widget_size_request (w, &float_item_requisition);
    }
}



static gint
size_allocate_h (GList *list, gint start_x, gint start_y, guint width,
                 gint direction)
{
  GtkAllocation allocation;

  allocation.x = start_x;
  allocation.y = start_y;
  allocation.width = width;

  if (direction < 0)
    list = g_list_last (list);
  while (list != NULL)
    {
      GtkWidget *w;

      w = GTK_WIDGET (list->data);
      allocation.height = w->requisition.height;

      if (direction > 0)
        {
          gtk_widget_size_allocate (w, &allocation);
          allocation.y += allocation.height;
          list = list->next;
        }
      else
        {
          allocation.y -= allocation.height;
          gtk_widget_size_allocate (w, &allocation);
          list = list->prev;
        }
    }

  return allocation.y;
}

static gint
size_allocate_v (GList *list, gint start_x, gint start_y, guint height,
                 gint direction)
{
  GtkAllocation allocation;

  allocation.x = start_x;
  allocation.y = start_y;
  allocation.height = height;

  if (direction < 0)
    list = g_list_last (list);

  while (list != NULL)
    {
      GtkWidget *w;

      w = GTK_WIDGET (list->data);
      allocation.width = w->requisition.width;

      if (direction > 0)
        {
          gtk_widget_size_allocate (w, &allocation);
          allocation.x += allocation.width;
          list = list->next;
        }
      else
        {
          allocation.x -= allocation.width;
          gtk_widget_size_allocate (w, &allocation);
          list = list->prev;
        }
    }

  return allocation.x;
}

static void
go_dock_size_allocate (GtkWidget *widget, GtkAllocation *allocation)
{
  GoDock *dock;
  gint top_bands_y, bottom_bands_y;
  gint left_bands_x, right_bands_x;
  GtkAllocation child_allocation;
  GList *lp;

  dock = GO_DOCK (widget);

  widget->allocation = *allocation;

  top_bands_y = size_allocate_h (dock->top_bands,
                                 allocation->x,
                                 allocation->y,
                                 allocation->width,
                                 +1);

  bottom_bands_y = size_allocate_h (dock->bottom_bands,
                                    allocation->x,
                                    allocation->y + allocation->height,
                                    allocation->width,
                                    -1);

  child_allocation.height = MAX (bottom_bands_y - top_bands_y, 1);

  left_bands_x = size_allocate_v (dock->left_bands,
                                  allocation->x,
                                  top_bands_y,
                                  child_allocation.height,
                                  +1);

  right_bands_x = size_allocate_v (dock->right_bands,
                                   allocation->x + allocation->width,
                                   top_bands_y,
                                   child_allocation.height,
                                   -1);

  child_allocation.width = MAX (right_bands_x - left_bands_x, 1);

  child_allocation.x = left_bands_x;
  child_allocation.y = top_bands_y;

  dock->client_rect = child_allocation;

  if (dock->client_area != NULL && GTK_WIDGET_VISIBLE (dock->client_area))
    gtk_widget_size_allocate (dock->client_area, &child_allocation);

  lp = dock->floating_children;
  while (lp != NULL)
    {
      GtkWidget *w;
      GtkAllocation float_item_allocation;

      w = lp->data;
      lp = lp->next;
      float_item_allocation.x = 0;
      float_item_allocation.y = 0;
      float_item_allocation.width = w->requisition.width;
      float_item_allocation.height = w->requisition.height;
      gtk_widget_size_allocate (w, &float_item_allocation);
    }
}



static void
map_widget (GtkWidget *w)
{
  if (w != NULL && GTK_WIDGET_VISIBLE (w) && ! GTK_WIDGET_MAPPED (w))
    gtk_widget_map (w);
}

static void
unmap_widget (GtkWidget *w)
{
  if (w != NULL && GTK_WIDGET_VISIBLE (w) && GTK_WIDGET_MAPPED (w))
    gtk_widget_unmap (w);
}

static void
map_widget_foreach (gpointer data,
                    gpointer user_data)
{
  map_widget (GTK_WIDGET (data));
}

static void
unmap_widget_foreach (gpointer data,
                      gpointer user_data)
{
  unmap_widget (GTK_WIDGET (data));
}

static void
map_band_list (GList *list)
{
  while (list != NULL)
    {
      GtkWidget *w;

      w = GTK_WIDGET (list->data);
      map_widget (w);

      list = list->next;
    }
}

static void
unmap_band_list (GList *list)
{
  while (list != NULL)
    {
      GtkWidget *w;

      w = GTK_WIDGET (list->data);
      unmap_widget (w);

      list = list->next;
    }
}

static void
go_dock_map (GtkWidget *widget)
{
  GoDock *dock;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GO_IS_DOCK(widget));

  GNOME_CALL_PARENT (GTK_WIDGET_CLASS, map, (widget));

  dock = GO_DOCK (widget);

  map_widget (dock->client_area);

  map_band_list (dock->top_bands);
  map_band_list (dock->bottom_bands);
  map_band_list (dock->left_bands);
  map_band_list (dock->right_bands);

  g_list_foreach (dock->floating_children, map_widget_foreach, NULL);
}

static void
go_dock_unmap (GtkWidget *widget)
{
  GoDock *dock;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GO_IS_DOCK(widget));

  dock = GO_DOCK (widget);

  unmap_widget (dock->client_area);

  unmap_band_list (dock->top_bands);
  unmap_band_list (dock->bottom_bands);
  unmap_band_list (dock->left_bands);
  unmap_band_list (dock->right_bands);

  g_list_foreach (dock->floating_children, unmap_widget_foreach, NULL);

  GNOME_CALL_PARENT (GTK_WIDGET_CLASS, unmap, (widget));
}



/* GtkContainer methods.  */

static void
go_dock_add (GtkContainer *container, GtkWidget *child)
{
  GoDock *dock;

  dock = GO_DOCK (container);
  go_dock_add_item (dock, GO_DOCK_ITEM(child), GO_DOCK_TOP, 0, 0, 0, TRUE);
}

static gboolean
remove_from_band_list (GList **list, GoDockBand *child)
{
  GList *lp;

  for (lp = *list; lp != NULL; lp = lp->next)
    {
      if (lp->data == child)
        {
          gtk_widget_unparent (GTK_WIDGET (child));
          *list = g_list_remove_link (*list, lp);
          g_list_free (lp);

          return TRUE;
        }
    }

  return FALSE;
}

static void
go_dock_remove (GtkContainer *container, GtkWidget *widget)
{
  GoDock *dock;

  dock = GO_DOCK (container);

  if (dock->client_area == widget)
    {
      gtk_widget_unparent (widget);
      dock->client_area = NULL;
      gtk_widget_queue_resize (GTK_WIDGET (dock));
    }
  else
    {
      /* Check if it's a floating child.  */
      {
        GList *lp;

        lp = dock->floating_children;
        while (lp != NULL)
          {
            GtkWidget *w;

            w = lp->data;
            if (w == widget)
              {
                gtk_widget_unparent (w);
                dock->floating_children
                  = g_list_remove_link (dock->floating_children, lp);
                g_list_free (lp);
                return;
              }

            lp = lp->next;
          }
      }

      /* Then it must be one of the bands.  */
      {
        GoDockBand *band;

        g_return_if_fail (GO_IS_DOCK_BAND (widget));

        band = GO_DOCK_BAND (widget);
        if (remove_from_band_list (&dock->top_bands, band)
            || remove_from_band_list (&dock->bottom_bands, band)
            || remove_from_band_list (&dock->left_bands, band)
            || remove_from_band_list (&dock->right_bands, band))
          {
            gtk_widget_queue_resize (GTK_WIDGET (dock));
            return;
          }
      }
    }
}

static void
forall_helper (GList *list,
               GtkCallback callback,
               gpointer callback_data)
{
  while (list != NULL)
    {
      GtkWidget *w;

      w = GTK_WIDGET(list->data);
      list = list->next;
      (* callback) (w, callback_data);
    }
}

static void
go_dock_forall (GtkContainer *container,
                   gboolean include_internals,
                   GtkCallback callback,
                   gpointer callback_data)
{
  GoDock *dock;
  GList *lp;

  g_return_if_fail (container != NULL);
  g_return_if_fail (GO_IS_DOCK (container));
  g_return_if_fail (callback != NULL);

  dock = GO_DOCK (container);

  forall_helper (dock->top_bands, callback, callback_data);
  forall_helper (dock->bottom_bands, callback, callback_data);
  forall_helper (dock->left_bands, callback, callback_data);
  forall_helper (dock->right_bands, callback, callback_data);

  lp = dock->floating_children;
  while (lp != NULL)
    {
      GtkWidget *w;

      w = lp->data;
      lp = lp->next;
      (* callback) (w, callback_data);
    }

  if (dock->client_area != NULL)
    (* callback) (dock->client_area, callback_data);
}

static void
go_dock_finalize (GObject *object)
{
  GoDock *self = GO_DOCK (object);

  g_free (self->_priv);
  self->_priv = NULL;

  if (G_OBJECT_CLASS (parent_class)->finalize)
    (* G_OBJECT_CLASS (parent_class)->finalize) (object);
}



static void
new_band_setup (GoDock    *dock,
		GtkWidget     *new_band,
		GtkOrientation orientation)
{
      go_dock_band_set_orientation (
	      GO_DOCK_BAND (new_band), orientation);
      gtk_widget_set_parent (GTK_WIDGET (new_band), GTK_WIDGET (dock));
      gtk_widget_queue_resize (GTK_WIDGET (new_band));
      gtk_widget_show (GTK_WIDGET (new_band));
}


/* When an item is being dragged, there can be 3 situations:

   (I)   A new band is created and the item is docked to it.

   (II)  The item is docked to an existing band.

   (III) The item must be floating, so it has to be detached if
         currently not floating, and moved around in its own window.  */

/* Case (I): Dock `item' into a new band next to `where' in the
   docking area `area'.  If `where' is NULL, the band becomes the
   first one in `area'.  */
static gboolean
drag_new (GoDock *dock,
          GoDockItem *item,
          GList **area,
          GList *where,
          gint x, gint y,
          gboolean is_vertical)
{
  GoDockBand *new_band;
  GList *next;

  DEBUG (("entering function"));

  new_band = NULL;

  /* We need a new band next to `where', but we try to re-use the band
     next to it if either it contains no children, or it only contains
     `item'.  */

  next = NULL;
  if (where == NULL && area != NULL)
    next = *area;
  else
    next = where->next;
  if (next != NULL)
    {
      GoDockBand *band;
      guint num_children;

      band = GO_DOCK_BAND (next->data);

      num_children = go_dock_band_get_num_children (band);

      if (num_children == 0
          || (num_children == 1
              && GTK_WIDGET (band) == GTK_WIDGET (item)->parent))
        new_band = GO_DOCK_BAND (next->data);
    }

  /* Create the new band and make it our child if we cannot re-use an
     existing one.  */
  if (new_band == NULL)
    {
      new_band = GO_DOCK_BAND (go_dock_band_new ());

      /* This is mostly to remember that `drag_allocation' for this
         child is bogus, as it was not previously allocated.  */
      new_band->new_for_drag = TRUE;

      if (where == NULL)
        *area = where = g_list_prepend (*area, new_band);
      else if (where->next == NULL)
        g_list_append (where, new_band);
      else
        g_list_prepend (where->next, new_band);

      new_band_setup (dock, GTK_WIDGET (new_band),
		      is_vertical ? GTK_ORIENTATION_VERTICAL
		                  : GTK_ORIENTATION_HORIZONTAL);
    }

  /* Move the item to the new band.  (This is a no-op if we are using
     `where->next' and it already contains `item'.)  */
  go_dock_item_attach (item, GTK_WIDGET (new_band), x, y);

  /* Prepare the band for dragging of `item'.  */
  go_dock_band_drag_begin (new_band, item);

  /* Set the offset of `item' in the band.  */
  if (is_vertical)
    go_dock_band_set_child_offset (new_band, GTK_WIDGET (item),
                                      MAX (y - dock->client_rect.y, 0));
  else
    go_dock_band_set_child_offset (new_band, GTK_WIDGET (item),
                                      MAX (x - GTK_WIDGET (dock)->allocation.x, 0));

  return TRUE;
}

/* Case (II): Drag into an existing band.  */
static gboolean
drag_to (GoDock *dock,
         GoDockItem *item,
         GList *where,
         gint x, gint y,
         gboolean is_vertical)
{
  DEBUG (("x %d y %d", x, y));

  return go_dock_band_drag_to (GO_DOCK_BAND (where->data), item, x, y);
}

/* Case (III): Move a floating (i.e. floating) item.  */
static gboolean
drag_floating (GoDock *dock,
               GoDockItem *item,
               gint x,
               gint y,
               gint rel_x,
               gint rel_y)
{
  GtkWidget *item_widget, *dock_widget;

  item_widget = GTK_WIDGET (item);
  dock_widget = GTK_WIDGET (dock);

  if (!item->is_floating && item_widget->parent != dock_widget)
    {
      GtkAllocation *dock_allocation, *client_allocation;

      /* The item is currently not floating (so it is not our child).
         Make it so if we are outside the docking areas.  */

      dock_allocation = &dock_widget->allocation;
      if (dock->client_area)
        client_allocation = &dock->client_area->allocation;
      else
        client_allocation = NULL;

      if (rel_x < 0
          || rel_x >= dock_allocation->width
          || rel_y < 0
          || rel_y >= dock_allocation->height
          || (client_allocation != NULL
              && rel_x >= client_allocation->x
              && rel_x < client_allocation->x + client_allocation->width
              && rel_y >= client_allocation->y
              && rel_y < client_allocation->y + client_allocation->height))
        {
          gtk_widget_ref (item_widget);

          gtk_container_remove (GTK_CONTAINER (item_widget->parent),
                                item_widget);
          gtk_widget_set_parent (item_widget, dock_widget);

          dock->floating_children = g_list_prepend (dock->floating_children,
                                                    item);

	  gtk_widget_realize (item_widget);
	  gtk_widget_map (item_widget);
	  gtk_widget_queue_resize (item_widget);

	  go_dock_item_detach (item, x, y);
          if (item->in_drag)
            go_dock_item_grab_pointer (item);

          gtk_widget_unref (item_widget);
        }
    }
  else
    {
      /* The item is already floating; all we have to do is move it to
         the current dragging position.  */
      go_dock_item_drag_floating (item, x, y);
    }

  return TRUE;
}



/* Check if `item' can be docked to any of the DockBands of the dock
   area `area'.  If so, dock it and return TRUE; otherwise, return
   FALSE.  */
static gboolean
drag_check (GoDock *dock,
            GoDockItem *item,
            GList **area,
            gint x, gint y,
            gboolean is_vertical)
{
  GList *lp;
  GtkAllocation *alloc;

  for (lp = *area; lp != NULL; lp = lp->next)
    {
      GoDockBand *band;

      band = GO_DOCK_BAND (lp->data);

      if (! band->new_for_drag)
        {
          alloc = &band->drag_allocation;

          if (x >= alloc->x - 10 && x < alloc->x + alloc->width
              && y >= alloc->y && y < alloc->y + alloc->height)
            {
              if (is_vertical)
                {
                  if (x < alloc->x + alloc->width / 2
                      && drag_to (dock, item, lp, x, y, TRUE))
                    return TRUE;
                  else
                    return drag_new (dock, item, area, lp, x, y, TRUE);
                }
              else
                {
                  if (y < alloc->y + alloc->height / 2
                      && drag_to (dock, item, lp, x, y, FALSE))
                    return TRUE;
                  else
                    return drag_new (dock, item, area, lp, x, y, FALSE);
                }
            }
        }
    }

  return FALSE;
}

/* Snap the GoDockItem `widget' to `dock' at the specified
   position.  */
static void
drag_snap (GoDock *dock,
           GtkWidget *widget,
           gint x, gint y)
{
#define SNAP 50
  GoDockItem *item;
  GoDockItemBehavior item_behavior;
  gint win_x, win_y;
  gint rel_x, rel_y;
  gboolean item_allows_horizontal, item_allows_vertical;

  item = GO_DOCK_ITEM (widget);

  item_behavior = go_dock_item_get_behavior (item);
  item_allows_horizontal = ! (item_behavior
                              & GO_DOCK_ITEM_BEH_NEVER_HORIZONTAL);
  item_allows_vertical = ! (item_behavior
                            & GO_DOCK_ITEM_BEH_NEVER_VERTICAL);

  gdk_window_get_origin (GTK_WIDGET (dock)->window, &win_x, &win_y);
  rel_x = x - win_x;
  rel_y = y - win_y;

  DEBUG (("(%d,%d)", x, y));
  DEBUG (("relative (%d,%d)", rel_x, rel_y));

  if (item_allows_horizontal
      && rel_x >= 0 && rel_x < GTK_WIDGET (dock)->allocation.width)
    {
      /* Check prepending to top/bottom bands.  */
      if (rel_y < 0 && rel_y >= -SNAP
          && drag_new (dock, item, &dock->top_bands, NULL,
                       rel_x, rel_y, FALSE))
        return;
      else if (rel_y >= dock->client_rect.y + dock->client_rect.height - SNAP
               && rel_y < dock->client_rect.y + dock->client_rect.height
               && drag_new (dock, item, &dock->bottom_bands, NULL,
                            rel_x, rel_y, FALSE))
        return;
    }

  if (item_allows_vertical
      && rel_y >= dock->client_rect.y
      && rel_y < dock->client_rect.y + dock->client_rect.height)
    {
      /* Check prepending to left/right bands.  */
      if (rel_x < 0 && rel_x >= -SNAP
          && drag_new (dock, item, &dock->left_bands, NULL,
                       rel_x, rel_y, TRUE))
        return;
      else if (rel_x >= dock->client_rect.x + dock->client_rect.width - SNAP
               && rel_x < dock->client_rect.x + dock->client_rect.width
               && drag_new (dock, item, &dock->right_bands, NULL,
                            rel_x, rel_y, TRUE))
        return;
    }

  /* Check dragging into bands.  */
  if (item_allows_horizontal
      && drag_check (dock, item, &dock->top_bands, rel_x, rel_y, FALSE))
    return;
  else if (item_allows_horizontal
           && drag_check (dock, item, &dock->bottom_bands, rel_x, rel_y, FALSE))
    return;
  else if (item_allows_vertical
           && drag_check (dock, item, &dock->left_bands, rel_x, rel_y, TRUE))
    return;
  else if (item_allows_vertical
           && drag_check (dock, item, &dock->right_bands, rel_x, rel_y, TRUE))
    return;

  /* We are not in any "interesting" area: the item must be floating
     if allowed to.  */
  if (dock->floating_items_allowed
      && ! (item_behavior & GO_DOCK_ITEM_BEH_NEVER_DETACH))
    drag_floating (dock, item, x, y, rel_x, rel_y);

  /* If still not floating, fall back to moving the item in its own
     band.  */
  if (! item->is_floating)
    go_dock_band_drag_to (GO_DOCK_BAND (GTK_WIDGET (item)->parent),
                             item, rel_x, rel_y);
}



/* "drag_begin" signal handling.  */
static void
drag_begin (GtkWidget *widget, gpointer data)
{
  GoDock *dock;
  GoDockItem *item;

  DEBUG (("entering function"));

  dock = GO_DOCK (data);
  item = GO_DOCK_ITEM (widget);

  /* Communicate all the bands that `widget' is currently being
     dragged.  */
  g_list_foreach (dock->top_bands, (GFunc) go_dock_band_drag_begin, item);
  g_list_foreach (dock->bottom_bands, (GFunc) go_dock_band_drag_begin, item);
  g_list_foreach (dock->right_bands, (GFunc) go_dock_band_drag_begin, item);
  g_list_foreach (dock->left_bands, (GFunc) go_dock_band_drag_begin, item);
}



/* "drag_end" signal handling.  */

static void
drag_end_bands (GList **list, GoDockItem *item)
{
  GList *lp;
  GoDockBand *band;

  lp = *list;
  while (lp != NULL)
    {
      band = GO_DOCK_BAND(lp->data);
      go_dock_band_drag_end (band, item);

      if (go_dock_band_get_num_children (band) == 0)
        {
          GList *next;

          next = lp->next;

          /* This will remove this link, too.  */
          gtk_widget_destroy (GTK_WIDGET (band));

          lp = next;
        }
      else
        lp = lp->next;
    }
}

static void
drag_end (GtkWidget *widget, gpointer data)
{
  GoDockItem *item;
  GoDock *dock;

  DEBUG (("entering function"));

  item = GO_DOCK_ITEM (widget);
  dock = GO_DOCK (data);

  /* Communicate to all the bands that `item' is no longer being
     dragged.  */
  drag_end_bands (&dock->top_bands, item);
  drag_end_bands (&dock->bottom_bands, item);
  drag_end_bands (&dock->left_bands, item);
  drag_end_bands (&dock->right_bands, item);

  g_signal_emit (data, dock_signals[LAYOUT_CHANGED], 0);
}



/* "drag_motion" signal handling.  */

/* Handle a drag motion on the GoDockItem `widget'.  This is
   connected to the "drag_motion" of all the children being added to
   the GoDock, and tries to dock the dragged item at the current
   (`x', `y') position of the pointer.  */
static void
drag_motion (GtkWidget *widget,
             gint x, gint y,
             gpointer data)
{
  drag_snap (GO_DOCK (data), widget, x, y);
}



static GoDockItem *
get_docked_item_by_name (GoDock *dock,
                         const gchar *name,
                         GoDockPlacement *placement_return,
                         guint *num_band_return,
                         guint *band_position_return,
                         guint *offset_return)
{
  {
    struct
    {
      GList *band_list;
      GoDockPlacement placement;
    }
    areas[] =
    {
      { NULL, GO_DOCK_TOP },
      { NULL, GO_DOCK_BOTTOM },
      { NULL, GO_DOCK_LEFT },
      { NULL, GO_DOCK_RIGHT },
      { NULL, GO_DOCK_FLOATING },
    };
    GList *lp;
    guint i;

    areas[0].band_list = dock->top_bands;
    areas[1].band_list = dock->bottom_bands;
    areas[2].band_list = dock->left_bands;
    areas[3].band_list = dock->right_bands;

    for (i = 0; i < 4; i++)
      {
        guint num_band;

        for (lp = areas[i].band_list, num_band = 0;
             lp != NULL;
             lp = lp->next, num_band++)
          {
            GoDockBand *band;
            GoDockItem *item;

            band = GO_DOCK_BAND(lp->data);
            item = go_dock_band_get_item_by_name (band,
                                                     name,
                                                     band_position_return,
                                                     offset_return);
            if (item != NULL)
              {
                if (num_band_return != NULL)
                  *num_band_return = num_band;
                if (placement_return != NULL)
                  *placement_return = areas[i].placement;

                return item;
              }
          }
      }
  }

  return NULL;
}

static GoDockItem *
get_floating_item_by_name (GoDock *dock,
                           const gchar *name)
{
  GList *lp;
  GoDockItem *item;

  for (lp = dock->floating_children; lp != NULL; lp = lp->next)
    {
      item = lp->data;
      if (strcmp (item->name, name) == 0)
        return item;
    }

  return NULL;
}

static void
connect_drag_signals (GoDock *dock,
                      GtkWidget *item)
{
  if (GO_IS_DOCK_ITEM (item))
    {
      DEBUG (("here"));
      g_signal_connect (item, "dock_drag_begin",
			G_CALLBACK (drag_begin), dock);
      g_signal_connect (item, "dock_drag_motion",
			G_CALLBACK (drag_motion), dock);
      g_signal_connect (item, "dock_drag_end",
			G_CALLBACK (drag_end), dock);
    }
}

/**
 * go_dock_new:
 *
 * Description: Creates a new #GoDock widget.
 *
 * Return value: The new widget.
 **/
GtkWidget *
go_dock_new (void)
{
  GoDock *dock;
  GtkWidget *widget;

  dock = g_object_new (go_dock_get_type (), NULL);
  widget = GTK_WIDGET (dock);

#if 0                           /* FIXME: should I? */
  if (GTK_WIDGET_VISIBLE (widget))
    gtk_widget_queue_resize (widget);
#endif

  return widget;
}

/**
 * go_dock_allow_floating_items:
 * @dock: A pointer to a #GoDock widget
 * @enable: Specifies whether floating items are allowed in this dock
 *
 * Description: Enable or disable floating items on @dock, according
 * to @enable.
 **/
void
go_dock_allow_floating_items (GoDock *dock,
                                 gboolean enable)
{
  dock->floating_items_allowed = enable;
}

static GList **
get_band_list (GoDock *dock, GoDockPlacement placement)
{
  GList **band_ptr = NULL;

  switch (placement)
    {
    case GO_DOCK_TOP:
      band_ptr = &dock->top_bands;
      break;
    case GO_DOCK_BOTTOM:
      band_ptr = &dock->bottom_bands;
      break;
    case GO_DOCK_LEFT:
      band_ptr = &dock->left_bands;
      break;
    case GO_DOCK_RIGHT:
      band_ptr = &dock->right_bands;
      break;
    default:
      break;
    }
  return band_ptr;
}

/**
 * go_dock_add_item:
 * @dock: A pointer to a #GoDock widget
 * @item: The item to add
 * @placement: Placement for the new item
 * @band_num: Number of the band the new item must be added to
 * @position: Position of the item in the specified band
 * @offset: Offset (in pixels) from the previous item in the same band
 * @in_new_band: Specifies whether a new band must be created for this item
 *
 * Description: Add @item to @dock.  @placement can be either
 * %GO_DOCK_TOP, %GO_DOCK_RIGHT, %GO_DOCK_BOTTOM or
 * %GO_DOCK_LEFT, and specifies what area of the dock should
 * contain the item.  If @in_new_band is %TRUE, a new dock band is
 * created at the position specified by @band_num; otherwise, the item
 * is added to the @band_num'th band.
 **/
void
go_dock_add_item (GoDock *dock,
		      GoDockItem *item,
		      GoDockPlacement placement,
		      guint band_num,
		      gint position,
		      guint offset,
		      gboolean in_new_band)
{
  GoDockBand *band;
  GList **band_ptr;
  GList *p;

  DEBUG (("band_num %d offset %d position %d in_new_band %d",
          band_num, offset, position, in_new_band));

  if (placement == GO_DOCK_FLOATING)
    {
      g_warning ("Floating dock items not supported by `go_dock_add_item'.");
      return;
    }
  band_ptr = get_band_list (dock, placement);
  g_return_if_fail (band_ptr != NULL);

  p = g_list_nth (*band_ptr, band_num);
  if (in_new_band || p == NULL)
    {
      GtkWidget *new_band;

      new_band = go_dock_band_new ();

      /* FIXME: slow.  */
      if (in_new_band)
        {
          *band_ptr = g_list_insert (*band_ptr, new_band, band_num);
          p = g_list_nth (*band_ptr, band_num);
          if (p == NULL)
            p = g_list_last (*band_ptr);
        }
      else
        {
          *band_ptr = g_list_append (*band_ptr, new_band);
          p = g_list_last (*band_ptr);
        }

      if (placement == GO_DOCK_TOP || placement == GO_DOCK_BOTTOM)
        go_dock_band_set_orientation (GO_DOCK_BAND (new_band),
                                         GTK_ORIENTATION_HORIZONTAL);
      else
        go_dock_band_set_orientation (GO_DOCK_BAND (new_band),
                                         GTK_ORIENTATION_VERTICAL);

      gtk_widget_set_parent (new_band, GTK_WIDGET (dock));
      gtk_widget_show (new_band);
      gtk_widget_queue_resize (GTK_WIDGET (dock));
    }

  band = GO_DOCK_BAND (p->data);
  go_dock_band_insert (band, GTK_WIDGET(item), offset, position);

  connect_drag_signals (dock, GTK_WIDGET(item));

  g_signal_emit (dock, dock_signals[LAYOUT_CHANGED], 0);
}

/**
 * go_dock_add_floating_item:
 * @dock: A #GoDock widget
 * @item: The item to be added
 * @x: X-coordinate for the floating item
 * @y: Y-coordinate for the floating item
 * @orientation: Orientation for the new item.
 *
 * Description: Add @item to @dock and make it floating at the
 * specified (@x, @y) coordinates (relative to the root window of the
 * screen).
 **/
void
go_dock_add_floating_item (GoDock *dock,
                              GoDockItem *item,
                              gint x, gint y,
                              GtkOrientation orientation)
{
  GtkWidget *widget;

  g_return_if_fail (GO_IS_DOCK_ITEM (item));

  go_dock_item_set_orientation (item, orientation);

  widget = GTK_WIDGET(item);
  gtk_widget_ref (widget);

#if 0
  if (widget->parent != NULL)
      gtk_container_remove (GTK_CONTAINER (widget->parent), widget);
#endif

  gtk_widget_set_parent (widget, GTK_WIDGET (dock));

  if (GTK_WIDGET_REALIZED (widget->parent))
    gtk_widget_realize (widget);

  if (GTK_WIDGET_VISIBLE (widget->parent) && GTK_WIDGET_VISIBLE (widget))
    {
      if (GTK_WIDGET_MAPPED (widget->parent))
	gtk_widget_map (widget);

      gtk_widget_queue_resize (widget);
    }

  go_dock_item_detach (item, x, y);
  dock->floating_children = g_list_prepend (dock->floating_children, widget);

  connect_drag_signals (dock, widget);

  gtk_widget_unref (widget);

  g_signal_emit (dock, dock_signals[LAYOUT_CHANGED], 0);
}

/**
 * go_dock_set_client_area:
 * @dock: A #GoDock widget
 * @widget: The widget to be used for the client area.
 *
 * Description: Specify a widget for the dock's client area.
 **/
void
go_dock_set_client_area (GoDock *dock, GtkWidget *widget)
{
  g_return_if_fail (dock != NULL);

  if (widget != NULL)
    gtk_widget_ref (widget);

  if (dock->client_area != NULL)
    gtk_widget_unparent (dock->client_area);

  if (widget != NULL)
    {
      gtk_widget_set_parent (widget, GTK_WIDGET (dock));
      dock->client_area = widget;

      if (GTK_WIDGET_REALIZED (widget->parent))
	gtk_widget_realize (widget);

      if (GTK_WIDGET_VISIBLE (widget->parent) && GTK_WIDGET_VISIBLE (widget))
	{
	  if (GTK_WIDGET_MAPPED (widget->parent))
	    gtk_widget_map (widget);

	  gtk_widget_queue_resize (widget);
	}
    }
  else
    {
      if (dock->client_area != NULL && GTK_WIDGET_VISIBLE (dock))
        gtk_widget_queue_resize (GTK_WIDGET (dock));
      dock->client_area = NULL;
    }

  if (widget != NULL)
    gtk_widget_unref (widget);
}

/**
 * go_dock_get_client_area:
 * @dock: A #GoDock widget.
 *
 * Description: Retrieve the widget being used as the client area in
 * @dock.
 *
 * Returns: The client area widget.
 **/
GtkWidget *
go_dock_get_client_area (GoDock *dock)
{
  return dock->client_area;
}

/**
 * go_dock_get_item_by_name:
 * @dock: A #GoDock widget.
 * @name: The name of the dock item to retrieve
 * @placement_return: A pointer to a variable holding the item's placement
 * @num_band_return: A pointer to a variable holding the band number
 * @band_position_return: A pointer to a variable holding the position
 * of the item within the band
 * @offset_return: A pointer to a variable holding the offset of the item
 * from the previous item in the same band
 *
 * Description: Retrieve the dock item named @name; information about
 * its position in the dock is returned via @placement_return,
 * @num_band_return, @band_position_return and @offset_return.  If
 * the placement is %GO_DOCK_FLOATING *@num_band_return,
 * *@band_position_return and *@offset_return are not set.
 *
 * Returns: The named #GoDockItem widget, or %NULL if no item with
 * such name exists.
 **/
GoDockItem *
go_dock_get_item_by_name (GoDock *dock,
                             const gchar *name,
                             GoDockPlacement *placement_return,
                             guint *num_band_return,
                             guint *band_position_return,
                             guint *offset_return)
{
  GoDockItem *item;

  item = get_docked_item_by_name (dock,
                                  name,
                                  placement_return,
                                  num_band_return,
                                  band_position_return,
                                  offset_return);
  if (item != NULL)
    return item;

  item = get_floating_item_by_name (dock, name);
  if (item != NULL)
    {
      if (placement_return != NULL)
        *placement_return = GO_DOCK_FLOATING;
      return item;
    }

  return NULL;
}



/* Layout functions.  */

static void
layout_add_floating (GoDock *dock,
                     GoDockLayout *layout)
{
  GList *lp;

  for (lp = dock->floating_children; lp != NULL; lp = lp->next)
    {
      GtkOrientation orientation;
      gint x, y;
      GoDockItem *item;

      item = GO_DOCK_ITEM (lp->data);

      orientation = go_dock_item_get_orientation (item);
      go_dock_item_get_floating_position (item, &x, &y);

      go_dock_layout_add_floating_item (layout, item,
                                           x, y,
                                           orientation);
    }
}

static void
layout_add_bands (GoDock *dock,
                  GoDockLayout *layout,
                  GoDockPlacement placement,
                  GList *band_list)
{
  guint band_num;
  GList *lp;

  for (lp = band_list, band_num = 0;
       lp != NULL;
       lp = lp->next, band_num++)
    {
      GoDockBand *band;

      band = GO_DOCK_BAND(lp->data);
      go_dock_band_layout_add (band, layout, placement, band_num);
    }
}

/**
 * go_dock_get_layout:
 * @dock: A #GoDock widget
 *
 * Description: Retrieve the layout of @dock.
 *
 * Returns: @dock's layout as a #GoDockLayout object.
 **/
GoDockLayout *
go_dock_get_layout (GoDock *dock)
{
  GoDockLayout *layout;

  layout = go_dock_layout_new ();

  layout_add_bands (dock, layout, GO_DOCK_TOP, dock->top_bands);
  layout_add_bands (dock, layout, GO_DOCK_BOTTOM, dock->bottom_bands);
  layout_add_bands (dock, layout, GO_DOCK_LEFT, dock->left_bands);
  layout_add_bands (dock, layout, GO_DOCK_RIGHT, dock->right_bands);

  layout_add_floating (dock, layout);

  return layout;
}

/**
 * go_dock_add_from_layout:
 * @dock: The #GoDock widget
 * @layout: A #GoDockLayout widget
 *
 * Description: Add all the items in @layout to the specified @dock.
 *
 * Returns: %TRUE if the operation succeeds, %FALSE if it fails.
 **/
gboolean
go_dock_add_from_layout (GoDock       *dock,
			     GoDockLayout *layout)
{
  return go_dock_layout_add_to_dock (layout, dock);
}

static GList **
find_band_list (GoDock          *dock,
		GoDockBand      *band,
		GoDockPlacement *placement)
{
  GList **band_list = NULL;

  if (g_list_find (dock->top_bands, band))
    {
      *placement = GO_DOCK_TOP;
      band_list = &dock->top_bands;
    }

  if (g_list_find (dock->bottom_bands, band))
    {
      *placement = GO_DOCK_BOTTOM;
      band_list = &dock->bottom_bands;
    }

  if (g_list_find (dock->left_bands, band))
    {
      *placement = GO_DOCK_LEFT;
      band_list = &dock->left_bands;
    }

  if (g_list_find (dock->right_bands, band))
    {
      *placement = GO_DOCK_RIGHT;
      band_list = &dock->right_bands;
    }

  return band_list;
}

static gboolean
insert_into_band_list (GoDock     *dock,
		       GList         **band_list,
		       GtkOrientation  orientation,
		       GoDockItem *item,
		       gboolean        prepend)
{
  GtkWidget *new_band;

  new_band = go_dock_band_new ();

  if (item->behavior & GO_DOCK_ITEM_BEH_NEVER_VERTICAL)
    orientation = GTK_ORIENTATION_HORIZONTAL;

  if (item->behavior & GO_DOCK_ITEM_BEH_NEVER_HORIZONTAL)
    orientation = GTK_ORIENTATION_VERTICAL;

  if (!go_dock_band_append (
	  GO_DOCK_BAND (new_band), GTK_WIDGET (item), 0))
    return FALSE;

  if (prepend)
    *band_list = g_list_prepend (*band_list, new_band);
  else
    *band_list = g_list_append (*band_list, new_band);

  new_band_setup (dock, new_band, orientation);

  return TRUE;
}

gint
_bonobo_dock_handle_key_nav (GoDock     *dock,
			     GoDockBand *band,
			     GoDockItem *item,
			     GdkEventKey    *event)
{
  GList   *entry;
  GList  **band_list;
  int      cross_band_dir = 0;
  int      switch_side_dir = 0;
  gboolean end_stop = FALSE;
  gboolean was_inserted = FALSE;
  GtkOrientation orientation;
  GoDockPlacement placement;

  if (!(event->state & GDK_CONTROL_MASK))
    return FALSE;

  switch (event->keyval)
    {
      case GDK_Up:
        cross_band_dir = -1;
	break;
      case GDK_Down:
        cross_band_dir = +1;
	break;
      case GDK_Left:
        switch_side_dir = -1;
	break;
      case GDK_Right:
        switch_side_dir = +1;
	break;
      default:
        return FALSE;
    }

  band_list = find_band_list (dock, band, &placement);
  g_return_val_if_fail (band_list != NULL, FALSE);

  if (placement == GO_DOCK_LEFT ||
      placement == GO_DOCK_RIGHT)
    {
      int tmp = switch_side_dir;
      switch_side_dir = cross_band_dir;
      cross_band_dir = tmp;
      orientation = GTK_ORIENTATION_VERTICAL;
    }
  else
    {
      orientation = GTK_ORIENTATION_HORIZONTAL;
    }

  g_object_ref (G_OBJECT (item));

  gtk_container_remove (GTK_CONTAINER (band), GTK_WIDGET (item));

  /*
   * Find somewhere new for it ...
   */
  entry = g_list_find (*band_list, band);
  g_return_val_if_fail (entry != NULL, FALSE);

  if (cross_band_dir == -1)
    {
      for (entry = entry->prev; !was_inserted && entry;
	   entry = entry->prev)
        was_inserted = go_dock_band_append (
		entry->data, GTK_WIDGET (item), 0);

      if (!was_inserted &&
	  ((*band_list)->data != band ||
	   go_dock_band_get_num_children (band) > 0))
        {
	  was_inserted = insert_into_band_list (
		  dock, band_list, orientation, item, TRUE);
	}

      if (!was_inserted)
        {
	  if (placement == GO_DOCK_BOTTOM)
	    {
	      was_inserted = insert_into_band_list (
		      dock, &dock->top_bands, orientation, item, FALSE);
	    }
	  else if (placement == GO_DOCK_RIGHT)
	    {
	      was_inserted = insert_into_band_list (
		      dock, &dock->left_bands, orientation, item, FALSE);
	    }
	  else
	    end_stop = TRUE;
	}
    }

  if (cross_band_dir == +1)
    {
      for (entry = entry->next; !was_inserted && entry;
	   entry = entry->next)
        was_inserted = go_dock_band_append (
		entry->data, GTK_WIDGET (item), 0);

      if (!was_inserted &&
	  (g_list_last (*band_list)->data != band ||
	   go_dock_band_get_num_children (band) > 0))
        {
	  was_inserted = insert_into_band_list (
		  dock, band_list, orientation, item, FALSE);
	}

      if (!was_inserted)
        {
	  if (placement == GO_DOCK_TOP)
	    {
	      was_inserted = insert_into_band_list (
		      dock, &dock->bottom_bands, orientation, item, TRUE);
	    }
	  else if (placement == GO_DOCK_LEFT)
	    {
	      was_inserted = insert_into_band_list (
		      dock, &dock->right_bands, orientation, item, TRUE);
	    }
	  else
	    end_stop = TRUE;
	}
    }

  if (!end_stop && !was_inserted)
    {
      orientation = (orientation == GTK_ORIENTATION_HORIZONTAL) ?
	      GTK_ORIENTATION_VERTICAL : GTK_ORIENTATION_HORIZONTAL;
      if (placement == GO_DOCK_TOP ||
	  placement == GO_DOCK_BOTTOM)
        {
          if (switch_side_dir == -1)
	    was_inserted = insert_into_band_list (
		    dock, &dock->left_bands, orientation, item, FALSE);
	  else
            was_inserted = insert_into_band_list (
		    dock, &dock->right_bands, orientation, item, TRUE);
	}
      else
        {
          if (switch_side_dir == -1)
	    was_inserted = insert_into_band_list (
		    dock, &dock->top_bands, orientation, item, FALSE);
	  else
            was_inserted = insert_into_band_list (
		    dock, &dock->bottom_bands, orientation, item, TRUE);
	}
    }

  if (!was_inserted)
    { /* geometry issue */
      if (!go_dock_band_append (band, GTK_WIDGET (item), 0))
	g_error ("no space in fallback original band");
    }

  if (go_dock_band_get_num_children (band) == 0)
    gtk_widget_destroy (GTK_WIDGET (band));

  g_object_unref (G_OBJECT (item));

  return TRUE;
}

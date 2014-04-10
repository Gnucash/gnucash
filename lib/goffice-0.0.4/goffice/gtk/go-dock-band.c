/* File import from bonoboui to libgoffice by import-bonobo.  Do not edit.  */

/* go-dock-band.c

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
   write to the Free Software Foundation, Inc., 51 Franklin St, Fifth
   Floor, Boston, MA  02110-1301 USA.
   Author: Ettore Perazzoli <ettore@comm2000.it>
*/

#include <goffice/goffice-config.h>
#include <glib/gi18n.h>
#include <string.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>
#include <libgnome/gnome-macros.h>
#include "go-dock.h"
#include "go-dock-band.h"
#include "go-dock-item.h"

GNOME_CLASS_BOILERPLATE (GoDockBand, go_dock_band,
			 GtkContainer, GTK_TYPE_CONTAINER);

#define noBONOBO_DOCK_BAND_DEBUG

/* FIXME: To be removed.  */
#if defined GO_DOCK_BAND_DEBUG && defined __GNUC__
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

static void     go_dock_band_size_request  (GtkWidget *widget,
                                               GtkRequisition *requisition);

static void     go_dock_band_size_allocate (GtkWidget *widget,
                                               GtkAllocation *allocation);

static void     go_dock_band_map           (GtkWidget *widget);
static void     go_dock_band_unmap         (GtkWidget *widget);

static void     go_dock_band_add           (GtkContainer *container,
                                               GtkWidget *child);

static void     go_dock_band_remove        (GtkContainer *container,
                                               GtkWidget *widget);

static void     go_dock_band_forall        (GtkContainer *container,
                                               gboolean include_internals,
                                               GtkCallback callback,
                                               gpointer callback_data);

static void     go_dock_band_finalize      (GObject *object);

static void     size_allocate_child           (GoDockBand *band,
                                               GoDockBandChild *child,
                                               guint space,
                                               GtkAllocation *child_allocation);

static void     size_allocate_small           (GoDockBand *band,
                                               GtkAllocation *allocation,
                                               guint space,
                                               guint requested_space);

static gboolean docking_allowed               (GoDockBand *band,
                                               GoDockItem *item);

static GList   *find_child                    (GoDockBand *band,
                                               GtkWidget *child);

static GList   *prev_if_floating              (GoDockBand *band,
                                               GList *c);

static GList   *next_if_floating              (GoDockBand *band,
                                               GList *c);

static GList   *prev_not_floating             (GoDockBand *band,
                                               GList *c);

static GList   *next_not_floating             (GoDockBand *band,
                                               GList *c);

static void     calc_prev_and_foll_space      (GoDockBand *band);

static guint    attempt_move_backward         (GoDockBand *band,
                                               GList *child,
                                               guint amount);

static guint    attempt_move_forward          (GoDockBand *band,
                                               GList *child,
                                               guint amount);

static gboolean dock_nonempty                 (GoDockBand *band,
                                               GoDockItem *item,
                                               GList *where,
                                               gint x, gint y);

static gboolean dock_empty                    (GoDockBand *band,
                                               GoDockItem *item,
                                               GList *where,
                                               gint x, gint y);

static gboolean dock_empty_right              (GoDockBand *band,
                                               GoDockItem *item,
                                               GList *where,
                                               gint x, gint y);

static gboolean check_guint_arg               (GObject *object,
					       const gchar *name,
					       guint *value_return);

static void
go_dock_band_class_init (GoDockBandClass *klass)
{
  GObjectClass *gobject_class;
  GtkWidgetClass *widget_class;
  GtkContainerClass *container_class;

  gobject_class = (GObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;
  container_class = (GtkContainerClass *) klass;

  gobject_class->finalize = go_dock_band_finalize;

  widget_class->map = go_dock_band_map;
  widget_class->unmap = go_dock_band_unmap;
  widget_class->size_request = go_dock_band_size_request;
  widget_class->size_allocate = go_dock_band_size_allocate;

  container_class->add = go_dock_band_add;
  container_class->remove = go_dock_band_remove;
  container_class->forall = go_dock_band_forall;
}

static void
go_dock_band_instance_init (GoDockBand *band)
{
  GtkWidget *widget = GTK_WIDGET (band);

  GTK_WIDGET_SET_FLAGS (band, GTK_NO_WINDOW);

  band->_priv = NULL;
  band->orientation = GTK_ORIENTATION_HORIZONTAL;

  band->children = NULL;
  band->num_children = 0;

  band->floating_child = NULL;

  band->doing_drag = FALSE;

  band->max_space_requisition = 0;
  band->tot_offsets = 0;

  band->drag_allocation.x = band->drag_allocation.y = -1;
  band->drag_allocation.width = band->drag_allocation.height = 0;

  band->new_for_drag = FALSE;

  if (GTK_WIDGET_VISIBLE (widget))
    gtk_widget_queue_resize (widget);
}



static void
go_dock_band_size_request (GtkWidget *widget,
                              GtkRequisition *requisition)
{
  GoDockBand *band;
  GList *lp;

  DEBUG (("entering function"));

  band = GO_DOCK_BAND (widget);

  band->max_space_requisition = 0;
  band->tot_offsets = 0;

  requisition->width = 0;
  requisition->height = 0;

  for (lp = band->children; lp != NULL; lp = lp->next)
    {
      GoDockBandChild *c = lp->data;

      if (GTK_WIDGET_VISIBLE (c->widget))
        {
          GtkRequisition req;

	  req.width = req.height = 0;

          if (GO_IS_DOCK_ITEM (c->widget))
            go_dock_item_handle_size_request(GO_DOCK_ITEM (c->widget),
                                                &req);
	  else
	    gtk_widget_size_request (c->widget, &req);

          if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
	    {
	      gboolean has_preferred_width;
	      guint preferred_width;

	      has_preferred_width = check_guint_arg (G_OBJECT (c->widget),
						     "preferred_width",
						     &preferred_width);

	      if (has_preferred_width)
		c->max_space_requisition = MAX ((int)preferred_width, req.width);
	      else
		c->max_space_requisition = req.width;
	    }
          else
	    {
	      gboolean has_preferred_height;
	      guint preferred_height;

	      has_preferred_height = check_guint_arg (G_OBJECT (c->widget),
						      "preferred_height",
						      &preferred_height);

	      if (has_preferred_height)
		c->max_space_requisition = MAX ((int)preferred_height, req.height);
	      else
		c->max_space_requisition = req.height;
	    }

          band->max_space_requisition += c->max_space_requisition;

          if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
            {
              requisition->height = MAX (requisition->height, req.height);
              requisition->width += req.width;
            }
          else
            {
              requisition->width = MAX (requisition->width, req.width);
              requisition->height += req.height;
            }

          c->widget->requisition = req;
          band->tot_offsets += c->offset;
        }
    }

  widget->requisition = *requisition;
}



static void
size_allocate_child (GoDockBand *band,
                     GoDockBandChild *child,
                     guint space,
                     GtkAllocation *child_allocation)
{
  GtkWidget *band_widget;

  band_widget = GTK_WIDGET (band);
  if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
    {
      child_allocation->x += child->real_offset;
      child_allocation->width = space;
      child_allocation->height = band_widget->allocation.height;
      DEBUG (("horizontal %d %d %d %d real_offset %d",
              child_allocation->x, child_allocation->y,
              child_allocation->width, child_allocation->height,
              child->real_offset));
      gtk_widget_size_allocate (child->widget, child_allocation);
      child_allocation->x += child_allocation->width;
    }
  else
    {
      child_allocation->y += child->real_offset;
      child_allocation->width = band_widget->allocation.width;
      child_allocation->height = space;
      DEBUG (("vertical %d %d %d %d real_offset %d",
              child_allocation->x, child_allocation->y,
              child_allocation->width, child_allocation->height,
              child->real_offset));
      gtk_widget_size_allocate (child->widget, child_allocation);
      child_allocation->y += child_allocation->height;
    }
}

/* The allocated space is smaller than the space needed to show all
   the items completely.  */
static void
size_allocate_small (GoDockBand *band,
                     GtkAllocation *allocation,
                     guint space,
                     guint requested_space)
{
  GtkAllocation child_allocation;
  GList *lp;
  guint max_space_requisition;

  DEBUG (("entering function"));

  child_allocation.x = allocation->x;
  child_allocation.y = allocation->y;

  max_space_requisition = band->max_space_requisition;

  for (lp = band->children; lp != NULL; lp = lp->next)
    {
      GoDockBandChild *child;

      child = lp->data;

      if (GTK_WIDGET_VISIBLE (child->widget))
        {
	  guint child_requested_space;

	  child->real_offset = 0;

	  if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
	    child_requested_space = child->widget->requisition.width;
	  else
	    child_requested_space = child->widget->requisition.height;

	  if (space < child->max_space_requisition
	      || (space - child->max_space_requisition
		  < requested_space - child_requested_space))
	    break;

	  space -= child->max_space_requisition;
	  requested_space -= child_requested_space;
	  max_space_requisition -= child->max_space_requisition;

	  size_allocate_child (band, child,
			       child->max_space_requisition,
			       &child_allocation);
	}
    }

  if (lp != NULL)
    {
      GoDockBandChild *child;
      guint child_space, child_requested_space;

      child = lp->data;

      if (GTK_WIDGET_VISIBLE (child->widget))
        {
	  child->real_offset = 0;

	  if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
	    child_requested_space = child->widget->requisition.width;
	  else
	    child_requested_space = child->widget->requisition.height;

	  requested_space -= child_requested_space;
	  child_space = space - requested_space;
	  space -= child_space;

	  size_allocate_child (band, child,
			       child_space,
			       &child_allocation);
	}

      lp = lp->next;
    }

  for (; lp != NULL; lp = lp->next)
    {
      GoDockBandChild *child;

      child = lp->data;

      if (GTK_WIDGET_VISIBLE (child->widget))
        {
	  child->real_offset = 0;
	  child->real_offset = 0;

	  if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
	    size_allocate_child (band, child,
				 child->widget->requisition.width,
				 &child_allocation);
	  else
	    size_allocate_child (band, child,
				 child->widget->requisition.height,
				 &child_allocation);
	}
    }
}

/* The allocation is enough to show all the items completely, but not
   to satisfy all the requested offsets.  */
static void
size_allocate_medium (GoDockBand *band,
                      GtkAllocation *allocation,
                      guint space,
                      guint requested_space)
{
  GtkAllocation child_allocation;
  GList *lp;
  gfloat factor;

  DEBUG (("entering function"));

  child_allocation.x = allocation->x;
  child_allocation.y = allocation->y;

  factor = (1.0 - ((float) (band->max_space_requisition + band->tot_offsets
                            - space)
                   / (float) band->tot_offsets));

  /* Shrink the offsets proportionally.  */
  for (lp = band->children; lp != NULL; lp = lp->next)
    {
      GoDockBandChild *child;

      child = lp->data;

      if (GTK_WIDGET_VISIBLE (child->widget))
        {
	  child->real_offset = (guint) ((float) child->offset * factor + .5);

	  size_allocate_child (band, child,
			       child->max_space_requisition,
			       &child_allocation);
	}
    }
}

/* The allocation is enough to show all the items completely, with the
   requested offsets.  */
static void
size_allocate_large (GoDockBand *band,
                     GtkAllocation *allocation,
                     guint space,
                     guint requested_space)
{
  GtkAllocation child_allocation;
  GList *lp;

  DEBUG (("entering function"));

  child_allocation.x = allocation->x;
  child_allocation.y = allocation->y;

  for (lp = band->children; lp != NULL; lp = lp->next)
    {
      GoDockBandChild *child;

      child = lp->data;

      if (GTK_WIDGET_VISIBLE (child->widget))
        {
	  child->real_offset = child->offset;

	  size_allocate_child (band, child,
			       child->max_space_requisition,
			       &child_allocation);
	}
    }
}

static void
go_dock_band_size_allocate (GtkWidget *widget,
                               GtkAllocation *allocation)
{
  GoDockBand *band;
  guint space, requested_space;

  band = GO_DOCK_BAND (widget);

  widget->allocation = *allocation;

  /* Check if we have a single exclusive item.  If so, allocate the
     whole space to it.  */
  if (band->num_children == 1)
    {
      GoDockBandChild *c;

      c = (GoDockBandChild *) band->children->data;
      if (GO_IS_DOCK_ITEM (c->widget) && GTK_WIDGET_VISIBLE (c->widget))
        {
          GoDockItemBehavior behavior;
          GoDockItem *item;

          item = GO_DOCK_ITEM (c->widget);
          behavior = go_dock_item_get_behavior (item);
          if (behavior & GO_DOCK_ITEM_BEH_EXCLUSIVE)
            {
              gtk_widget_size_allocate (c->widget, allocation);
              return;
            }
        }
    }

  if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
    {
      space = allocation->width;
      requested_space = widget->requisition.width;
    }
  else
    {
      space = allocation->height;
      requested_space = widget->requisition.height;
    }

  if (space <= band->max_space_requisition)
    size_allocate_small (band, allocation, space, requested_space);
  else if (space < band->max_space_requisition + band->tot_offsets)
    size_allocate_medium (band, allocation, space, requested_space);
  else
    size_allocate_large (band, allocation, space, requested_space);

  calc_prev_and_foll_space (band);
}



static void
go_dock_band_map (GtkWidget *widget)
{
  GoDockBand *band = GO_DOCK_BAND (widget);
  GList *lp;

  g_return_if_fail(widget != NULL);
  g_return_if_fail(GO_IS_DOCK_BAND(widget));

  GNOME_CALL_PARENT (GTK_WIDGET_CLASS, map, (widget));

  for (lp = band->children; lp != NULL; lp = lp->next)
    {
      GoDockBandChild *c;

      c = lp->data;
      if (GTK_WIDGET_VISIBLE (c->widget) && ! GTK_WIDGET_MAPPED (c->widget))
        gtk_widget_map (c->widget);
    }
}

static void
go_dock_band_unmap (GtkWidget *widget)
{
  GoDockBand *band = GO_DOCK_BAND (widget);
  GList *lp;

  g_return_if_fail(widget != NULL);
  g_return_if_fail(GO_IS_DOCK_BAND(widget));

  GNOME_CALL_PARENT (GTK_WIDGET_CLASS, unmap, (widget));

  for (lp = band->children; lp != NULL; lp = lp->next)
    {
      GoDockBandChild *c;

      c = lp->data;
      if (GTK_WIDGET_VISIBLE (c->widget) && GTK_WIDGET_MAPPED (c->widget))
        gtk_widget_unmap (c->widget);
    }
}


/* GtkContainer methods.  */

static void
go_dock_band_add (GtkContainer *container, GtkWidget *child)
{
  GoDockBand *band = GO_DOCK_BAND (container);

  g_return_if_fail (go_dock_band_prepend (band, child, 0));
}

static void
go_dock_band_remove (GtkContainer *container, GtkWidget *widget)
{
  GoDockBand *band;
  GList *child;

  band = GO_DOCK_BAND (container);
  if (band->num_children == 0)
    return;

  child = find_child (band, widget);
  if (child != NULL)
    {
      gboolean was_visible;

      if (child == band->floating_child)
        band->floating_child = NULL;

      was_visible = GTK_WIDGET_VISIBLE (widget);
      gtk_widget_unparent (widget);

      band->children = g_list_remove_link (band->children, child);
      g_free (child->data);
      g_list_free (child);

      if (band->doing_drag)
        {
          GList *p;

          for (p = band->children; p != NULL; p = p->next)
            {
              GoDockBandChild *c;

              c = (GoDockBandChild *) p->data;
              c->offset = c->real_offset = c->drag_offset;
            }
        }

      gtk_widget_queue_resize (GTK_WIDGET (band));

      band->num_children--;
      DEBUG (("now num_children = %d", band->num_children));
    }
}

static void
go_dock_band_forall (GtkContainer *container,
                        gboolean include_internals,
                        GtkCallback callback,
                        gpointer callback_data)
{
  GoDockBand *band;
  GoDockBandChild *child;
  GList *children;

  band = GO_DOCK_BAND (container);

  children = band->children;
  while (children)
    {
      child = children->data;
      children = children->next;
      (* callback) (child->widget, callback_data);
    }
}

static void
go_dock_band_finalize (GObject *object)
{
  GoDockBand *self = GO_DOCK_BAND (object);

  g_free (self->_priv);
  self->_priv = NULL;

  GNOME_CALL_PARENT (G_OBJECT_CLASS, finalize, (object));
}


/* Utility functions.  */

static gboolean
docking_allowed (GoDockBand *band, GoDockItem *item)
{
  GoDockItemBehavior behavior;
  GoDockBandChild *c;

  if (band->num_children == 0)
    return TRUE;

  behavior = go_dock_item_get_behavior (item);

  if (behavior & GO_DOCK_ITEM_BEH_EXCLUSIVE)
    return FALSE;

  c = (GoDockBandChild *) band->children->data;
  if (GO_IS_DOCK_ITEM (c->widget))
    {
      behavior = go_dock_item_get_behavior (GO_DOCK_ITEM (c->widget));
      if (behavior & GO_DOCK_ITEM_BEH_EXCLUSIVE)
          return c->widget == GTK_WIDGET (item);
    }

  return TRUE;
}

static GList *
find_child (GoDockBand *band, GtkWidget *child)
{
  GList *children;

  children = band->children;

  while (children != NULL)
    {
      GoDockBandChild *c;

      c = (GoDockBandChild *) children->data;
      if (c->widget == child)
        return children;

      children = children->next;
    }

  return NULL;
}

static GList *
next_if_floating (GoDockBand *band, GList *c)
{
  if (c != NULL && c == band->floating_child)
    return c->next;
  else
    return c;
}

static GList *
prev_if_floating (GoDockBand *band, GList *c)
{
  if (c != NULL && c == band->floating_child)
    return c->prev;
  else
    return c;
}

static GList *
next_not_floating (GoDockBand *band, GList *c)
{
  if (c == NULL)
    return NULL;
  else
    return next_if_floating (band, c->next);
}

static GList *
prev_not_floating (GoDockBand *band, GList *c)
{
  if (c == NULL)
    return NULL;
  else
    return prev_if_floating (band, c->prev);
}



static GList *
find_where (GoDockBand *band, gint offset, gboolean *is_empty)
{
  guint count;                  /* FIXME: used for debugging only */
  gint offs;
  GList *lp;

  if (offset < 0)
    offset = 0;

  offs = 0;
  count = 0;                    /* FIXME */
  for (lp = band->children; lp != NULL; lp = lp->next)
    {
      GoDockBandChild *child;

      child = lp->data;

      if (lp == band->floating_child)
        {
          if (lp->next == NULL)
            {
              DEBUG (("empty last %d", count));
              *is_empty = TRUE;

              return lp == band->floating_child ? lp->prev : lp;
            }
          DEBUG (("%d: is floating or dragged.", count++));
          continue;
        }

      DEBUG (("%d: Checking for x %d, width %d, offs %d (%d)",
              count, child->drag_allocation.x,
              child->drag_allocation.width, offs, offset));

      if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
        {
          if (offset >= offs && offset <= child->drag_allocation.x)
            {
              *is_empty = TRUE;
              DEBUG (("empty %d (allocation.x %d)",
                      count, child->drag_allocation.x));

              return prev_if_floating (band, lp->prev);
            }

          offs = child->drag_allocation.x + child->drag_allocation.width;
          if (offset > child->drag_allocation.x && offset < offs)
            {
              *is_empty = FALSE;
              DEBUG (("%d", count));
              return lp->prev;
            }
        }
      else
        {
          if (offset >= offs && offset <= child->drag_allocation.y)
            {
              *is_empty = TRUE;
              DEBUG (("empty %d (allocation.y %d)",
                      count, child->drag_allocation.y));

              return prev_if_floating (band, lp->prev);
            }

          offs = child->drag_allocation.y + child->drag_allocation.height;
          if (offset > child->drag_allocation.y && offset < offs)
            {
              *is_empty = FALSE;
              DEBUG (("%d", count));
              return lp->prev;
            }
        }

      if (lp->next == NULL)
        {
          DEBUG (("empty last %d", count));
          *is_empty = TRUE;
          return lp;
        }

      count++;                  /* FIXME */
    }

  DEBUG (("nothing done."));

  /* Make compiler happy.  */
  *is_empty = TRUE;
  return lp;
}



static void
calc_prev_and_foll_space (GoDockBand *band)
{
  GtkWidget *widget;
  GList *lp;

  if (band->children == NULL)
    return;

  widget = GTK_WIDGET (band);

  lp = next_if_floating (band, band->children);
  if (lp != NULL)
    {
      GoDockBandChild *c;
      guint prev_space, foll_space;

      prev_space = 0;

      while (1)
        {
          GList *next;

          c = lp->data;
          prev_space += c->real_offset;
          c->prev_space = prev_space;

          if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
            prev_space += (c->widget->allocation.width
                           - c->widget->requisition.width);
          else
            prev_space += (c->widget->allocation.height
                           - c->widget->requisition.height);

          next = next_not_floating (band, lp);
          if (next == NULL)
            break;

          lp = next;
        }

      if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
        foll_space = (widget->allocation.x + widget->allocation.width
                      - (c->widget->allocation.x
                         + c->widget->requisition.width));
      else
        foll_space = (widget->allocation.y + widget->allocation.height
                      - (c->widget->allocation.y
                         + c->widget->requisition.height));

      DEBUG(("foll_space %d", foll_space));

      for (; lp != NULL; lp = prev_not_floating (band, lp))
        {
          c = lp->data;
          if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
            foll_space += (c->widget->allocation.width
                           - c->widget->requisition.width);
          else
            foll_space += (c->widget->allocation.height
                           - c->widget->requisition.height);
          c->foll_space = foll_space;

          foll_space += c->real_offset;

        }
    }
}



static guint
attempt_move_backward (GoDockBand *band, GList *child, guint amount)
{
  GList *lp;
  guint effective_amount;

  effective_amount = 0;

  for (lp = prev_if_floating (band, child);
       lp != NULL && amount > 0;
       lp = prev_not_floating (band, lp))
    {
      GoDockBandChild *c;

      c = lp->data;

      if (c->drag_offset > amount)
        {
          c->real_offset = c->drag_offset - amount;
          effective_amount += amount;
          amount = 0;
        }
      else
        {
          c->real_offset = 0;
          effective_amount += c->drag_offset;
          amount -= c->drag_offset;
        }
      c->offset = c->real_offset;
    }

  return effective_amount;
}

static guint
attempt_move_forward (GoDockBand *band, GList *child, guint requirement)
{
  GList *lp;
  guint effective_amount;

  effective_amount = 0;
  for (lp = next_if_floating (band, child);
       lp != NULL && requirement > 0;
       lp = next_not_floating (band, lp))
    {
      GoDockBandChild *c;

      c = lp->data;

      DEBUG (("requirement = %d", requirement));
      if (c->drag_offset > requirement)
        {
          c->real_offset = c->drag_offset - requirement;
          effective_amount += requirement;
          requirement = 0;
        }
      else
        {
          c->real_offset = 0;
          effective_amount += c->drag_offset;
          requirement -= c->drag_offset;
        }
      c->offset = c->real_offset;
    }

  return effective_amount;
}



static void
reparent_if_needed (GoDockBand *band,
                    GoDockItem *item,
                    gint x, gint y)
{
  if (GTK_WIDGET (item)->parent != GTK_WIDGET (band))
    {
      go_dock_item_attach (item, GTK_WIDGET (band), x, y);

      /* Reparenting causes the new floating child to be the first
         item on the child list (see the `remove' method).  */
      band->floating_child = band->children;

      /* Reparenting will remove the grab, so we need to redo it.  */
      go_dock_item_grab_pointer (item);
    }
}

static gboolean
dock_nonempty (GoDockBand *band,
               GoDockItem *item,
               GList *where,
               gint x, gint y)
{
  GoDockBandChild *c, *floating_child;
  GtkOrientation orig_item_orientation;
  GtkRequisition item_requisition;
  GList *lp, *next;
  gint amount, requirement;

  DEBUG (("entering function"));

  if (! docking_allowed (band, item))
    return FALSE;

  if (where == NULL)
    lp = band->children;
  else
    lp = next_not_floating (band, where);

  c = lp->data;

  orig_item_orientation = go_dock_item_get_orientation (item);
  if (orig_item_orientation != band->orientation
      && ! go_dock_item_set_orientation (item, band->orientation))
    return FALSE;

  go_dock_item_handle_size_request (item, &item_requisition);
  if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
    requirement = item_requisition.width;
  else
    requirement = item_requisition.height;

  if ((c->drag_prev_space + c->drag_foll_space) < requirement)
    {
      DEBUG (("not enough space %d %d",
              c->drag_prev_space + c->drag_foll_space,
              requirement));

      /* Restore original orientation.  */
      if (orig_item_orientation != band->orientation)
        go_dock_item_set_orientation (item, orig_item_orientation);

      return FALSE;
    }

  gtk_widget_size_request (GTK_WIDGET (item), &item_requisition);
  if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
    requirement = item_requisition.width;
  else
    requirement = item_requisition.height;

  if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
    amount = c->drag_allocation.x + c->drag_allocation.width - x;
  else
    amount = c->drag_allocation.y + c->drag_allocation.height - y;

  DEBUG (("amount %d requirement %d", amount, requirement));
  amount = attempt_move_backward (band, lp, amount);

  if (requirement < amount)
    requirement = 0;
  else
    {
      requirement -= amount;
      next = next_not_floating (band, lp);
      if (next != NULL)
        attempt_move_forward (band, next, requirement);
    }

  if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
    reparent_if_needed (band, item, x, GTK_WIDGET (band)->allocation.y);
  else
    reparent_if_needed (band, item, GTK_WIDGET (band)->allocation.x, y);

  floating_child = band->floating_child->data;
  floating_child->offset = floating_child->real_offset = 0;

  if (band->floating_child->prev != lp)
    {
      DEBUG (("moving"));
      band->children = g_list_remove_link (band->children,
                                           band->floating_child);
      band->floating_child->next = lp->next;
      if (band->floating_child->next != NULL)
        band->floating_child->next->prev = band->floating_child;
      band->floating_child->prev = lp;
      lp->next = band->floating_child;
    }

  gtk_widget_queue_resize (floating_child->widget);

  return TRUE;
}

static gboolean
dock_empty (GoDockBand *band,
            GoDockItem *item,
            GList *where,
            gint x, gint y)
{
  GoDockBandChild *floating_child;
  GoDockBandChild *c1, *c2;
  GtkOrientation orig_item_orientation;
  GtkRequisition item_requisition;
  GList *lp;
  guint new_offset;
  GtkWidget *item_widget;

  DEBUG (("entering function"));

  if (! docking_allowed (band, item))
    return FALSE;

  if (where != NULL)
    {
      lp = next_not_floating (band, where);

      if (lp == NULL)
        /* Extreme right is a special case.  */
        return dock_empty_right (band, item, where, x, y);

      c1 = where->data;
    }
  else
    {
      c1 = NULL;
      lp = next_if_floating (band, band->children);

      if (lp == NULL)
        {
          /* Only one floating element.  Easy.  */
          GoDockBandChild *c;

          if (! go_dock_item_set_orientation (item, band->orientation))
            return FALSE;

          if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
            reparent_if_needed (band, item,
                                x, GTK_WIDGET (band)->allocation.y);
          else
            reparent_if_needed (band, item,
                                GTK_WIDGET (band)->allocation.x, y);

          c = band->floating_child->data;

          if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
            c->real_offset = x - GTK_WIDGET (band)->allocation.x;
          else
            c->real_offset = y - GTK_WIDGET (band)->allocation.y;
          c->offset = c->real_offset;

          DEBUG (("simple case offset %d", c->offset));

          gtk_widget_queue_resize (c->widget);

          return TRUE;
        }
    }

  c2 = lp->data;

  item_widget = GTK_WIDGET (item);

  orig_item_orientation = go_dock_item_get_orientation (item);
  if (! go_dock_item_set_orientation (item, band->orientation))
    return FALSE;

  /* Check whether there is enough space for the widget.  */
  {
    gint space;

    if (c1 != NULL)
      space = c1->drag_foll_space;
    else
      {
        space = c2->real_offset + c2->drag_foll_space;
        if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
          space += c2->widget->allocation.width - c2->widget->requisition.width;
        else
          space += c2->widget->allocation.height - c2->widget->requisition.height;
      }

    go_dock_item_handle_size_request (item, &item_requisition);
    if (space < (band->orientation == GTK_ORIENTATION_HORIZONTAL
                 ? item_requisition.width
                 : item_requisition.height))
      {
        DEBUG (("not enough space %d", space));

        /* Restore original orientation.  */
        if (orig_item_orientation != band->orientation)
          go_dock_item_set_orientation (item, orig_item_orientation);

        return FALSE;
      }

  }

  gtk_widget_size_request (item_widget, &item_requisition);

  if (c1 == NULL)
    {
      if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
        new_offset = x - GTK_WIDGET (band)->allocation.x;
      else
        new_offset = y - GTK_WIDGET (band)->allocation.y;
    }
  else
    {
      if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
        new_offset = x - (c1->drag_allocation.x + c1->drag_allocation.width);
      else
        new_offset = y - (c1->drag_allocation.y + c1->drag_allocation.height);
    }

  DEBUG (("new_offset %d", new_offset));

  if (c2->drag_offset >= (new_offset
                          + (band->orientation == GTK_ORIENTATION_HORIZONTAL
                             ? item_requisition.width
                             : item_requisition.height)))
    {
      c2->real_offset = (c2->drag_offset
                         - (new_offset
                            + (band->orientation == GTK_ORIENTATION_HORIZONTAL
                               ? item_requisition.width
                               : item_requisition.height)));
      c2->offset = c2->real_offset;
    }
  else
    {
      guint requisition;
      GList *lp1;

      requisition = new_offset + (band->orientation == GTK_ORIENTATION_HORIZONTAL
                                  ? item_requisition.width
                                  : item_requisition.height);

      DEBUG (("Moving forward %d!", requisition));

      for (lp1 = lp; lp1 != NULL && requisition > 0; )
        {
          GoDockBandChild *tmp = lp1->data;
          GList *lp1next;

          if (tmp->drag_offset > requisition)
            {
              tmp->real_offset = tmp->drag_offset - requisition;
              requisition = 0;
            }
          else
            {
              requisition -= tmp->drag_offset;
              tmp->real_offset = 0;
            }
          tmp->offset = tmp->real_offset;

          DEBUG (("Offset %d (drag %d)", tmp->real_offset, tmp->drag_offset));
          lp1next = next_not_floating (band, lp1);
          if (lp1next == NULL)
            {
              if (tmp->drag_foll_space > requisition)
                requisition = 0;
              else
                requisition -= tmp->drag_foll_space;
            }

          lp1 = lp1next;
        }

      if (requisition > 0)
        new_offset -= requisition;
    }

  if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
    reparent_if_needed (band, item, x, GTK_WIDGET (band)->allocation.y);
  else
    reparent_if_needed (band, item, GTK_WIDGET (band)->allocation.x, y);

  floating_child = (GoDockBandChild *) band->floating_child->data;
  floating_child->real_offset = floating_child->offset = new_offset;

  band->children = g_list_remove_link (band->children, band->floating_child);

  if (where == NULL)
    {
      band->floating_child->next = band->children;
      band->children->prev = band->floating_child;
      band->children = band->floating_child;
    }
  else
    {
      band->floating_child->next = where->next;
      band->floating_child->prev = where;
      if (where->next != NULL)
        where->next->prev = band->floating_child;
      where->next = band->floating_child;
    }

  gtk_widget_queue_resize (((GoDockBandChild *) band->floating_child->data)->widget);

  return TRUE;
}

static gboolean
dock_empty_right (GoDockBand *band,
                  GoDockItem *item,
                  GList *where,
                  gint x, gint y)
{
  GoDockBandChild *c, *floating_child;
  GtkOrientation orig_item_orientation;
  GtkRequisition item_requisition;
  GtkWidget *item_widget;
  gint new_offset;

  g_return_val_if_fail (next_not_floating (band, where) == NULL, FALSE);
  g_return_val_if_fail (band->floating_child != where, FALSE);

  DEBUG (("entering function"));

  if (! docking_allowed (band, item))
    return FALSE;

  item_widget = GTK_WIDGET (item);

  c = where->data;

  orig_item_orientation = go_dock_item_get_orientation (item);
  if (orig_item_orientation != band->orientation
      && ! go_dock_item_set_orientation (item, band->orientation))
    return FALSE;

  go_dock_item_handle_size_request (item, &item_requisition);
  if ((c->drag_prev_space + c->drag_foll_space)
      < (band->orientation == GTK_ORIENTATION_HORIZONTAL
                 ? item_requisition.width
                 : item_requisition.height))
    {
      DEBUG (("not enough space %d ", c->drag_prev_space+ c->drag_foll_space));

      /* Restore original orientation.  */
      if (orig_item_orientation != band->orientation)
        go_dock_item_set_orientation (item, orig_item_orientation);

      return FALSE;
    }

  gtk_widget_size_request (item_widget, &item_requisition);

  if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
    new_offset = x - (c->widget->allocation.x + c->widget->allocation.width);
  else
    new_offset = y - (c->widget->allocation.y + c->widget->allocation.height);

  DEBUG (("x %d y %d new_offset %d width %d foll_space %d",
          x, y, new_offset, item_widget->allocation.width,
          c->drag_foll_space));

  if ((guint) (new_offset
               + (band->orientation == GTK_ORIENTATION_HORIZONTAL
                  ? item_requisition.width
                  : item_requisition.height)) > c->drag_foll_space)
    {
      gint excess = (new_offset
                     + (band->orientation == GTK_ORIENTATION_HORIZONTAL
                        ? item_requisition.width
                        : item_requisition.height)
                     - c->drag_foll_space);

      DEBUG (("excess %d new_offset %d", excess, new_offset));
      if (excess < new_offset)
        new_offset -= excess;
      else
        {
          attempt_move_backward (band, where, excess - new_offset);
          new_offset = 0;
        }
    }

  if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
    reparent_if_needed (band, item,
                        x, GTK_WIDGET (band)->allocation.y);
  else
    reparent_if_needed (band, item,
                        GTK_WIDGET (band)->allocation.x, y);

  floating_child = band->floating_child->data;
  floating_child->offset = floating_child->real_offset = new_offset;

  band->children = g_list_remove_link (band->children, band->floating_child);
  where->next = band->floating_child;
  band->floating_child->prev = where;

  gtk_widget_queue_resize (floating_child->widget);

  return TRUE;
}

/* Helper function.  */

static gboolean
check_guint_arg (GObject *object,
		 const gchar *name,
		 guint *value_return)
{
  GParamSpec *pspec;

  g_return_val_if_fail (object != NULL, FALSE);

  pspec = g_object_class_find_property (G_OBJECT_GET_CLASS (object), name);
  if (pspec != NULL) {
    GValue value = { 0, };

    g_value_init (&value, G_TYPE_UINT);
    g_object_get_property (G_OBJECT (object), name, &value);
    *value_return = g_value_get_uint (&value);
    g_value_unset (&value);

    return TRUE;
  } else
    return FALSE;
}



/* Exported interface.  */

/**
 * go_dock_band_new:
 *
 * Description: Create a new GoDockBand widget.
 *
 * Returns: The new GoDockBand widget.
 **/
GtkWidget *
go_dock_band_new (void)
{
  GoDockBand *band;

  band = g_object_new (go_dock_band_get_type (), NULL);
  return GTK_WIDGET (band);
}

/**
 * go_dock_band_set_orientation:
 * @band: A GoDockBand widget
 * @orientation: New orientation for @band
 *
 * Description: Set the orientation for @band.
 **/
void
go_dock_band_set_orientation (GoDockBand *band,
                                 GtkOrientation orientation)
{
  g_return_if_fail (orientation == GTK_ORIENTATION_HORIZONTAL
                    || orientation == GTK_ORIENTATION_VERTICAL);

  band->orientation = orientation;
}

/**
 * go_dock_band_get_orientation:
 * @band: A GoDockBand widget
 *
 * Description: Retrieve the orientation of the specified @band.
 *
 * Returns: The orientation of @band.
 **/
GtkOrientation
go_dock_band_get_orientation (GoDockBand *band)
{
  return band->orientation;
}

/**
 * go_dock_band_insert:
 * @band: A GoDockBand widget
 * @child: The widget to be added to @band
 * @offset: Offset from the previous item
 * @position: Position within the @band
 *
 * Description: Add @child to @band at the specified @position, with
 * the specified @offset from the previous item (or from the beginning
 * of the band, if this is the first item).
 *
 * Returns: %TRUE if successful, %FALSE if the operation fails.
 **/
gboolean
go_dock_band_insert (GoDockBand *band,
                        GtkWidget *child,
                        guint offset,
                        gint position)
{
  GoDockBandChild *band_child;

  DEBUG (("%08x", (unsigned int) band));

  if (GO_IS_DOCK_ITEM (child)
      && !docking_allowed (band, GO_DOCK_ITEM (child)))
    return FALSE;

  if (GO_IS_DOCK_ITEM (child) &&
      !go_dock_item_set_orientation (GO_DOCK_ITEM (child),
					 band->orientation))
      return FALSE;

  if (position < 0 || position > (gint) band->num_children)
    position = band->num_children;

  band_child = g_new (GoDockBandChild, 1);
  band_child->widget = child;
  band_child->offset = offset;
  band_child->real_offset = 0;

  if (position == 0)
    band->children = g_list_prepend (band->children, band_child);
  else if ((guint) position == band->num_children)
    band->children = g_list_append (band->children, band_child);
  else
    {
      GList *p;

      p = g_list_nth (band->children, position);
      g_list_prepend (p, band_child);
    }

  gtk_widget_set_parent (child, GTK_WIDGET (band));

  if (GTK_WIDGET_REALIZED (child->parent))
    gtk_widget_realize (child);

  if (GTK_WIDGET_VISIBLE (child->parent) && GTK_WIDGET_VISIBLE (child))
    {
      if (GTK_WIDGET_MAPPED (child->parent))
	gtk_widget_map (child);

      gtk_widget_queue_resize (child);
    }

  band->num_children++;
  DEBUG (("now num_children = %d", band->num_children));

  return TRUE;
}

void
go_dock_band_move_child (GoDockBand *band,
                            GList *old_child,
                            guint new_num)
{
  GList *children;
  GList *lp;

  children = band->children;

  lp = old_child;

  children = g_list_remove_link (children, lp);

  children = g_list_insert (children, lp->data, new_num);

  g_list_free (lp);

  band->children = children;

  /* FIXME */
  gtk_widget_queue_resize (GTK_WIDGET (band));
}

/**
 * go_dock_band_prepend:
 * @band: A GoDockBand widget
 * @child: A widget to be added to @band
 * @offset: Offset (in pixels) from the beginning of the band
 *
 * Description: Add @child to @band with the specified @offset as the
 * first element.
 *
 * Returns: %TRUE if successful, %FALSE if the operation fails.
 **/
gboolean
go_dock_band_prepend (GoDockBand *band,
                         GtkWidget *child,
                         guint offset)
{
  return go_dock_band_insert (band, child, offset, 0);
}

/**
 * go_dock_band_append:
 * @band: A GoDockBand widget
 * @child: A widget to be added to @band
 * @offset: Offset (in pixels) from the last item of the band
 *
 * Description: Add @child to @band with the specified @offset as the
 * last element.
 *
 * Returns: %TRUE if successful, %FALSE if the operation fails.
 **/
gboolean
go_dock_band_append (GoDockBand *band,
                        GtkWidget *child,
                        guint offset)
{
  return go_dock_band_insert (band, child, offset, -1);
}

/**
 * go_dock_band_set_child_offset:
 * @band: A GoDockBand widget
 * @child: Child of @band whose offset must be changed
 * @offset: New offset value for @child
 *
 * Description: Set the offset for the specified @child of @band.
 **/
void
go_dock_band_set_child_offset (GoDockBand *band,
                                  GtkWidget *child,
                                  guint offset)
{
  GList *p;

  p = find_child (band, child);
  if (p != NULL)
    {
      GoDockBandChild *c;

      c = (GoDockBandChild *) p->data;
      c->offset = offset;
      gtk_widget_queue_resize (c->widget);
    }
}

/**
 * go_dock_band_get_child_offset:
 * @band: A GoDockBand widget
 * @child: Child of @band whose offset must be retrieved
 *
 * Description: Retrieve the offset of @child in @band.
 *
 * Returns: The offset of @child.
 **/
guint
go_dock_band_get_child_offset (GoDockBand *band,
                                  GtkWidget *child)
{
  GList *p;

  p = find_child (band, child);
  if (p != NULL)
    {
      GoDockBandChild *c;

      c = (GoDockBandChild *) p->data;
      return c->offset;
    }

  return 0;
}

/**
 * go_dock_band_get_num_children:
 * @band: A GoDockBand widget
 *
 * Description: Retrieve the number of children in @band.
 *
 * Returns: The number of children in @band.
 **/
guint
go_dock_band_get_num_children (GoDockBand *band)
{
  return band->num_children;
}



/* Private interface.  */

void
go_dock_band_drag_begin (GoDockBand *band, GoDockItem *item)
{
  GList *lp;
  GtkWidget *floating_widget;
  GtkWidget *item_widget;
  guint extra_offset = 0;

  DEBUG (("entering function"));

  item_widget = GTK_WIDGET (item);
  floating_widget = NULL;

  for (lp = band->children; lp != NULL;)
    {
      GoDockBandChild *c;

      c = lp->data;

      c->drag_allocation = c->widget->allocation;
      c->drag_offset = c->real_offset + extra_offset;
      c->drag_prev_space = c->prev_space;
      c->drag_foll_space = c->foll_space;

      c->offset = c->real_offset;

      if (c->widget == item_widget)
        {
          band->floating_child = lp;
          floating_widget = item_widget;
          if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
            extra_offset = c->widget->allocation.width + c->real_offset;
          else
            extra_offset = c->widget->allocation.height + c->real_offset;
        }
      else
        extra_offset = 0;

      if (lp->next == NULL)
        break;

      lp = lp->next;
    }

  if (floating_widget != NULL)
    {
      for (lp = band->floating_child->prev; lp != NULL; lp = lp->prev)
        {
          GoDockBandChild *c;

          c = lp->data;
          if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
            c->drag_foll_space += item_widget->requisition.width;
          else
            c->drag_foll_space += item_widget->requisition.height;
        }
      for (lp = band->floating_child->next; lp != NULL; lp = lp->next)
        {
          GoDockBandChild *c;

          c = lp->data;
          if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
            c->drag_prev_space += item_widget->requisition.width;
          else
            c->drag_prev_space += item_widget->requisition.height;
        }
    }

  band->doing_drag = TRUE;
  band->drag_allocation = GTK_WIDGET (band)->allocation;
}

gboolean
go_dock_band_drag_to (GoDockBand *band,
                         GoDockItem *item,
                         gint x, gint y)
{
  GtkAllocation *allocation;
  GList *where;
  gboolean is_empty;

  g_return_val_if_fail (band->doing_drag, FALSE);

  DEBUG (("%d %d", x, y));

  allocation = & GTK_WIDGET (band)->allocation;

  if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
    {
      if (x < allocation->x)
        x = allocation->x;
      if (x >= allocation->x + allocation->width)
        x = allocation->x + allocation->width - 1;
      where = find_where (band, x, &is_empty);
    }
  else
    {
      if (y < allocation->y)
        y = allocation->y;
      if (y >= allocation->y + allocation->height)
        y = allocation->y + allocation->height - 1;
      where = find_where (band, y, &is_empty);
    }

  {
    GList *p;

    for (p = next_if_floating (band, band->children);
         p != NULL;
         p = next_not_floating (band, p))
      {
        GoDockBandChild *c = p->data;

        c->real_offset = c->offset = c->drag_offset;
      }
  }

  if (is_empty)
    return dock_empty (band, item, where, x, y);
  else
    return dock_nonempty (band, item, where, x, y);
}

void
go_dock_band_drag_end (GoDockBand *band, GoDockItem *item)
{
  g_return_if_fail (band->doing_drag);

  DEBUG (("entering function"));

  if (band->floating_child != NULL)
    {
      GoDockBandChild *f;

      /* Minimal sanity check.  */
      f = (GoDockBandChild *) band->floating_child->data;
      g_return_if_fail (f->widget == GTK_WIDGET (item));

      gtk_widget_queue_resize (f->widget);
      band->floating_child = NULL;
    }

  band->doing_drag = FALSE;
  band->new_for_drag = FALSE;
}



/**
 * go_dock_band_get_item_by_name:
 * @band: A GoDockBand widget
 * @name: Name of the child to be retrieved
 * @position_return: Pointer to a variable holding the position of
 * the named child
 * @offset_return:  Pointer to a variable holding the offset of the
 * named child
 *
 * Description: Retrieve a named item from @band, and return its
 * position and offset in *@position_return and @offset_return.
 *
 * Return value: The child whose name is @name, or %NULL if no child
 * of @band has such name.
 **/
GoDockItem *
go_dock_band_get_item_by_name (GoDockBand *band,
                                  const char *name,
                                  guint *position_return,
                                  guint *offset_return)
{
  guint pos;
  GList *lp;

  for (lp = band->children, pos = 0; lp != NULL; lp = lp->next, pos++)
    {
      GoDockBandChild *c;

      c = lp->data;
      if (GO_IS_DOCK_ITEM (c->widget))
        {
          GoDockItem *item;

          item = GO_DOCK_ITEM (c->widget);
          if (strcmp (item->name, name) == 0)
            {
              if (position_return != NULL)
                *position_return = pos;
              if (offset_return != NULL)
                *offset_return = c->offset;
              return item;
            }
        }
    }

  return NULL;
}



void
go_dock_band_layout_add (GoDockBand *band,
                            GoDockLayout *layout,
                            GoDockPlacement placement,
                            guint band_num)
{
  guint child_num;
  GList *lp;

  for (lp = band->children, child_num = 0;
       lp != NULL;
       lp = lp->next, child_num++)
    {
      GoDockBandChild *child;
      GtkWidget *item;

      child = lp->data;
      item = child->widget;

      if (GO_IS_DOCK_ITEM (item))
        go_dock_layout_add_item (layout,
                                    GO_DOCK_ITEM (item),
                                    placement, band_num,
                                    child_num, child->offset);
    }
}

static GoDock *
get_dock (GtkWidget *widget)
{
	while (widget && !GO_IS_DOCK (widget))
		widget = widget->parent;

	return (GoDock *) widget;
}

gint
_bonobo_dock_band_handle_key_nav (GoDockBand *band,
				  GoDockItem *item,
				  GdkEventKey    *event)
{
  gboolean handled = FALSE;

  g_return_val_if_fail (GO_IS_DOCK_BAND (band), FALSE);
  g_return_val_if_fail (GO_IS_DOCK_ITEM (item), FALSE);

  if (event->state & GDK_CONTROL_MASK)
    {
      GList *l;
      int cur_idx = 0;
      int dest_idx;
      int num_children = g_list_length (band->children);

      for (l = band->children; l; l = l->next)
        {
          GoDockBandChild *child = l->data;
          if (child->widget == (GtkWidget *)item)
            break;
          cur_idx++;
	}

      g_return_val_if_fail (l != NULL, FALSE);

      dest_idx = cur_idx;
      if (band->orientation == GTK_ORIENTATION_HORIZONTAL)
        {
	  if (event->keyval == GDK_Left)

	      dest_idx--;
	  if (event->keyval == GDK_Right)
	      dest_idx++;
	}
      else
        {
	  if (event->keyval == GDK_Up)
	      dest_idx--;
	  if (event->keyval == GDK_Down)
	      dest_idx++;
	}

      if (dest_idx >= num_children)
	  dest_idx = num_children - 1;
      if (dest_idx < 0)
	  dest_idx = 0;
      if (dest_idx != cur_idx)
        {
          handled = TRUE;
	  go_dock_band_move_child (band, l, dest_idx);
	}
  }

  if (!handled)
    {
      GoDock *dock = get_dock (GTK_WIDGET (band));

      if (dock)
        handled = _bonobo_dock_handle_key_nav (dock, band, item, event);
    }

  return handled;
}

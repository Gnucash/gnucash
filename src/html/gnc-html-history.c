/********************************************************************
 * gnc-html-history.c -- keep a HTML history                        *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 ********************************************************************/

#include "config.h"

#include <gtk/gtk.h>
#include <string.h>

#include "gnc-html-history.h"

struct _gnc_html_history
{
    GList * nodes;
    GList * current_node;
    GList * last_node;

    /* call this whenever a node is destroyed */
    gnc_html_history_destroy_cb destroy_cb;
    gpointer                    destroy_cb_data;
};

/********************************************************************
 * gnc_html_history_new
 ********************************************************************/

gnc_html_history *
gnc_html_history_new(void)
{
    gnc_html_history * hist = g_new0(gnc_html_history, 1);
    hist->nodes         = NULL;
    hist->current_node  = NULL;
    hist->last_node     = NULL;
    return hist;
}



/********************************************************************
 * gnc_html_history_destroy
 ********************************************************************/

void
gnc_html_history_destroy(gnc_html_history * hist)
{
    GList * n;

    for (n = hist->nodes; n ; n = n->next)
    {
        if (hist->destroy_cb)
        {
            (hist->destroy_cb)((gnc_html_history_node *)n->data,
                               hist->destroy_cb_data);
        }
        gnc_html_history_node_destroy((gnc_html_history_node *)n->data);
    }
    g_list_free(hist->nodes);

    hist->nodes         = NULL;
    hist->current_node  = NULL;
    hist->last_node     = NULL;
    g_free(hist);
}

/********************************************************************
 * gnc_html_history_set_node_destroy_cb
 ********************************************************************/

void
gnc_html_history_set_node_destroy_cb(gnc_html_history * hist,
                                     gnc_html_history_destroy_cb cb,
                                     gpointer cb_data)
{
    hist->destroy_cb = cb;
    hist->destroy_cb_data = cb_data;
}

static int
g_strcmp(char * a, char * b)
{
    if (!a && b)
    {
        return 1;
    }
    else if (a && !b)
    {
        return -1;
    }
    else if (!a && !b)
    {
        return 0;
    }
    else
    {
        return strcmp(a, b);
    }

}


/********************************************************************
 * gnc_html_history_append
 ********************************************************************/
void
gnc_html_history_append(gnc_html_history * hist,
                        gnc_html_history_node * node)
{
    GList * n;
    gnc_html_history_node * hn;

    if (hist->current_node)
    {
        hn = hist->current_node->data;
        if ((hn->type == node->type) &&
                !g_strcmp(hn->location, node->location) &&
                !g_strcmp(hn->label, node->label))
        {
            if (hist->destroy_cb)
            {
                (hist->destroy_cb)(hn, hist->destroy_cb_data);
            }
            gnc_html_history_node_destroy(node);
            return;
        }

        /* blow away the history after this point, if there is one */
        for (n = hist->current_node->next; n; n = n->next)
        {
            if (hist->destroy_cb)
            {
                (hist->destroy_cb)((gnc_html_history_node *)n->data,
                                   hist->destroy_cb_data);
            }
            gnc_html_history_node_destroy((gnc_html_history_node *)n->data);
        }
        g_list_free(hist->current_node->next);
        hist->current_node->next = NULL;
        hist->last_node = hist->current_node;
    }

    n = g_list_alloc();
    n->data = (gpointer) node;
    n->next = NULL;
    n->prev = NULL;

    if (hist->nodes && hist->last_node)
    {
        n->prev               = hist->last_node;  /* back pointer */
        hist->last_node->next = n;                /* add n to the list */
        hist->last_node       = n;                /* n is last */
        hist->current_node    = n;
    }
    else
    {
        /* this is the first entry in the list */
        if (hist->nodes)
        {
            g_print ("???? hist->nodes non-NULL, but no last_node \n");
        }
        hist->nodes        = n;
        hist->last_node    = n;
        hist->current_node = n;
    }
}


/********************************************************************
 * gnc_html_history_get_current
 ********************************************************************/

gnc_html_history_node *
gnc_html_history_get_current(gnc_html_history * hist)
{
    if (!hist || !(hist->current_node)) return NULL;

    return hist->current_node->data;
}


/********************************************************************
 * gnc_html_history_forward
 ********************************************************************/

gnc_html_history_node *
gnc_html_history_forward(gnc_html_history * hist)
{
    if (!hist || !(hist->current_node))
    {
        return NULL;
    }

    if (hist->current_node->next)
    {
        hist->current_node = hist->current_node->next;
    }

    return hist->current_node->data;
}


/********************************************************************
 * gnc_html_history_back
 ********************************************************************/

gnc_html_history_node *
gnc_html_history_back(gnc_html_history * hist)
{

    if (!hist || !(hist->current_node))
    {
        return NULL;
    }

    if (hist->current_node->prev)
    {
        hist->current_node = hist->current_node->prev;
    }

    return hist->current_node->data;
}


/********************************************************************
 * gnc_html_history_back_p
 * is it possible to go back?
 ********************************************************************/

int
gnc_html_history_back_p(gnc_html_history * hist)
{
    if (hist && hist->current_node && hist->current_node->prev)
    {
        return TRUE;
    }
    else
    {
        return FALSE;
    }
}


/********************************************************************
 * gnc_html_history_forward_p
 * is it possible to go forward?
 ********************************************************************/

int
gnc_html_history_forward_p(gnc_html_history * hist)
{
    if (hist && hist->current_node && hist->current_node->next)
    {
        return TRUE;
    }
    else
    {
        return FALSE;
    }
}


/********************************************************************
 * gnc_html_history_node_new
 ********************************************************************/

gnc_html_history_node *
gnc_html_history_node_new(URLType type, const gchar * location,
                          const gchar * label)
{
    gnc_html_history_node * rv = g_new0(gnc_html_history_node, 1);

    rv->type      = type;
    rv->location  = g_strdup(location);
    rv->label     = g_strdup(label);
    return rv;
}


/********************************************************************
 * gnc_html_history_node_destroy
 ********************************************************************/

void
gnc_html_history_node_destroy(gnc_html_history_node * node)
{

    /* free the url resources and cached text */
    g_free(node->location);
    g_free(node->label);

    node->location = NULL;
    node->label    = NULL;

    g_free(node);
}

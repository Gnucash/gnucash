/********************************************************************\
 * window-help.c -- a help window for hypertext help.               *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
 * Copyright (C) 1999 Jeremy Collins ( gtk-xmhtml port )            *
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
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#include <gnome.h>

#include "config.h"

#include "window-help.h"
#include "window-html.h"
#include "Sheet.h"
#include "File.h"
#include "messages.h"
#include "util.h"

static short module = MOD_HTML; 

static HTMLWindow *helpwindow = NULL;


typedef struct _HelpData HelpData;
struct _HelpData
{
  gchar *htmlfile;
  gchar *label;
  gchar *text;
};


static HelpData *
help_data_new()
{
  HelpData *hd;
  
  hd = g_new0(HelpData, 1);

  return hd;
}

static void
help_data_destroy(HTMLHistoryData history_data)
{
  HelpData *hd = history_data;

  g_free(hd->htmlfile);
  hd->htmlfile = NULL;

  g_free(hd->label);
  hd->label = NULL;

  g_free(hd->text);
  hd->text = NULL;

  g_free(hd);
}

static void
help_data_set_file(HelpData *hd, const gchar *htmlfile)
{
  g_free(hd->htmlfile);
  hd->htmlfile = g_strdup(htmlfile);
}

static void
help_data_set_label(HelpData *hd, const gchar *label)
{
  g_free(hd->label);
  hd->label = g_strdup(label);
}

static void
help_data_set_text(HelpData *hd, const gchar *text)
{
  g_free(hd->text);
  hd->text = g_strdup(text);
}


static HTMLHistoryData
helpAnchorCB(XmHTMLAnchorCallbackStruct *acbs, HTMLHistoryData history_data)
{
  HelpData *hd;

  switch(acbs->url_type)
  {
    /* a local file with a possible jump to label */
    case ANCHOR_FILE_LOCAL:
      hd = help_data_new();
      help_data_set_file(hd, acbs->href);
      return hd;

    /* other types are unsupported, but it would be fun if they were ... */
    case ANCHOR_FTP:
      PERR(" this help window doesn't support ftp: %s\n", acbs->href);
      break;
    case ANCHOR_HTTP:
      PERR (" this help window doesn't support http: %s\n", acbs->href);
      break;
    case ANCHOR_MAILTO:
      PERR(" this help window doesn't support email: %s\n", acbs->href);
      break;
    case ANCHOR_UNKNOWN:
    default:
      PERR(" don't know this type of url: %s\n", acbs->href);
      break;
  }

  return NULL;
}

static void
helpJumpCB(HTMLHistoryData history_data, char **set_text, char **set_label)
{
  HelpData *hd = (HelpData *) history_data;
  char *text = NULL;
  char *label = NULL;

  *set_text = NULL;
  *set_label = NULL;

  if (hd->text != NULL)
  {
    *set_text = hd->text;
    *set_label = hd->label;
    return;
  }

  if (hd->htmlfile == NULL)
    return;

  /* see if this anchor contains a jump */
  label = strpbrk(hd->htmlfile, "#?");
  if (label != NULL)
  {
    help_data_set_label(hd, label);

    /* truncate # from name */
    hd->htmlfile[label - hd->htmlfile] = 0x0;
  }

  /* see if the anchor is an "active gnucash page" */
  if (strstr(hd->htmlfile, ".phtml"))
    text = gncReport(hd->htmlfile);

  /* if text to display wasn't specified, use the truncated name to read */
  if (text == NULL)
    text = gncReadFile(hd->htmlfile);

  if (text != NULL)
  {
    help_data_set_text(hd, text);
    free(text);
  }

  *set_text = hd->text;
  *set_label = hd->label;
}


/********************************************************************\
 * helpWindow                                                       * 
 *   opens up a help window, and displays html                      * 
 *                                                                  * 
 * Args:   parent   - the parent widget                             * 
 *         title    - the title of the window                       * 
 *         htmlfile - the file name of the help file to display     * 
 * Return: none                                                     * 
\********************************************************************/
void
helpWindow(GtkWidget *parent, const char *title, const char *htmlfile)
{
  HelpData *hd;

  if (helpwindow == NULL)
    helpwindow = gnc_html_window_new(help_data_destroy, helpAnchorCB,
                                     helpJumpCB);
 
  hd = help_data_new();
  help_data_set_file(hd, htmlfile);

  htmlWindow(parent, &helpwindow, title, hd, NULL, 0);
}


/********************************************************************\
 * gnc_ui_destroy_help_windows                                      * 
 *   destroys any open help windows                                 * 
 *                                                                  * 
 * Args:   none                                                     * 
 * Return: none                                                     * 
\********************************************************************/
void
gnc_ui_destroy_help_windows()
{
  gnc_html_window_destroy(helpwindow);
  helpwindow = NULL;

  DEBUG("help windows destroyed.\n");
}

/* ----------------------- END OF FILE ---------------------  */

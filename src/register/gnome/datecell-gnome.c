/********************************************************************\
 * datecell-gnome.c -- implement date cell handler in gnome         *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/*
 * FILE: datecell-gnome.c
 *
 * FUNCTION: Implement gnome portion of datecell widget
 *           embedded in a table cell.
 *
 * HISTORY:
 * Copyright (c) 2000 Dave Peticolas <dave@krondo.com>
 */

#include "config.h"

#include <ctype.h>
#include <gnome.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "datecell.h"
#include "gnc-engine-util.h"
#include "gnc-ui-util.h"
#include "gnucash-date-picker.h"
#include "gnucash-item-edit.h"
#include "gnucash-sheet.h"


#define DATE_BUF 30

typedef struct _PopBox
{
  GnucashSheet  *sheet;
  ItemEdit      *item_edit;
  GNCDatePicker *date_picker;

  gboolean signals_connected; /* date picker signals connected? */
  gboolean calendar_popped;   /* calendar is popped up? */
  gboolean in_date_select;

  struct tm date;
} PopBox;


static void block_picker_signals (DateCell *cell);
static void unblock_picker_signals (DateCell *cell);
static void realizeDate (BasicCell *bcell, gpointer w);
static void setDateCellValue (BasicCell *bcell, const char *value);
static void moveDate (BasicCell *bcell, VirtualLocation virt_loc);
static void destroyDate (BasicCell *bcell);
static void DateMV (BasicCell *_cell,
                    const GdkWChar *change,
                    int change_len,
                    const GdkWChar *newval,
                    int newval_len,
                    int *cursor_position,
                    int *start_selection,
                    int *end_selection);
static gboolean enterDate (BasicCell *bcell,
                           int *cursor_position,
                           int *start_selection,
                           int *end_selection);
static void leaveDate (BasicCell *bcell);

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GTK_REG;


/* ================================================ */

static void
xaccParseDate (struct tm *parsed, const char * datestr)
{
  int day, month, year;

  if (!parsed) return;
  if (!datestr) return;

  scanDate (datestr, &day, &month, &year);

  parsed->tm_mday = day;
  parsed->tm_mon  = month - 1;
  parsed->tm_year = year - 1900;
  parsed->tm_sec = 0;
  parsed->tm_min = 0;
  parsed->tm_hour = 0;
  parsed->tm_isdst = -1;

  if (mktime (parsed) == -1)
  {
    time_t secs = time (NULL);

    *parsed = *localtime (&secs);

    parsed->tm_sec = 0;
    parsed->tm_min = 0;
    parsed->tm_hour = 0;
    parsed->tm_isdst = -1;
  }

  mktime (parsed);
}

/* =============================================== */

static void
printDateCellDate (DateCell *cell, char *buff)
{
  PopBox *box = cell->cell.gui_private;

  printDate (buff,
             box->date.tm_mday,
             box->date.tm_mon + 1,
             box->date.tm_year+1900);
}

/* ================================================ */

static char *
DateCellHelpValue (BasicCell *bcell)
{
  DateCell *cell = (DateCell *) bcell;
  PopBox *box = cell->cell.gui_private;

  if ((bcell->value != NULL) && (bcell->value[0] != 0))
  {
    char string[1024];
    struct tm time;

    if (bcell->value != NULL)
      xaccParseDate (&time, bcell->value);
    else
    {
      time.tm_mday = box->date.tm_mday;
      time.tm_mon  = box->date.tm_mon;
      time.tm_year = box->date.tm_year;
      time.tm_sec = 0;
      time.tm_min = 0;
      time.tm_hour = 0;
      time.tm_isdst = -1;

      mktime (&time);
    }

    strftime (string, sizeof (string), "%A %d %B %Y", &time);

    return g_strdup (string);
  }

  if (bcell->blank_help != NULL)
    return g_strdup (bcell->blank_help);

  return NULL;
}

/* =============================================== */

static void
xaccInitDateCell (DateCell *cell)
{
  PopBox *box;
  time_t secs;
  char buff[DATE_BUF];

  xaccInitBasicCell (&(cell->cell));

  cell->cell.is_popup = TRUE;

  cell->cell.realize = realizeDate;
  cell->cell.destroy = destroyDate;
  cell->cell.modify_verify = DateMV;
  cell->cell.set_value = setDateCellValue;
  cell->cell.get_help_value = DateCellHelpValue;

  box = g_new0 (PopBox, 1);

  box->sheet = NULL;
  box->item_edit = NULL;
  box->date_picker = NULL;

  box->signals_connected = FALSE;
  box->calendar_popped = FALSE;
  box->in_date_select = FALSE;

  cell->cell.gui_private = box;

  /* default value is today's date */
  time (&secs);
  box->date = *localtime (&secs);
  printDateCellDate (cell, buff);

  xaccSetBasicCellValueInternal (&cell->cell, buff);
}

DateCell *
xaccMallocDateCell (void)
{
   DateCell *cell;

   cell = g_new0 (DateCell, 1);

   xaccInitDateCell (cell);

   return cell;
}

/* =============================================== */

static void
date_picked_cb (GNCDatePicker *gdp, gpointer data)
{
  DateCell *cell = data;
  PopBox *box = cell->cell.gui_private;
  guint day, month, year;
  char buffer[DATE_BUF];

  gtk_calendar_get_date (gdp->calendar, &year, &month, &day);

  printDate (buffer, day, month + 1, year);

  box->in_date_select = TRUE;
  gnucash_sheet_modify_current_cell (box->sheet, buffer);
  box->in_date_select = FALSE;

  item_edit_hide_popup (box->item_edit);
  box->calendar_popped = FALSE;
}

static void
date_selected_cb (GNCDatePicker *gdp, gpointer data)
{
  DateCell *cell = data;
  PopBox *box = cell->cell.gui_private;
  guint day, month, year;
  char buffer[DATE_BUF];

  gtk_calendar_get_date (gdp->calendar, &year, &month, &day);

  printDate (buffer, day, month + 1, year);

  box->in_date_select = TRUE;
  gnucash_sheet_modify_current_cell (box->sheet, buffer);
  box->in_date_select = FALSE;
}

static void
key_press_item_cb (GNCDatePicker *gdp, GdkEventKey *event, gpointer data)
{
  DateCell *cell = data;
  PopBox *box = cell->cell.gui_private;

  switch(event->keyval)
  {
    case GDK_Escape:
      item_edit_hide_popup (box->item_edit);
      box->calendar_popped = FALSE;
      break;

    default:
      gtk_widget_event(GTK_WIDGET (box->sheet), (GdkEvent *) event);
      break;
  }
}

static void
date_picker_disconnect_signals (DateCell *cell)
{
  PopBox *box = cell->cell.gui_private;

  if (!box->signals_connected)
    return;

  if (GTK_OBJECT_DESTROYED (GTK_OBJECT (box->date_picker)))
    return;

  gtk_signal_disconnect_by_data (GTK_OBJECT (box->date_picker), cell);

  box->signals_connected = FALSE;
}

static void
date_picker_connect_signals (DateCell *cell)
{
  PopBox *box = cell->cell.gui_private;

  if (box->signals_connected)
    return;

  if (GTK_OBJECT_DESTROYED (GTK_OBJECT (box->date_picker)))
    return;

  gtk_signal_connect (GTK_OBJECT(box->date_picker), "date_selected",
                      GTK_SIGNAL_FUNC(date_selected_cb), cell);

  gtk_signal_connect(GTK_OBJECT(box->date_picker), "date_picked",
                     GTK_SIGNAL_FUNC(date_picked_cb), cell);

  gtk_signal_connect(GTK_OBJECT(box->date_picker), "key_press_event",
                     GTK_SIGNAL_FUNC(key_press_item_cb), cell);

  box->signals_connected = TRUE;
}

static void
block_picker_signals (DateCell *cell)
{
  PopBox *box = cell->cell.gui_private;

  if (!box->signals_connected)
    return;

  gtk_signal_handler_block_by_data (GTK_OBJECT (box->date_picker), cell);
}

static void
unblock_picker_signals (DateCell *cell)
{
  PopBox *box = cell->cell.gui_private;

  if (!box->signals_connected)
    return;

  gtk_signal_handler_unblock_by_data (GTK_OBJECT (box->date_picker), cell);
}

/* =============================================== */

static void
destroyDate (BasicCell *bcell)
{
  PopBox *box = bcell->gui_private;
  DateCell *cell = (DateCell *) bcell;

  if (cell->cell.realize == NULL)
  {
    if (box != NULL && box->date_picker != NULL)
    {
      date_picker_disconnect_signals (cell);
      gtk_object_unref (GTK_OBJECT (box->date_picker));
      box->date_picker = NULL;
    }

    /* allow the widget to be shown again */
    cell->cell.realize = realizeDate;
    cell->cell.move = NULL;
    cell->cell.enter_cell = NULL;
    cell->cell.leave_cell = NULL;
    cell->cell.destroy = NULL;
  }

  DEBUG ("date destroyed\n");
}

/* =============================================== */

void
xaccDestroyDateCell (DateCell *cell)
{
  PopBox *box = cell->cell.gui_private;

  destroyDate (&(cell->cell));

  g_free (box);

  cell->cell.gui_private = NULL;
  cell->cell.realize = NULL;

  xaccDestroyBasicCell (&(cell->cell));
}

/* =============================================== */

void 
xaccSetDateCellValue (DateCell *cell, int day, int mon, int year)
{
  PopBox *box = cell->cell.gui_private;
  struct tm dada;
  char buff[DATE_BUF];

  dada.tm_mday = day;
  dada.tm_mon  = mon - 1;
  dada.tm_year = year - 1900;
  dada.tm_sec = 0;
  dada.tm_min = 0;
  dada.tm_hour = 0;
  dada.tm_isdst = -1;

  mktime (&dada);

  box->date.tm_mday = dada.tm_mday;
  box->date.tm_mon  = dada.tm_mon;
  box->date.tm_year = dada.tm_year;

  printDate (buff, dada.tm_mday, dada.tm_mon + 1, dada.tm_year + 1900);

  xaccSetBasicCellValueInternal (&cell->cell, buff);

  if (!box->date_picker)
    return;

  block_picker_signals (cell);
  gnc_date_picker_set_date (box->date_picker, day, mon - 1, year);
  unblock_picker_signals (cell);
}

/* =============================================== */

void 
xaccSetDateCellValueSecs (DateCell *cell, time_t secs)
{
  PopBox *box = cell->cell.gui_private;
  char buff[DATE_BUF];
  struct tm * stm;

  stm = localtime (&secs);
  box->date = *stm;

  printDate (buff,
             box->date.tm_mday, 
             box->date.tm_mon + 1, 
             box->date.tm_year + 1900);

  xaccSetBasicCellValueInternal (&cell->cell, buff);

  if (!box->date_picker)
    return;

  block_picker_signals (cell);
  gnc_date_picker_set_date (box->date_picker,
                            box->date.tm_mday,
                            box->date.tm_mon,
                            box->date.tm_year + 1900);
  unblock_picker_signals (cell);
}

/* ================================================ */

#define THIRTY_TWO_YEARS 0x3c30fc00LL

void 
xaccSetDateCellValueSecsL (DateCell *cell, long long secs)
{
  PopBox *box = cell->cell.gui_private;
  char buff[DATE_BUF];
  struct tm * stm;

  /* try to deal with dates earlier than December 1901 
   * or later than Jan 2038.  Note that xaccValidateDate
   * should be handling centential (non-) leap years.
   * The suffix LL indicates that consts should be handled
   * long long 64-bit consts.
   */
  if ((0x80000000LL > secs) || (0x7fffffffLL < secs)) 
  {
    int yrs;
    time_t rem;
    rem = secs % THIRTY_TWO_YEARS;
    yrs = secs / THIRTY_TWO_YEARS;
    stm = localtime (&rem);
    box->date = *stm;
    box->date.tm_year += 32 * yrs;
    box->date.tm_sec = 0;
    box->date.tm_min = 0;
    box->date.tm_hour = 0;
    box->date.tm_isdst = -1;
    mktime (&(box->date));
  }
  else
  {
    /* OK, time value is an unsigned 32-bit int */
    time_t sicko;
    sicko = secs;
    stm = localtime (&sicko);
    box->date = *stm;
    box->date.tm_sec = 0;
    box->date.tm_min = 0;
    box->date.tm_hour = 0;
    box->date.tm_isdst = -1;
    mktime (&(box->date));
  }

  printDate (buff,
             box->date.tm_mday,
             box->date.tm_mon + 1, 
             box->date.tm_year + 1900);

  xaccSetBasicCellValueInternal (&cell->cell, buff);

  if (!box->date_picker)
    return;

  block_picker_signals (cell);
  gnc_date_picker_set_date (box->date_picker,
                            box->date.tm_mday,
                            box->date.tm_mon,
                            box->date.tm_year + 1900);
  unblock_picker_signals (cell);
}

/* ================================================ */

void
xaccCommitDateCell (DateCell *cell)
{
  PopBox *box = cell->cell.gui_private;
  char buff[DATE_BUF];

  if (!cell)
    return;

  xaccParseDate (&(box->date), cell->cell.value);

  printDate (buff,
             box->date.tm_mday, 
             box->date.tm_mon + 1,
             box->date.tm_year + 1900);

  xaccSetBasicCellValueInternal (&cell->cell, buff);

  if (!box->date_picker)
    return;

  block_picker_signals (cell);
  gnc_date_picker_set_date (box->date_picker,
                            box->date.tm_mday,
                            box->date.tm_mon,
                            box->date.tm_year + 1900);
  unblock_picker_signals (cell);
}

/* =============================================== */

static void
DateMV (BasicCell *_cell,
        const GdkWChar *change,
        int change_len,
        const GdkWChar *newval,
        int newval_len,
        int *cursor_position,
        int *start_selection,
        int *end_selection)
{
  DateCell *cell = (DateCell *) _cell;
  PopBox *box = cell->cell.gui_private;
  gboolean accept = FALSE;
  char buff[DATE_BUF];
  struct tm *date;

  if (box->in_date_select)
  {
    char *newval_mb = gnc_wcstombs (newval);
    xaccSetBasicCellValue (_cell, newval_mb);
    g_free (newval_mb);
    return;
  }

  /* if user hit backspace, accept the change */
  if (change == NULL)
    accept = TRUE;
  else if (change_len == 0)
    accept = TRUE;
  else
  {
    int i, count = 0;
    char separator = dateSeparator ();
    gboolean ok = TRUE;

    for (i = 0; i < change_len; i++)
    {
      /* accept only numbers or a date separator. Note that the
       * separator of '-' (for DATE_FORMAT_ISO) takes precedence
       * over the accelerator below! */
      if (!isdigit (change[i]) && (separator != change[i]))
        ok = FALSE;

      if (separator == change[i])
        count++;
    }

    for (i = 0; i < _cell->value_len; i++)
      if (separator == _cell->value_w[i])
        count++;

    if (2 < count)
      ok = FALSE;

    if (ok)
      accept = TRUE;
  }

  /* keep a copy of the new value */
  if (accept)
  {
    char *newval_mb = gnc_wcstombs (newval);

    xaccSetBasicCellWCValueInternal (&cell->cell, newval);
    xaccParseDate (&(box->date), newval_mb);
    g_free (newval_mb);

    if (!box->date_picker)
      return;

    block_picker_signals (cell);
    gnc_date_picker_set_date (box->date_picker,
                              box->date.tm_mday,
                              box->date.tm_mon,
                              box->date.tm_year + 1900);
    unblock_picker_signals (cell);

    return;
  }

  /* otherwise, maybe its an accelerator key. */
  if (change_len != 1)
    return;

  date = &(box->date);

  /* handle accelerator keys */
  switch (change[0])
  {
    case '+':
    case '=':
      /* increment day */
      date->tm_mday++;
      break;

    case '_':
    case '-':
      /* decrement day */
      date->tm_mday--;
      break;

    case '}':
    case ']':
      /* increment month */
      date->tm_mon++;
      break;

    case '{':
    case '[':
      /* decrement month */
      date->tm_mon--;
      break;

    case 'M':
    case 'm':
      /* beginning of month */
      date->tm_mday = 1;
      break;

    case 'H':
    case 'h':
      /* end of month */
      date->tm_mon++;
      date->tm_mday = 0;
      break;

    case 'Y':
    case 'y':
      /* beginning of year */
      date->tm_mday = 1;
      date->tm_mon = 0;
      break;

    case 'R':
    case 'r':
      /* end of year */
      date->tm_mday = 31;
      date->tm_mon = 11;
      break;

    case 'T':
    case 't': {
      /* today */
      time_t secs;
      struct tm *now;

      time (&secs);
      now = localtime (&secs);
      *date = *now;
      break;
    }

    default:
      /* reject other changes */
      return;
  }

  date->tm_isdst = -1;

  mktime (date);

  printDate (buff, date->tm_mday, date->tm_mon + 1, date->tm_year + 1900);

  xaccSetBasicCellValueInternal (&cell->cell, buff);

  if (!box->date_picker)
    return;

  block_picker_signals (cell);
  gnc_date_picker_set_date (box->date_picker,
                            box->date.tm_mday,
                            box->date.tm_mon,
                            box->date.tm_year + 1900);
  unblock_picker_signals (cell);
}

/* =============================================== */

static void
realizeDate (BasicCell *bcell, gpointer data)
{
  GnucashSheet *sheet = data;
  GnomeCanvasItem *item = sheet->item_editor;
  ItemEdit *item_edit = ITEM_EDIT (item);
  DateCell *cell = (DateCell *) bcell;
  PopBox *box = cell->cell.gui_private;

  /* initialize gui-specific, private data */
  box->sheet = sheet;
  box->item_edit = item_edit;
  box->date_picker = item_edit_new_date_picker (box->item_edit);
  gtk_object_ref (GTK_OBJECT(box->date_picker));
  gtk_object_sink (GTK_OBJECT(box->date_picker));

  /* to mark cell as realized, remove the realize method */
  cell->cell.realize = NULL;
  cell->cell.move = moveDate;
  cell->cell.enter_cell = enterDate;
  cell->cell.leave_cell = leaveDate;
}

/* =============================================== */

static void
moveDate (BasicCell *bcell, VirtualLocation virt_loc)
{
  PopBox *box = bcell->gui_private;

  date_picker_disconnect_signals ((DateCell *) bcell);

  item_edit_set_popup (box->item_edit, NULL, NULL,
                       NULL, NULL, NULL, NULL, NULL);

  box->calendar_popped = FALSE;
}

/* =============================================== */

static int
get_popup_height (GnomeCanvasItem *item,
                  int space_available,
                  int row_height,
                  gpointer user_data)
{
  GtkWidget *cal = GTK_WIDGET (GNC_DATE_PICKER (item)->calendar);
  GtkRequisition req;

  req.height = 0;
  req.width = 0;

  gtk_widget_size_request (cal, &req);

  return req.height;
}

static void
popup_set_focus (GnomeCanvasItem *item,
                 gpointer user_data)
{
  gtk_widget_grab_focus (GTK_WIDGET (GNC_DATE_PICKER (item)->calendar));
}

static gboolean
enterDate (BasicCell *bcell,
           int *cursor_position,
           int *start_selection,
           int *end_selection)
{
  DateCell *cell = (DateCell *) bcell;
  PopBox *box = bcell->gui_private;

  item_edit_set_popup (box->item_edit, GNOME_CANVAS_ITEM (box->date_picker),
                       get_popup_height, NULL, popup_set_focus,
                       NULL, NULL, NULL);

  block_picker_signals (cell);
  gnc_date_picker_set_date (box->date_picker,
                            box->date.tm_mday,
                            box->date.tm_mon,
                            box->date.tm_year + 1900);
  unblock_picker_signals (cell);

  date_picker_connect_signals ((DateCell *) bcell);

  return TRUE;
}

/* =============================================== */

static void
leaveDate (BasicCell *bcell)
{
  PopBox *box = bcell->gui_private;

  date_picker_disconnect_signals ((DateCell *) bcell);

  item_edit_set_popup (box->item_edit, NULL, NULL,
                       NULL, NULL, NULL, NULL, NULL);

  box->calendar_popped = FALSE;
}

/* ================================================ */

void
xaccDateCellGetDate (DateCell *cell, Timespec *ts)
{
  PopBox *box = cell->cell.gui_private;

  if (!cell || !ts)
    return;

  xaccParseDate (&(box->date), cell->cell.value);

  ts->tv_sec = mktime (&box->date);
  ts->tv_nsec = 0;
}

/* ================================================ */

static void 
setDateCellValue (BasicCell *_cell, const char *str)
{
  DateCell *cell = (DateCell *) _cell;
  PopBox *box = cell->cell.gui_private;
  char buff[DATE_BUF];

  xaccParseDate (&(box->date), str);

  printDate (buff,
             box->date.tm_mday, 
             box->date.tm_mon + 1, 
             box->date.tm_year + 1900);

  xaccSetBasicCellValueInternal (_cell, buff);

  if (!box->date_picker)
    return;

  block_picker_signals (cell);
  gnc_date_picker_set_date (box->date_picker,
                            box->date.tm_mday,
                            box->date.tm_mon,
                            box->date.tm_year + 1900);
  unblock_picker_signals (cell);
}

/* =============== end of file =================== */

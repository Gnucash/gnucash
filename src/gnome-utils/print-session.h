/********************************************************************\
 * print-session.h -- data structures for printing via gnome print  *  
 *                       (GnuCash)                                  *
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
\********************************************************************/

#ifndef PRINT_SESSION_H
#define PRINT_SESSION_H

/** @addtogroup Printing
    @{ */
/** @file print-session.h
    @brief Functions for printing
    @author Copyright (C) 2000 Bill Gribble <grib@billgribble.com>
*/

/** @addtogroup Basic Session Functions
    @{ */

#ifdef HAVE_GTK_2_10

#include <gtk/gtkprintoperation.h>

/**
 * Retrieve the print settings from the GtkPrintOperation @a op and save them in
 * a static variable.
 *
 * @param op non-NULL print operation
 */
void gnc_print_operation_save_print_settings(GtkPrintOperation *op);

/**
 * If print settings have been saved by
 * gnc_print_operation_save_print_settings(), then set them on the given
 * GtkPrintOperation @a op.  Set the default page setup as well.
 *
 * @param op non-NULL print operation
 */
void gnc_print_operation_init(GtkPrintOperation *op);

/**
 * Run a page setup dialog and save the resulting GtkPageSetup in a static
 * variable.
 *
 * @param parent Transient parent, or NULL
 */
void gnc_ui_page_setup(GtkWindow *parent);

#endif  /* HAVE_GTK_2_10 */


#ifndef GTKHTML_USES_GTKPRINT

#include <libgnomeprint/gnome-print.h>
#include <libgnomeprint/gnome-print-job.h>
#include <libgnomeprintui/gnome-print-dialog.h>
#include <libgnomeprintui/gnome-print-preview.h>

typedef struct {
  gboolean             hand_built_pages;
  gint                 print_type;

  GnomePrintJob      * job;
  GnomePrintContext  * context;		/* Convenience only. Owned by the job */
  GnomeFont          * default_font;
} PrintSession;


/** Create a new print 'session'.  Once created, a series of commands
 *  can be issued on the session to create the output page.  The
 *  output will be printed when the session is done.  This function
 *  will present the standard print/preview selection box to the user
 *  and wait for the result.
 *
 *  If the hand_built_pages argument is set to TRUE, this function
 *  will perform a couple of extra setup steps.  Specifically it will
 *  call the gnome begin page, set color and set font functions.  The
 *  code will also call close page when the #gnc_print_session_done
 *  function is called.
 *
 *  @param hand_built_pages If TRUE, this funciton will perform extra setup.
 *
 *  @return A pointer to the data structure describing this print session.
 */
PrintSession * gnc_print_session_create(gboolean hand_built_pages);


/** Destroy a print 'session' without producing any output.
 *
 *  @param ps A pointer to the session to be destroyed.
 */
void gnc_print_session_destroy(PrintSession * ps);


/** Finish a print 'session'.  The output from this session will be
 *  printed to the device selected when the session was created.
 *
 *  @param ps A pointer to the session to be closed.
 */
void gnc_print_session_done(PrintSession * ps);

#endif  /* !GTKHTML_USES_GTKPRINT */

/** @} */
/** @} */

#endif

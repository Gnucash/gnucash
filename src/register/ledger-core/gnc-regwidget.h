/********************************************************************\
 * gnc-regwidget.h -- A widget for the common register look-n-feel. *
 * Copyright (C) 2001 Joshua Sled <jsled@asynchronous.org>          *
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
\********************************************************************/

#ifndef GNC_REGWIDGET_H
#define GNC_REGWIDGET_H

#include <g-wrap-runtime-guile.h>
#include <gtk/gtk.h>
#include "config.h"

#include "gnc-ledger-display.h"
#include "gnucash-sheet.h"

#define GNC_REGWIDGET(obj)         GTK_CHECK_CAST( obj, gnc_regWidget_get_type(), GNCRegWidget )
#define GNC_REGWIDGET_CLASS(klass) GTK_CHECK_CLASS_CAST( klass, gnc_regWidget_get_type(), GNCRegWidgetClass )
#define IS_GNC_REGWIDGET(obj)      GTK_CHECK_TYPE( obj, gnc_regWidget_get_type() )

typedef struct _GNCRegWidget GNCRegWidget;
typedef struct _GNCRegWidgetClass GNCRegWidgetClass;

struct _GNCRegWidget {
  /* The "parent" widget */
  GtkVBox vbox;

  /* Top level window */
  /* jsled: used by the jump cbs; can we get by w/o knowing this? inv. layering */
  GtkWidget * window;
  gint width;
  gint height;

  SCM toolbar_change_callback_id;
  GtkWidget * toolbar;
  GtkWidget * statusbar;

  GtkWidget * popup_menu;

  GtkWidget * style_menu;
  GtkWidget * sort_menu;
  GtkWidget * edit_menu;
  GtkWidget * transaction_menu;
  GtkWidget * help_menu;

  GtkWidget * double_line_check;

  GtkWidget * split_button;
  GtkWidget * split_menu_check;
  GtkWidget * split_popup_check;

  GNCLedgerDisplay * ledger;
  /* The actual sheet widget */
  GnucashRegister *reg;

  sort_type_t sort_type;
};

struct _GNCRegWidgetClass {
  GtkVBoxClass parent_class;

  /* Signal prototype holders */
  void (*enter_ent_cb)( GNCRegWidget *w, gpointer user_data );
  void (*cancel_ent_cb)( GNCRegWidget *w, gpointer user_data );
  void (*delete_ent_cb)( GNCRegWidget *w, gpointer user_data );
  void (*dup_ent_cb)( GNCRegWidget *w, gpointer user_data );
  void (*expand_ent_cb)( GNCRegWidget *w, gpointer user_data );
  void (*blank_cb)( GNCRegWidget *w, gpointer user_data );
  void (*jump_cb)( GNCRegWidget *w, gpointer user_data );
};

guint gnc_regWidget_get_type( void );

GtkWidget *gnc_regWidget_new( GNCLedgerDisplay *ld, GtkWindow *parent );

GnucashSheet *gnc_regWidget_get_sheet( GNCRegWidget *rw );

GtkWidget *gnc_regWidget_get_style_menu( GNCRegWidget *rw );
GtkWidget *gnc_regWidget_get_sort_menu( GNCRegWidget *rw );
GtkWidget *gnc_regWidget_get_edit_menu( GNCRegWidget *rw );
GtkWidget *gnc_regWidget_get_transaction_menu( GNCRegWidget *rw );
GtkWidget *gnc_regWidget_get_help_menu( GNCRegWidget *rw );

GtkWidget *gnc_regWidget_get_toolbar( GNCRegWidget *rw );
GtkWidget *gnc_regWidget_get_statusbar( GNCRegWidget *rw );
GtkWidget *gnc_regWidget_get_popup( GNCRegWidget *rw );

#endif /* !defined(GNC_REGWIDGET_H) */

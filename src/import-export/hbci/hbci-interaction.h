/* hbci-interactor.h
   Copyright 2002 by Christian Stimming <stimming@tuhh.de> */

/***************************************************************************
 *                                                                         *
 *   This library is free software; you can redistribute it and/or         *
 *   modify it under the terms of the GNU Lesser General Public            *
 *   License as published by the Free Software Foundation; either          *
 *   version 2.1 of the License, or (at your option) any later version.    *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   Lesser General Public License for more details.                       *
 *                                                                         *
 *   You should have received a copy of the GNU Lesser General Public      *
 *   License along with this library; if not, write to the Free Software   *
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *
 *   MA  02110-1301  USA                                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef HBCI_INTERACTION_H
#define HBCI_INTERACTION_H

#include <aqbanking/banking.h>
#include <gnome.h>

#define GCONF_SECTION "dialogs/import/hbci"
#define KEY_CLOSE_ON_FINISH "close_on_finish"
#define KEY_REMEMBER_PIN    "remember_pin"
#define KEY_VERBOSE_DEBUG   "verbose_debug"

typedef struct _inter_data GNCInteractor;

/** Adds the interactor and progressmonitor classes to the api. */
GNCInteractor *gnc_AB_BANKING_interactors (AB_BANKING *api, GtkWidget *parent);

gboolean GNCInteractor_aborted(const GNCInteractor *i);
void GNCInteractor_show(GNCInteractor *i);
void GNCInteractor_show_nodelete(GNCInteractor *i);
void GNCInteractor_hide(GNCInteractor *i);
void GNCInteractor_delete(GNCInteractor *i);
void GNCInteractor_erasePIN(GNCInteractor *i);
void GNCInteractor_reparent (GNCInteractor *i, GtkWidget *new_parent);
gboolean GNCInteractor_get_cache_valid(const GNCInteractor *i);
void GNCInteractor_set_cache_valid(GNCInteractor *i, gboolean value);
GtkWidget *GNCInteractor_parent(GNCInteractor *i);
void GNCInteractor_add_log_text (GNCInteractor *i, const char *msg);
gboolean GNCInteractor_hadErrors (const GNCInteractor *i);


#endif

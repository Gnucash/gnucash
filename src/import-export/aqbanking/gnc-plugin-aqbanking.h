/*
 * gnc-plugin-aqbanking.h --
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/**
 * @addtogroup Import_Export
 * @{
 * @addtogroup AqBanking
 * @{
 * @file gnc-plugin-aqbanking.h
 * @brief Plugin registration of the AqBanking module
 * @author Copyright (C) 2003 David Hampton <hampton@employees.org>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#ifndef GNC_PLUGIN_AQBANKING_H
#define GNC_PLUGIN_AQBANKING_H

#include <glib.h>

#include "gnc-plugin.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_AQBANKING            (gnc_plugin_aqbanking_get_type())
#define GNC_PLUGIN_AQBANKING(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GNC_TYPE_PLUGIN_AQBANKING, GncPluginAqBanking))
#define GNC_PLUGIN_AQBANKING_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass), GNC_TYPE_PLUGIN_AQBANKING, GncPluginAqBankingClass))
#define GNC_IS_PLUGIN_AQBANKING(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), GNC_TYPE_PLUGIN_AQBANKING))
#define GNC_IS_PLUGIN_AQBANKING_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass), GNC_TYPE_PLUGIN_AQBANKING))
#define GNC_PLUGIN_AQBANKING_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), GNC_TYPE_PLUGIN_AQBANKING, GncPluginAqBankingClass))

#define GNC_PLUGIN_AQBANKING_NAME "gnc-plugin-aqbanking"

/* typedefs & structures */
typedef struct
{
    GncPlugin gnc_plugin;
} GncPluginAqBanking;

typedef struct
{
    GncPluginClass gnc_plugin;
} GncPluginAqBankingClass;

/* function prototypes */

/**
 * @return The glib runtime type of an aqbanking plugin page
 **/
GType gnc_plugin_aqbanking_get_type(void);

/**
 * @return A new GncPluginAqBanking object
 */
GncPlugin* gnc_plugin_aqbanking_new(void);

/**
 * Create a new GncPluginAqBanking object and register it.
 */
void gnc_plugin_aqbanking_create_plugin(void);

G_END_DECLS

/** @} */
/** @} */

#endif /* GNC_PLUGIN_AQBANKING_H */

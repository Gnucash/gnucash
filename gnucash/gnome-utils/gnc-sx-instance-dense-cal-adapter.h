/*
 * gnc-sx-instance-dense-cal-adapter.h
 *
 * Copyright (C) 2006 Josh Sled <jsled@asynchronous.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 and/or version 3 of the GNU General Public
 * License as published by the Free Software Foundation.
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

#ifndef _GNC_SX_INSTANCE_DENSE_CAL_ADAPTER_H
#define _GNC_SX_INSTANCE_DENSE_CAL_ADAPTER_H

#include <config.h>
#include <glib.h>
#include "gnc-sx-instance-model.h"

#ifdef __cplusplus
extern "C" {
#endif

#define GNC_TYPE_SX_INSTANCE_DENSE_CAL_ADAPTER	          (gnc_sx_instance_dense_cal_adapter_get_type ())
G_DECLARE_FINAL_TYPE (GncSxInstanceDenseCalAdapter, gnc_sx_instance_dense_cal_adapter, GNC, SX_INSTANCE_DENSE_CAL_ADAPTER, GObject)

GncSxInstanceDenseCalAdapter* gnc_sx_instance_dense_cal_adapter_new(GncSxInstanceModel *instances);

#ifdef __cplusplus
}
#endif

#endif // _GNC_SX_INSTANCE_DENSE_CAL_ADAPTER_H

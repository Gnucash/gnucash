/*
 * gnc-sx-instance-dense-cal-adapter.h
 *
 * Copyright (C) 2006 Josh Sled <jsled@asynchronous.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
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

#include "config.h"
#include <glib.h>
#include "gnc-sx-instance-model.h"

typedef struct _GncSxInstanceDenseCalAdapterClass GncSxInstanceDenseCalAdapterClass;
typedef struct _GncSxInstanceDenseCalAdapter GncSxInstanceDenseCalAdapter;

#define GNC_TYPE_SX_INSTANCE_DENSE_CAL_ADAPTER	          (gnc_sx_instance_dense_cal_adapter_get_type ())
#define GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(obj)	          (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_SX_INSTANCE_DENSE_CAL_ADAPTER, GncSxInstanceDenseCalAdapter))
#define GNC_SX_INSTANCE_DENSE_CAL_ADAPTER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_SX_INSTANCE_DENSE_CAL_ADAPTER, GncSxInstanceDenseCalAdapterClass))
#define GNC_IS_SX_INSTANCE_DENSE_CAL_ADAPTER(obj)	  (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_SX_INSTANCE_DENSE_CAL_ADAPTER))
#define GNC_IS_SX_INSTANCE_DENSE_CAL_ADAPTER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_SX_INSTANCE_DENSE_CAL_ADAPTER))
#define GNC_SX_INSTANCE_DENSE_CAL_ADAPTER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_SX_INSTANCE_DENSE_CAL_ADAPTER, GncSxInstanceDenseCalAdapterClass))

GncSxInstanceDenseCalAdapter* gnc_sx_instance_dense_cal_adapter_new(GncSxInstanceModel *instances);
GType gnc_sx_instance_dense_cal_adapter_get_type(void);

#endif // _GNC_SX_INSTANCE_DENSE_CAL_ADAPTER_H

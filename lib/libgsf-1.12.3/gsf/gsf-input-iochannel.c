/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-input-iochannel.c: GIOChannel based input
 *
 * Copyright (C) 2003-2004 Rodrigo Moya (rodrigo@gnome-db.org)
 * Copyright (C) 2003-2004 Dom Lachowicz (cinamod@hotmail.com)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2.1 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <gsf-config.h>
#include <gsf-input-iochannel.h>

/**
 * gsf_input_memory_new_from_iochannel :
 * @channel : a #GIOChannel.
 * @error : a #GError
 *
 * Returns a new #GsfInputMemory or NULL.
 */
GsfInput * 
gsf_input_memory_new_from_iochannel (GIOChannel *channel,
				     GError **err)
{
	gchar *buf;
	gsize  len;

	g_return_val_if_fail (channel != NULL, NULL);

	if (G_IO_STATUS_NORMAL != g_io_channel_read_to_end (channel, &buf, &len, err))
		return NULL;

	return gsf_input_memory_new (buf, len, TRUE);
}

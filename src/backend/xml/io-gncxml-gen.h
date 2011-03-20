/********************************************************************\
 * gnc-xml-gen.h -- api for gnucash xml i/o                         *
 *                                                                  *
 * Copyright (C) 2001 James LewisMoss <dres@debian.org>             *
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
 *                                                                  *
\********************************************************************/

#ifndef IO_GNCXML_GEN_H
#define IO_GNCXML_GEN_H

#include <glib.h>
#include "sixtp.h"

typedef gboolean (*gxpf_callback)(const char *tag, gpointer parsedata,
                                  gpointer data);

struct gxpf_data_struct
{
    gxpf_callback cb;
    gpointer parsedata;
    gpointer bookdata;
};

typedef struct gxpf_data_struct gxpf_data;

gboolean
gnc_xml_parse_file(sixtp *top_parser, const char *filename,
                   gxpf_callback callback, gpointer parsedata,
                   gpointer bookdata);

#endif /* IO_GNCXML_GEN_H */

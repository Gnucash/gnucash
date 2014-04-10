/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-msole-impl.h: 
 *
 * Copyright (C) 2002-2004 Jody Goldberg (jody@gnome.org)
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

#ifndef GSF_MSOLE_IMPL_H
#define GSF_MSOLE_IMPL_H

#include <gsf/gsf.h>

G_BEGIN_DECLS

#define OLE_HEADER_SIZE		 0x200	/* independent of big block size size */
#define OLE_HEADER_SIGNATURE	 0x00
#define OLE_HEADER_CLSID	 0x08	/* See ReadClassStg */
#define OLE_HEADER_MINOR_VER	 0x18	/* 0x33 and 0x3e have been seen */
#define OLE_HEADER_MAJOR_VER	 0x1a	/* 0x3 been seen in wild */
#define OLE_HEADER_BYTE_ORDER	 0x1c	/* 0xfe 0xff == Intel Little Endian */
#define OLE_HEADER_BB_SHIFT      0x1e
#define OLE_HEADER_SB_SHIFT      0x20
/* 0x22..0x27 reserved == 0 */
#define OLE_HEADER_CSECTDIR	 0x28
#define OLE_HEADER_NUM_BAT	 0x2c
#define OLE_HEADER_DIRENT_START  0x30
/* 0x34..0x37 transacting signature must be 0 */
#define OLE_HEADER_THRESHOLD	 0x38
#define OLE_HEADER_SBAT_START    0x3c
#define OLE_HEADER_NUM_SBAT      0x40
#define OLE_HEADER_METABAT_BLOCK 0x44
#define OLE_HEADER_NUM_METABAT   0x48
#define OLE_HEADER_START_BAT	 0x4c
#define BAT_INDEX_SIZE		 4
#define OLE_HEADER_METABAT_SIZE	 ((OLE_HEADER_SIZE - OLE_HEADER_START_BAT) / BAT_INDEX_SIZE)

#define DIRENT_MAX_NAME_SIZE	0x40
#define DIRENT_DETAILS_SIZE	0x40
#define DIRENT_SIZE		(DIRENT_MAX_NAME_SIZE + DIRENT_DETAILS_SIZE)
#define DIRENT_NAME_LEN		0x40	/* length in bytes incl 0 terminator */
#define DIRENT_TYPE		0x42
#define DIRENT_COLOUR		0x43
#define DIRENT_PREV		0x44
#define DIRENT_NEXT		0x48
#define DIRENT_CHILD		0x4c
#define DIRENT_CLSID		0x50	/* only for dirs */
#define DIRENT_USERFLAGS	0x60	/* only for dirs */
#define DIRENT_CREATE_TIME	0x64	/* for files */
#define DIRENT_MODIFY_TIME	0x6c	/* for files */
#define DIRENT_FIRSTBLOCK	0x74
#define DIRENT_FILE_SIZE	0x78
/* 0x7c..0x7f reserved == 0 */

#define DIRENT_TYPE_INVALID	0
#define DIRENT_TYPE_DIR		1
#define DIRENT_TYPE_FILE	2
#define DIRENT_TYPE_LOCKBYTES	3	/* ? */
#define DIRENT_TYPE_PROPERTY	4	/* ? */
#define DIRENT_TYPE_ROOTDIR	5
#define DIRENT_MAGIC_END	0xffffffff

/* flags in the block allocation list to denote special blocks */
#define BAT_MAGIC_UNUSED	0xffffffff	/*		   -1 */
#define BAT_MAGIC_END_OF_CHAIN	0xfffffffe	/*		   -2 */
#define BAT_MAGIC_BAT		0xfffffffd	/* a bat block,    -3 */
#define BAT_MAGIC_METABAT	0xfffffffc	/* a metabat block -4 */

G_END_DECLS

#endif /* GSF_MSOLE_IMPL_H */

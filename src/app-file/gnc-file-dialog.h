/********************************************************************\
 * gnc-file-dialog.c -- the file dialog dialog                      *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (c) 2001 Linux Developers Group                        *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#ifndef GNC_FILE_DIALOG_H
#define GNC_FILE_DIALOG_H

#include "config.h"

/** PROTOTYPES ******************************************************/
const char * gnc_file_dialog (const char * title,
                              const char * filter,
                              const char * default_name);

#endif

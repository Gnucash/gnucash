/*******************************************************************\
 * gnc-common.h -- define platform independent items                *
 *                                                                  *
 * Copyright (C) 1999 Rob Browning                                  *
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
 *                                                                  *
\********************************************************************/

#ifndef __GNC_COMMON_H__
#define __GNC_COMMON_H__

#if defined(GNOME)
  #include <gtk/gtk.h>
#elif defined(MOTIF)
  #include <Xm/Xm.h>
#endif

typedef char gncBoolean;
#define GNC_F 0
#define GNC_T (! GNC_F)

typedef unsigned int uint32;

#if defined(GNOME)
  typedef GtkWidget *gncUIWidget;
#elif defined(MOTIF)
  typedef Widget gncUIWidget;
#elif defined(KDE)
  typedef void *gncUIWidget;
#endif

#endif

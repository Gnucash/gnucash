/********************************************************************\
 * QuickFill.h -- the quickfill tree data structure                 *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
 * Copyright (C) 2000 Dave Peticolas                                *
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
 *                                                                  *
\********************************************************************/

#ifndef __QUICKFILL_H__
#define __QUICKFILL_H__

#include "config.h"

#include <gdk/gdk.h>
#include <glib.h>


typedef enum
{
  QUICKFILL_LIFO,
  QUICKFILL_ALPHA
} QuickFillSort;

typedef struct _QuickFill QuickFill;


/** PROTOTYPES ******************************************************/

QuickFill *  gnc_quickfill_new (void);
void         gnc_quickfill_destroy (QuickFill *qf);

const char * gnc_quickfill_string (QuickFill *qf);

QuickFill *  gnc_quickfill_get_char_match (QuickFill *qf, GdkWChar wc);

QuickFill *  gnc_quickfill_get_string_match (QuickFill *qf,
                                             const GdkWChar *str);

QuickFill *  gnc_quickfill_get_string_len_match (QuickFill *qf,
                                                 const GdkWChar *str, int len);

QuickFill *  gnc_quickfill_get_unique_len_match (QuickFill *qf, int *len);

void         gnc_quickfill_insert (QuickFill *qf, const char *text,
                                   QuickFillSort sort_code);

void         gnc_quickfill_insert_wc (QuickFill *qf, const GdkWChar *text,
                                      QuickFillSort sort_code);

#endif /* __QUICKFILL_H__ */

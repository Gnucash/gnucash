/********************************************************************\
 * io-example-account.h -- api for example accounts                 *
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

#ifndef IO_EXAMPLE_ACCOUNT_H
#define IO_EXAMPLE_ACCOUNT_H

#include <glib.h>

#include "sixtp.h"
#include "gnc-engine.h"

struct GncExampleAccount_struct
{
    gchar *title;
    gchar *filename;
    QofBook *book;
    Account *root;
    gchar *short_description;
    gchar *long_description;
    gboolean exclude_from_select_all;
    gboolean start_selected;
};
typedef struct GncExampleAccount_struct GncExampleAccount;

void gnc_destroy_example_account(GncExampleAccount *gea);

gboolean gnc_write_example_account(GncExampleAccount *gea,
                                   const gchar *filename);
GncExampleAccount *gnc_read_example_account(const gchar *filename);


void gnc_free_example_account_list(GSList *list);
GSList* gnc_load_example_account_list(const char *dirname);

gboolean gnc_is_example_account_xml(const gchar *name);

#endif /* IO_EXAMPLE_ACCOUNT_H */

/********************************************************************
 * gnc-gpg.h -- encrypt/decrypt data using GPG and the gnucash      *
 * keyrings                                                         *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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
 ********************************************************************/

#ifndef __GNC_GPG_H__
#define __GNC_GPG_H__

#include "config.h"

void  gnc_gpg_init(void);
char  * gnc_gpg_export(const gchar * keyname);
char  * gnc_gpg_encrypt(const gchar * cleartext, int cleartext_size, 
                        const gchar * recipient, const gchar * passphrase);
char  * gnc_gpg_decrypt(const gchar * cleartext, int cleartext_size,
                        const gchar * passphrase);
void  gnc_gpg_make_keypair(const gchar * name, const gchar * id,
                           const gchar * email, const gchar * passphrase);

#endif

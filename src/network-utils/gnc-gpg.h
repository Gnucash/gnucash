/********************************************************************
 * gnc-gpg.h -- encrypt/decrypt data using GPG and the gnucash      *
 * keyrings                                                         *
 * Copyright (C) 2000-2001 Bill Gribble <grib@billgribble.com>      *
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

#ifndef GNC_GPG_H
#define GNC_GPG_H

#include <glib.h>

typedef void (* GncGPGCb)(char * output, gpointer data);

char  * gnc_gpg_export(const gchar * keyname);
void    gnc_gpg_export_async(const gchar * keyname, GncGPGCb cb, 
                             gpointer data);

char  * gnc_gpg_encrypt(const gchar * cleartext, int cleartext_size, 
                        const gchar * recipient, const gchar * passphrase);
void    gnc_gpg_encrypt_async(const gchar * cleartext, int cleartext_size, 
                              const gchar * recipient, const gchar * pp,
                              GncGPGCb cb, gpointer data);

char  * gnc_gpg_decrypt(const gchar * crypttext, int crypttext_size,
                        const gchar * passphrase);
void    gnc_gpg_decrypt_async(const gchar * crypttext, int crypttext_size,
                              const gchar * passphrase,
                              GncGPGCb cb, gpointer data);

char *  gnc_gpg_make_keypair(const gchar * name, const gchar * id,
                             const gchar * email, const gchar * passphrase);
void    gnc_gpg_make_keypair_async(const gchar * name, const gchar * id,
                                   const gchar * email, const gchar * pp,
                                   GncGPGCb cb, gpointer data);


#endif

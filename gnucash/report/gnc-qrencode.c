/********************************************************************\
 * gnc-qrencode.c -- link to libqrencode                            *
 * Copyright (C) 2021 Christopher Lam                               *
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
\********************************************************************/

#include <config.h>
#include <qrencode.h>
#include <stdio.h>

#include "gnc-qrencode.h"
#include "gnc-engine.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = "gnc.qrencode";

SCM gnc_qrcode_encodestring (const char *str)
{
    QRcode *qr = QRcode_encodeString (str, 0, QR_ECLEVEL_M, QR_MODE_8, FALSE);
    SCM output = SCM_EOL;

    if (!qr)
    {
        PERR ("QRcode_encodeString failed: %s", strerror (errno));
        return output;
    }

    for (unsigned int i = (qr->width * qr->width); i != 0; )
    {
        i--;
        output = scm_cons ((qr->data[i] & 1) ? SCM_BOOL_T : SCM_BOOL_F, output);
    }

    output = scm_cons (scm_from_uint (qr->width), output);
    output = scm_cons (scm_from_uint (qr->version), output);

    QRcode_free (qr);
    return output;
}

SCM gnc_qrcode_available ()
{
    return SCM_BOOL_T;
}

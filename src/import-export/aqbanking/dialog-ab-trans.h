/*
 * dialog-ab-trans.h --
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/**
 * @addtogroup Import_Export
 * @{
 * @addtogroup AqBanking
 * @{
 * @file dialog-ab-trans.h
 * @brief Dialog for AqBanking transaction data
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2004 Bernd Wagner
 * @author Copyright (C) 2006 David Hampton <hampton@employees.org>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#ifndef DIALOG_AB_TRANS_H
#define DIALOG_AB_TRANS_H

#include <glib.h>

G_BEGIN_DECLS

typedef struct _ABTransDialog ABTransDialog;

typedef enum _GncABTransType GncABTransType;
enum _GncABTransType {
    SINGLE_TRANSFER = 0,
    SINGLE_DEBITNOTE,
    SINGLE_INTERNAL_TRANSFER
};

G_END_DECLS

/** @} */
/** @} */

#endif /* DIALOG_AB_TRANS_H */

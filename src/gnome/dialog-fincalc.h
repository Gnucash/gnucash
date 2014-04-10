/********************************************************************\
 * dialog-fincalc.h : dialog for a financial calculator             *
 * Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>      *
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
\********************************************************************/

#ifndef __DIALOG_FINCALC_H_
#define __DIALOG_FINCALC_H_

#include "config.h"

#include <gnome.h>

#include "glade-gnc-dialogs.h"

typedef struct _FinCalcDialog FinCalcDialog;

FinCalcDialog * gnc_ui_fincalc_dialog_create(void);
void gnc_ui_fincalc_dialog_destroy(FinCalcDialog *fcd);

#endif

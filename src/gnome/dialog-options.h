/********************************************************************\
 * dialog-options.h -- GNOME option handling                        *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
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

#ifndef __OPTIONS_DIALOG_H__
#define __OPTIONS_DIALOG_H__

#include <gnome.h>
#include <guile/gh.h>

#include "option-util.h"


void gnc_build_options_dialog_contents(GnomePropertyBox *propertybox,
                                       GNCOptionDB *odb);

SCM  gnc_option_get_ui_value(GNCOption *option);
void gnc_option_set_ui_value(GNCOption *option, gboolean use_default);


#endif /* __OPTIONS_DIALOG_H__ */

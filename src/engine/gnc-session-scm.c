/********************************************************************\
 * gnc-sesssion.c -- session access (connection to backend)         *
 *    Scheme specific code.                                        *
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

/*
 * FILE:
 * gnc-session-scm.c
 *
 * FUNCTION:
 * Encapsulate a connection to a GnuCash backend.
 *
 * HISTORY:
 * Created by David Hampton, September 2002
 * Copyright (c) 2002 Linas Vepstas <linas@linas.org>
 */

#include "config.h"

#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "gnc-engine.h"
#include "qofsession.h"
#include "gnc-session-scm.h"

//static short module = MOD_IO;

static SCM gnc_session_scm_gui_cb = SCM_BOOL_F;

static void
gnc_session_scm_gui_cb_helper (const char *message, double percent)
{
    if (gnc_session_scm_gui_cb != SCM_BOOL_F)
    {
        SCM string = scm_makfrom0str(message);
        SCM scm_percent = scm_make_real(percent);
        scm_call_2 (gnc_session_scm_gui_cb, string, scm_percent);
    }
}

void
gnc_session_scm_load (QofSession *session)
{
    qof_session_load (session, gnc_session_scm_gui_cb_helper);
}

void
gnc_session_scm_save (QofSession *session)
{
    qof_session_save (session, gnc_session_scm_gui_cb_helper);
}

/*
 * Set the callback that will be used for any calls to the session
 * load/save functions from the scheme side of the code.
 */
void
gnc_session_scm_set_callback (SCM percentage_cb)
{
    if (gnc_session_scm_gui_cb != SCM_BOOL_F)
        scm_gc_unprotect_object(gnc_session_scm_gui_cb);

    gnc_session_scm_gui_cb = percentage_cb;
    if (gnc_session_scm_gui_cb != SCM_BOOL_F)
        scm_gc_protect_object(gnc_session_scm_gui_cb);
}


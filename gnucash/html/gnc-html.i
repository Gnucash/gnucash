/********************************************************************\
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

%module sw_gnc_html
%{
/* Includes the header in the wrapper code */
#include <config.h>
#include <gtk/gtk.h>
#include <glib-object.h>
#include <dialog-options.h>
#include <dialog-utils.h>
#include <gnc-amount-edit.h>
#include <gnc-date-edit.h>
#include <gnc-file.h>
#include <gnc-gnome-utils.h>
#include <gnc-gui-query.h>
#include <gnc-html.h>
%}
#if defined(SWIGGUILE)
%{
#include "guile-mappings.h"

SCM scm_init_sw_gnc_html_module(void);
%}
#endif

%import "base-typemaps.i"

/* Parse the header file to generate wrappers */
%newobject gnc_build_url;

%include "gnc-html-extras.h"


%init {
  {
    char tmp[100];

#define SET_ENUM(e) snprintf(tmp, 100, "(set! %s (%s))", (e), (e));  \
    scm_c_eval_string(tmp);

    SET_ENUM("URL-TYPE-FILE");
    SET_ENUM("URL-TYPE-JUMP");
    SET_ENUM("URL-TYPE-HTTP");
    SET_ENUM("URL-TYPE-FTP");
    SET_ENUM("URL-TYPE-SECURE");
    SET_ENUM("URL-TYPE-REGISTER");
    SET_ENUM("URL-TYPE-ACCTTREE");
    SET_ENUM("URL-TYPE-REPORT");
    SET_ENUM("URL-TYPE-OPTIONS");
    SET_ENUM("URL-TYPE-SCHEME");
    SET_ENUM("URL-TYPE-HELP");
    SET_ENUM("URL-TYPE-XMLDATA");
    SET_ENUM("URL-TYPE-PRICE");
    SET_ENUM("URL-TYPE-OTHER");

#undef SET_ENUM
  }

}

%module sw_gnc_html
%{
/* Includes the header in the wrapper code */
#include <config.h>
#include <gtk/gtk.h>
#include <glib-object.h>
#include <dialog-options.h>
#include <dialog-utils.h>
#include <druid-utils.h>
#include <gnc-amount-edit.h>
#include <gnc-date-edit.h>
#include <gnc-file.h>
#include <gnc-gnome-utils.h>
#include <gnc-gui-query.h>
#include <gnc-html.h>

SCM scm_init_sw_gnc_html_module(void);
%}

%import "base-typemaps.i"

/* Parse the header file to generate wrappers */
%include "gnc-html-extras.h"
%include "gnc-html-graph-gog-extras.h"


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

    SET_ENUM("GNC-CHART-PIE");
    SET_ENUM("GNC-CHART-BAR");
    SET_ENUM("GNC-CHART-LINE");
    SET_ENUM("GNC-CHART-SCATTER");

#undefine SET_ENUM
  }

}

/*********************************************************************
 * gncmod-html.c
 * module definition/initialization for the html utilities
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/

#include "config.h"

#include <gmodule.h>
#include <libguile.h>
#include <gtk/gtk.h>

#include "gnc-module.h"
#include "gnc-module-api.h"

#include "gnc-html.h"
#include "qof.h"

GNC_MODULE_API_DECL(libgncmod_html)

/* version of the gnc module system interface we require */
int libgncmod_html_gnc_module_system_interface = 0;

/* module versioning uses libtool semantics. */
int libgncmod_html_gnc_module_current  = 0;
int libgncmod_html_gnc_module_revision = 0;
int libgncmod_html_gnc_module_age      = 0;


char *
libgncmod_html_gnc_module_path( void )
{
	return g_strdup( "gnucash/html" );
}

char *
libgncmod_html_gnc_module_description( void )
{
	return g_strdup( "Utilities for using HTML with GnuCash" );
}

static void
lmod( char* mn )
{
	char* form = g_strdup_printf( "(use-modules %s)\n", mn );
	scm_c_eval_string( form );
	g_free( form );
}

extern SCM scm_init_sw_gnc_html_module( void );

int
libgncmod_html_gnc_module_init( int refcount )
{
	/* load the engine (we depend on it) */
	if( !gnc_module_load( "gnucash/engine", 0 ) ) {
		return FALSE;
	}

	/* load the calculation module (we depend on it) */
	if( !gnc_module_load( "gnucash/calculation", 0 ) ) {
		return FALSE;
	}

	if( !gnc_module_load( "gnucash/app-utils", 0 ) ) {
		return FALSE;
	}

	scm_init_sw_gnc_html_module();
	lmod( "(sw_gnc_html)" );
//	lmod( "(gnucash gnc_html)" );

	return TRUE;
}

int
libgncmod_html_gnc_module_end( int refcount )
{
	return TRUE;
}

/***************************************************************************
 *            test-load-backend.c
 *
 *  Replaces the guile version to test the GModule file backend loading.
 *
 *  Sun Oct  9 18:58:47 2005
 *  Copyright  2005  Neil Williams
 *  linux@codehelp.co.uk
 ****************************************************************************/

/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
 
#include "qof.h"
#include "cashobjects.h"
#include "test-stuff.h"

#define PG_LIB_NAME "libgnc-backend-postgres.la"
#define PG_LIB_INIT "pgend_provider_init"

int main (int argc, char ** argv)
{
	qof_init();
	cashobjects_register();
	do_test(
		qof_load_backend_library (QOF_LIB_DIR, PG_LIB_NAME, PG_LIB_INIT),
		" loading gnc-backend-postgres GModule failed");
	print_test_results();
	qof_close();
	return 0;
}

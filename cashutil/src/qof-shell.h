/***************************************************************************
 *            qof-shell.h
 *
 *  Mon Nov 28 13:03:44 2005
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
 /** @addtogroup QOFCLI Query Object Framework Command Line Interface.

The CLI uses a top level shell and two sub shells. The top level shell provides
general interactivity with object types, entire books and SQL support.

The select sub-shell identifies a single instance of an object. e.g. the print 
command in the top shell uses the select subshell to locate the instance to be
printed and the delete command, the instance to be deleted. These commands then
return to the top shell.

The edit sub-shell allows data to be set in a selected instance. The edit command
uses a select sub-shell to identify the instance then changes to an edit sub-shell
to handle setting the individual parameters of that instance. Before returning to the
top shell, the edited data can be saved to the instance using 'commit' or the edit can
be aborted using 'quit'. 

The add command creates a new instance and passes that instance, already selected,
to the edit sub-shell for data entry.

\note CashUtil relies on installed versions of the QOF, libcashobjects and
libgnc-backend-file libraries. If you change any source files for these libraries,
ensure you run 'make install' rather than just 'make' or your changes will have no
effect. There is no support for loading local or 'test' versions of the libraries.
You can usually run cashutil against a freshly installed set of QOF libraries without
recompiling cashutil, depending on the level of changes in QOF.
@{
*/
/** @file qof-shell.h
  @brief Shell functions for the QOF external framework CLI
  @author Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
*/

#ifndef _QOF_SHELL_H
#define _QOF_SHELL_H

#include <qof.h>
#include "qof-main.h"
#include "qofundo.h"

#define QOF_SHELL_CLI "qof-cli-shell"

#define QOF_SHELL_OPTIONS \
	{"shell", 0, POPT_ARG_NONE, NULL, qof_op_shell, \
	 _("Enter the QOF interactive shell"), NULL},


/** Indent and pad the shell output nicely.*/
#define QOF_SHELL_FORMAT "    %-30s%s"

/** \brief Handle the type of each subshell. */
typedef enum {
	NO_SHELL,
	TOP_SHELL,     /**< the first, top level shell. */
	EDIT_SHELL,    /**< Edit the selected instance */
}qof_subshell;

typedef enum {
	NO_OP,
	PRINT_MODE,
	DELETE_MODE,
	EDIT_MODE,
}qof_cli_mode;

typedef struct qof_shell_s {
	qof_main_context qof;       /**< Wrap the CLI context */
	gchar *shortname;           /**< A shortname for this program if truncation to first 4 characters is not suitable. */
	int argc;                   /**< Shell copy of argc */
	char **argv;                /**< Shell copy of commands */
	QofBook *book;              /**< the current book for the shell function. */
	qof_subshell shell_type;    /**< the type of subshell, top or edit */
	qof_cli_mode cli_mode;      /**< current operation mode. */
	QofIdTypeConst inst_type;   /**< The current registered QofObject type. */
	QofInstance *instance;      /**< The currently selected instance. */
	gint counter;
	GHashTable *select_table;
}qof_shell_context;

/** \brief load the QOF interactive shell

Where available, uses READLINE to store a history of
previous shell commands.
*/
void qof_cmd_shell(qof_shell_context *context);

void qof_shell_free (qof_shell_context *context);

#endif /* _QOF_SHELL_H */

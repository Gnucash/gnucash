/** \mainpage GnuCash design and developer's reference

This is the new developer and design manual for GnuCash.

\section maindocs Documentation Sections

The documentation is organised in a rough sequence:

-# \ref manualguide Start with the main GnuCash manual.
-# \ref highlevel Pages that cover topics at a higher level. Most also have
	links to the full API reference they relate to.
-# \ref maingeneral How to work with Doxygen in your own code.

\section manualguide External documentation

Please refer to the main
<a href="https://www.gnucash.org/docs.phtml">documentation
page</a> on the gnucash website which includes links to the
GnuCash Manual and the Concepts Guide in various formats.

\section highlevel Highlevel overviews

Pages that cover topics at a higher level. Most also have
links to the full API reference they relate to.

- \ref deprecated
- \ref loanhandling
- \ref kvpvalues
- \ref lotsoverview
- \ref python_bindings_page
- \ref todo

\section maingeneral General Doxygen help

- \ref tipshints
- \ref reference

*/

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


/* Editing this file? Comments can be inserted in this file
 Remember to use C syntax but skip the extra asterisk so that doxygen
ignores the extra notes, like this one.
*/
/* Hacking alert: When adding new sections, keep the references in
alphabetical order OF THE DESCRIPTION, not the reference title.

Also, make sure all reference titles are unique across the entire
Doxygen output.

Keep each section within one C comment block and any text before
the first section of a page must be in the same comment block as the
page title itself.

*/

/** \section hacking Hacking on this documentation

There is the beginning of a style guide for documenting under
\ref tipshints

Feel free to start documenting or playing with doxygen configuration.
This main page can be found in src/doc/doxygen_main_page.c .

This main page is just an introduction to doxygen markup, see the
Doxygen manual for the full command set.

- \ref tipshints Tips and hints for using doxygen
- \ref reference Links to the Doxygen manual

Code snippets need special handling in the text overviews. Change all
comment markers to // (so that the overview comment remains intact)
and then wrap each code snippet in the \a verbatim \a endverbatim
doxygen markers.

One useful method is to edit these .txt files using the syntax highlighting
of normal C files.

*/
/**
\page tipshints Useful tips for doxygen in C files

 - \ref index Introduction
 - \ref reference The Doxygen manual

\section tips An introduction to doxygen markup

\subsection Locations What to document

All declarations for:

-# typedef
-# struct
-# enum
-# functions

This will enable doxygen to link all parameter types to the declarations
every time the type is used in a function - very helpful to new developers.

\subsection Files Private files

If your declarations are in separate files, like private header files,
a simple block can still be linked into doxygen as long as the file is
identified to doxygen using a '\\file' section:

** \\file filename.h\n
	\\brief one-liner summary of the file purpose\n
	\\author the usual copyright statement

\subsection Methods How to document

Every doxygen comment block starts with an adapted comment marker.
You can use an extra slash /// or an extra asterisk ** . Blocks end
in the usual way. Doxygen accepts commands using a backslash.

To put a description with each function or structure, use '\\brief'
End the brief description with a blank line. The rest of the documentation will
then be shown in the body of the doxygen page.

Commands may begin with \\ or @

\subsection Presentation Extras

	-# Start a line with a hyphen to start a list - the indent determines the
nesting of the list:
		- To create a numbered list, use -# e.g. for a sublist:
			-# start a numbered list
		- revert to previous list

	End the list with a blank line.
Use :: at the start of a function or structure to link to the page
for that function in the doxygen documentation. e.g. ::qof_class_foreach

Use the param command to describe function parameters in the text.

Use the 'back reference' to document enumerator values:\n
enum testenum {\n
	enum_one **< less than marker tells doxygen to use this line
		to document enum_one.

\subsection config Editing Doxygen configuration

To edit the doxygen configuration, you can use:
*
cd src/doc
*
doxywizard doxygen.cfg &

*/

/** \page reference Doxygen reference documentation

- \ref index Introduction
- \ref tipshints Tips and hints for using doxygen


The Doxygen web site (https://www.stack.nl/~dimitri/doxygen/ [DEAD LINK]) has a
complete user manual.  For the impatient, here are the most
interesting sections:

- How to write grouped documentation for files, functions, variables,
etc.: https://www.stack.nl/~dimitri/doxygen/grouping.html [DEAD LINK] .  Do not
forget to add a file documentation block (\@file) at the top of your
file. Otherwise, all documentation in that file will <i>not</i> appear
in the html output.

- List of the special commands you can use within your documentation
blocks: https://www.stack.nl/~dimitri/doxygen/commands.html [DEAD LINK]

\section contact Contacts

\subsection web Web Site
News about GnuCash as well as the latest version can always be found at https://www.gnucash.org/

\subsection email Email
If you have any suggestions concerning this documentation, do not hesitate to send suggestions to
gnucash-devel (see https://www.gnucash.org/en/lists.phtml for details)

Benoit Grégoire <bock@step.polymtl.ca>
Neil Williams <linux@codehelp.co.uk>
 */

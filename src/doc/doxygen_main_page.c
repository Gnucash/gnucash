/** \mainpage GnuCash design and developer's reference

This is the new developer and design manual for GnuCash.

\section maindocs Documentation Sections.

The documentation is organised in a rough sequence:

-# \ref manualguide Start with the main GnuCash manual.
-# \ref texihtml Design overview.
-# \ref doxylist Individual topic overviews, linked into the
	full API reference for each topic, generated from source code.
-# \ref maingeneral How to work with Doxygen in your own code.

\subsection manualguide External documentation.

Please refer to the main
<a href="http://www.gnucash.org/docs.phtml">documentation
page</a> on the gnucash website which includes links to the
GnuCash Manual and the Concepts Guide in various formats.

\subsection texihtml Documentation elsewhere in the source tree.

See also <a href="http://code.neil.williamsleesmill.me.uk/texi/gnucash-design.html">
GnuCash Design Overview</a> which can be generated from the source using texi2html
from the texinfo files in src/doc/design.

\section doxylist Doxygen overviews.

Where possible, each overview is dated - take care to review older texts
in relation to existing code.

- \ref backuppolicy
- \ref bookperiods
- \ref business1
- \ref currencies
- \ref deprecated
- \ref engine
- \ref backendold
- \ref financeconstraints
- \ref druidframework
- \ref guid
- \ref gnome2
- \ref networkoverview
- \ref backenderrors
- \ref loanhandling
- \ref kvpvalues
- \ref lotsoverview
- \ref multicurrency
- \ref plugindesign
- \ref pricedocs
- \ref gnucashextension
- \ref qif
- \ref backendapi
- \ref budgetplan
- \ref taxnotes
- \ref todo
- \ref userprefs

Each overview in this section is generated directly from the
source files using Doxygen but some topics need updating.

\section maingeneral General Doxygen help.

- \ref tipshints
- \ref reference
- \ref stylediscussion

*/

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
\ref tipshints.

The Book Merge files are an attempt to document "by the book".
\ref BookMerge\n
Feel free to start documenting or playing with doxygen configuration.
This main page can be found in src/doc/doxygen_main_page.c .

Each doxygen section must be within a single comment block although
large comment blocks can be split into separate pages:
\ref stylediscussion.

This main page is just an introduction to doxygen markup, see the
Doxygen manual for the full command set.

- \ref tipshints Tips and hints for using doxygen
- \ref stylediscussion Long comments, pages, editors
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
 - \ref stylediscussion Long comments, pages, editors
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

/** \page stylediscussion Style discussion

- \ref index Introduction
- \ref tipshints Tips and hints for using doxygen
- \ref reference Links to the Doxygen manual

[codehelpgpg 2004-07-25] Doxygen now copes with addgroup and this page
can be handled more easily by splitting the old single comment into repeated
comments, split into pages. I've worked on doxygen files in Kate, KWrite and XCode (MacOSX) and
the comment higlighting works fine. If you do have problems, particularly when
you start a new line within an existing comment, enter a character at the end of
the last highlighted line to refresh the highlighting. Some editors have a specific
refresh option.

[cstim 2003-03-25] The "Data Structures" page of doxygen doesn't show
anything useful for GnuCash. Obviously doxygen only accepts "real" C
struct definitions for inclusion on that page. However, all GnuCash
data structures are defined somewhere in private headers, and only the
typedefs are publically visible. Isn't there a way to have doxygen
show the documentation for the <i>typedefs</i> on the "Data
Structures" page? Unfortunately I don't know how.

[codehelpgpg 2004-07-25] Yes, there is a way of linking to these data structures.
Make sure that the private header is included in the documentation by including a
\\file command in the private header file. Then include a short doxygen comment above
the declaration. Doxygen will accept both valid C struct definition formats.

*/

/** \page reference Doxygen reference documentation

- \ref index Introduction
- \ref tipshints Tips and hints for using doxygen
- \ref stylediscussion Long comments, pages, editors

The Doxygen web site (http://www.stack.nl/~dimitri/doxygen/) has a
complete user manual.  For the impatient, here are the most
interesting sections:

- How to write grouped documentation for files, functions, variables,
etc.: http://www.stack.nl/~dimitri/doxygen/grouping.html .  Do not
forget to add a file documentation block (\@file) at the top of your
file. Otherwise, all documentation in that file will <i>not</i> appear
in the html output.

- List of the special commands you can use within your documentation
blocks: http://www.stack.nl/~dimitri/doxygen/commands.html

\section contact Contacts

\subsection web Web Site
News about GnuCash as well as the latest version can always be found at http://www.gnucash.org/

\subsection email Email
If you have any suggestions concerning this documentation, do not hesitate to send suggestions to
gnucash-devel (see http://www.gnucash.org/en/lists.phtml for details)

Benoit Grégoire <bock@step.polymtl.ca>
Neil Williams <linux@codehelp.co.uk>
 */


/*! \mainpage GnuCash design and developer's reference
  
\section intro Introduction

This is the new GnuCash developer and design manual for GnuCash.
Previous documentation will slowly be integrated into this, and
eventually it should always be up to date since it is generated
directly from the source files using Doxygen.

\section hacking Hacking on this documentation

I will eventually post a style guide for documenting, and document one
of the files "by the book".  Until then, feel free to start
documenting or playing with doxygen configuration. This main page can
be found in src/doc/doxygen_main_page.c .

\subsection style_discussion Style discussion

[cstim 2003-03-25] It's too bad that doxygen doesn't show the "Member
Groups" (those created on-the-fly with @{ and @}) inside the Module
pages (named groups, created with \\addtogroup). This means that the
"member group" grouping of the account-related functions is only shown
in the documentation of the Account.h header file. But in the
documentation of the Engine group (module) all these functions are
lumped together into one unreadable long list. This is bad (because
1. nobody will read through the long list, 2. hyperlinks don't even
work properly since the document is simply too big). Isn't there a way
to have this member groups also show up on the module pages...? 
Unfortunately nesting several named modules (groups) into one another
doesn't really solve this problem, because that way all
account-related functions will show up in one separate group each --
which makes these unreadable as well. Maybe it would even be better to
not have these function names show up on the module page at all. If
they appear *only* on the file page, they will show up in their
respective "member group" grouping and everything would be fine.

[cstim 2003-03-25] The "Data Structures" page of doxygen doesn't show
anything useful for gnucash. Obviously doxygen only accepts "real" C
struct definitions for inclusion on that page. However, all gnucash
data structures are defined somewhere in private headers, and only the
typedefs are publically visible. Isn't there a way to have doxygen
show the documentation for the <i>typedefs</i> on the "Data
Structures" page? Unfortunately I don't know how.

[cstim 2003-03-25] This mainpage file should at some time be moved to
a file name ending in .txt or even .html. It's a PITA to edit one huge
C comment. All text editors will run amok sooner or later (well,
XEmacs does :) and will not support proper HTML markup and editing
inside this C comment... well, that's not really a problem of the
outcome, yes. It would just make life easier for editing this
non-source file.

\subsection config Editing Doxygen configuration

To edit the doxygen configuration, you can use:
*
cd src/doc
*
doxywizard doxygen.cfg &

\subsection reference Doxygen reference documentation

The Doxygen web site (http://www.stack.nl/~dimitri/doxygen/) has a
complete user manual.  For the impatient, here are the most
interesting sections:

- How to write grouped documentation for files, functions, variables,
etc.: http://www.stack.nl/~dimitri/doxygen/grouping.html .  Do not
forget to add a file documentation block (@file) at the top of your
file. Otherwise, all documentation in that file will <i>not</i> appear
in the html output.

- List of the special commands you can use within your documentation
blocks: http://www.stack.nl/~dimitri/doxygen/commands.html

\section contact Contacts

\subsection web Web Site
News about GnuCash as well as the latest version can always be found at http://www.gnucash.org/

\subsection email Email
If you have any suggestions concerning this documentation, do not hesitate to send suggestions to gnucash-devel (see http://www.gnucash.org/en/lists.phtml for details)

Benoit Grégoire mailto:bock@step.polymtl.ca
 */

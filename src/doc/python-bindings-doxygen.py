## @file 
# @brief Documentation file for GnuCashs python bindings, input file for doxygen.
#
# This file holds the more explanatory parts of the doxygen-source-documentation.
# You will find the contents at @ref python_bindings_page.
#
# @par To-Do:
# @li Work out the relation of scheme/guile and python-bindings
# @li maybe join python_bindings_page and group
# @li work on the structure of the documentation to make it more clear
# @li try to make SWIG include the documentation of the c-source
# @li make funtion-links in SWIG-generated files work.
# @li some words to the tests
#
# @author Christoph Holtermann
# @date December 2010
# @ingroup python_bindings


## @defgroup python_bindings
#  Also have a look at the page @ref python_bindings_page.

## @defgroup python_bindings_examples
#  @ingroup python_bindings
#  The python-bindings come with quite a lot of example scripts.

##  @page python_bindings_page Python bindings
#   Also have a look at group @ref python_bindings.
#
#   For the moment the python-bindings are only available via svn. They may be included
#   in GnuCash 2.4.
#
#   They can be found in src/optional/python-bindings.
#
#   To enable them in the compilation process you have to add --enable-python-bindings
#   to the call of ./configure.
#
#   As a starting point have a look at the \link python_bindings_examples example-scripts\endlink.
#
#   @section possibilities What are the Python bindings good for ?
#
#   The python bindings supply the ability to access a wide range of the core funtions of GnuCash. You
#   can read and write Transactions, Commodities, Lots, access the business stuff... You gain the ability
#   to manipulate your financial data with a flexible scripting language.
#
#   Not everything GnuCash can is possible to access though. The bindings focus on basic accounting functions.
#   Have a look at the examples to get an impression.
#
#   @section python_bindings_section Principles
#   The python-bindings are generated using SWIG from parts of the source-files of GnuCash.
#
#   @note Python-scripts should not be executed while GnuCash runs. GnuCash is designed as
#   a single user application with only one program accessing the data at one time. You can force your 
#   access but that may corrupt data. Maybe one day that may change but for the moment there is no active development on that.
#
#   @subsection swigworks What SWIG does
#
#   SWIG extracts informations from the c-sources and provides access to the structures
#   to python. It's work is controlled by interface files :
# 
#   @li gnucash_core.i
#   @li timespec.i
#   @li glib.i 
#   @li @link base-typemaps.i src/base-typemaps.i @endlink This file is shared with Guile.
#
#   it outputs:
#
#   @li gnucash_core.c
#   @li gnucash_core_c.py
#
#   If you have generated your own local doxygen documentation (by "make doc") after having compiled the python-bindings, doxygen
#   will include SWIGs output-files. The official version at http://svn.gnucash.org/docs/HEAD/ does not include
#   these files. It's actually quite interesting to have a look at them through doxygen, because they contain all that you can
#   access from python.
#
#   This c-style-api is the bottom layer. It is a quite raw extract and close to the original source. Some more details are described further down.
#
#   For some parts there is a second layer of a nice pythonic interface. It is declared
#   in 
#   @li gnucash_core.py and 
#   @li gnucash_business.py.
#   @li function_class.py contains helper functions for that.
#
#   @section howto How to use the Python bindings
#   @subsection highlevel High level python wrapper classes
#   If you
# 
#   @code >> import gnucash @endcode
#
#   You can access the structures of the high level api. For Example you get a Session object by
#
#   @code >> session=gnucash.Session() @endcode
#
#   Here you will find easy to use things. But sometimes - and at the current level rather sooner than
#   later - you may be forced to search for solutions at the :
#
#   @subsection c_style_api C-style-api
#
#   If you
#
#   @code >> import gnucash @endcode
#
#   The c-style-api can be accessed via gnucash.gnucash_core_c. You can have a look at all the possibilities
#   at gnucash_core_c.py.
#
#   You will find a lot of pointers here which you can just ignore if input and output of the function have the
#   same type.
#
#   For example you could start a session by gnucash.gnucash_core_c.qof_session_begin(). But if you just try
#
#   @code session=gnucash.gnucash_core_c.qof_session_begin() @endcode
#
#   you will get an error message and realize the lack of convenience for you have to add the correct function parameters.
#
#   Not all of the available structures will work. SWIG just takes everything from the sources that it is fed with and translates it. Not everything
#   is a working translation, because not everything has been worked through. At this point you are getting closer to the developers who you can 
#   contact at the mailing-list gnucash-devel@gnucash.org. There may be a workaround. Maybe the problem can only be fixed by changing SWIGs input 
#   files to correctly translate the c-source. Feel free to post a question at the developers list. It may awaken the interest of someone who creates
#   some more beautiful python-interfaces.
#
#   @section Thisorthat When to use which api ?
#
#   The start would surely be the high-level api for you can be quite sure to have something working and you will maybe find
#   explanations in the example-scripts. If you search for something that is not yet implemented in that way you will have to
#   take your way to the c-style-api.
#
#   @section pydoc (Further) documentation
#
#   The documentation you just read uses doxygen. It collects documentation in GnuCash's sources. Besides that there is
#   the classic python-documentation using help() and docstrings. Have a look at both.
#   You may also have a look into the archives of gnucash-devel@gnucash.org. On Bugzilla there is also some interesting
#   talk regarding the development process. Then you can use the abilitys of svn to see the history of the code by 
#   "svn log" done in the directory of the python-bindings.
#


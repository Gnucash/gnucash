/***************************************************************************
 *            qofchoice.h
 *
 *  Thu Jul  7 12:25:24 2005
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

#ifndef _QOFCHOICE_H
#define _QOFCHOICE_H

/** @addtogroup Choice

Objects can be linked together one-to-one by simply using the name of
the related object as the parameter type in the QofClass parameter list.

\verbatim
{ FOO_PARAM, BAR_ID, (QofAccessFunc)qofFooGetBar, (QofSetterFunc)qofFooSetBar },
\endverbatim

This is limited as each FOO entity can contain only one reference to a
single BAR entity per parameter. Also, this parameter cannot be used to
link to a similar object, OBJ. This requires "one to many" links.

There are two types of one-to-many links in QOF.

-# ::QOF_TYPE_COLLECT - one to many entities all of only one type.
	- Handles links between one object and a series of objects
	that are \b all of the same type, e.g. a simple list.
-# ::QOF_TYPE_CHOICE - one to a single entity of many possible types.
	- Handles links between one object and a series of
\b dissimilar objects, one of each type.

Currently, there is no explicit way to support many-to-many links
but existing methods can be combined to give approximately the same results.

A QOF_TYPE_CHOICE object is like a C++ template. QOF_TYPE_CHOICE doesn't
really exist by itself:
\verbatim
QOF_TYPE_CHOICE<QOF_X, QOF_Y, QOF_Z>
\endverbatim
It holds a single entity of type X, Y, or Z for the purposes of QOF
or ::QSF. For querying the object it queries as if it's an X, Y, or Z.

Each choice type has it's own definition of the allowable objects -
each of which need to be registered as normal. Objects can declare
themselves to be one option of a particular choice. There is no
requirement for any object to be either a choice or an option for a
choice object.

-# Each ::QOF_TYPE_CHOICE parameter provides access to \b ONE entity of
a pre-determined set of object types.
-# The entity type within the choice can be determined at run time.
-# Each entity can have a different *type* of entity to it's siblings,
provided that it is one of the pre-determined object types.
-# Objects declare themselves as containing choices and other objects
can add themselves to the list of acceptable choices of suitable objects.
-# QOF_TYPE_CHOICE is transparent - objects should retrieve the e_type of
the received entity and handle the entity appropriately.
-# The number of different entity types that can be pre-determined for
any one QOF_TYPE_CHOICE parameter is not fixed. You can have as many
types as the ::QofAccessFunc and ::QofSetterFunc can handle.
-# Any one parameter can only have one QOF type. You cannot have a parameter
that is both ::QOF_TYPE_COLLECT and QOF_TYPE_CHOICE any more than you can have
one parameter that is both ::QOF_TYPE_BOOLEAN and ::QOF_TYPE_NUMERIC.
-# When setting references using QOF_TYPE_CHOICE, QOF passes a single entity
to the QofSetterFunc and it is left to the function to determine how to
handle that entity type.  When retrieving references using QOF_TYPE_CHOICE,
the object must return a single entity matching one of the choice types.

 @{
*/

/** @file qofchoice.h
	@brief Linking one entity to other entities of many possible types.
	@author Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
*/
#include "qofclass.h"
#include "qofobject.h"

#define QOF_MOD_CHOICE "qof.choice"

/** \note Choice
@{
*/

/** \brief Identify an object as containing a choice. */
#define QOF_TYPE_CHOICE "choice"

/** \brief Does this object contain a choice parameter?

Returns TRUE if any parameter in the object definition
uses a choice of elements, whether or not those
parameters contain any data.

@param type Type of object/entity.

@return TRUE if one or more choice parameters has been
registered using the object definition, otherwise FALSE.
*/
gboolean qof_object_is_choice(QofIdTypeConst type);

/** \brief Set an object as using QOF_TYPE_CHOICE. */
gboolean  qof_choice_create(char* type);

/** \brief Add the choices for this parameter to the object.

@param choice The choice object.
@param add  The object to be added as an option.
@param param_name The parameter that will be used to get or set options.

@return FALSE if object is not a choice object or on error
	otherwise TRUE.
*/
gboolean qof_choice_add_class(const char* choice, char* add, char* param_name);

/** \brief Return the list of all object types usable with this parameter.

@param type The choice object type.
@param param The name of the parameter that will be used to
	get or set options.

@return NULL on error or if no options exist for this parameter,
	otherwise a GList of all QofIdType object type(s) available
	via this choice object using this parameter.
*/
GList* qof_object_get_choices(QofIdType type, QofParam *param);

/** \brief Is the choice valid for this param_name?

@param choice_obj The object containing the QOF_TYPE_CHOICE parameter.
@param param_name The name of a QOF_TYPE_CHOICE parameter in this object.
@param choice The QofIdType to look for in the list of choices.

@return TRUE if choice is found in the list of allowed choices for
this parameter of this object. Otherwise, FALSE
*/
gboolean qof_choice_check(const char* choice_obj,
			  const char *param_name,
			  const char* choice);
/** @} */

/** @} */
#endif /* _QOFCHOICE_H */

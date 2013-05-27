/********************************************************************\
 * gnc-basic-gobject.h                                              *
 *                                                                  *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/


#ifndef GNC_BASIC_GOBJECT_H
#define GNC_BASIC_GOBJECT_H

/* A simple macro to define simple gobjects */

#define GNC_BASIC_GOBJECT_TYPE(type_struct,klass_struct,parent,klass_init,inst_init,get_type_fcn)	\
GType 										\
get_type_fcn (void)								\
{										\
  static GType type = 0;							\
										\
  if (type == 0) {								\
    GTypeInfo type_info = {							\
      sizeof (klass_struct),							\
      NULL,									\
      NULL,									\
      (GClassInitFunc) klass_init,						\
      NULL,									\
      NULL,									\
      sizeof (type_struct),							\
      0,									\
      (GInstanceInitFunc) inst_init,						\
    };										\
										\
    type = g_type_register_static (parent, #type_struct, &type_info, 0);	\
  }										\
										\
  return type;									\
}

#define GNC_BASIC_GOBJECT_NEW(type_struct,new_fcn,get_type_fcn)			\
type_struct *									\
new_fcn (void)									\
{										\
  return (type_struct *) g_object_new(get_type_fcn (), NULL);			\
}

#define GNC_BASIC_GOBJECT(type_struct,klass_struct,parent,klass_init,inst_init,get_type_fcn,new_fcn)	\
GNC_BASIC_GOBJECT_TYPE(type_struct,klass_struct,parent,klass_init,inst_init,get_type_fcn) \
GNC_BASIC_GOBJECT_NEW(type_struct,new_fcn,get_type_fcn)

#endif /* GNC_BASIC_GOBJECT_H */

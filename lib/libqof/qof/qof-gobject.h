/********************************************************************\
 * qof-gobject.h -- helper macros for qof objects using gobject     *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef QOF_GOBJECT_H
#define QOF_GOBJECT_H

#include <glib-object.h>

/**
 * This is a simple macro for use in your QOF header files.
 * In addition to using this macro (which you don't need to use,
 * you can define the get_type() function directory if you wish)
 * you also need to define the gobject type cast macros.  For example,
 * for the QofInstance type you would need to define the following
 * macros:
 *
 * #define QOF_TYPE_INSTANCE            (qof_instance_get_type ())
 * #define QOF_INSTANCE(o)              \
 *    (G_TYPE_CHECK_INSTANCE_CAST ((o), QOF_TYPE_INSTANCE, QofInstance))
 * #define QOF_INSTANCE_CLASS(k)        \
 *    (G_TYPE_CHECK_CLASS_CAST((k), QOF_TYPE_INSTANCE, QofInstanceClass))
 * #define QOF_IS_INSTANCE(o)           \
 *    (G_TYPE_CHECK_INSTANCE_TYPE ((o), QOF_TYPE_INSTANCE))
 * #define QOF_IS_INSTANCE_CLASS(k)	\
 *    (G_TYPE_CHECK_CLASS_TYPE ((k), QOF_TYPE_INSTANCE))
 * #define QOF_INSTANCE_GET_CLASS(o)    \
 *    (G_TYPE_INSTANCE_GET_CLASS ((o), QOF_TYPE_INSTANCE, QofInstanceClass))
 *
 * @param type_name    The function type_name for this type
 */
#define QOF_GOBJECT_DECL(type_name)		\
  GType type_name##_get_type(void);

/**
 * The following macros are for convenience in your QOF object
 * implementation files.   Generally you only need to use
 * QOF_GOBJECT_IMPL() or QOF_GOBJECT_IMPL_WITH_CODE()
 */

#define QOF_GOBJECT_GET_TYPE(TypeName, type_name, TYPE_PARENT, CODE)	\
  G_DEFINE_TYPE_WITH_CODE(TypeName, type_name, TYPE_PARENT, CODE);

#define QOF_GOBJECT_CLASS_INIT(type_name, TypeName)			\
  static void type_name##_dispose(GObject *object);			\
  static void type_name##_finalize(GObject *object);			\
  static void type_name##_class_init(TypeName##Class *klass)		\
  {									\
    GObjectClass *object_class = G_OBJECT_CLASS(klass);			\
    object_class->dispose = type_name##_dispose;			\
    object_class->finalize = type_name##_finalize;			\
  }

#define QOF_GOBJECT_DISPOSE(type_name)					\
  static void type_name##_dispose_real(GObject* object);		\
  static void type_name##_dispose(GObject *object)			\
  {									\
    type_name##_dispose_real(object);					\
    G_OBJECT_CLASS(type_name##_parent_class)->dispose(object);		\
  }

#define QOF_GOBJECT_FINALIZE(type_name)					\
  static void type_name##_finalize_real(GObject* object);		\
  static void type_name##_finalize(GObject *object)			\
  {									\
    type_name##_finalize_real(object);					\
    G_OBJECT_CLASS(type_name##_parent_class)->finalize(object);		\
  }

#define QOF_GOBJECT_IMPL_WITH_CODE(type_name, TypeName, TYPE_PARENT, CODE) \
  QOF_GOBJECT_GET_TYPE(TypeName, type_name, TYPE_PARENT, CODE);		\
  QOF_GOBJECT_CLASS_INIT(type_name, TypeName);				\
  QOF_GOBJECT_DISPOSE(type_name);					\
  QOF_GOBJECT_FINALIZE(type_name);

#define QOF_GOBJECT_IMPL(type_name, TypeName, TYPE_PARENT) \
  QOF_GOBJECT_IMPL_WITH_CODE(type_name, TypeName, TYPE_PARENT, {})


#endif /* QOF_GOBJECT_H */

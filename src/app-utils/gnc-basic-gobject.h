

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

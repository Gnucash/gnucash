
#ifndef ___egg_marshal_MARSHAL_H__
#define ___egg_marshal_MARSHAL_H__

#include	<glib-object.h>

G_BEGIN_DECLS

/* VOID:OBJECT,OBJECT (eggmarshalers.list:1) */
extern void _egg_marshal_VOID__OBJECT_OBJECT (GClosure     *closure,
                                              GValue       *return_value,
                                              guint         n_param_values,
                                              const GValue *param_values,
                                              gpointer      invocation_hint,
                                              gpointer      marshal_data);

/* VOID:OBJECT,STRING,LONG,LONG (eggmarshalers.list:2) */
extern void _egg_marshal_VOID__OBJECT_STRING_LONG_LONG (GClosure     *closure,
                                                        GValue       *return_value,
                                                        guint         n_param_values,
                                                        const GValue *param_values,
                                                        gpointer      invocation_hint,
                                                        gpointer      marshal_data);

/* VOID:OBJECT,LONG (eggmarshalers.list:3) */
extern void _egg_marshal_VOID__OBJECT_LONG (GClosure     *closure,
                                            GValue       *return_value,
                                            guint         n_param_values,
                                            const GValue *param_values,
                                            gpointer      invocation_hint,
                                            gpointer      marshal_data);

/* VOID:OBJECT,STRING,STRING (eggmarshalers.list:4) */
extern void _egg_marshal_VOID__OBJECT_STRING_STRING (GClosure     *closure,
                                                     GValue       *return_value,
                                                     guint         n_param_values,
                                                     const GValue *param_values,
                                                     gpointer      invocation_hint,
                                                     gpointer      marshal_data);

/* VOID:UINT,UINT (eggmarshalers.list:5) */
extern void _egg_marshal_VOID__UINT_UINT (GClosure     *closure,
                                          GValue       *return_value,
                                          guint         n_param_values,
                                          const GValue *param_values,
                                          gpointer      invocation_hint,
                                          gpointer      marshal_data);

/* BOOLEAN:INT (eggmarshalers.list:6) */
extern void _egg_marshal_BOOLEAN__INT (GClosure     *closure,
                                       GValue       *return_value,
                                       guint         n_param_values,
                                       const GValue *param_values,
                                       gpointer      invocation_hint,
                                       gpointer      marshal_data);

/* BOOLEAN:ENUM (eggmarshalers.list:7) */
extern void _egg_marshal_BOOLEAN__ENUM (GClosure     *closure,
                                        GValue       *return_value,
                                        guint         n_param_values,
                                        const GValue *param_values,
                                        gpointer      invocation_hint,
                                        gpointer      marshal_data);

/* BOOLEAN:VOID (eggmarshalers.list:8) */
extern void _egg_marshal_BOOLEAN__VOID (GClosure     *closure,
                                        GValue       *return_value,
                                        guint         n_param_values,
                                        const GValue *param_values,
                                        gpointer      invocation_hint,
                                        gpointer      marshal_data);

/* OBJECT:VOID (eggmarshalers.list:9) */
extern void _egg_marshal_OBJECT__VOID (GClosure     *closure,
                                       GValue       *return_value,
                                       guint         n_param_values,
                                       const GValue *param_values,
                                       gpointer      invocation_hint,
                                       gpointer      marshal_data);

/* VOID:VOID (eggmarshalers.list:10) */
#define _egg_marshal_VOID__VOID	g_cclosure_marshal_VOID__VOID

/* VOID:INT,INT (eggmarshalers.list:11) */
extern void _egg_marshal_VOID__INT_INT (GClosure     *closure,
                                        GValue       *return_value,
                                        guint         n_param_values,
                                        const GValue *param_values,
                                        gpointer      invocation_hint,
                                        gpointer      marshal_data);

/* VOID:UINT,UINT (eggmarshalers.list:12) */

/* VOID:BOOLEAN (eggmarshalers.list:13) */
#define _egg_marshal_VOID__BOOLEAN	g_cclosure_marshal_VOID__BOOLEAN

/* VOID:OBJECT,ENUM,BOXED (eggmarshalers.list:14) */
extern void _egg_marshal_VOID__OBJECT_ENUM_BOXED (GClosure     *closure,
                                                  GValue       *return_value,
                                                  guint         n_param_values,
                                                  const GValue *param_values,
                                                  gpointer      invocation_hint,
                                                  gpointer      marshal_data);

/* VOID:BOXED (eggmarshalers.list:15) */
#define _egg_marshal_VOID__BOXED	g_cclosure_marshal_VOID__BOXED

/* BOOLEAN:BOOLEAN (eggmarshalers.list:16) */
extern void _egg_marshal_BOOLEAN__BOOLEAN (GClosure     *closure,
                                           GValue       *return_value,
                                           guint         n_param_values,
                                           const GValue *param_values,
                                           gpointer      invocation_hint,
                                           gpointer      marshal_data);

/* BOOLEAN:OBJECT,STRING,STRING (eggmarshalers.list:17) */
extern void _egg_marshal_BOOLEAN__OBJECT_STRING_STRING (GClosure     *closure,
                                                        GValue       *return_value,
                                                        guint         n_param_values,
                                                        const GValue *param_values,
                                                        gpointer      invocation_hint,
                                                        gpointer      marshal_data);

G_END_DECLS

#endif /* ___egg_marshal_MARSHAL_H__ */


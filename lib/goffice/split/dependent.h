#ifndef GNUMERIC_EVAL_H
#define GNUMERIC_EVAL_H

#include "gnumeric.h"
#include <stdio.h>

struct _GnmDependent
{
	guint	  flags;
	Sheet	 *sheet;
	GnmExpr const *expression;

	/* Double-linked list.  */
	struct _GnmDependent *next_dep, *prev_dep;
};

typedef struct {
	void (*eval) (GnmDependent *dep);
	void (*set_expr) (GnmDependent *dep, GnmExpr const *new_expr);
	void (*debug_name) (GnmDependent const *dep, FILE *out);
} DependentClass;

typedef enum {
	DEPENDENT_NO_FLAG	   = 0,

	/* Types */
	DEPENDENT_CELL 		   = 0x00000001,	/* builtin type */
	DEPENDENT_DYNAMIC_DEP	   = 0x00000002,	/* builtin type */
	DEPENDENT_NAME 		   = 0x00000003,	/* builtin pseudo type */
	DEPENDENT_TYPE_MASK	   = 0x00000fff,

	/* Linked into the workbook wide expression list */
	DEPENDENT_IS_LINKED	   = 0x00001000,
	DEPENDENT_NEEDS_RECALC	   = 0x00002000,
	DEPENDENT_BEING_CALCULATED = 0x00004000,
	/* GnmDependent is in the midst of a cyclic calculation */
	DEPENDENT_BEING_ITERATED   = 0x00008000,

	DEPENDENT_GOES_INTERSHEET  = 0x00010000,
	DEPENDENT_GOES_INTERBOOK   = 0x00020000,
	DEPENDENT_USES_NAME	   = 0x00040000,
	DEPENDENT_HAS_3D	   = 0x00080000,
	DEPENDENT_ALWAYS_UNLINK    = 0x00100000,
	DEPENDENT_HAS_DYNAMIC_DEPS = 0x00200000,
	DEPENDENT_LINK_FLAGS	   = 0x003ff000,

	/* An internal utility flag */
	DEPENDENT_FLAGGED	   = 0x01000000,
	DEPENDENT_CAN_RELOCATE	   = 0x02000000
} DependentFlags;

#define dependent_type(dep)		((dep)->flags & DEPENDENT_TYPE_MASK)
#define dependent_is_cell(dep)		(dependent_type (dep) == DEPENDENT_CELL)
#define dependent_needs_recalc(dep)	((dep)->flags & DEPENDENT_NEEDS_RECALC)
#define dependent_is_linked(dep)	((dep)->flags & DEPENDENT_IS_LINKED)

struct _GnmDepContainer {
	GnmDependent *head, *tail;

	/* Large ranges hashed on 'range' to accelerate duplicate culling. This
	 * is tranversed by g_hash_table_foreach mostly.
	 */
	GHashTable **range_hash;
	GnmMemChunk *range_pool;

	/* Single ranges, this maps an GnmEvalPos * to a GSList of its
	 * dependencies.
	 */
	GHashTable *single_hash;
	GnmMemChunk *single_pool;

	/* All of the ExprNames that refer to this container */
	GHashTable *referencing_names;

	/* Dynamic Deps */
	GHashTable *dynamic_deps;
};

typedef void (*DepFunc) (GnmDependent *dep, gpointer user);

guint32	 dependent_type_register   (DependentClass const *klass);
void	 dependent_types_init	   (void);
void	 dependent_types_shutdown  (void);

void	 dependent_set_expr	   (GnmDependent *dep, GnmExpr const *new_expr);
void	 dependent_set_sheet	   (GnmDependent *dep, Sheet *sheet);
void	 dependent_link		   (GnmDependent *dep, GnmCellPos const *pos);
void	 dependent_unlink	   (GnmDependent *dep, GnmCellPos const *pos);
gboolean dependent_eval		   (GnmDependent *dep);
void	 dependent_queue_recalc	   (GnmDependent *dep);
void	 dependent_add_dynamic_dep (GnmDependent *dep, GnmValueRange const *v);

GSList  *dependents_relocate	    (GnmExprRelocateInfo const *info);
void     dependents_unrelocate      (GSList *info);
void     dependents_unrelocate_free (GSList *info);
void	 dependents_link	    (GSList *deps, GnmExprRewriteInfo const *rwinfo);

void	 cell_queue_recalc	    (GnmCell const *cell);
void	 cell_foreach_dep	    (GnmCell const *cell, DepFunc func, gpointer user);
gboolean cell_eval_content	    (GnmCell *cell);

void sheet_region_queue_recalc	 (Sheet const *sheet, GnmRange const *range);
void sheet_deps_destroy		 (Sheet *sheet);
void workbook_deps_destroy	 (Workbook *wb);
void workbook_queue_all_recalc	 (Workbook *wb);

GnmDepContainer *gnm_dep_container_new  (void);
void		 gnm_dep_container_dump	(GnmDepContainer const *deps);

#define DEPENDENT_CONTAINER_FOREACH_DEPENDENT(dc, dep, code)	\
  do {								\
	GnmDependent *dep = (dc)->head;				\
	while (dep) {						\
		GnmDependent *_next = dep->next_dep;		\
		code;						\
		dep = _next;					\
	}							\
  } while (0)

#define DEPENDENT_MAKE_TYPE(t, set_expr_handler)		\
guint								\
t ## _get_dep_type (void)					\
{								\
	static guint32 type = 0;				\
	if (type == 0) {					\
		static DependentClass klass;			\
		klass.eval	 = &t ## _eval;			\
		klass.set_expr	 = set_expr_handler;		\
		klass.debug_name = &t ## _debug_name;		\
		type = dependent_type_register (&klass);	\
	}							\
	return type;						\
}

void dependent_debug_name (GnmDependent const *dep, FILE *out);

#endif /* GNUMERIC_EVAL_H */

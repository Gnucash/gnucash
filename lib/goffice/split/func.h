#ifndef GNUMERIC_FUNC_H
#define GNUMERIC_FUNC_H

#include "gnumeric.h"
#include "dependent.h"

/* Setup of the symbol table */
void functions_init     (void);
void functions_shutdown (void);

/* Used to build manual */
void function_dump_defs (char const *filename, gboolean def_or_state);

/******************************************************************************/
/* Function group support */

typedef struct {
	GnmString *internal_name, *display_name;
	gboolean has_translation;
	GSList *functions;
} GnmFuncGroup;

GnmFuncGroup *gnm_func_group_get_nth (gint n);
GnmFuncGroup *gnm_func_group_fetch     		    (char const *name);
GnmFuncGroup *gnm_func_group_fetch_with_translation (char const *name,
						     char const *translation);

/******************************************************************************/

/*
 * Function registration routines
 *
 * Functions come in two fashions:  Those that only deal with
 * very specific data types and a constant number of arguments,
 * and those who don't.
 *
 * The former kind of functions receives a precomputed array of
 * GnmValue pointers.
 *
 * The latter sort of functions receives the plain ExprNodes and
 * it is up to that routine to do the value computations and range
 * processing.
 */

/**
 *  Argument tokens passed in 'args'
 *
 * With intersection and iteration support
 * 	f : float 		(no errors, string conversion attempted)
 * 	b : boolean		(identical to f, Do we need this ?)
 * 	s : string		(no errors)
 * 	S : 'scalar': any non-error value
 * 	E : scalar including errors
 * Without intersection or iteration support
 *	r : cell range	content is _NOT_ guaranteed to have been evaluated yet
 *	A : area	either range or array (as above)
 *	a : array
 *	? : anything
 *
 *  For optional arguments do:
 * "ff|ss" where the strings are optional
 **/

typedef enum {
	GNM_FUNC_TYPE_ARGS,	/* Arguments get marshalled by type */
	GNM_FUNC_TYPE_NODES,	/* Takes unevaulated expers directly */

	/* implementation has not been loaded yet, but we know where it is */
	GNM_FUNC_TYPE_STUB
} GnmFuncType;

typedef enum {
	GNM_FUNC_SIMPLE			= 0,
	GNM_FUNC_VOLATILE		= 1 << 0, /* eg now(), today() */
	GNM_FUNC_RETURNS_NON_SCALAR	= 1 << 1, /* eg transpose(), mmult() */

	/* For functions that are not exactly compatible with various import
	 * formats.  We need to recalc their results to avoid changing values
	 * unexpectedly when we recalc later.  This probably needs to be done
	 * on a per import format basis.  It may not belong here.
	 */
	GNM_FUNC_RECALC_ONLOAD 		= 1 << 2,

	/* an unknown function that will hopefully be defined later */
	GNM_FUNC_IS_PLACEHOLDER		= 1 << 3,
	GNM_FUNC_FREE_NAME		= 1 << 4,
	GNM_FUNC_IS_WORKBOOK_LOCAL	= 1 << 5,

	GNM_FUNC_AUTO_UNKNOWN           = 0 << 8,
	GNM_FUNC_AUTO_MONETARY          = 1 << 8,  /* Like PV */
	GNM_FUNC_AUTO_DATE              = 2 << 8,  /* Like DATE */
	GNM_FUNC_AUTO_TIME              = 3 << 8,  /* Like TIME */
	GNM_FUNC_AUTO_PERCENT           = 4 << 8,  /* Like IRR */
	GNM_FUNC_AUTO_FIRST             = 5 << 8,  /* Like SUM */
	GNM_FUNC_AUTO_SECOND            = 6 << 8,  /* Like IF */
	GNM_FUNC_AUTO_UNITLESS          = 7 << 8,  /* Like COUNT */
	GNM_FUNC_AUTO_MASK              = 7 << 8   /* The bits we use for AUTO.  */
} GnmFuncFlags;

/* I do not like this it is going to be different for different apps
 * probably want to split it into bit file with our notion of its state, and 2
 * bits of state per import format.
 */
typedef enum {
	GNM_FUNC_IMPL_STATUS_EXISTS = 0,
	GNM_FUNC_IMPL_STATUS_UNIMPLEMENTED,
	GNM_FUNC_IMPL_STATUS_SUBSET,
	GNM_FUNC_IMPL_STATUS_COMPLETE,
	GNM_FUNC_IMPL_STATUS_SUPERSET,
	GNM_FUNC_IMPL_STATUS_SUBSET_WITH_EXTENSIONS,
	GNM_FUNC_IMPL_STATUS_UNDER_DEVELOPMENT,
	GNM_FUNC_IMPL_STATUS_UNIQUE_TO_GNUMERIC
} GnmFuncImplStatus;

typedef enum {
	GNM_FUNC_TEST_STATUS_UNKNOWN = 0,
	GNM_FUNC_TEST_STATUS_NO_TESTSUITE,
	GNM_FUNC_TEST_STATUS_BASIC,
	GNM_FUNC_TEST_STATUS_EXHAUSTIVE,
	GNM_FUNC_TEST_STATUS_UNDER_DEVELOPMENT
} GnmFuncTestStatus;
typedef struct _GnmFuncDescriptor GnmFuncDescriptor;

typedef GnmValue 	*(*GnmFuncArgs)	  (FunctionEvalInfo *ei, GnmValue **args);
typedef GnmValue 	*(*GnmFuncNodes)  (FunctionEvalInfo *ei, GnmExprList *l);
typedef DependentFlags	 (*GnmFuncLink)	  (FunctionEvalInfo *ei);
typedef void		 (*GnmFuncUnlink) (FunctionEvalInfo *ei);

typedef void	 (*GnmFuncRefNotify) (GnmFunc *f, int refcount);
typedef gboolean (*GnmFuncLoadDesc)  (GnmFunc const *f, GnmFuncDescriptor *fd);

struct _GnmFuncDescriptor {
	char const *name;
	char const *arg_spec;
	char const *arg_names;
	char const **help;	/* this is easier for compilers */
	GnmFuncArgs	  fn_args;
	GnmFuncNodes	  fn_nodes;
	GnmFuncLink	  linker;
	GnmFuncUnlink	  unlinker;
	GnmFuncRefNotify  ref_notify;
	GnmFuncFlags	  flags;
	GnmFuncImplStatus impl_status;
	GnmFuncTestStatus test_status;
};

struct _GnmFunc {
	char const *name;
	char const *arg_names;
	char const *help;
	GnmFuncType fn_type;
	union {
		GnmFuncNodes nodes;
		struct {
			char const *arg_spec;
			GnmFuncArgs  func;
			int min_args, max_args;
			char *arg_types;
		} args;
		GnmFuncLoadDesc	load_desc;
	} fn;
	GnmFuncGroup		*fn_group; /* most recent it was assigned to */
	GnmFuncLink		 linker;
	GnmFuncUnlink		 unlinker;
	GnmFuncRefNotify	 ref_notify;
	GnmFuncImplStatus	 impl_status;
	GnmFuncTestStatus	 test_status;
	GnmFuncFlags		 flags;

	gint         		 ref_count;
	gpointer     		 user_data;
};

struct _FunctionEvalInfo {
	GnmEvalPos const *pos;
	GnmExprFunction const *func_call;
};

void	    gnm_func_free	     (GnmFunc *func);
void	    gnm_func_ref	     (GnmFunc *func);
void	    gnm_func_unref	     (GnmFunc *func);
void	    gnm_func_load_stub	     (GnmFunc *fn_def);
char const *gnm_func_get_name	     (GnmFunc const *fn_def);
gpointer    gnm_func_get_user_data   (GnmFunc const *func);
void        gnm_func_set_user_data   (GnmFunc *func, gpointer user_data);
GnmFunc	   *gnm_func_lookup	     (char const *name, Workbook const *scope);
GnmFunc    *gnm_func_add	     (GnmFuncGroup *group,
				      GnmFuncDescriptor const *descriptor);
GnmFunc    *gnm_func_add_stub	     (GnmFuncGroup *group,
				      char const *name,
				      GnmFuncLoadDesc  load_desc,
				      GnmFuncRefNotify opt_ref_notify);
GnmFunc    *gnm_func_add_placeholder (Workbook *optional_context,
				      char const *name,
				      char const *type,
				      gboolean copy_name);
GnmExpr const *gnm_func_placeholder_factory (const char *name,
					     GnmExprList *args,
					     GnmExprConventions *convs);


/* TODO */
void        function_def_count_args    (GnmFunc const *fn_def,
                                        gint *min, int *max);
char        function_def_get_arg_type  (GnmFunc const *fn_def,
                                        gint arg_idx);
char const *function_def_get_arg_type_string  (GnmFunc const *fn_def,
                                        gint arg_idx);
char       *function_def_get_arg_name  (GnmFunc const *fn_def,
                                        gint arg_idx);

/*************************************************************************/

GnmValue *function_call_with_list	     (FunctionEvalInfo *ei, GnmExprList *args,
				      GnmExprEvalFlags flags);
GnmValue *function_call_with_values     (GnmEvalPos const *ep, char const *name,
                                      gint argc, GnmValue *values []);
GnmValue *function_def_call_with_values (GnmEvalPos const *ep, GnmFunc const *fn,
                                      gint argc, GnmValue *values []);

/* Utilies to interate through ranges and argument lists */
typedef GnmValue * (*FunctionIterateCB) (GnmEvalPos const *ep,
                                      GnmValue *value, gpointer user_data);
GnmValue *function_iterate_argument_values	(GnmEvalPos const	   *ep,
                                         FunctionIterateCB  cb,
                                         gpointer           user_data,
                                         GnmExprList       *expr_node_list,
                                         gboolean           strict,
                                         CellIterFlags	    iter_flags);
GnmValue *function_iterate_do_value	(GnmEvalPos const      *ep,
					 FunctionIterateCB   cb,
					 gpointer            user_data,
					 GnmValue              *value,
					 gboolean            strict,
					 CellIterFlags	     iter_flags);

/******************************************************************************/

/* Detailed function help */
typedef struct {
	GPtrArray *sections;
	gboolean   help_is_localized;
	char     *help_copy;
	GnmFunc const *fndef;
} TokenizedHelp;

TokenizedHelp *tokenized_help_new     (GnmFunc const *fn_def);
char const    *tokenized_help_find    (TokenizedHelp *tok, char const *token);
void           tokenized_help_destroy (TokenizedHelp *tok);

#endif /* GNUMERIC_FUNC_H */

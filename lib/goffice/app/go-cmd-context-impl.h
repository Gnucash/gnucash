#ifndef GO_CMD_CONTEXT_IMPL_H
#define GO_CMD_CONTEXT_IMPL_H

#include <goffice/app/go-cmd-context.h>

typedef struct {
	GTypeInterface base;

	char *  (*get_password)		(GOCmdContext *gcc,
					 char const *filename);
	void    (*set_sensitive)	(GOCmdContext *gcc,
					 gboolean sensitive);
	struct {
		void (*error)		(GOCmdContext *gcc, GError *err);
		void (*error_info)  	(GOCmdContext *gcc, GOErrorStack *stack);
	} error;

	void    (*progress_set)		(GOCmdContext *gcc, float val);
	void    (*progress_message_set)	(GOCmdContext *gcc, gchar const *msg);
} GOCmdContextClass;

#define GO_CMD_CONTEXT_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST((k), GO_CMD_CONTEXT_TYPE, GOCmdContextClass))
#define IS_GO_CMD_CONTEXT_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE((k), GO_CMD_CONTEXT_TYPE))

/* protected, these do not really belong here, they are associated with io-context */
void  cmd_context_progress_set	       (GOCmdContext *gcc, float f);
void  cmd_context_progress_message_set (GOCmdContext *gcc, char const *msg);

#endif /* GO_CMD_CONTEXT_IMPL_H */

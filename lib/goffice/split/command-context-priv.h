#ifndef GNUMERIC_GNM_CMD_CONTEXT_PRIV_H
#define GNUMERIC_GNM_CMD_CONTEXT_PRIV_H

#include "command-context.h"

typedef struct {
	GTypeInterface base;

	char *  (*get_password)		(GnmCmdContext *cc,
					 char const *filename);
	void    (*set_sensitive)	(GnmCmdContext *cc,
					 gboolean sensitive);
	void    (*progress_set)		(GnmCmdContext *cc, gfloat val);
	void    (*progress_message_set)	(GnmCmdContext *cc, gchar const *msg);
	struct {
		void (*error)		(GnmCmdContext *cc, GError *err);
		void (*error_info)  	(GnmCmdContext *ctxt, ErrorInfo *error);
	} error;
} GnmCmdContextClass;

#define GNM_CMD_CONTEXT_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST((k), GNM_CMD_CONTEXT_TYPE, GnmCmdContextClass))
#define IS_GNM_CMD_CONTEXT_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), GNM_CMD_CONTEXT_TYPE))

/* protected, these do not really belong here, they are associated with io-context */
void  cmd_context_progress_set	       (GnmCmdContext *cc, gfloat f);
void  cmd_context_progress_message_set (GnmCmdContext *cc, char const *msg);

#endif /* GNUMERIC_GNM_CMD_CONTEXT_PRIV_H */

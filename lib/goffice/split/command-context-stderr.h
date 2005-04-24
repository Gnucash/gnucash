#ifndef GNUMERIC_COMMAND_CONTEXT_STDERR_H
#define GNUMERIC_COMMAND_CONTEXT_STDERR_H

#include "gnumeric.h"
#include <glib-object.h>

#define CMD_CONTEXT_STDERR_TYPE		(cmd_context_stderr_get_type ())
#define COMMAND_CONTEXT_STDERR(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), CMD_CONTEXT_STDERR_TYPE, CmdContextStderr))
#define IS_COMMAND_CONTEXT_STDERR(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), CMD_CONTEXT_STDERR_TYPE))

typedef struct _CmdContextStderr CmdContextStderr;

GType		cmd_context_stderr_get_type   (void);
GnmCmdContext  *cmd_context_stderr_new	      (void);
void		cmd_context_stderr_set_status (CmdContextStderr *, int status);
int		cmd_context_stderr_get_status (CmdContextStderr *);

#endif /* GNUMERIC_COMMAND_CONTEXT_STDERR_H */

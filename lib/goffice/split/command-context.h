#ifndef GNM_CMD_CONTEXT_H
#define GNM_CMD_CONTEXT_H

#include "gnumeric.h"
#include <glib-object.h>

#define GNM_CMD_CONTEXT_TYPE        (gnm_cmd_context_get_type ())
#define GNM_CMD_CONTEXT(o)          (G_TYPE_CHECK_INSTANCE_CAST ((o), GNM_CMD_CONTEXT_TYPE, GnmCmdContext))
#define IS_GNM_CMD_CONTEXT(o)       (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNM_CMD_CONTEXT_TYPE))

GType gnm_cmd_context_get_type (void);

void  gnm_cmd_context_error		 (GnmCmdContext *cc, GError *err);
char *gnm_cmd_context_get_password	 (GnmCmdContext *cc, char const *fname);
void  gnm_cmd_context_set_sensitive	 (GnmCmdContext *cc, gboolean flag);

/* utility routines for common errors */
void  gnm_cmd_context_error_system	 (GnmCmdContext *cc, char const *msg);
void  gnm_cmd_context_error_import	 (GnmCmdContext *cc, char const *msg);
void  gnm_cmd_context_error_export	 (GnmCmdContext *cc, char const *msg);
void  gnm_cmd_context_error_invalid	 (GnmCmdContext *cc,
					 char const *msg, char const *val);
void  gnm_cmd_context_error_info	 (GnmCmdContext *cc, ErrorInfo *error);

/* An initial set of std errors */
GQuark gnm_error_system  (void);
GQuark gnm_error_import  (void);
GQuark gnm_error_export  (void);
GQuark gnm_error_invalid (void);

/***************************************************************************/
/* some gnumeric specific utility routines */
void  gnm_cmd_context_error_calc	 (GnmCmdContext *cc, char const *msg);
void  gnm_cmd_context_error_splits_array (GnmCmdContext *cc, char const *cmd,
					  GnmRange const *array);

GQuark gnm_error_array   (void);
GQuark gnm_error_calc    (void);

#endif /* GNM_CMD_CONTEXT_H */

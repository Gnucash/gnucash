#ifndef GNUMERIC_ERROR_INFO_H
#define GNUMERIC_ERROR_INFO_H

#include <glib.h>

typedef enum {
	GNM_WARNING = 1,
	GNM_ERROR
} GnmSeverity;

ErrorInfo   *error_info_new_str			  (char const *msg);
ErrorInfo   *error_info_new_printf		  (char const *msg_format, ...) G_GNUC_PRINTF (1, 2);
ErrorInfo   *error_info_new_vprintf		  (GnmSeverity severity,
						   char const *msg_format,
						   va_list args);
ErrorInfo   *error_info_new_str_with_details	  (char const *msg, ErrorInfo *details);
ErrorInfo   *error_info_new_str_with_details_list (char const *msg, GSList *details);
ErrorInfo   *error_info_new_from_error_list	  (GSList *errors);
ErrorInfo   *error_info_new_from_errno		  (void);
void         error_info_add_details		  (ErrorInfo *error, ErrorInfo *details);
void         error_info_add_details_list	  (ErrorInfo *error, GSList *details);
void         error_info_free			  (ErrorInfo *error);
void         error_info_print			  (ErrorInfo *error);
char const  *error_info_peek_message		  (ErrorInfo *error);
GSList       *error_info_peek_details		  (ErrorInfo *error);
GnmSeverity  error_info_peek_severity		  (ErrorInfo *error);

#define GNM_INIT_RET_ERROR_INFO(ret_error) \
G_STMT_START { \
	g_assert (ret_error != NULL); \
	*ret_error = NULL; \
} G_STMT_END

#endif /* GNUMERIC_ERROR_INFO_H */

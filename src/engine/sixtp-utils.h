
#ifndef _SIXTP_UTILS_H_
#define _SIXTP_UTILS_H_

#include "date.h"

typedef struct {
  Timespec ts;
  guint s_block_count;
  guint ns_block_count;
} TimespecParseInfo;


gboolean isspace_str(const gchar *str, int nomorethan);

gboolean allow_and_ignore_only_whitespace(GSList *sibling_data,
                                          gpointer parent_data,
                                          gpointer global_data,
                                          gpointer *result,
                                          const char *text,
                                          int length);

gboolean generic_accumulate_chars(GSList *sibling_data,
                                  gpointer parent_data,
                                  gpointer global_data,
                                  gpointer *result,
                                  const char *text,
                                  int length);


void generic_free_data_for_children(gpointer data_for_children,
                                    GSList* data_from_children,
                                    GSList* sibling_data,
                                    gpointer parent_data,
                                    gpointer global_data,
                                    gpointer *result,
                                    const gchar *tag);

gchar * concatenate_child_result_chars(GSList *data_from_children);

gboolean string_to_double(const char *str, double *result);

gboolean string_to_gint64(const gchar *str, gint64 *v);

gboolean string_to_gint32(const gchar *str, gint32 *v);

gboolean hex_string_to_binary(const gchar *str,  void **v, guint64 *data_len);

gboolean generic_return_chars_end_handler(gpointer data_for_children,
                                          GSList* data_from_children,
                                          GSList* sibling_data,
                                          gpointer parent_data,
                                          gpointer global_data,
                                          gpointer *result,
                                          const gchar *tag);

sixtp* simple_chars_only_parser_new(sixtp_end_handler end_handler);

gboolean string_to_timespec_secs(const gchar *str, Timespec *ts);
gboolean string_to_timespec_nsecs(const gchar *str, Timespec *ts);


gboolean generic_timespec_start_handler(GSList* sibling_data,
                                        gpointer parent_data,
                                        gpointer global_data,
                                        gpointer *data_for_children,
                                        gpointer *result,
                                        const gchar *tag, gchar **attrs);

gboolean timespec_parse_ok(TimespecParseInfo *info);

gboolean generic_timespec_secs_end_handler(
    gpointer data_for_children,
    GSList  *data_from_children, GSList *sibling_data,
    gpointer parent_data, gpointer global_data,
    gpointer *result, const gchar *tag);

gboolean generic_timespec_nsecs_end_handler(
    gpointer data_for_children,
    GSList  *data_from_children, GSList *sibling_data,
    gpointer parent_data, gpointer global_data,
    gpointer *result, const gchar *tag);


sixtp* generic_timespec_parser_new(sixtp_end_handler end_handler);

gboolean generic_guid_end_handler(
    gpointer data_for_children,
    GSList  *data_from_children, GSList *sibling_data,
    gpointer parent_data, gpointer global_data,
    gpointer *result, const gchar *tag);

sixtp* generic_guid_parser_new(void);

gboolean generic_gnc_numeric_end_handler(
    gpointer data_for_children,
    GSList  *data_from_children, GSList *sibling_data,
    gpointer parent_data, gpointer global_data,
    gpointer *result, const gchar *tag);

sixtp* generic_gnc_numeric_parser_new(void);

sixtp* restore_char_generator(sixtp_end_handler ender);



#endif /* _SIXTP_UTILS_H_ */


#ifndef _SIXTP_STACK_H_
#define _SIXTP_STACK_H_

typedef struct sixtp_stack_frame {
  sixtp *parser;
  gchar *tag;
  gpointer data_for_children;
  GSList *data_from_children; /* in reverse chronological order */
  gpointer frame_data;
} sixtp_stack_frame;

void sixtp_stack_frame_destroy(sixtp_stack_frame *sf);

void sixtp_stack_frame_print(sixtp_stack_frame *sf, gint indent, FILE *f);

GSList* sixtp_pop_and_destroy_frame(GSList *frame_stack);

void sixtp_print_frame_stack(GSList *stack, FILE *f);


#endif /* _SIXTP_STACK_H_ */

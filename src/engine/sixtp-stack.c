#include "sixtp.h"
#include "sixtp-stack.h"

void
sixtp_stack_frame_destroy(sixtp_stack_frame *sf) {
  GSList *lp;

  /* cleanup all the child data */
  for(lp = sf->data_from_children; lp; lp = lp->next) {
    sixtp_child_result_destroy((sixtp_child_result *) lp->data);
  }
  g_slist_free(sf->data_from_children);
  sf->data_from_children = NULL;

  g_free(sf);
}

void
sixtp_stack_frame_print(sixtp_stack_frame *sf, gint indent, FILE *f) {
  gchar *is = g_strnfill(indent, ' ');

  fprintf(f, "%s(stack-frame %p\n", is, sf);
  fprintf(f, "%s             (parser %p)\n", is, sf->parser);
  fprintf(f, "%s             (tag %s)\n", is, sf->tag);
  fprintf(f, "%s             (data-for-children %p)\n", is, sf->data_for_children);

  {
    GSList *lp;
    fprintf(f, "%s             (data-from-children", is);
    for(lp = sf->data_from_children; lp; lp = lp->next) {
      fputc(' ', f);
      sixtp_child_result_print((sixtp_child_result *) lp->data, f);
    }
    fprintf(f, ")\n");
  }

  fprintf(f, "%s             (frame-data %p))\n", is, sf->frame_data);
  fflush(f);
}

GSList*
sixtp_pop_and_destroy_frame(GSList *frame_stack) {
  sixtp_stack_frame *dead_frame = (sixtp_stack_frame *) frame_stack->data;
  GSList* result;

  result = g_slist_next(frame_stack);
  sixtp_stack_frame_destroy(dead_frame);
  g_slist_free_1(frame_stack);
  return(result);
}

void
sixtp_print_frame_stack(GSList *stack, FILE *f) {
  /* first, some debugging output */
  GSList *printcopy = g_slist_reverse(g_slist_copy(stack));
  GSList *lp;
  int indent = 0;
  
  for(lp = printcopy; lp; lp = lp->next) {
    sixtp_stack_frame *frame = (sixtp_stack_frame *) lp->data;
    sixtp_stack_frame_print(frame, indent, f);
    indent += 2;
  }

}

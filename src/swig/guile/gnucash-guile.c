#include <stdio.h>
#include <readline.h>
#include <history.h>
#include <guile/gh.h>
#include <FileIO.h>

/* Because swig doesn't declare the prototype anywhere */
void gnucash_swig_init();

static void
guile_entry_point(int argc, char *argv[]) {
  char *input;
  // gnucash_swig_init();
  
/*    input = readline("gnucash> "); */
/*    while(input) { */
/*      SCM result = gh_eval_str(input); */
/*      gh_display(result); */
/*      gh_newline(); */
/*      add_history(input); */
/*      free(input); */
/*      input = readline("gnucash> "); */
/*    } */

  gnucash_swig_init();
  gh_repl(argc, argv);
}

int
main(int argc, char *argv[]) {
  gh_enter(argc, argv, guile_entry_point);
  return 0;
}

#include "config.h"
#include "qofsession.h"

int main()
{
  qof_log_init();
  qof_init();
  gnc_module_system_init();
  char * no_args[1] = { NULL };
  gnc_engine_init(0, no_args);

  QofSession * s = qof_session_new();
  qof_session_begin(s, "sqlite3:///tmp/blah.gnucash", 0, 1, 0);
  qof_session_load(s, NULL);
  qof_session_save(s, NULL);
  qof_session_end(s);
  return 0;
}

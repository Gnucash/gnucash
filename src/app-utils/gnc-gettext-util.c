#include <string.h>
#include <messages.h>

#include "gnc-gettext-util.h"

char *
gnc_gettext_helper(const char *string)
{
  return strdup(_(string));
}


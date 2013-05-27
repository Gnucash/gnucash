/* libfoo.  this is a dependency-free client library, equivalent to
 * the engine or some other core component that's ignorant of guile
 * and the module system */

#include <stdio.h>

#include "bar.h"

int
bar_hello(void)
{
    return 1;
}

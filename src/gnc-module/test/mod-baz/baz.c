/* libbaz.  this library depends on foo */

#include <stdio.h>

#include "baz.h"
#include "foo.h"

int
baz_hello(void)
{
    foo_hello();
    return 1;
}


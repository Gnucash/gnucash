#ifndef __IO_UTILS_H__
#define __IO_UTILS_H__

#include <stdio.h>

#include "gnc-book.h"
#include "Group.h"

void write_account_group(FILE *out, AccountGroup *grp);
void write_accounts(FILE *out, GNCBook *book);

void write_emacs_trailer(FILE *out);


#endif /* __IO_UTILS_H__ */    

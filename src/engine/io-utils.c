#include <stdio.h>

#include <glib.h>

#include "Group.h"
#include "gnc-book.h"
#include "gnc-xml.h"
#include "gnc-xml-helper.h"
#include "io-utils.h"

static const gchar *emacs_trailer =
"<!-- Local variables: -->\n"
"<!-- mode: xml        -->\n"
"<!-- End:             -->\n";


void
write_emacs_trailer(FILE *out)
{
    fprintf(out, emacs_trailer);
}

void
write_account_group(FILE *out, AccountGroup *grp)
{
    GList *list;
    GList *node;

    list = xaccGroupGetAccountList(grp);

    for (node = list; node; node = node->next) {
        xmlNodePtr accnode;
        AccountGroup *newgrp;
        
        accnode = gnc_account_dom_tree_create((Account*)(node->data));

        xmlElemDump(out, NULL, accnode);
        fprintf(out, "\n");

        xmlFreeNode(accnode);

        newgrp = xaccAccountGetChildren((Account*)(node->data));

        if(grp)
        {
            write_account_group(out, newgrp);
        }
    }
}

void
write_accounts(FILE *out, GNCBook *book)
{
    write_account_group(out, gnc_book_get_group(book));
}


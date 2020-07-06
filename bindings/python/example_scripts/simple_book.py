#!/usr/bin/env python3

##  @file
#   @brief Simple example for a book 
#   @ingroup python_bindings_examples

import sys
from gnucash import Session, SessionOpenMode

# We need to tell GnuCash the data format to create the new file as (xml://)
uri = "xml:///tmp/simple_book.gnucash"

print("uri:", uri)
with Session(uri, SessionOpenMode.SESSION_NEW_STORE) as ses:
    book = ses.get_book()

    #Call some methods that produce output to show that Book works
    book.get_root_account().SetDescription("hello, book")
    print("Book is saved:", not book.session_not_saved())

    #As long as there's no exceptions, book is automatically saved
    #when session ends.
    print("saving...")

print("Book is saved:", not book.session_not_saved())

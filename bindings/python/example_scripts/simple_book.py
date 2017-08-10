#!/usr/bin/env python

##  @file
#   @brief Simple example for a book 
#   @ingroup python_bindings_examples

import sys
from gnucash import Session

# We need to tell GnuCash the data format to create the new file as (xml://)
uri = "xml:///tmp/simple_book.gnucash"

print "uri:", uri
ses = Session(uri, is_new=True)
book = ses.get_book()

#Call some methods that produce output to show that Book works
book.get_root_account().SetDescription("hello, book")
print "Book is saved:", not book.session_not_saved()

print "saving..."
ses.save()

print "Book is saved:", not book.session_not_saved()
ses.end()

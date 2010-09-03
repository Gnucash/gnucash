#!/usr/bin/env python
import sys
from gnucash import Session

uri = "xml:///tmp/simple_book.gnucash"

print "uri:", uri
ses = Session(uri, is_new=True)
book = ses.get_book()

#Call some methods that produce output to show that Book works
book.get_root_account().SetDescription("hello, book")
print "Book is saved:", not book.not_saved()

print "saving..."
ses.save()

print "Book is saved:", not book.not_saved()
ses.end()

#!/usr/bin/env python
from gnucash import Book

book = Book()

#Call some methods that produce output to show that Book works
print "New book:"
book.print_dirty()
book.mark_saved()
print "\nBook marked saved:"
book.print_dirty()

book.destroy()

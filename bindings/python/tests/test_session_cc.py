# test cases for c++ Session wrapper object
#
# @date 2020-06-05
# @author Christoph Holtermann <mail@c-holtermann.net>

from unittest import TestCase, main

from gnucash import Book
from gnucash.gnucash_core_cc import QofSessionImpl

class TestQofSessionImpl(TestCase):
    def test_create_empty_session(self):
        book = Book()
        self.ses = QofSessionImpl(book.instance)
        book_2 = self.ses.get_book()
        self.assertEqual(book.instance, book_2)

if __name__ == '__main__':
    main()

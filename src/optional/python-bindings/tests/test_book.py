from unittest import TestCase, main

from gnucash import Book

class BookSession( TestCase ):
    def setUp(self):
        self.book = Book()

class TestBook( BookSession ):
    def test_markclosed(self):
        self.book.mark_closed()

if __name__ == '__main__':
    main()

from unittest import TestCase, main

from gnucash import Session

class BookSession(TestCase):
    def setUp(self):
        self.ses = Session()
        self.book = self.ses.get_book()
        self.table = self.book.get_table()
        self.currency = self.table.lookup('CURRENCY', 'EUR')

class TestBook(BookSession):
    def test_markclosed(self):
        self.ses.end()

if __name__ == '__main__':
    main()

from unittest import TestCase, main

from gnucash import Session

class BookSession( TestCase ):
    def setUp(self):
        self.ses = Session()
        self.book = self.ses.get_book()

class TestBook( BookSession ):
    def test_markclosed(self):
        self.ses.end()

if __name__ == '__main__':
    main()

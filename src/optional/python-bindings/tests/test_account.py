from unittest import main

from gnucash import Book, Account, Split

from test_book import BookSession

class AccountSession( BookSession ):
    def setUp(self):
        BookSession.setUp(self)
        self.account = Account(self.book)

class TestAccount( AccountSession ):
    def test_name(self):
        NAME = "Money"
        self.assertEquals( '', self.account.GetName() )
        self.account.SetName(NAME)
        self.assertEquals( NAME, self.account.GetName() )

    def test_split(self):
        SPLIT = Split(self.book)
        self.assertTrue(self.account.insert_split(SPLIT))
        self.assertTrue(self.account.find_split(SPLIT))
        self.assertTrue(self.account.remove_split(SPLIT))

if __name__ == '__main__':
    main()

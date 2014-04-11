from unittest import main

from gnucash import Book, Account, Split, Transaction

from test_book import BookSession

class SplitSession( BookSession ):
    def setUp(self):
        BookSession.setUp(self)
        self.split = Split(self.book)

class TestSplit( SplitSession ):
    def test_memo(self):
        MEMO = "cookie monster"
        self.assertEquals( '', self.split.GetMemo() )
        self.split.SetMemo(MEMO)
        self.assertEquals( MEMO, self.split.GetMemo() )

    def test_account(self):
        ACCT = Account(self.book)
        self.split.SetAccount(ACCT)
        self.assertTrue( ACCT.Equal(self.split.GetAccount(), True) )

    def test_transaction(self):
        TRANS = Transaction(self.book)
        self.split.SetParent(TRANS)
        TRANS.SetDescription("Foo")
        self.assertEquals( TRANS.GetDescription(), self.split.GetParent().GetDescription() )

    def test_equal(self):
        COPY = self.split
        self.assertTrue( self.split.Equal(COPY, True, False, False) )

if __name__ == '__main__':
    main()

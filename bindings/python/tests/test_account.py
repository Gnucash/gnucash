from unittest import main
from datetime import datetime
from gnucash import Book, Account, Split, GncCommodity, GncNumeric, \
    Transaction

from test_book import BookSession

class AccountSession( BookSession ):
    def setUp(self):
        BookSession.setUp(self)
        self.account = Account(self.book)

class TestAccount( AccountSession ):
    def test_name(self):
        NAME = "Money"
        self.assertEqual( '', self.account.GetName() )
        self.account.SetName(NAME)
        self.assertEqual( NAME, self.account.GetName() )

    def test_split(self):
        SPLIT = Split(self.book)
        self.assertTrue(self.account.insert_split(SPLIT))
        self.assertTrue(self.account.remove_split(SPLIT))

    def test_assignlots(self):
        abc = GncCommodity(self.book, 'ABC Fund',
            'COMMODITY','ABC','ABC',100000)
        self.table.insert(abc)
        self.account.SetCommodity(abc)

        other = Account(self.book)
        other.SetCommodity(self.currency)

        tx = Transaction(self.book)
        tx.BeginEdit()
        tx.SetCurrency(self.currency)
        tx.SetDateEnteredSecs(datetime.now())
        tx.SetDatePostedSecs(datetime.now())

        s1a = Split(self.book)
        s1a.SetParent(tx)
        s1a.SetAccount(self.account)
        s1a.SetAmount(GncNumeric(1.3))
        s1a.SetValue(GncNumeric(100.0))

        s1b = Split(self.book)
        s1b.SetParent(tx)
        s1b.SetAccount(other)
        s1b.SetAmount(GncNumeric(-100.0))
        s1b.SetValue(GncNumeric(-100.0))

        s2a = Split(self.book)
        s2a.SetParent(tx)
        s2a.SetAccount(self.account)
        s2a.SetAmount(GncNumeric(-1.3))
        s2a.SetValue(GncNumeric(-100.0))

        s2b = Split(self.book)
        s2b.SetParent(tx)
        s2b.SetAccount(other)
        s2b.SetAmount(GncNumeric(100.0))
        s2b.SetValue(GncNumeric(100.0))

        tx.CommitEdit()

        self.account.ScrubLots()
        self.assertEqual(len(self.account.GetLotList()),1)

if __name__ == '__main__':
    main()

from unittest import main

from gnucash import Book, Account, Split, Transaction
from unittest_support import *

from test_book import BookSession

class SplitSession( BookSession ):
    def setUp(self):

        BookSession.setUp(self)
        self.split = Split(self.book)

    def tearDown(self):
        pass

class TestSplit( SplitSession ):
    def test_memo(self):
        MEMO = "cookie monster"
        self.assertEqual( '', self.split.GetMemo() )
        self.split.SetMemo(MEMO)
        self.assertEqual( MEMO, self.split.GetMemo() )

    def test_account(self):
        ACCT = Account(self.book)
        ACCT.SetCommodity(self.currency)
        self.split.SetAccount(ACCT)
        self.assertTrue( ACCT.Equal(self.split.GetAccount(), True) )

    def test_transaction(self):
        domain1 = "gnc.engine.scrub"
        msg1 = "[xaccScrubUtilityGetOrMakeAccount()] No currency specified!"
        level = G_LOG_LEVEL_CRITICAL
        check1 = TestErrorStruct()
        check1.log_domain = domain1
        check1.log_level = level
        check1.msg = msg1
        hdlr1 = test_set_checked_handler(domain1, level, check1)
        domain2 = "gnc.engine"
        msg2 = "[xaccTransScrubSplits()] Transaction doesn't have a currency!"
        level = G_LOG_LEVEL_CRITICAL
        check2 = TestErrorStruct()
        check2.log_domain = domain2
        check2.log_level = level
        check2.msg = msg2
        hdlr2 = test_set_checked_handler(domain2, level, check2)

        TRANS = Transaction(self.book)
        self.split.SetParent(TRANS)
        TRANS.SetCurrency(self.currency)
        TRANS.SetDescription("Foo")
        self.assertEqual( TRANS.GetDescription(), self.split.GetParent().GetDescription() )

        g_log_remove_handler(domain2, hdlr2)
        g_log_remove_handler(domain1, hdlr1)

    def test_equal(self):
        COPY = self.split
        self.assertTrue( self.split.Equal(COPY, True, False, False) )

if __name__ == '__main__':
    main()

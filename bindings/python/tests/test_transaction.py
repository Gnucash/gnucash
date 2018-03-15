from unittest import main

from gnucash import Transaction, Book, Account, Split
from unittest_support import *

from test_book import BookSession

class TransactionSession( BookSession ):
    def setUp(self):
        self.domain1 = "gnc.engine"
        self.domain2 = "gnc.engine.scrub"
        level =  G_LOG_LEVEL_CRITICAL
        check1 = TestErrorStruct()
        check1.log_domain = self.domain1
        check1.log_level = level
        check1.msg = "[xaccScrubUtilityGetOrMakeAccount()] No currency specified!"
        test_add_error(check1)
        check2 = TestErrorStruct()
        check2.log_domain = self.domain1
        check2.log_level = level
        check2.msg = "[xaccTransScrubSplits()] Transaction doesn't have a currency!"
        test_add_error(check2)
        self.hdlr1 = test_set_list_handler(self.domain1, level, None)
        check3 = TestErrorStruct()
        check3.log_domain = "gnc.engine.scrub"
        check3.log_level = level
        check3.msg = "[xaccScrubUtilityGetOrMakeAccount()] No currency specified!"
        self.hdlr2 = test_set_checked_handler(self.domain2, level, check3)
        BookSession.setUp(self)
        self.trans = Transaction(self.book)
        #Evil bug means we must set a split for the transaction before making
        #any other changes (is slightly useful for later tests)
        self.split = Split(self.book)
        self.split.SetParent(self.trans)
        ############
        self.trans.SetCurrency(self.currency)

    def tearDown(self):
        g_log_remove_handler(self.domain1, self.hdlr1)
        g_log_remove_handler(self.domain2, self.hdlr2)
        test_clear_error_list ()

class TestTransaction( TransactionSession ):
    def test_equal(self):
        TRANS = self.trans
        self.assertTrue( TRANS.Equal(self.trans, True, False, False, False) )

    def test_clone(self):
        domain = "gnc.engine"
        level =  G_LOG_LEVEL_WARNING
        check = TestErrorStruct()
        check.log_domain = domain
        check.log_level = level
        check.msg = "[xaccTransEqual()] GUIDs differ"
        hdlr = test_set_checked_handler(domain, level, check)

        TRANS = self.trans.Clone()
       #Clone and original should have different GUIDs
        self.assertFalse( TRANS.Equal(self.trans, True, False, False, False) )
        #Clone and original should have the same balance
        self.assertTrue( TRANS.Equal(self.trans, False, False, True, False) )

        g_log_remove_handler(domain, hdlr)

    def test_setcurrency(self):
        self.assertTrue( self.currency.equal( self.trans.GetCurrency() ) )

    def test_edit(self):
        self.assertFalse( self.trans.IsOpen() )
        self.trans.BeginEdit()
        self.assertTrue( self.trans.IsOpen() )
        self.trans.CommitEdit()
        self.assertFalse( self.trans.IsOpen() )

    def test_rollback(self):
        self.assertEqual( '', self.trans.GetDescription() )
        self.trans.BeginEdit()
        DESC = 'Food'
        self.trans.SetDescription(DESC)
        self.assertEqual( DESC, self.trans.GetDescription() )
        self.trans.RollbackEdit()
        self.assertEqual( '', self.trans.GetDescription() )

    def test_findsplit(self):
        ACCT = Account(self.book)
        ACCT.SetCommodity(self.currency)
        self.split.SetAccount( ACCT )
        SPLIT = self.trans.FindSplitByAccount( ACCT )
        self.assertTrue( SPLIT.Equal(self.split, True, False, False) )

    def test_getsplit(self):
        SPLIT = self.trans.GetSplit(0)
        self.assertTrue( SPLIT.Equal(self.split, True, False, False) )

    def test_getsplitindex(self):
        self.assertEqual( 0, self.trans.GetSplitIndex(self.split) )

    def test_countsplits(self):
        self.assertEqual( 1, self.trans.CountSplits() )

    def test_readonly(self):
        self.assertEqual( None, self.trans.GetReadOnly() )
        REASON = 'none'
        self.trans.SetReadOnly(REASON)
        self.assertEqual( REASON, self.trans.GetReadOnly() )
        self.trans.ClearReadOnly()
        self.assertEqual( None, self.trans.GetReadOnly() )

    def test_txntype(self):
        self.assertEqual( '\x00', self.trans.GetTxnType() )
        TYPE = 'I'
        self.trans.SetTxnType(TYPE)
        self.assertEqual( TYPE, self.trans.GetTxnType() )
        TYPE = 'P'
        self.trans.SetTxnType(TYPE)
        self.assertEqual( TYPE, self.trans.GetTxnType() )

    def test_num(self):
        NUM = '5'
        self.assertEqual( '', self.trans.GetNum() )
        self.trans.SetNum(NUM)
        self.assertEqual( NUM, self.trans.GetNum() )

    def test_description(self):
        DESCR = 'Groceries'
        self.assertEqual( '', self.trans.GetDescription() )
        self.trans.SetDescription(DESCR)
        self.assertEqual( DESCR, self.trans.GetDescription() )

    def test_notes(self):
        NOTE = 'For dinner party'
        self.assertEqual( None, self.trans.GetNotes() )
        self.trans.SetNotes(NOTE)
        self.assertEqual( NOTE, self.trans.GetNotes() )

if __name__ == '__main__':
    main()

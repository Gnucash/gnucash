from unittest import main

from gnucash import Transaction, Book, Account, Split

from test_book import BookSession

class TransactionSession( BookSession ):
    def setUp(self):
        BookSession.setUp(self)
        self.trans = Transaction(self.book)
        #Evil bug means we must set a split for the transaction before making
        #any other changes (is slightly useful for later tests)
        self.split = Split(self.book)
        self.split.SetParent(self.trans)
        ############

class TestTransaction( TransactionSession ):
    def test_equal(self):
        TRANS = self.trans
        self.assertTrue( TRANS.Equal(self.trans, True, False, False, False) )

    def test_clone(self):
        TRANS = self.trans.Clone()
        #Clone and original should have different GUIDs
        self.assertFalse( TRANS.Equal(self.trans, True, False, False, False) )
        #Clone and original should have the same balance
        self.assertTrue( TRANS.Equal(self.trans, False, False, True, False) )

    def test_edit(self):
        self.assertFalse( self.trans.IsOpen() )
        self.trans.BeginEdit()
        self.assertTrue( self.trans.IsOpen() )
        self.trans.CommitEdit()
        self.assertFalse( self.trans.IsOpen() )

    def test_rollback(self):
        self.assertEquals( '', self.trans.GetDescription() )
        self.trans.BeginEdit()
        DESC = 'Food'
        self.trans.SetDescription(DESC)
        self.assertEquals( DESC, self.trans.GetDescription() )
        self.trans.RollbackEdit() 
        self.assertEquals( '', self.trans.GetDescription() )

    def test_findsplit(self):
        ACCT = Account(self.book)
        self.split.SetAccount( ACCT )
        SPLIT = self.trans.FindSplitByAccount( ACCT )
        self.assertTrue( SPLIT.Equal(self.split, True, False, False) )
    
    def test_getsplit(self):
        SPLIT = self.trans.GetSplit(0)
        self.assertTrue( SPLIT.Equal(self.split, True, False, False) )
        
    def test_getsplitindex(self):
        self.assertEquals( 0, self.trans.GetSplitIndex(self.split) )

    def test_countsplits(self):
        self.assertEquals( 1, self.trans.CountSplits() )

    def test_readonly(self):
        self.assertEquals( None, self.trans.GetReadOnly() )
        REASON = 'none'
        self.trans.SetReadOnly(REASON)
        self.assertEquals( REASON, self.trans.GetReadOnly() )
        self.trans.ClearReadOnly()
        self.assertEquals( None, self.trans.GetReadOnly() )

    def test_txntype(self):
        self.assertEquals( '\x00', self.trans.GetTxnType() )
        TYPE = 'I'
        self.trans.SetTxnType(TYPE)
        self.assertEquals( TYPE, self.trans.GetTxnType() )
        TYPE = 'P'
        self.trans.SetTxnType(TYPE)
        self.assertEquals( TYPE, self.trans.GetTxnType() )

    def test_num(self):
        NUM = '5'
        self.assertEquals( '', self.trans.GetNum() )
        self.trans.SetNum(NUM)
        self.assertEquals( NUM, self.trans.GetNum() )

    def test_description(self):
        DESCR = 'Groceries'
        self.assertEquals( '', self.trans.GetDescription() )
        self.trans.SetDescription(DESCR)
        self.assertEquals( DESCR, self.trans.GetDescription() )

    def test_notes(self):
        NOTE = 'For dinner party'
        self.assertEquals( None, self.trans.GetNotes() )
        self.trans.SetNotes(NOTE)
        self.assertEquals( NOTE, self.trans.GetNotes() )

if __name__ == '__main__':
    main()

from unittest import main
from gnucash import Book, Account, GncLot, Split, GncNumeric

from test_account import AccountSession
# from test_split import SplitSession

class LotSession(AccountSession):
    def setUp(self):
        AccountSession.setUp(self)
        self.NUM = 10000
        self.amount = GncNumeric(self.NUM, 100)

    def setup_buysplit(self):
        self.buysplit = Split(self.book)
        self.buysplit.SetAccount(self.account)
        self.buysplit.SetAmount(self.amount)

    def setup_sellsplit(self):
        self.sellsplit = Split(self.book)
        self.sellsplit.SetAccount(self.account)
        self.sellsplit.SetAmount(self.amount.neg())

class TestLot(LotSession):
    def test_make_default(self):
        self.lot = GncLot.make_default(self.account)
        self.assertIsInstance(self.lot, GncLot)

    def test_AssignToLot(self):
        self.lot = GncLot.make_default(self.account)
        
        self.setup_buysplit()
        self.buysplit.AssignToLot(self.lot)
        self.assertEqual(self.NUM, self.lot.get_balance().num())
        self.assertTrue(not self.lot.is_closed())

        self.setup_sellsplit()
        self.sellsplit.AssignToLot(self.lot)
        self.assertEqual(0, self.lot.get_balance().num())
        self.assertTrue(self.lot.is_closed())

    def test_Split_GetLot(self):
        self.lot = GncLot.make_default(self.account)
        self.setup_buysplit()
        self.buysplit.AssignToLot(self.lot)
        rtn_lot = self.buysplit.GetLot()
        self.assertEqual(rtn_lot.get_title(), self.lot.get_title())
 
    def test_get_split_list(self):
        self.lot = GncLot.make_default(self.account)
        self.setup_buysplit()
        self.buysplit.AssignToLot(self.lot)
        splits = self.lot.get_split_list()
        self.assertEqual(self.NUM, splits[0].GetAmount().num())
        self.assertEqual(self.account.name, splits[0].GetAccount().name)

    def test_Account_GetLotList(self):
        self.lot = GncLot.make_default(self.account)
        self.setup_buysplit()
        self.buysplit.AssignToLot(self.lot)
        lots = self.account.GetLotList()
        self.assertEqual(self.account.name, lots[0].get_account().name)

if __name__ == '__main__':
    unittest.main()

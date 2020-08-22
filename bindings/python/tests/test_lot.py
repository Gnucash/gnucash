from unittest import main
from gnucash import Book, Account, GncLot

from test_account import AccountSession

class LotSession(AccountSession):
    def setUp(self):
        AccountSession.setUp(self)

class TestLot(LotSession):
    def test_make_default(self):
        lot = GncLot.make_default(self.account)
        self.assertIsInstance(lot, GncLot)

if __name__ == '__main__':
    unittest.main()

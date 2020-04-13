from unittest import TestCase, main

from gnucash import Session

class CommoditySession(TestCase):
    def setUp(self):
        self.ses = Session()
        self.book = self.ses.get_book()
        self.table = self.book.get_table()

    def tearDown(self):
        self.ses.end()

class TestCommodity(CommoditySession):
    def test_iso_currency(self):
        eur = self.table.lookup('CURRENCY', 'EUR')
        self.assertIsNotNone(eur)

class TestCommodityNamespace(CommoditySession):
    def test_namespaces(self):
        #print(self.table.__class__)
        namespace_names = self.table.get_namespaces()
        #print(namespace_names)
        self.assertEqual(namespace_names, ['AMEX', 'NYSE', 'NASDAQ', 'EUREX', 'FUND', 'template', 'CURRENCY'])

    def test_namespaces_list(self):
        namespaces = self.table.get_namespaces_list()
        namespace_names = [ns.get_name() for ns in namespaces]
        self.assertEqual(namespace_names, ['AMEX', 'NYSE', 'NASDAQ', 'EUREX', 'FUND', 'template', 'CURRENCY'])

if __name__ == '__main__':
    main()

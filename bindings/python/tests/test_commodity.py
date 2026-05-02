from unittest import TestCase, main
from unittest.mock import Mock, patch

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
        self.assertEqual(namespace_names, ['template', 'CURRENCY'])

    def test_namespaces_list(self):
        namespaces = self.table.get_namespaces_list()
        namespace_names = [ns.get_name() for ns in namespaces]
        self.assertEqual(namespace_names, ['template', 'CURRENCY'])

    def test_get_namespaces_py_mocked(self):
        # Create some mock namespace objects that respond to get_name()
        mock_ns1 = Mock()
        mock_ns1.get_name.return_value = 'mock_ns_1'
        mock_ns2 = Mock()
        mock_ns2.get_name.return_value = 'mock_ns_2'

        # Use patch.object to mock self.table.get_namespaces_list to return our mock objects
        with patch.object(self.table, 'get_namespaces_list', return_value=[mock_ns1, mock_ns2]):
            namespace_names = self.table._get_namespaces_py()

            # Verify the result is exactly the mapped list of get_name() calls
            self.assertEqual(namespace_names, ['mock_ns_1', 'mock_ns_2'])

            # Verify get_namespaces_list was called once
            self.table.get_namespaces_list.assert_called_once()

            # Verify get_name was called once on each mock
            mock_ns1.get_name.assert_called_once()
            mock_ns2.get_name.assert_called_once()

    def test_get_namespaces_py_empty(self):
        # Edge case: getting an empty list of namespaces
        with patch.object(self.table, 'get_namespaces_list', return_value=[]):
            namespace_names = self.table._get_namespaces_py()

            self.assertEqual(namespace_names, [])
            self.table.get_namespaces_list.assert_called_once()

if __name__ == '__main__':
    main()

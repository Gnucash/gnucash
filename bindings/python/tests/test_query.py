from unittest import TestCase, main

from gnucash import Query
from gnucash.gnucash_core_c import GNC_ID_INVOICE


class TestQuery(TestCase):
    def test_create(self):
        query = Query()
        self.assertIsInstance(query, Query)

    def test_search_for(self):
        query = Query()

        query.search_for(GNC_ID_INVOICE)
        self.assertEqual(query.get_search_for(), GNC_ID_INVOICE)

        obj_type = 'gncInvoice'
        query.search_for(obj_type)
        self.assertEqual(query.get_search_for(), obj_type)

if __name__ == '__main__':
    main()

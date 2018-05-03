
set(example_scripts_DATA
        account_analysis.py
        change_tax_code.py
        Invoice.tex
        latex_invoices.py
        new_book_with_opening_balances.py
        simple_book.py
        simple_business_create.py
        simple_invoice_insert.py
        simple_session.py
        simple_sqlite_create.py
        simple_test.py
        test_imbalance_transaction.py
        rest-api/gnucash_rest.py
        rest-api/gnucash_simple.py
        rest-api/README
        )

set_local_dist(example_scripts_DIST_local CMakeLists.txt ${example_scripts_DATA})
set(example_scripts_DIST ${example_scripts_DIST_local} PARENT_SCOPE)

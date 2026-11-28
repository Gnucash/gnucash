
from tempfile import TemporaryDirectory
from unittest import (
    TestCase,
    expectedFailure,
    main,
    )

from gnucash import (
    Session,
    SessionOpenMode,
    )


class BookSession(TestCase):
    def setUp(self):
        self.ses = Session()
        self.book = self.ses.get_book()
        self.table = self.book.get_table()
        self.currency = self.table.lookup('CURRENCY', 'EUR')


class TestBook(BookSession):
    def test_markclosed(self):
        self.ses.end()


@expectedFailure
class TestBookSqlite(TestCase):
    """Testing books with the sqlite backend.
    """
    def test_create_empty(self):
        """Test if just creating a book leaves it in a usable state (not locked).
        """

        with TemporaryDirectory() as tmpdir:
            targetfile = f"sqlite3://{tmpdir}/new_empty.gnucash"
            with Session(targetfile, SessionOpenMode.SESSION_NEW_STORE):
                pass
                # get_root_account() seemed necessary in 5.15 to trigger creation of tables.
                # session.book.get_root_account()
            print(f"Gnucash file {targetfile} created.")

            with Session(targetfile, SessionOpenMode.SESSION_NORMAL_OPEN):
                print("Opening again worked.")
            print("Closed again.")


if __name__ == '__main__':
    main()

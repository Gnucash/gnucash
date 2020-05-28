# test gettext
#
# @date 2020-04-08
# @author Christoph Holtermann <mail@c-holtermann.net>

from unittest import TestCase, main
import gnucash
from gnucash import _sw_core_utils


class TestGettext(TestCase):
    def test_import_gettext(self):
        import gettext

    def test_get_localedir(self):
        _localedir = _sw_core_utils.gnc_path_get_localedir()

    def test_translation(self):
        import gettext
        _localedir = _sw_core_utils.gnc_path_get_localedir()
        t = gettext.translation(_sw_core_utils.GETTEXT_PACKAGE, _localedir)
        self.assertIn("project-id-version", t.info())
        self.assertIn("GnuCash", t.info()["project-id-version"])

if __name__ == '__main__':
    main()

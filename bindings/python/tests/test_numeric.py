from unittest import TestCase, main

from gnucash import GncNumeric, GNC_DENOM_AUTO, GNC_HOW_DENOM_FIXED, \
    GNC_HOW_RND_NEVER, GNC_HOW_RND_FLOOR, GNC_HOW_RND_CEIL

class TestGncNumeric( TestCase ):
    def test_defaut(self):
        num = GncNumeric()
        self.assertEqual(str(num), "0/1")
        self.assertEqual(num.num(), 0)
        self.assertEqual(num.denom(), 1)

    def test_from_num_denom(self):
        num = GncNumeric(1, 2)
        self.assertEqual(str(num), "1/2")
        self.assertEqual(num.num(), 1)
        self.assertEqual(num.denom(), 2)

    def test_from_int(self):
        num = GncNumeric(3)
        self.assertEqual(str(num), "3/1")
        self.assertEqual(num.num(), 3)
        self.assertEqual(num.denom(), 1)

        with self.assertRaises(Exception) as context:
            GncNumeric((2**64)+1)

        #On Linux it raises an OverflowError while on MacOS it's a TypeError.
        self.assertTrue(isinstance(context.exception, TypeError) or
                        isinstance(context.exception, OverflowError))

    def test_from_float(self):
        num = GncNumeric(3.1, 20, GNC_HOW_DENOM_FIXED | GNC_HOW_RND_NEVER)
        self.assertEqual(str(num), "62/20")
        self.assertEqual(num.num(), 62)
        self.assertEqual(num.denom(), 20)

        num = GncNumeric(1/3.0, 10000000000, GNC_HOW_RND_FLOOR)
        self.assertEqual(str(num), "3333333333/10000000000")
        self.assertEqual(num.num(), 3333333333)
        self.assertEqual(num.denom(), 10000000000)

        num = GncNumeric(1/3.0, 10000000000, GNC_HOW_RND_CEIL)
        self.assertEqual(str(num), "3333333334/10000000000")
        self.assertEqual(num.num(), 3333333334)
        self.assertEqual(num.denom(), 10000000000)

    def test_from_float_auto(self):
        num = GncNumeric(3.1)
        self.assertEqual(str(num), "31/10")
        self.assertEqual(num.num(), 31)
        self.assertEqual(num.denom(), 10)

    def test_from_instance(self):
        orig = GncNumeric(3)
        num = GncNumeric(instance=orig.instance)
        self.assertEqual(str(num), "3/1")
        self.assertEqual(num.num(), 3)
        self.assertEqual(num.denom(), 1)

    def test_from_str(self):
        num = GncNumeric("3.1")
        self.assertEqual(str(num), "31/10")
        self.assertEqual(num.num(), 31)
        self.assertEqual(num.denom(), 10)

        num = GncNumeric("1/3")
        self.assertEqual(str(num), "1/3")
        self.assertEqual(num.num(), 1)
        self.assertEqual(num.denom(), 3)

    def test_to_str(self):
        num = GncNumeric("1000/3")
        self.assertEqual(str(num), "1000/3")

        num = GncNumeric(1, 0)
        self.assertEqual(str(num), "1/0")

    def test_to_double(self):
        for test_num in [0.0, 1.1, -1.1, 1/3.0]:
            self.assertEqual(GncNumeric(test_num).to_double(), test_num)

    def test_to_fraction(self):
        fraction = GncNumeric("1000/3").to_fraction()
        self.assertEqual(fraction.numerator, 1000)
        self.assertEqual(fraction.denominator, 3)

    def test_incorect_args(self):
        with self.assertRaises(TypeError):
            GncNumeric(1, 2, 3)

        with self.assertRaises(TypeError):
            GncNumeric("1", 2)

        with self.assertRaises(TypeError):
            GncNumeric(1.1, "round")

        with self.assertRaises(TypeError):
            GncNumeric(complex(1, 1))

if __name__ == '__main__':
    main()

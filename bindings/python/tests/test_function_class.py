# test cases for function_class.py
#
# @date 2020-06-18
# @author Christoph Holtermann <mail@c-holtermann.net>

import sys
from unittest import TestCase, main
from gnucash.function_class import (
    ClassFromFunctions,
    default_arguments_decorator,
    return_instance_if_value_has_it,
    process_list_convert_to_instance,
    process_dict_convert_to_instance,
    extract_attributes_with_prefix,
)


class Instance:
    """instance class for ClassFromFunction tests"""

    pass


class ReturnClass(object):
    def __init__(self, **kargs):
        self.instance = kargs.get("instance")


def prefix_new_function():
    """new function for ClassFromFunction tests
    
    returns instance of Instance class"""
    return Instance()


def prefix_test_function(self):
    """test function for ClassFromFunction tests"""
    return True


def prefix_test_function_return_args(self, *args, **kargs):
    return self, args, kargs


b_default = "b default value"


def prefix_test_function_return_arg_karg(self, a, b=b_default):
    return {"self": self, "a": a, "b": b}


def other_function(self, arg=None):
    return self, arg


def returns_instance_data(self):
    return "some_data"


def returns_instance_data_list(self):
    return ["data1", "data2"]


class TestClass(ClassFromFunctions):
    _module = sys.modules[__name__]

    pass


class TestFunctionClass(TestCase):
    def setUp(self):
        # Ensure TestClass is properly set up with a constructor
        TestClass.add_constructor_and_methods_with_prefix("prefix_", "new_function")

    def test_add_constructor_and_methods_with_prefix(self):
        self.TestClass = TestClass
        self.testClass = TestClass()
        self.assertIsInstance(self.testClass.instance, Instance)
        self.assertTrue(self.testClass.test_function())

    def test_add_method(self):
        """test if add_method adds method and if in case of FunctionClass
        Instance instances get returned instead of FunctionClass instances"""
        TestClass.add_method("other_function", "other_method")
        self.t = TestClass()
        obj, arg = self.t.other_method()
        self.assertIsInstance(obj, Instance)
        obj, arg = self.t.other_method(self.t)
        self.assertIsInstance(arg, Instance)
        obj, arg = self.t.other_method(arg=self.t)
        self.assertIsInstance(arg, Instance)

    def test_ya_add_method(self):
        """test if ya_add_method adds method and if in case of FunctionClass
        Instance instances get returned instead of FunctionClass instances
        with the exception of self (first) argument"""
        TestClass.ya_add_method("other_function", "other_method")
        self.t = TestClass()
        obj, arg = self.t.other_method()
        self.assertIsInstance(obj, TestClass)
        obj, arg = self.t.other_method(self.t)
        self.assertIsInstance(arg, Instance)
        obj, arg = self.t.other_method(arg=self.t)
        self.assertIsInstance(arg, Instance)

    def test_ya_add_classmethod(self):
        """test if ya_add_classmethod adds classmethod and if in case of FunctionClass
        Instance instances get returned instead of FunctionClass instances
        with the exception of self (first) argument"""
        TestClass.ya_add_classmethod("other_function", "other_classmethod")
        obj, arg = TestClass.other_classmethod()
        self.assertIs(obj, TestClass)
        self.t = TestClass()
        obj, arg = TestClass.other_classmethod(self.t)
        self.assertIsInstance(arg, Instance)
        obj, arg = TestClass.other_classmethod(arg=self.t)
        self.assertIsInstance(arg, Instance)

    def test_decorate_functions(self):
        """test decorate_functions()"""

        def simple_decorator(func):
            def wrapper(*args, **kargs):
                return "decorated", func(*args, **kargs)

            return wrapper

        TestClass.decorate_functions(
            simple_decorator, "test_function", "test_function_return_args"
        )
        t = TestClass()
        self.assertEqual(t.test_function()[0], "decorated")
        self.assertEqual(t.test_function_return_args()[0], "decorated")

    def test_methods_return_instance(self):
        """test methods_return_instance()"""
        from gnucash.function_class import methods_return_instance

        # TestClass.returns_instance_data is already defined in module scope
        # but we need it as a method
        TestClass.add_method("returns_instance_data", "returns_instance_data")

        methods_return_instance(TestClass, {"returns_instance_data": TestClass})
        t = TestClass()
        result = t.returns_instance_data()
        self.assertIsInstance(result, TestClass)
        self.assertEqual(result.instance, "some_data")

    def test_methods_return_instance_lists(self):
        """test methods_return_instance_lists()"""
        from gnucash.function_class import methods_return_instance_lists

        TestClass.add_method("returns_instance_data_list", "returns_instance_data_list")

        methods_return_instance_lists(
            TestClass, {"returns_instance_data_list": TestClass}
        )
        t = TestClass()
        result_list = t.returns_instance_data_list()
        self.assertIsInstance(result_list, list)
        self.assertEqual(len(result_list), 2)
        self.assertIsInstance(result_list[0], TestClass)
        self.assertEqual(result_list[0].instance, "data1")

    def test_method_function_returns_instance(self):
        """test method_function_returns_instance()"""
        from gnucash.function_class import method_function_returns_instance

        def returns_instance_data(self):
            return Instance()

        def returns_none(self):
            return None

        decorated_returns_instance = method_function_returns_instance(
            returns_instance_data, TestClass
        )
        decorated_returns_none = method_function_returns_instance(
            returns_none, TestClass
        )

        t = TestClass()

        result_instance = decorated_returns_instance(t)
        self.assertIsInstance(result_instance, TestClass)
        self.assertIsInstance(result_instance.instance, Instance)

        result_none = decorated_returns_none(t)
        self.assertIsNone(result_none)

    def test_method_function_returns_instance_list(self):
        """test method_function_returns_instance_list()"""
        from gnucash.function_class import method_function_returns_instance_list

        def returns_instance_data_list(self):
            return [Instance(), Instance()]

        decorated_returns_list = method_function_returns_instance_list(
            returns_instance_data_list, TestClass
        )

        t = TestClass()

        result_list = decorated_returns_list(t)
        self.assertIsInstance(result_list, list)
        self.assertEqual(len(result_list), 2)
        for item in result_list:
            self.assertIsInstance(item, TestClass)
            self.assertIsInstance(item.instance, Instance)

    def test_methods_return_instance_lists_with_returns_list(self):
        """test methods_return_instance_lists()"""
        from gnucash.function_class import methods_return_instance_lists

        def returns_list(self):
            return [Instance(), Instance()]

        TestClass.returns_list = returns_list
        methods_return_instance_lists(TestClass, {"returns_list": TestClass})

        t = TestClass()
        result_list = t.returns_list()
        self.assertIsInstance(result_list, list)
        self.assertEqual(len(result_list), 2)
        for item in result_list:
            self.assertIsInstance(item, TestClass)
            self.assertIsInstance(item.instance, Instance)

    def test_methods_return_instance_lists_dict(self):
        """test methods_return_instance_lists() on line 249"""
        from gnucash.function_class import methods_return_instance_lists

        class LocalTestClass(ClassFromFunctions):
            _module = sys.modules[__name__]
            def returns_list(self): return [Instance(), Instance()]

        methods_return_instance_lists(LocalTestClass, {"returns_list": TestClass})

        ltc = LocalTestClass(instance=Instance())
        result_list = ltc.returns_list()
        self.assertIsInstance(result_list, list)
        self.assertEqual(len(result_list), 2)
        for item in result_list:
            self.assertIsInstance(item, TestClass)
            self.assertIsInstance(item.instance, Instance)

    def test_methods_return_instance_with_returns_single(self):
        """test methods_return_instance()"""
        from gnucash.function_class import methods_return_instance

        def returns_single(self):
            return Instance()

        TestClass.returns_single = returns_single
        methods_return_instance(TestClass, {"returns_single": TestClass})

        t = TestClass()
        result = t.returns_single()
        self.assertIsInstance(result, TestClass)
        self.assertIsInstance(result.instance, Instance)

    def test_default_arguments_decorator(self):
        """test default_arguments_decorator()"""
        TestClass.backup_test_function_return_args = TestClass.test_function_return_args
        TestClass.backup_test_function_return_arg_karg = (
            TestClass.test_function_return_arg_karg
        )
        self.t = TestClass()

        arg1 = "arg1"
        arg2 = "arg2"
        arg3 = {"arg3": arg2}
        arg4 = 4
        TestClass.decorate_method(
            default_arguments_decorator, "test_function_return_args", arg1, arg2
        )
        self.assertEqual(
            self.t.test_function_return_args(), (self.t.instance, (arg2,), {})
        )  # default arg1 gets overwritten by class instances instance attribute
        self.assertEqual(
            self.t.test_function_return_args(arg3), (self.t.instance, (arg3,), {})
        )
        self.assertEqual(
            self.t.test_function_return_args(arg1, arg3),
            (self.t.instance, (arg1, arg3), {}),
        )
        self.assertEqual(
            self.t.test_function_return_args(arg1, arg3, arg4=arg4),
            (self.t.instance, (arg1, arg3), {"arg4": arg4}),
        )

        TestClass.test_function_return_args = TestClass.backup_test_function_return_args
        TestClass.decorate_method(
            default_arguments_decorator,
            "test_function_return_args",
            arg1,
            arg2,
            arg4=arg4,
        )
        # Verify kargs_pos fix: call multiple times
        for _ in range(2):
            self.assertEqual(
                self.t.test_function_return_args(),
                (self.t.instance, (arg2,), {"arg4": arg4}),
            )
        self.assertEqual(
            self.t.test_function_return_args(arg1, arg3, arg4=arg2),
            (self.t.instance, (arg1, arg3), {"arg4": arg2}),
        )

        with self.assertRaises(TypeError):
            # should fail because a is set both as a positional and as a keyword argument
            TestClass.decorate_method(
                default_arguments_decorator,
                "test_function_return_arg_karg",
                None,
                arg1,
                a=arg2,
                kargs_pos={"a": 1, "b": 2},
            )
        TestClass.decorate_method(
            default_arguments_decorator,
            "test_function_return_arg_karg",
            None,
            a=arg1,
            kargs_pos={"a": 1, "b": 2},
        )
        self.assertEqual(
            self.t.test_function_return_arg_karg(),
            {"self": self.t.instance, "a": arg1, "b": b_default},
        )

        TestClass.test_function_return_arg_karg = (
            TestClass.backup_test_function_return_arg_karg
        )
        TestClass.decorate_method(
            default_arguments_decorator,
            "test_function_return_arg_karg",
            None,
            arg1,
            kargs_pos={"a": 1, "b": 2},
        )
        self.assertEqual(
            self.t.test_function_return_arg_karg(),
            {"self": self.t.instance, "a": arg1, "b": b_default},
        )
        self.assertEqual(
            self.t.test_function_return_arg_karg(arg2),
            {"self": self.t.instance, "a": arg2, "b": b_default},
        )
        self.assertEqual(
            self.t.test_function_return_arg_karg(arg2, arg3),
            {"self": self.t.instance, "a": arg2, "b": arg3},
        )

        # Test positional argument overwriting keyword default (covers line 295)
        # We need a function where a keyword argument's position is less than the number of positional args
        def test_func_overlap(self, a, b="default_b"):
            return a, b

        TestClass.test_func_overlap = test_func_overlap
        TestClass.decorate_method(
            default_arguments_decorator,
            "test_func_overlap",
            b="default_b",
            kargs_pos={"b": 2},
        )
        # Calling with 2 positional args, the second one should overwrite the keyword default for 'b'
        self.assertEqual(self.t.test_func_overlap("val_a", "val_b"), ("val_a", "val_b"))

    def test_return_instance_if_value_has_it(self):
        """test return_instance_if_value_has_it()"""
        t = TestClass()
        self.assertEqual(return_instance_if_value_has_it(t), t.instance)
        self.assertEqual(return_instance_if_value_has_it(5), 5)
        self.assertEqual(return_instance_if_value_has_it("string"), "string")
        self.assertIsNone(return_instance_if_value_has_it(None))

        # Object with instance attribute but not ClassFromFunctions
        class NotFunctionClass:
            def __init__(self):
                self.instance = "fake"

        not_fc = NotFunctionClass()
        self.assertEqual(return_instance_if_value_has_it(not_fc), not_fc)

    def test_process_list_convert_to_instance(self):
        """test process_list_convert_to_instance()"""
        t1 = TestClass()
        t2 = TestClass()
        input_list = [t1, 5, t2, "string", None]
        expected_list = [t1.instance, 5, t2.instance, "string", None]
        self.assertEqual(process_list_convert_to_instance(input_list), expected_list)

        # Empty list
        self.assertEqual(process_list_convert_to_instance([]), [])

        # Non-mutation check
        original_list = [t1]
        process_list_convert_to_instance(original_list)
        self.assertIs(original_list[0], t1)

        # Nested list (not deep converted)
        nested_list = [[t1]]
        result = process_list_convert_to_instance(nested_list)
        self.assertIs(result[0][0], t1)

    def test_process_dict_convert_to_instance(self):
        """test process_dict_convert_to_instance()"""
        t1 = TestClass()
        t2 = TestClass()
        input_dict = {"a": t1, "b": 5, "c": t2, "d": "string", "e": None}
        expected_dict = {
            "a": t1.instance,
            "b": 5,
            "c": t2.instance,
            "d": "string",
            "e": None,
        }
        self.assertEqual(process_dict_convert_to_instance(input_dict), expected_dict)

        # Empty dict
        self.assertEqual(process_dict_convert_to_instance({}), {})

        # Non-mutation check
        original_dict = {"a": t1}
        process_dict_convert_to_instance(original_dict)
        self.assertIs(original_dict["a"], t1)

        # Shallow conversion check (nested dict)
        nested_dict = {"outer": {"inner": t1}}
        result = process_dict_convert_to_instance(nested_dict)
        self.assertIs(result["outer"]["inner"], t1)

        # Object with instance attribute but not ClassFromFunctions
        class NotFunctionClass:
            def __init__(self):
                self.instance = "fake"

        not_fc = NotFunctionClass()
        self.assertEqual(process_dict_convert_to_instance({"a": not_fc}), {"a": not_fc})

        # kwargs expansion explicitly (simulating actual function_class.py usage)
        def dummy_func(**kwargs):
            return kwargs

        test_kwargs = {"a": t1, "other": "string"}
        processed_kwargs = process_dict_convert_to_instance(test_kwargs)
        self.assertEqual(dummy_func(**processed_kwargs), {"a": t1.instance, "other": "string"})

    def test_extract_attributes_with_prefix(self):
        """test extract_attributes_with_prefix()"""

        class Sample:
            prefix_a = 1
            prefix_b = 2
            other = 3
            prefix_ = 4

        # Test with class
        results = list(extract_attributes_with_prefix(Sample, "prefix_"))
        self.assertEqual(len(results), 3)
        self.assertIn(("prefix_a", 1, "a"), results)
        self.assertIn(("prefix_b", 2, "b"), results)
        self.assertIn(("prefix_", 4, ""), results)

        # Test with instance
        s = Sample()
        s.prefix_c = 5
        results = list(extract_attributes_with_prefix(s, "prefix_"))
        # Instance __dict__ only contains instance attributes
        self.assertEqual(len(results), 1)
        self.assertIn(("prefix_c", 5, "c"), results)

        # Test with module
        import gnucash.function_class as fc
        results = list(extract_attributes_with_prefix(fc, "process_"))
        self.assertEqual(len(results), 2)
        self.assertIn(("process_list_convert_to_instance", fc.process_list_convert_to_instance, "list_convert_to_instance"), results)
        self.assertIn(("process_dict_convert_to_instance", fc.process_dict_convert_to_instance, "dict_convert_to_instance"), results)

        # Test no matches
        results = list(extract_attributes_with_prefix(Sample, "nonexistent"))
        self.assertEqual(len(results), 0)

        # Test empty prefix
        results = list(extract_attributes_with_prefix(s, ""))
        self.assertIn(("prefix_c", 5, "prefix_c"), results)

    def test_ya_add_classmethod(self):
        """test ya_add_classmethod()"""
        TestClass.ya_add_classmethod("prefix_test_function", "test_classmethod")
        self.assertEqual(TestClass.test_classmethod(), True)

    def test_decorate_functions_with_local_class(self):
        """test decorate_functions()"""
        def simple_decorator(func):
            def wrapper(*args, **kwargs):
                return "decorated"
            return wrapper

        class LocalTestClass(ClassFromFunctions):
            _module = sys.modules[__name__]
            def f1(self): return 1
            def f2(self): return 2

        LocalTestClass.decorate_functions(simple_decorator, "f1", "f2")
        ltc = LocalTestClass(instance=Instance())
        self.assertEqual(ltc.f1(), "decorated")
        self.assertEqual(ltc.f2(), "decorated")

    def test_methods_return_instance_with_local_class(self):
        """test methods_return_instance()"""
        from gnucash.function_class import methods_return_instance

        class LocalTestClass(ClassFromFunctions):
            _module = sys.modules[__name__]
            def get_data(self): return Instance()

        methods_return_instance(LocalTestClass, {"get_data": TestClass})
        ltc = LocalTestClass(instance=Instance())
        result = ltc.get_data()
        self.assertIsInstance(result, TestClass)
        self.assertIsInstance(result.instance, Instance)

    def test_methods_return_instance_lists_with_local_class(self):
        """test methods_return_instance_lists()"""
        from gnucash.function_class import methods_return_instance_lists

        class LocalTestClass(ClassFromFunctions):
            _module = sys.modules[__name__]
            def get_list(self): return [Instance(), Instance()]

        methods_return_instance_lists(LocalTestClass, {"get_list": TestClass})
        ltc = LocalTestClass(instance=Instance())
        result = ltc.get_list()
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 2)
        for item in result:
            self.assertIsInstance(item, TestClass)
            self.assertIsInstance(item.instance, Instance)



    def test_extract_attributes_with_prefix(self):
        """test extract_attributes_with_prefix()"""

        class Sample:
            prefix_a = 1
            prefix_b = 2
            other = 3
            prefix_ = 4

        # Test with class
        results = list(extract_attributes_with_prefix(Sample, "prefix_"))
        self.assertEqual(len(results), 3)
        self.assertIn(("prefix_a", 1, "a"), results)
        self.assertIn(("prefix_b", 2, "b"), results)
        self.assertIn(("prefix_", 4, ""), results)

        # Test with instance
        s = Sample()
        s.prefix_c = 5
        results = list(extract_attributes_with_prefix(s, "prefix_"))
        # Instance __dict__ only contains instance attributes
        self.assertEqual(len(results), 1)
        self.assertIn(("prefix_c", 5, "c"), results)

        # Test with module
        import gnucash.function_class as fc
        results = list(extract_attributes_with_prefix(fc, "process_"))
        self.assertEqual(len(results), 2)
        self.assertIn(("process_list_convert_to_instance", fc.process_list_convert_to_instance, "list_convert_to_instance"), results)
        self.assertIn(("process_dict_convert_to_instance", fc.process_dict_convert_to_instance, "dict_convert_to_instance"), results)

        # Test no matches
        results = list(extract_attributes_with_prefix(Sample, "nonexistent"))
        self.assertEqual(len(results), 0)

        # Test empty prefix
        results = list(extract_attributes_with_prefix(s, ""))
        self.assertIn(("prefix_c", 5, "prefix_c"), results)

if __name__ == "__main__":
    main()

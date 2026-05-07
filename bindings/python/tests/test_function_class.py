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
)


class Instance:
    """instance class for ClassFromFunction tests"""

    pass


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


class TestClass(ClassFromFunctions):
    _module = sys.modules[__name__]

    pass


class TestFunctionClass(TestCase):
    def test_add_constructor_and_methods_with_prefix(self):
        TestClass.add_constructor_and_methods_with_prefix("prefix_", "new_function")
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

    def test_return_instance_if_value_has_it(self):
        """test return_instance_if_value_has_it()"""
        t = TestClass()
        self.assertEqual(return_instance_if_value_has_it(t), t.instance)
        self.assertEqual(return_instance_if_value_has_it(5), 5)
        self.assertEqual(return_instance_if_value_has_it("string"), "string")
        self.assertIsNone(return_instance_if_value_has_it(None))

    def test_process_list_convert_to_instance(self):
        """test process_list_convert_to_instance()"""
        t1 = TestClass()
        t2 = TestClass()
        input_list = [t1, 5, t2, "string", None]
        expected_list = [t1.instance, 5, t2.instance, "string", None]
        self.assertEqual(process_list_convert_to_instance(input_list), expected_list)

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


if __name__ == "__main__":
    main()

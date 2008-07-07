# function_class.py -- Library for making python classes from a set
#                      of functions. 
#
# Copyright (C) 2008 ParIT Worker Co-operative <paritinfo@parit.ca>
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, contact:
# Free Software Foundation           Voice:  +1-617-542-5942
# 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
# Boston, MA  02110-1301,  USA       gnu@gnu.org
#
# @author Mark Jenkins, ParIT Worker Co-operative <mark@parit.ca>

INSTANCE_ARGUMENT = "instance"

class ClassFromFunctions(object):
    """Inherit this class to give yourself a python class that wraps a set of
    functions that together consitute the methods of the class.

    The method functions must all have as a first argument an object
    holding the instance data. There must also be a function that
    returns a new instance of the class, the constructor.

    Your subclass must define
    _module - The module where the method functions, including the
    constructor can be found
    _new_instance - The name of a function that serves as a constructor,
    returning the instance data.

    To access the instance data, use the read-only property instance.

    To add some functions from _module as methods, call classmethods like
    add_method and add_methods_with_prefix.
    """
    def __new__(cls, *args, **kargs):
        # why reimpliment __new__? Because later on we're going to
        # use new to avoid creating new instances when existing instances
        # already exist with the same __instance value, or equivlent __instance
        # values, where this is desirable...
        return super(ClassFromFunctions, cls).__new__(cls, *args, **kargs)
    
    def __init__(self, *args, **kargs):
        """Construct a new instance, using either the function
        self._module[self._new_instance] or using existing instance
        data. (specified with the keyword argument, instance)

        Pass the arguments that should be passed on to
        self._module[self._new_instance] . Any arguments of that
        are instances of ClassFromFunctions will be switched with the instance
        data. (by calling the .instance property)
        """
        if INSTANCE_ARGUMENT in kargs:
            self.__instance = kargs[INSTANCE_ARGUMENT]
        else:
            self.__instance = getattr(self._module, self._new_instance)(
                *process_list_convert_to_instance(args) )

    def get_instance(self):
        """Get the instance data.

        You can also call the instance property
        """
        return self.__instance

    instance = property(get_instance)

    # CLASS METHODS

    @classmethod
    def add_method(cls, function_name, method_name):
        """Add the function, method_name to this class as a method named name
        """
        def method_function(self, *meth_func_args):
            return getattr(self._module, function_name)(
                self.instance,
                *process_list_convert_to_instance(meth_func_args) )
        
        setattr(cls, method_name, method_function)
        setattr(method_function, "__name__", method_name)
        return method_function
    
    @classmethod
    def add_methods_with_prefix(cls, prefix):
        """Add a group of functions with the same prefix 
        """
        for function_name, function_value, after_prefix in \
            extract_attributes_with_prefix(cls._module, prefix):
                cls.add_method(function_name, after_prefix)
    
    @classmethod
    def add_constructor_and_methods_with_prefix(cls, prefix, constructor):
        """Add a group of functions with the same prefix, and set the
        _new_instance attribute to prefix + constructor
        """
        cls.add_methods_with_prefix(prefix)
        cls._new_instance = prefix + constructor

    @classmethod
    def decorate_functions(cls, decorator, *args):
        for function_name in args:
            setattr( cls, function_name,
                     decorator( getattr(cls, function_name) ) )

def method_function_returns_instance(method_function, cls):
    """A function decorator that is used to decorates method functions that
    return instance data, to return instances instead.

    You can't use this decorator with @, because this function has a second
    argument.
    """
    assert( 'instance' == INSTANCE_ARGUMENT )
    def new_function(*args):
        kargs = { INSTANCE_ARGUMENT : method_function(*args) }
        return cls( **kargs )
    
    return new_function

def default_arguments_decorator(function, *args):
    """Decorates a function to give it default, positional arguments

    You can't use this decorator with @, because this function has more
    than one argument.
    """
    def new_function(*function_args):
        new_argset = list(function_args)
        new_argset.extend( args[ len(function_args): ] )
        return function( *new_argset )
    return new_function
    
def return_instance_if_value_has_it(value):
    """Return value.instance if value is an instance of ClassFromFunctions,
    else return value
    """
    if isinstance(value, ClassFromFunctions):
        return value.instance
    else:
        return value

def process_list_convert_to_instance( value_list ):
    """Return a list built from value_list, where if a value is in an instance
    of ClassFromFunctions, we put value.instance in the list instead.

    Things that are not instances of ClassFromFunctions are returned to
    the new list unchanged.
    """
    return [ return_instance_if_value_has_it(value)
             for value in value_list ]

def extract_attributes_with_prefix(obj, prefix):
    """Generator that iterates through the attributes of an object and
    for any attribute that matches a prefix, this yields
    the attribute name, the attribute value, and the text that appears
    after the prefix in the name
    """
    for attr_name, attr_value in obj.__dict__.iteritems():
        if attr_name.startswith(prefix):
            after_prefix = attr_name[ len(prefix): ]
            yield attr_name, attr_value, after_prefix

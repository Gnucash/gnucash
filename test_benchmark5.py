import timeit

setup = """
keyname = 'Unknown'
"""

stmt_list = """
if keyname in ['Return', 'KP_Enter']:
    pass
"""

stmt_set = """
if keyname in {'Return', 'KP_Enter'}:
    pass
"""

list_time = timeit.timeit(stmt_list, setup, number=10000000)
set_time = timeit.timeit(stmt_set, setup, number=10000000)

print(f"List (Not found): {list_time:.4f}s")
print(f"Set (Not found):  {set_time:.4f}s")
print(f"Improvement: {(list_time - set_time) / list_time * 100:.2f}%")

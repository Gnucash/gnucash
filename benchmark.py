import timeit
import dis

def test_list(key):
    return key in ['Return', 'KP_Enter']

def test_set(key):
    return key in {'Return', 'KP_Enter'}

print("--- Bytecode for list ---")
dis.dis(test_list)
print("--- Bytecode for set ---")
dis.dis(test_set)

list_time = timeit.timeit("key in ['Return', 'KP_Enter']", setup="key='KP_Enter'", number=10000000)
set_time = timeit.timeit("key in {'Return', 'KP_Enter'}", setup="key='KP_Enter'", number=10000000)

print(f"List time (10M ops): {list_time:.4f}s")
print(f"Set time (10M ops):  {set_time:.4f}s")

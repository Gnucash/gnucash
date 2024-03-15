#!/usr/bin/env python3

##  @file
#   @brief Some examples for GncNumeric to illustrate conversion and comparison
#   @author Christoph Holtermann
#   @date 2022-03-20
#   @ingroup python_bindings_examples

from gnucash import GncNumeric
from gnucash import GNC_DENOM_AUTO, GNC_HOW_DENOM_SIGFIG, GNC_HOW_RND_ROUND

# Create from int
n1 = 1
g1 = GncNumeric(n1)
print("1 - create from int")
print(n1, "(", type(n1), ") =>", g1)
print(n1, "==", g1, ":", n1 == g1)
print()

# Create from String
n2 = "1.1"
g2 = GncNumeric(n2)
print("2 - create from string")
print(n2, "(", type(n2), ") =>", g2)
print()

# Create from two ints
n3 = (11,10)
g3 = GncNumeric(*n3)
print("3 - create from two ints")
print(n3, "(", type(n3), ") =>", g3)
print()

# Create from float
n4 = 2.2
g4 = GncNumeric(n4)
print("4 - create from float")
print(n4, "(", type(n4), ") =>", g4)
print(n4, "==", g4, ":", n4 == g4)
print('Same float with higher precision: %.32f' % n4)
print()

# Create from float
n5 = 2245.67
g5 = GncNumeric(n5)
print("5 - create from float")
print(n5, "(", type(n5), ") =>", g5)
print(n5, "==", g5, ":", n5 == g5)
print('Same float with higher precision: %.32f' % n5)
print()

# Compare float, string and int
print("Compare GncNumeric from float (4) to GncNumeric from string (2)")
print(g2, "==", g4, ":", g2 == g4)
print()

print("Compare GncNumeric from float (4) to GncNumeric from int (3)")
print(g3, "==", g4, ":", g3 == g4)
print()

print("Compare GncNumeric from string (2) to GncNumeric from int (3)")
print(g2, "==", g3, ":", g2 == g3)
print()

def GNC_HOW_DENOM_SIGFIGS(n):
    """Leaned on the C Macro"""
    return GNC_HOW_DENOM_SIGFIG | (n << 8 & GNC_NUMERIC_SIGFIGS_MASK)

GNC_NUMERIC_SIGFIGS_MASK = 0x0000ff00

# Create from float with specified significant figures
print(f"Create from float (Example 4, ={n4}) with specified significant figures")
for sigfigs in range(0,20):
    print(sigfigs, GncNumeric(n4, GNC_DENOM_AUTO, GNC_HOW_DENOM_SIGFIGS(sigfigs)))
print()

# Convert GncNumeric from float to remove errors
print("Convert float to different denominator to remove conversion errors")
g4b = g4.convert(1000, GNC_HOW_RND_ROUND)
print(g4, "(", type(g4), ") =>", g4b)
print()

# Create from float with specified significant figures
print(f"Create from float (Example 5, ={n5}) with specified significant figures")
for sigfigs in range(0,20):
    print(sigfigs, GncNumeric(n5, GNC_DENOM_AUTO, GNC_HOW_DENOM_SIGFIGS(sigfigs)))
print()

# Convert GncNumeric from float to remove errors
print("Convert float to different denominator to remove conversion errors")
g5b = g5.convert(1000, GNC_HOW_RND_ROUND)
print(g5, "(", type(g5), ") =>", g5b)


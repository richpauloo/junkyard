## ------------------------------------------------ 
# https://docs.python.org/3/tutorial/inputoutput.html#input-and-output
# facier output formatting

# use f"{arg}" for formatted string literals
#%%
year = 2021
event = "Aftertimes"
f"The year is {year} and we are in the {event}!"

# use str.format(), but it's more complex
# https://docs.python.org/3/library/string.html#formatstrings
#%%
yes_votes = 42_572_654
no_votes = 43_132_495
percentage = yes_votes / (yes_votes + no_votes)
'{:-9} YES votes  {:2.2%}'.format(yes_votes, percentage)

# note that underscores break up a 
# long numeric literal for readability

#%% use str() and repr() for debugging and to display values
# by converting them into "strings" and "representations"
s = "Hello world"
str(s)

# %%
repr(s)

# %%
str(1/7)

# %%
x = 10 * 3.25
y = 200 * 200
s = 'The value of x is ' + repr(x) + ', and y is ' + repr(y) + '...'
print(s)

# %% The repr() of a string adds string quotes and backslashes:
hello = 'hello, world\n'
hellos = repr(hello)
print(hellos)

# %% The argument to repr() may be any Python object:
repr((x, y, ('spam', 'eggs')))

#%% more formatted sting literals (f strings)
import math
print(f"The value of pi is approximately {math.pi:.3f}")

# %% passing an integer after the ":" will cause that
# field to be a minimum number of characters wide. This is
# useful for making columns line up.
cats = {"louis": "9 lbs", "nina": "7 lbs"}
for cat, weight in cats.items():
    print(f"{cat:6} ==> {weight}")

# %% modifiers convert value before its formatted
animals = "kittens"
print(f"My home is full of {animals!r}")


## ------------------------------------------------ 
# string .format() method

#%% general use
"my kittens are named {} and {}".format("louis", "nina")

# %% use indices 
"my kittens are named {1} and {0}".format("louis", "nina")

# %% use keywords
"my kittens are {louis} and {nina}".format(
    louis = "silly", nina = "smart"
)

# %% combine indices and keywords
"my kittens are {1}, {0}, {adopted}".format(
    "louis", "nina", adopted = "ella"
)

# %% to format a long string, use a dictionary
table = {'Sjoerd': 4127, 'Jack': 4098, 'Dcab': 8637678}
print('Jack: {0[Jack]:d}; Sjoerd: {0[Sjoerd]:d}; '
      'Dcab: {0[Dcab]:d}'.format(table))

# %% use **
table = {'Sjoerd': 4127, 'Jack': 4098, 'Dcab': 8637678}
print('Jack: {Jack:d}; Sjoerd: {Sjoerd:d}; Dcab: {Dcab:d}'.format(**table))

# %% another example: integers and their squares and cubes
for x in range(1, 11):
    print("{0:2d} {1:3d} {2:4d}".format(x, x*x, x*x*x))


## ------------------------------------------------ 
# manual string formatting
# %%
for x in range(1, 11):
    print(repr(x).rjust(2), repr(x*x).rjust(3), end=' ')
    # Note use of 'end' on previous line
    print(repr(x*x*x).rjust(4))

# %% use .zfill() to pad a numeric string with zeros on the left
"1".zfill(3)
# %%
"1.01".zfill(3)
# %%
for i in range(0, 10):
    print(f"{str(i).zfill(3)}_file.csv")
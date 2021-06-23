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

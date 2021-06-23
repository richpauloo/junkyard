# can modify a dictionary in-place
#%%
users = {
  "Louis": "active",
  "Nina":  "inactive"
}

#for user, status in users.copy().items():
#    if status == "inactive":
#        del users[user]

# but it's perferable to use create a new object
# # and use the .items() method 
# %%
active_users = {}
for user, status in users.items():
    if status == "active":
        active_users[user] = status

print(active_users)

# add entries to a dictionary
# %%
active_users["Rich"] = "cool"

# range() creates an iterable
#%% 
for i in range(0, 10, 2):
    print(i)

# len() the length of a list
#%%
a = ["rich", "says", "hi"]
for i in range(len(a)):
    print(i, a[i])

# function def
#%%
def parrot(voltage, state='a stiff', action='voom', type='Norwegian Blue'):
    print("-- This parrot wouldn't", action, end=' ')
    print("if you put", voltage, "volts through it.")
    print("-- Lovely plumage, the", type)
    print("-- It's", state, "!")

parrot('a million', 'bereft of life', 'jump') 

# .join() method 
# %%
" ".join(["hi","there"])

# use * (splat) to split a list into arguments, which can 
# then be passed to a function
#%%
list(range(3,6))
args = [3,6]
list(range(*args))

# use ** to split a dictionary into arguments
#%% 
d = {
    "voltage": "four million", 
    "state": "bleedin' demised", 
    "action": "VOOM", 
    "type": "LouisandNina"
    }
parrot(**d)

# sort a list
# %%
cats = ["louis", "nina", "ella"]
#cats.count("nina")
sorted(cats)

# looping with dictionaries using .items() method
#%%
cats = {"louis": "is silly", "nina": "is smart"}
for cat, trait in cats.items():
    print(cat, trait)

# use enumerate to access indices
#%%
for i, v in enumerate(["hi","there"]):
    print(i, v)

# use zip to loop over multiple lists at the same time
#%%
cats = ["louis", "nina"]
traits = ["silly", "smart"]
for cat, trait in zip(cats, traits):
    print(f"{cat} is {trait}.")

# set() is like unique()
#%%
unique_cats = set(["louis","louis","nina","ella"])
unique_cats

# del is like rm()
#%%%
del unique_cats


# tuples are immutatble
#%%
t = 1, 2, "rich"
t

# slicing works
#%%
t[0:2]

#%%
t[1:]

# execute module as script - if a module is run with
# `python module.py <arguments>` then the code will
# be executed with __name__ set to "__main__". Thus add
# this to the end of your script:
# if __name__ == "__main__":
#     import sys
#     my_function(int(sys.argv[1]))


# %%
class Citizen:
    """Long lost Pythonic citizen class"""
    def __init__(self, first_name, last_name):
        self.first_name = first_name
        self.last_name  = last_name
    
    def full_name(self):
        fn = f"{self.first_name} {self.last_name}"
        return fn 
    
    greeting = "For the glory of Python!"

x = Citizen(first_name="rich",last_name="pauloo")
x.full_name()

# %%
x  = Citizen("rich", "pauloo")
x.full_name()
# %%
x.greeting
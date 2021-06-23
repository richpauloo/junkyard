# https://gist.github.com/MRobertEvers/55a989b4883ea8d7715d2e156627f034
# https://docs.python.org/3/tutorial/modules.html#packages

# by placing an __init_.py in a directory makes any directory with
# __init__.py importable as a module. This allows us to abstract
# modules into a separate directory and keep things nice and clean
from modules.fibo import fib

fib(1000)  
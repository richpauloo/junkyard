## ------------------------------------------------ 
# modules

# use "import" to import a module by name
# function definitions in the module accessible by .<fun_name>
import fibo
fibo.fib(1000)

# use "as" to import module with a name
import fibo as fib
fib.fib(500)

# use "from" to import a function directly
from fibo import fib
fib(100)

# use "from" to import a function directly and rename
from fibo import fib as fibonacci
fibonacci(50)

## ------------------------------------------------ 
# execute modules as scripts

# when running a module as a script, e.g., python fibo.py <args>
# then __name__ is set to "__main__". Use

# if __name__ == "__main__":
#     import sys
#     fib(int(sys.argv[1]))

# at the end of script to allow it to be callable as a script
# and cmd line tool. 
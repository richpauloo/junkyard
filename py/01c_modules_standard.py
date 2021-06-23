## ------------------------------------------------ 
# standard modules
import sys, fibo

# append a new path to the path object (a list)
#sys.path.append("some/new/path")

# dir(<module>) is a sorted list of all names a module defines
print("\n".join(dir(sys)))

# dir() without arguments lists names currently defined
#%%
RICH_IS_COOL = True
z = "NOTICE ME IN THE PRINTED OUTPUT"
dir()
print("\n".join(dir()))

## ------------------------------------------------ 
# use __init__.py to initialize package structure, e.g., 
# https://docs.python.org/3/tutorial/modules.html#packages

# sound/                          Top-level package
#       __init__.py               Initialize the sound package
#       formats/                  Subpackage for file format conversions
#               __init__.py
#               wavread.py
#               wavwrite.py
#               aiffread.py
#               aiffwrite.py
#               auread.py
#               auwrite.py
#               ...
#       effects/                  Subpackage for sound effects
#               __init__.py
#               echo.py
#               surround.py
#               reverse.py
#               ...
#       filters/                  Subpackage for filters
#               __init__.py
#               equalizer.py
#               vocoder.py
#               karaoke.py
#               ...


# bad practice to import everything, i.e., 
# from package.module import *
import sys

# capture first input (not script name)
secret_message = sys.argv[1]

python_is_glorious = True
failure_is_option = False

proper_greeting = False

if secret_message == "For the glory of Python!":
    proper_greeting = True
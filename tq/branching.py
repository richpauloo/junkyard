# add two numbers passed to script together

import sys

# convert input arguments from srings to integers
first_number  = int(sys.argv[1])
second_number = int(sys.argv[2])

my_sum = first_number + second_number

if my_sum <= 0:
    print("You have chosen the path of destitution.")

if my_sum >= 1 and my_sum <= 100:
    print("You have chosen the path of plenty.")

if my_sum > 100:
    print("You have chosen the path of excess.")
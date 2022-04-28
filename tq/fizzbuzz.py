import sys

# capture input and remove script name
vals = sys.argv
vals.pop(0)

# loop over all inputs
for item in vals:
    # convert input to number
    as_num = int(item)
    
    output_string = ""
 
    div_by_3 = as_num % 3
    div_by_5 = as_num % 5
    
    if div_by_3 == 0:
        output_string = "fizz"

    if div_by_5 == 0:
        output_string = "buzz"

    if div_by_3 == 0 and div_by_5 == 0:
        output_string = "fizzbuzz"

    if div_by_3 > 0 and div_by_5 > 0:
        output_string = as_num

    print(output_string)

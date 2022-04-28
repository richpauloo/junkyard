import sys

leaders = sys.argv
leaders.pop(0)

for index, item in enumerate(leaders, start = 1):
    print(f"{index}. {item}")
import sys

input = sys.stdin.readline

T = int(input())

for _ in range(T):
    arg = input().split()
    if len(arg) == 1:
        print(''.join(' '*int(arg[0])))
    else:
        R, S = arg
        print(''.join(ch*int(R) for ch in S))


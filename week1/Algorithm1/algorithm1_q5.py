import sys

n = int(sys.stdin.readline())
s = set(map(int,sys.stdin.readline().split()))

dp = [1 for _ in range(n+1)]
 
for j in s:
    if j==1:
        continue
    for i in range(1,n+1):
        if i-j>=0:
            dp[i] += dp[i-j]

print(dp[n])

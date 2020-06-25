import sys

n = int(sys.stdin.readline())
dp = []


for _ in range(n):
    dp.append(list(map(int,sys.stdin.readline().split())))

res = 0
for i in range(n):
    for j in range(n):
        if (dp[i][j]==1 and dp[i][j-1]!=0 and dp[i-1][j-1]!=0 and dp[i-1][j]!=0):
            dp[i][j] = min(dp[i][j-1], dp[i-1][j-1], dp[i-1][j]) + 1
            if res < dp[i][j]:
                res = dp[i][j]

print(res**2)


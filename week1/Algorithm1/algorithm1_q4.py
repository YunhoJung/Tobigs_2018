import sys

n = int(sys.stdin.readline())
mat = []
for _ in range(n):
    mat.append(list(map(int,sys.stdin.readline().split())))

dp = [[0] * n for _ in range(n)]

print(dp)

for i in range(1,n):
    for j in range(0,n-i):
        if i==1:
            dp[j][j+i] = mat[j][0] * mat[j][1] * mat[j+1][1]
            
        dp[j][j+i] = 2**32
        for k in range(j,j+i):
            dp[j][j+i] = min(dp[j][j+i], dp[j][k] + dp[k+1][j+1] + mat[j][0] * mat[k][1] * mat[j+i][1])

print(dp[0][n-1])        

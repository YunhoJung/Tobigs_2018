import sys

input = sys.stdin.readline

word = input().upper()
word_table = {}

for ch in word:
    if ch not in word_table:
        word_table[ch] = 1
    else:
        word_table[ch] += 1

max_count = max(word_table.values())
max_count = -1
max_ch = ''
for key, value in word_table.items():
    if value >= max_count:
        if value == max_count:
            max_ch = '?'
        else:
            max_count = value
            max_ch = key

print(max_ch)

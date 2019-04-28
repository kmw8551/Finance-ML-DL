```python

#1번 문제
r = lambda q : q * 2
s = lambda q : q * 4

#r(5) + s(8)의 결과 값은?



#2번 문제
data = [x for x in range(5)] 
temp = [x for x in range(7) if x in data and x%2==0] 
print(temp) 

#print(temp)의 결과 값으로 알맞은 것은?
#1. [0,2,4,6]
#2. [0,2,4]
#3. 정답 없음


#3번 문제
arr = [4,5,6,7]

#arr을 우측으로 3번 회전시키는 코드를 작성해보세요

'''
ex) 좌측 회전 
i = 0
while i < len(arr):
  arr = arr[1:] + arr[0]
  i += 1
'''

#4번 문제
geekCodes = [1,2,3,4]
geekCodes.append([5,6,7,8])
print(len(geekCodes))

#len은 길이를 구하는 함수
#다음 중 print(len(geekCodes))로 맞는 것은?

#1. 5
#2. 8
#3. 4
#4. 정답 없음



```

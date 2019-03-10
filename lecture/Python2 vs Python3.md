```python
#####################################
참고:http://goodtogreate.tistory.com/entry/Python-2-%EC%99%80-Python-3%EC%9D%98-%EC%B0%A8%EC%9D%B4%EC%A0%90

1.xrange vs range

파이썬에서는 3점대 버전부터 xrange를 지원하지 않는다.
하지만 연산의 속도는 xrange가 더 빠른데 이는 xrange는 정해진 숫자만큼 객체를 생성하며 
list를 생성하는 range와 다른 방식이다.

python2            |          python3
xrange(10)                    range(10)
range(10)                     list(range(10))

파이썬 3의 코드를 파이썬 2에서 수행할 때:

import sys
if sys.version_info < (3,):
    range = xrange

2. imap vs map

imap은 itertools 함수 import할 경우에 사용가능하나, 3버전에서는 아무리해도 import가 되질 않는다
이는 기본 map함수로 이전하였기 때문다.

3. print문

python2에서는 print " "
python3에서는 print()

4. 나눗셈

파이썬 3버전에서는 정수/정수는 실수 타입으로 나온다
2버전에서는 정수/정수는 정수 타입으로 나와 사실상 몫 계산이 되었지만 
3버전에서는 몫 계산은   '//' , 나머지 계산은 '%' 으로 아예 따로 구현이 되어있다.

5.asyncio 패키지

파이썬 3버전에서도 3.6이상부터 사용할 수 있다. 
동시성 처리와 관련된 

```

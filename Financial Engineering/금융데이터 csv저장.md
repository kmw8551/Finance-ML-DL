```python
import pandas as pd
import numpy as np
import itertools as it

stocksDict = {'005930':'삼성전자', '005380':'현대차', '000660':'SK하이닉스', '015760':'한국전력',
          '005490':'POSCO', '035420':'NAVER', '017670':'SK텔레콤', '012330':'현대모비스',
          '055550':'신한지주', '000270':'기아차', '105560':'KB금융', '051910':'LG화학',
          '034220':'LG디스플레이', '066570':'LG전자',
          '086790':'하나금융지주', '009540':'현대중공업', '023530':'롯데쇼핑', '030200':'KT',
          '024110':'기업은행', '096770':'SK이노베이션', '006400':'삼성SDI', '004020':'현대제철',
          '010130':'고려아연', '035250':'강원랜드', '011170':'롯데케미칼', '010950':'S-OIL',
          '032640':'LG유플러스', '010140':'삼성중공업', '000720':'현대건설', '036460':'한국가스공사',
          '001040':'CJ', '036570':'엔씨소프트', '009150':'삼성전기', '078930':'GS',
          '008770':'호텔신라', '047050':'대우인터내셔널', '006800':'대우증권', '003490':'대한항공',
          '034020':'두산중공업', '000210':'대림산업', '042670':'두산인프라코어', '000080':'하이트진로',
          '006360':'GS건설', '012450':'삼성테크윈'}

stockList = list(stocksDict.items())

# Pairs Trading 용 Feature를 계산한다
def CalFeatureSet(A, B, nDay):
    # 두 종목의 종가만 추출한다
    pair = pd.concat([A['Close'], B['Close']], axis=1)
    pair.columns = ['A', 'B']
    
    # 종가의 로그 가격을 구한다
    pair['logA'] = np.log(pair['A'])
    pair['logB'] = np.log(pair['B'])
    
    # Spread를 계산한다 (Z-Score Normalization)
    pair['spread'] = pair['logA'] - pair['logB']
    
    # 종가 기준 (로그) 수익률을 계산한다
    pair['rtnA'] = pd.DataFrame(pair['logA']).apply(lambda x: x - x.shift(1))
    pair['rtnB'] = pd.DataFrame(pair['logB']).apply(lambda x: x - x.shift(1))
    pair = pair.dropna()

    # 과거 n-기간 동안의 수익률 상관계수와 회귀분석 베타를 구한다.
    pair['rcor'] = np.nan
    pair['pcor'] = np.nan
    pair['rbeta'] = np.nan
    pair['pbeta'] = np.nan
    pair['zspread'] = np.nan
    fr = 0
    for i in range(nDay-1, len(pair)):
        # n-기간 동안의 수익률 상관계수를 계산한다 
        pair['rcor'][i] = np.corrcoef(pair['rtnA'][fr:i], pair['rtnB'][fr:i])[0,1]
        
        # n-기간 동안의 로그 가격 상관계수를 계산한다 
        pair['pcor'][i] = np.corrcoef(pair['logA'][fr:i], pair['logB'][fr:i])[0,1]
        
        # n-기간 동안의 수익률 기준 회귀분석 베타를 계산한다 (Moving Beta). 종목-B가 독립변수.
        v = np.var(pair['rtnB'][fr:i])
        c = np.cov(pair['rtnA'][fr:i], pair['rtnB'][fr:i])[0,1]
        pair['rbeta'][i] = c / v
        
        # n-기간 동안의 로그 가격 기준 회귀분석 베타를 계산한다 (Moving Beta). 종목-B가 독립변수.
        v = np.var(pair['logB'][fr:i])
        c = np.cov(pair['logA'][fr:i], pair['logB'][fr:i])[0,1]
        pair['pbeta'][i] = c / v
        
        # n-기간 동안의 Dynamic Normalized Spread를 계산한다
        mu = np.mean(pair['spread'][fr:i])
        sd = np.std(pair['spread'][fr:i])
        pair['zspread'][i] = (pair['spread'][i] - mu) / sd
        
        fr += 1
    
    return pair.dropna()
    

# 44개 종목의 전체 조합으로 Pairs Traing용 학습 데이터를 만든다.
def create(nDay, nFrom, nTo):
    combination = it.product(range(0, len(stocksDict)), repeat = 2)
    
    # 44 종목 ~ 1,892개 조합 (순서가 바뀐 것도 학습에 포함함)
    i = 1
    for p in combination:
        if p[0] == p[1]:
            continue
        
        if i < nFrom:
            i += 1
            continue
        
        if i > nTo:
            break
        
        stockA = pd.read_pickle('data/stockData/' + stockList[p[0]][0] + '.KS')
        stockB = pd.read_pickle('data/stockData/' + stockList[p[1]][0] + '.KS')
        pair = CalFeatureSet(stockA, stockB, nDay)
        pair.to_csv("data/pairData/train" + str(i) + ".csv")
        print("%d) %s-%s 학습용 데이터를 저장했습니다." % (i, stockList[p[0]][1], stockList[p[1]][1]))
        i += 1
```

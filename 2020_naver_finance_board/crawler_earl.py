import io
import re
import time
import os

import requests
from bs4 import BeautifulSoup
import pandas as pd

import multiprocessing
from db_manager import DB_manager


BASE_URL = 'https://finance.naver.com'
HEADERS = {'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.149 Safari/537.36', 
          'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8', 
          'Accept-Encoding': 'gzip', 
          'Accept-Language': 'ko'}

n_process = 1 #os.cpu_count()
db = DB_manager()


def get_stock_df():
    """
    현재 상장되어있는 종목 리스트를 df로 반환
    (참조: https://woosa7.github.io/krx_stock_master/)
    """
    url = 'http://kind.krx.co.kr/corpgeneral/corpList.do'
    data = {
        'method': 'download',
        'orderMode': '1',  # 정렬컬럼
        'orderStat': 'D',  # 정렬 내림차순
        'searchType': '13',  # 검색유형: 상장법인
        'fiscalYearEnd': 'all',  # 결산월: 전체
        'location': 'all',  # 지역: 전체
    }
    

    r = requests.post(url, data=data, headers=HEADERS)
    f = io.BytesIO(r.content)
    dfs = pd.read_html(f, header=0, parse_dates=['상장일'])
    df = dfs[0].copy()

    # 숫자를 앞자리가 0인 6자리 문자열로 변환
    df['종목코드'] = df['종목코드'].astype(str)
    df['종목코드'] = df['종목코드'].str.zfill(6)
    return df

stock_df = get_stock_df()

#page = 2
#fetch_by_page(code, page, event)

def fetch_by_page(code, page, event):
    """
    Multithreading을 사용하여 한 게시판 페이지 내의 글들을 크롤링하는 메소드
    :param code: 종목코드
    :param page: 페이지 번호
    :param event: 크롤링 중단 플래그로 사용
    :return: 한 페이지의 게시글들을 dict of list 로 반환. e.g posts['content'][0] == 페이지의 최상단 게시글의 내용
    """
    if not event.is_set():
        # print(BASE_URL + '/item/board.nhn?code=' + code + '&page=%d' % page, flush=True)
        msg = 'cur_page={}'.format(page)
        print(msg, end=len(msg)*'\b', flush=True)
        req = requests.get(BASE_URL + '/item/board.nhn?code=' + code + '&page=%d' % page, headers=HEADERS)
        page_soup = BeautifulSoup(req.text, 'lxml')
        title_atags = page_soup.select('td.title > a')

        
#        title_atag =  title_atags[0]        
        def fetch_by_post(title_atag):
            req = requests.get(BASE_URL + title_atag.get('href'), headers=HEADERS)
            content_soup = BeautifulSoup(req.text, 'lxml')

            date = content_soup.select_one('tr > th.gray03.p9.tah').text
            uid = content_soup.select_one('tr > th>span.gray03').text  #추가됨_earl_0329

            post_info = content_soup.select_one('tr > th:nth-of-type(2)')
            post_info = post_info.getText(',', strip=True).split(',')

            content = content_soup.select_one('#body')
            content = content.getText().replace(u'\xa0\r', '\n')
            content = content.replace('\r', '\n')

            href = title_atag.get('href')

            posts = {}
            posts['title'] = title_atag.get('title')
            posts['nid'] = int(re.search('(?<=nid=)[0-9]+', href)[0])
            posts['date'] = date
            posts['view'] = post_info[1]
            posts['agree'] = post_info[3]
            posts['disagree'] = post_info[5]
            posts['content'] = content
            posts['uid'] = uid   #추가됨_earl_0329
            
            return posts

        pool = multiprocessing.pool.ThreadPool(10)  # 한 페이지에 20개의 글이 표시됨을 참고.
        posts = [pool.apply_async(fetch_by_post, args={title_atag: title_atag}) for title_atag in title_atags]
        pool.close()
        pool.join()
        posts = [post.get() for post in posts]

        # list of dict -> dict of list
        posts = {k: [dic[k] for dic in posts] for k in posts[0]}

        db_latest_nid = db.latest_nid.get(code, 0)
        # 최신글 부터 DB에 저장된 날짜까지 다 크롤링 한 경우, 중단!
        # 단, 아래 코드가 정상적으로 작동하려면
        # min(all fetched posts' nid) <= min(this page's posts' nid) 이어야 함.
        if min(posts['nid']) < db_latest_nid:
            event.set()
            #return('gg') #추가됨_earl_0329_임시

        return posts
#code = '570032'
def fetch_by_code(code):
    """
    Multiprocessing을 사용하여 한 종목 토론실 글을 모두 크롤링하는 메소드
    :param code: 종목코드
    :return: DB 저장 형식의 pd.DataFrame()
    """
    req = requests.get(BASE_URL + '/item/board.nhn?code=' + code, headers=HEADERS)
    page_soup = BeautifulSoup(req.text, 'lxml')
    total_page_num = page_soup.select_one('tr > td.pgRR > a')
    if total_page_num is not None:
        total_page_num = total_page_num.get('href').split('=')[-1]
        total_page_num = int(total_page_num)
    else:
        total_page_num = 1

    print('total_pages={}'.format(total_page_num), end=' ', flush=True)
    pool = multiprocessing.Pool(n_process)
    m = multiprocessing.Manager()
    event = m.Event()
    posts_list = [pool.apply_async(fetch_by_page, args=(code, i, event)) for i in range(1, total_page_num + 1)]
    pool.close()
    pool.join()
    posts_list = [res.get() for res in posts_list]

    df = pd.concat(list(map(pd.DataFrame, posts_list)))
    df.date = pd.to_datetime(df.date)
    df.sort_values(by='nid', inplace=True)
    df.set_index('nid', inplace=True)

    print('\r' + code + ': Done.', end=' ')
    return df

def is_up_to_date(code):
    """
    종목 토톤실의 가장 최근 글의 날짜와 DB에 저장된 가장 최근 글의 날짜를 비교하여,
    DB가 최신인지 아닌지 여부를 반환함.
    (nid를 비교하는 것이 더욱 정확하지만 date로 비교해도 문제는 없다.)

    주의!: 최신글이 답변글인 경우, 글이 게시판 최상단에 위치하지 않아 True가 반환된다.
    """
    db = DB_manager()
        
    req = requests.get(BASE_URL + '/item/board.nhn?code=' + code, headers=HEADERS)
    page_soup = BeautifulSoup(req.text, 'lxml')
    web_latest_date = page_soup.select_one('tbody > tr:nth-of-type(3) > td:nth-of-type(1) > span')
 
    if web_latest_date is None :
        return True
    else :
        web_latest_date = pd.to_datetime(web_latest_date.text)

    db_latest_date = db.latest_date.get(code, 0)
    if db_latest_date == 0:
        return False
    elif db_latest_date < web_latest_date:
        return False
    else:
        return True

def is_up_to_nid(code):
    """
    종목 토톤실의 가장 최근 글의 nid와 DB에 저장된 가장 최근 code의 글의 nid를 비교하여,
    DB가 최신인지 아닌지 여부를 반환함.

    주의!: 최신글이 답변글인 경우, 글이 게시판 최상단에 위치하지 않아 True가 반환된다고 함 
    """
    db = DB_manager()
        
    req = requests.get(BASE_URL + '/item/board.nhn?code=' + code, headers=HEADERS)
    page_soup = BeautifulSoup(req.text, 'lxml')
    web_latest_nid = page_soup.select_one('td.title > a ,href')
        
    if web_latest_nid is None :
        return True
    else :
        web_latest_nid = int(re.search('(?<=nid=)[0-9]+', web_latest_nid['href'])[0])

    db_latest_nid = db.latest_nid.get(code, 0)
    if db_latest_nid == 0:
        return False
    elif db_latest_nid < web_latest_nid:
        return False
    else:
        return True


def fetch_one(code):
    """
    하나의 종목코드에 대해 종목 토론실 게시글 모두 크롤링하는 메소드.
    :output: data/*.db
    """
    print(code, end=' ')
    #if is_up_to_date(code):
    if is_up_to_nid(code):
        print('\r'+code+': Already up-to-date')
    else:
        t = time.time()
        df = fetch_by_code(code)
        print('({:.2f}sec)'.format(time.time() - t)+' '*30)
        db.write(code, df)
        del df

def fetch_all():
    """
    모든 종목 토론실 게시글을 크롤링하는 메소드.
    :output: data/*.db
    """
    for i, code in enumerate(sorted(stock_df['종목코드'])):
        print('{:>4}/{:>4} {}'.format(i+1, len(stock_df), code), end=': ')
        #if is_up_to_date(code):
        if is_up_to_nid(code):
            print('\r' + code + ': Already up-to-date')
            continue
        try:
            t = time.time()
            df = fetch_by_code(code)
            print('({:.2f}sec)'.format(time.time() - t)+' '*30)
            db.write(code, df)
            del df
        except:
            print('Failed:{}'.format(code))
            continue

#### 아래는 데이터 처음부터 쌓는 과정 

# max try error = host_site에서 요청을 너무 많이 던졌다고 연결을 끊어버리는 것, none이 반환되는 경우도 이와 같음
# 유니콘(http 주소 속이기?) 켜놓고 cpu 1개로 천천히 진행하면 끊기지는 않음

import sqlite3

DB_DIR = 'data'
DB_FILENAME = 'naver_board.db'
DB_PATH = os.path.join(DB_DIR, DB_FILENAME)

with sqlite3.connect(DB_PATH) as con:
    cursor = con.cursor()
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table';")
    db_code_list = cursor.fetchall()
    db_code_list = [code[0] for code in db_code_list]

#code = '005930'
#fetch_one(code)
#t_code = stock_df['종목코드'][~stock_df['종목코드'].isin(db_code_list)]


# 과거 데이터 부터 쌓는 쿼리 
# 위 쿼리와 다른 부분 - 중간에 에러 나서 끊겨도 해당 부분부터 재시작함 
# 쿼리가 돌다가 중간에 에러 발생하면 그냥 끝까지 돌아버림..


targetList = pd.read_csv('./data/marketList.csv')
t_code = targetList['종목코드'][~targetList['종목코드'].isin(db_code_list)]
t_code.size

targetList = pd.read_csv('./data/targetList.csv')
t_code = targetList['itemCode'][~targetList['itemCode'].isin(db_code_list)]
t_code.size

operList = pd.read_csv('./data/operList.csv')
targetList['itemCode'] = operList.itemCode.str.replace("N", "")
t_code = targetList['itemCode'][~targetList['itemCode'].isin(db_code_list)]
t_code.size


j = 0

for code in t_code:
    j = j +1
    print(j)

    forsedEnd = False
    
    print(code, end=' ')
    if is_up_to_nid(code):
        print('\r'+code+': Already up-to-date')
    else:
        t = time.time()

        req = requests.get(BASE_URL + '/item/board.nhn?code=' + code, headers=HEADERS)
        page_soup = BeautifulSoup(req.text, 'lxml')
        total_page_num = page_soup.select_one('tr > td.pgRR > a')
        if total_page_num is not None:
            total_page_num = total_page_num.get('href').split('=')[-1]
            total_page_num = int(total_page_num)
        else:
            total_page_num = 1

        curr_num = total_page_num

        print('total_pages={}'.format(total_page_num), end=' ', flush=True)
        pool = multiprocessing.Pool(n_process)        
        m = multiprocessing.Manager()
        event = m.Event()

        df = pd.DataFrame()

        # 역순으로 가져오게 변경
        for i in range(curr_num , 0, -1):
            try:
                posts_list = fetch_by_page(code, i, event)
                tmp = pd.DataFrame(posts_list)
                tmp.date = pd.to_datetime(tmp.date)
                df = pd.concat([df,tmp])
            except KeyboardInterrupt:
                forsedEnd = True
                break
            except:
                print('except')
                i = i - 1
                temp_df = df
                if curr_num == 0 :
                    pass
                continue

        if forsedEnd :
            break

        pool.close()
        pool.join()

        df.sort_values(by='nid', inplace=True)
        df.set_index('nid', inplace=True)

        print('\r' + code + ': Done.', end=' ')
        print('({:.2f}sec)'.format(time.time() - t)+' '*30)
        db.write(code, df)
#        del df

code = '005930'
code = '000020'
temp_df.to_csv('./data/temp.csv')

df = pd.read_csv('./data/temp.csv')

len(db_code_list)


t_code = targetList['종목코드']

code = t_code[500]
t_code = db_code_list[1:5]

fetch_one(code)

db.get_latest_nid()[code]
db.get_latest_date()[code]
len(db_code_list)




targetList = pd.read_csv('./data/marketList.csv')
t_code = targetList['종목코드']

j = 0

#for code in db_code_list:
for code in t_code:
    j = j +1
    print(j/len(t_code))
    
    forsedEnd = False
    
    print(code, end=' ')
    #if is_up_to_date(code):
    if is_up_to_nid(code):
        print('\r'+code+': Already up-to-date')
    else:
        t = time.time()
        
        req = requests.get(BASE_URL + '/item/board.nhn?code=' + code, headers=HEADERS)
        page_soup = BeautifulSoup(req.text, 'lxml')
        total_page_num = page_soup.select_one('tr > td.pgRR > a')
        if total_page_num is not None:
            total_page_num = total_page_num.get('href').split('=')[-1]
            total_page_num = int(total_page_num)
        else:
            total_page_num = 1
    
        print('total_pages={}'.format(total_page_num), end=' ', flush=True)
        pool = multiprocessing.Pool(n_process)
        m = multiprocessing.Manager()
        event = m.Event()
        
        df = pd.DataFrame()

        for i in range(1, total_page_num + 1):
            try:
                posts_list = fetch_by_page(code, i, event)
                if posts_list is None :
                    print('finish')
                    break
                tmp = pd.DataFrame(posts_list)
                tmp.date = pd.to_datetime(tmp.date)
                df = pd.concat([df,tmp])
            except KeyboardInterrupt:
                forsedEnd = True
                break
            except:
                print('except')
                i = i + 1
                temp_df = df
                if i == total_page_num + 1 :
                    pass
                continue
        
        if forsedEnd :
            break

        pool.close()
        pool.join()

        df.sort_values(by='nid', inplace=True)
        df.set_index('nid', inplace=True)

        print('\r' + code + ': Done.', end=' ')
        print('({:.2f}sec)'.format(time.time() - t)+' '*30)
        db.write(code, df)

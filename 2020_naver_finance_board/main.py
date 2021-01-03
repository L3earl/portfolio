import os
import argparse
from crawler import Crawler


def parse_args():
    parser = argparse.ArgumentParser(description="네이버 금융 종목 토론실 크롤러")
    parser.add_argument('-n', '--n_process', help="병렬 프로세스 개수", type=int, default=os.cpu_count())
    parser.add_argument('-code', '--code', help="종목코드. 미설정시 전 종목 크롤링", type=str, default=None)
    return parser.parse_args()


def main():
    args = parse_args()
    naver_crawler = Crawler(args.n_process)
    if args.code:
        naver_crawler.fetch_one(args.code)
    else:
        naver_crawler.fetch_all()


if __name__ == '__main__':
    main()

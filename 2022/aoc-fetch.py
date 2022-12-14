#!/usr/bin/env python3

import http.client, http.cookies
from datetime import datetime
from zoneinfo import ZoneInfo
import sys
import os

day = datetime.now(ZoneInfo("America/New_York")).day
if (len(sys.argv) >= 2 and sys.argv[1].isnumeric() and int(sys.argv[1]) <= 25 and int(sys.argv[1]) > 0):
    day = sys.argv[1]

# make rkt file

if not os.path.isfile(f'./{day}.rkt'):
    open(f'./{day}.rkt', 'w').write(
f""" #lang racket

(require "util.rkt")
(define input (fsplit {day} "\\n"))
input


""")
    print(f'created {day}.rkt')
else:
    print (f'{day}.rkt already exists')


# fetch input

session_cookie = {'Cookie': 'session={0}'.format(open('./session.txt').read().strip()) }

conn = http.client.HTTPSConnection('adventofcode.com')
conn.request('GET', f'/2022/day/{day}/input', headers=session_cookie)
r = conn.getresponse()
print('adventofcode.com response', r.status, r.reason)

data = r.read()

open(f'./input/{day}.txt', 'wb').write(data)
print(f'Data written to ./input/{day}.txt\n')
# print(data)

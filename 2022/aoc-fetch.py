#!/usr/bin/env python3

import http.client, http.cookies
from datetime import datetime
from zoneinfo import ZoneInfo
import sys

session_cookie = {'Cookie': 'session={0}'.format(open('./session.txt').read().strip()) }

day = datetime.now(ZoneInfo("America/New_York")).day
if (len(sys.argv) >= 2 and sys.argv[1].isnumeric() and int(sys.argv[1]) <= 25 and int(sys.argv[1]) > 0):
    day = sys.argv[1]

conn = http.client.HTTPSConnection('adventofcode.com')
conn.request('GET', f'/2022/day/{day}/input', headers=session_cookie)
r = conn.getresponse()
print(r.status, r.reason)

data = r.read()

open(f'./input/{day}.txt', 'wb').write(data)
print(f'Data written to ./input/{day}.txt')
print(data)

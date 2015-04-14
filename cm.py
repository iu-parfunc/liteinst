#!/usr/bin/python

import fileinput
import re

# _ZStorSt13_Ios_OpenmodeS_|0000000000403927|   W  |              FUNC|0000000000000044|     |.text

result = ""
for line in fileinput.input():
  regex = "([a-zA-Z0-9_.\s@]+)\|[0-9a-z\s]+\|[a-zA-Z0-9_\s]+\|\s+FUNC.*"
  matchObj = re.match(r'%s' % regex, line, re.M|re.I)
  if matchObj:
    name = matchObj.group(1).strip()
    result += name
    result += ","

print result

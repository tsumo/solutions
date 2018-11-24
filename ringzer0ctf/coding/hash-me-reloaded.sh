#!/bin/bash

# Same as "Hash me please". The only difference is that now secret message
# comes in binary and should be converted to ascii before hashing.
# Look at hash-me-please.sh for complete notes.

PHPSESSID=''
USER=''
PASSWORD=''

curl 'https://ringzer0ctf.com/login' \
    -s \
    -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:63.0) Gecko/20100101 Firefox/63.0' \
    -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' \
    -H 'Accept-Language: en-US,en;q=0.5' \
    --compressed \
    -H 'Referer: https://ringzer0ctf.com/challenges' \
    -H 'Connection: keep-alive' \
    -H 'Cookie: PHPSESSID='$PHPSESSID \
    -H 'Upgrade-Insecure-Requests: 1' \
    -H 'TE: Trailers' \
    > login.html

CSRF=`grep "var _[0-9a-f]* = '[0-9a-f]*';" login.html | sed 's/[ \t;]//g' | sed 's/var_[0-9a-f]*=//g' | sed "s/'//g"`
echo -e "\tCSRF\n$CSRF"

curl 'https://ringzer0ctf.com/login' \
    -s \
    -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:63.0) Gecko/20100101 Firefox/63.0' \
    -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' \
    -H 'Accept-Language: en-US,en;q=0.5' \
    --compressed \
    -H 'Referer: https://ringzer0ctf.com/login' \
    -H 'Content-Type: application/x-www-form-urlencoded' \
    -H 'Connection: keep-alive' \
    -H 'Cookie: PHPSESSID='$PHPSESSID \
    -H 'Upgrade-Insecure-Requests: 1' \
    -H 'TE: Trailers' \
    --data 'username='$USER'&password='$PASSWORD'&csrf='$CSRF'&check=true' \
    > /dev/null

curl 'https://ringzer0ctf.com/challenges/14' \
    -s \
    -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:63.0) Gecko/20100101 Firefox/63.0' \
    -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' \
    -H 'Accept-Language: en-US,en;q=0.5' \
    --compressed \
    -H 'Referer: https://ringzer0ctf.com/login' \
    -H 'Connection: keep-alive' \
    -H 'Cookie: PHPSESSID='$PHPSESSID \
    -H 'Upgrade-Insecure-Requests: 1' \
    -H 'Cache-Control: max-age=0' \
    -H 'TE: Trailers' \
    > message.html

MESSAGEBIN=`cat message.html | sed -n '/BEGIN MESSAGE/,/END MESSAGE/p' | sed 's/-* \(BEGIN\|END\) MESSAGE -*//g' | sed 's/[ \t\r]//g' | sed 's/<.*>//g' | tr -d '\n' | head -c 8192`
echo -e "\tMESSAGEBIN\n$MESSAGEBIN"

MESSAGE=`echo -n "$MESSAGEBIN" | perl -lpe '$_=pack"B*",$_'`
echo -e "\tMESSAGE\n$MESSAGE"

SHA=`echo -n $MESSAGE | sha512sum - | head -c 128`
echo -e "\tSHA\n$SHA"

curl "https://ringzer0ctf.com/challenges/14/$SHA" \
    -s \
    -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:63.0) Gecko/20100101 Firefox/63.0' \
    -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' \
    -H 'Accept-Language: en-US,en;q=0.5' \
    --compressed \
    -H 'Connection: keep-alive' \
    -H 'Cookie: PHPSESSID='$PHPSESSID \
    -H 'Upgrade-Insecure-Requests: 1' \
    -H 'TE: Trailers' \
    > result.html

# Flag now is in the result.html
grep 'FLAG' result.html
rm login.html message.html result.html


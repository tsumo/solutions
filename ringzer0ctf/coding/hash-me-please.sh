#!/bin/bash

# Challenge involves accessing a page hidden behind authorization and getting a
# secret message. After accessing a page you have 2 seconds to hash a secret
# message and use that hash as the part of the link to get a page with the
# flag.

# ringzer0 site has some basic protection from automatic logins, but it's easy
# to get around them. I copied all http requests straight from the browser.

# Hardcode your PHPSESSID here. You can find it in your browser cookies after
# opening any page on the site.
PHPSESSID=''
# Hardcode your credentals here. Don't forget to percent-encode them.
USER=''
PASSWORD=''

# Load login page
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

# By saving login page we get access to another crucial piece of data - csrf
# protection token. Site doesn't return a page with csrf input tag filled, and
# instead fills it using javascript like this:
#
# <input type="hidden" name="csrf" class="b94e12099a991ecd4ea58b8cc1955f89" value="">
#
# var _88bd7525 = 'f7d1ecdc51556c2490f53449c8be500a';
# $(".b94e12099a991ecd4ea58b8cc1955f89").val(_88bd7525);
#
# Extracting it is trivial.
CSRF=`grep "var _[0-9a-f]* = '[0-9a-f]*';" login.html | sed 's/[ \t;]//g' | sed 's/var_[0-9a-f]*=//g' | sed "s/'//g"`
echo -e "\tCSRF\n$CSRF"

# Make login request, dump response
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

# Now we are authorized and can retrive the secret message.
curl 'https://ringzer0ctf.com/challenges/13' \
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

# Extract secret message from the challenge page.
MESSAGE=`cat message.html | sed -n '/BEGIN MESSAGE/,/END MESSAGE/p' | sed 's/-* \(BEGIN\|END\) MESSAGE -*//g' | sed 's/[ \t\r]//g' | sed 's/<.*>//g' | tr -d '\n' | head -c 1024`
echo -e "\tMESSAGE\n$MESSAGE"

# Compute hash
SHA=`echo -n $MESSAGE | sha512sum - | head -c 128`
echo -e "\tSHA\n$SHA"

# Access secret page
curl "https://ringzer0ctf.com/challenges/13/$SHA" \
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


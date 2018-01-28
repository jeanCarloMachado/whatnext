#!/usr/bin/env bash


ssh  root@$BLOG_IP -t '
cd whatnext  && git pull origin master
'

scp -r "dist" root@$BLOG_IP:"/root/whatnext/src/web/"


ssh  root@$BLOG_IP -t '
pkill -f "SimpleHTTPServer" ;
'

echo "Deploying"
ssh -n -f root@$BLOG_IP "sh -c 'cd /root/whatnext/src/web/dist && nohup python -m SimpleHTTPServer 5001  > /dev/null 2>&1 &'"

ssh  root@$BLOG_IP -t '
pkill -f "webserver.py" ;
'


echo "Deploying"
ssh -n -f root@$BLOG_IP "sh -c 'cd /root/whatnext/src/web && nohup /usr/bin/python3.6 /root/whatnext/src/web/webserver.py  > /dev/null 2>&1 &'"

exit 0

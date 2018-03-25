cd ..
git add --all .
str=":white_flower:"++$(date  +%Y-%m-%d\ %H:%M:%S)
git commit -m "$str"
git push

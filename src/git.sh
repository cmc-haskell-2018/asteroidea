cd ..
git add --all .
str=":white_flower::memo:"$(date  +%Y-%m-%d\ %H:%M:%S)
git commit -m "$str"
git push

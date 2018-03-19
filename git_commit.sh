#!/bin/bash -x

minimumsize=99000000

for f in $(find . -maxdepth 1 -type f)
do 

 actualsize=$(wc -c <"$f")
 echo $f $actualsize

 if [ "$actualsize" -lt "$minimumsize" ] 
  then 
#    echo 'small enough'

 env -i git add $f
 env -i git commit -m "$f"
 git push -u origin master

 fi
done


#!/bin/sh

while [ 1 ] 
do
  # получаем %памяти, имя процесса, его PID и юзера:
  ps axo %mem,comm,pid,euser | \
  # если процесс занимает больше 10% - убиваем:
  awk -F' ' '{\
    if( $1 > 40 ) {\
      printf( "KILL %s:%s - %s\n",$2,$4,$3 );\
      system( "kill -9 " $3 );\
    }\
  }'
  sleep 1
done

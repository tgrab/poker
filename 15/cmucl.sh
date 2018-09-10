if [ -n "$1" ]
then
/opt/cmucl/bin/lisp -core /opt/cmucl/main.core -load $1
else
/opt/cmucl/bin/lisp -core /opt/cmucl/main.core
fi

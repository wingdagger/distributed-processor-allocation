#!/bin/zsh

INETPORT=2121

if [ ARGC -lt 4 ]; then
    echo Need args algorithm and test case
fi

algorithm=$1
test=$2
environ=$3
size=$4

if [ $environ = "sim" ]; then
    machine_list=""
else
    if [ $size = "1" ]; then
	machine_list="(\"plantain\" . $INETPORT)"

    elif [ $size = "5" ]; then
	machine_list="(\"plantain\" . $INETPORT) (\"moseley\" . $INETPORT) (\"elvis\" . $INETPORT) (\"ernst\" . $INETPORT) (\"praline\" . $INETPORT)"

    elif [ $size = "14" ]; then
	machine_list="(\"plantain\" . $INETPORT) (\"plantain\" . $INETPORT) (\"moseley\" . $INETPORT) (\"elvis\" . $INETPORT) (\"ernst\" . $INETPORT) (\"praline\" . $INETPORT) (\"plantain\" . $INETPORT) (\"moseley\" . $INETPORT) (\"elvis\" . $INETPORT) (\"ernst\" . $INETPORT) (\"129.81.192.234\" . $INETPORT) (\"129.81.192.234\" . $INETPORT) (\"praline\" . $INETPORT) (\"praline\" . $INETPORT)"

    elif [ $size = "21" ]; then
	machine_list="(\"plantain\" . $INETPORT) (\"praline\" . $INETPORT) (\"moseley\" . $INETPORT) (\"elvis\" . $INETPORT) (\"ernst\" . $INETPORT) (\"plantain\" . $INETPORT) (\"praline\" . $INETPORT) (\"moseley\" . $INETPORT) (\"elvis\" . $INETPORT) (\"ernst\" . $INETPORT) (\"troy\" . $INETPORT) (\"plantain\" . $INETPORT) (\"troy\" . $INETPORT) (\"praline\" . $INETPORT) (\"plantain\" . $INETPORT) (\"praline\" . $INETPORT) (\"moseley\" . $INETPORT) (\"elvis\" . $INETPORT) (\"ernst\" . $INETPORT) (\"troy\" . $INETPORT) (\"plantain\" . $INETPORT)"

    else
	echo Error in ds.sh: Unknown size: $size
    fi
fi


run-ds<<EOF
    (commissar '($machine_list))
    ,config ,load $HOME/thesis/implementation/ds.mod
    ,load-package ds
    ,in ds
    ,load $HOME/thesis/implementation/$algorithm.scm
    (start-ds $size #f)
    ($test)
    ,exit
EOF



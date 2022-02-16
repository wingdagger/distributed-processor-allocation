#!/bin/zsh

foreach f in `find $1 -name "quasi" -print`; do
    echo Processing $f

    if [ -d $f ]; then
	foreach g in a b c; do
	    if [ $g = "a" ]; then
		cd $f/test-longer-creation-interval
	    elif [ $g = "b" ]; then
		cd $f/test-same-location
	    else
		cd $f/test-fault-tolerance
	    fi

	    echo `pwd`

	    if [ -f ds-logfile.ds ]; then		
		foreach h in *.ds; do
		    rm -f $h
		    cvs delete $h
		done
	    fi

	    foreach it in 1 2 3; do
		cvs add iter$it
 		cd iter$it
 		cvs add *.ds
		cd ..
	    done
	 done
    fi
done


#!/bin/zsh


environ="quasi"
test="test-preemption"
scale="5"


for alg in 1 2 3; do
    if [ $alg = 1 ]; then
	algorithm="random-dpa"
    elif [ $alg = 2 ]; then
	algorithm="centralized"
    else
	algorithm="token"
    fi

    for iteration in 1 2 3; do
	echo Performing $HOME/thesis/testing/$algorithm/$environ/$test/iter$iteration

	ds.sh $algorithm $test $environ $scale

	    if [ -d $HOME/thesis/testing/$algorithm/$environ/$test/iter$iteration ]; then
		echo Error: dir exists
	    else
		echo "Copying files from /tmp/*.ds"
		mkdir $HOME/thesis/testing/$algorithm/$environ/$test/iter$iteration
		cp /tmp/*.ds $HOME/thesis/testing/$algorithm/$environ/$test/iter$iteration
		rm -f /tmp/*.ds
	    fi
	done
    done
done





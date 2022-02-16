#!/bin/zsh


environ="quasi"
test="test-scalability"


for alg in 1; do
    if [ $alg = 1 ]; then
	algorithm="random-dpa"
    elif [ $alg = 2 ]; then
	algorithm="centralized"
    else
	algorithm="token"
    fi

    for scale in 1; do

	for iteration in 1 2; do
	    echo Performing $HOME/thesis/testing/$algorithm/$environ/$test/$scale/iter$iteration

	    ds.sh $algorithm $test $environ $scale

#	    if [ -d $HOME/thesis/testing/$algorithm/$environ/$test/$scale/iter$iteration ]; then
#		echo Error: dir exists
#	    else
		echo "Copying files from /tmp/*.ds"
#		mkdir $HOME/thesis/testing/$algorithm/$environ/$test/$scale/iter$iteration
		cp /tmp/*.ds $HOME/thesis/testing/$algorithm/$environ/$test/$scale/iter$iteration
		rm -f /tmp/*.ds
#	    fi
	done
    done
done





#!/bin/zsh

scale=5

if [ ARGC -lt 1 ]; then
    echo Setting environment to quasi
    environ="quasi"
else
    environ=$1
fi




for alg in 1 2 3; do
    if [ $alg = 1 ]; then
	algorithm="random-dpa"
    elif [ $alg = 2 ]; then
	algorithm="centralized"
    else
	algorithm="token"
    fi

    for tst in 1 2 3 4; do
	if [ $tst = 1 ]; then
	    test="testa"
	elif [ $tst = 2 ]; then
	    test="testb"
	elif [ $tst = 3 ]; then
	    test="testc"
	else
	    test="testd"
	fi

	for iteration in 1 2 3; do
	    echo Performing $HOME/thesis/testing/$algorithm/$environ/$test/iter$iteration
	    ds.sh $algorithm $test $environ $scale

#	    if [ -d $HOME/thesis/testing/$algorithm/$environ/$test/iter$iteration ]; then
#		echo Error: dir exists
#	    else
		echo "Copying files from /tmp/*.ds"
#		mkdir $HOME/thesis/testing/$algorithm/$environ/$test/iter$iteration
		cp /tmp/*.ds $HOME/thesis/testing/$algorithm/$environ/$test/iter$iteration
		rm -f /tmp/*.ds
#	    fi
	done
    done
done


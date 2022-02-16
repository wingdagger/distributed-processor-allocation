#! /bin/zsh -f

foreach f in `find $1 -name "iter?" -print`; do
    if ! [ -f $f/process-hop-counts.ds ]; then
	echo $f
    fi
done

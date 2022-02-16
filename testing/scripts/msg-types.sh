#!/bin/zsh


PRINTER=psrich
PS_OUTPUT=/tmp/msg-type-counts.ps

# check to see if user wants to print or process data
if [ $# -gt 1 ] ; then
    if [ $2 = "-p" ]; then

	# print data
	echo printing $PS_OUTPUT
	lpr -P$PRINTER $PS_OUTPUT
	echo removing $PS_OUTPUT
	rm -f $PS_OUTPUT
    fi
else

    foreach f in `find $1 -name "iter?" -print`; do
	infile=$f/msg-type-counts.ds
	outfile=$f/msg-type-counts.txt
	echo processing $infile
	echo Message Type Counts for $f >! $outfile
	echo "\n" >> $outfile
	echo "Message Type\t Count" >> $outfile
	echo "------------\t -----" >> $outfile
	cat $infile | sort >> $outfile
    done

    # make postscript file of all text files
    echo writing $PS_OUTPUT
    a2ps -1 -p `find $1 -name "msg-type-counts.txt" -print` >! $PS_OUTPUT

fi

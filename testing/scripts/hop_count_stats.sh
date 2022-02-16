#!/bin/zsh


PRINTER=psrich
PS_OUTPUT=/tmp/hop_count_data.ps

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
	infile=$f/process-hop-counts.ds
	outfile=$f/hop-count-stats.txt
	echo processing $infile
	hop_count_stats.pl $infile >! $outfile
	echo "\n\n\n-------------------- Hop count data -----------------\n\n" >> $outfile
	echo "Num Hops\t Number of processes" >> $outfile
	echo "--------\t -------------------" >> $outfile
	cat $f/process-hop-counts.ds | sort >> $outfile
    done

    # make postscript file of all text files
    echo writing $PS_OUTPUT
    a2ps -1 -p `find $1 -name "hop-count-stats.txt" -print` >! $PS_OUTPUT

fi

#!/bin/zsh


PRINTER=psrich
PS_OUTPUT=/tmp/process_data.ps

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

    #process data
    foreach f in `find $1 -name "iter?" -print`; do
	outfile=$f/process-data-stats.txt
	infile=$f/process-data-stats.ds
	echo Computing process statistics for $infile
	echo Process Statistics for $f >! $outfile
	echo "\n" >> $outfile
	echo "process id\t response time\t total_time" >> $outfile
	echo "----------\t -------------\t ----------" >> $outfile
	gawk -f $HOME/lib/awk/sort.awk -f process_data.awk $infile | sort >> $outfile
    done

    # make postscript file of all text files
    echo writing $PS_OUTPUT
    a2ps -1 -p `find $1 -name "process-data-stats.txt" -print` >! $PS_OUTPUT

fi










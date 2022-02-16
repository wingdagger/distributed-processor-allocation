#!/usr/local/bin/zsh


foreach f in `find $1 -name "test?" -print`; do
    echo Computing process statistics for $f/process-data-stats.ds
    echo process id response time total_time > $f/process-data-stats.txt
    echo ---------- ------------- ---------- >> $f/process-data-stats.txt
    gawk -f $HOME/lib/awk/sort.awk -f process_data.awk $f/process-data-stats.ds | sort >> $f/process-data-stats.txt
done

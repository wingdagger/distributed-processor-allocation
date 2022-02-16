BEGIN {
#    print "process id\t response time\t total_time";
#    print "----------\t -------------\t ----------";
}

{
    stats[1] = $2;
    stats[2] = $3;
    stats[3] = $4;
    sort(stats, 3)

    resp_time = (stats[2] - stats[1]) / 1000;
    total_time = (stats[3] - stats[1]) / 1000;
    print $1, "\t\t", resp_time, "\t\t", total_time;
}

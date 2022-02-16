#!/usr/local/bin/perl

&main;

sub main
{
    &open_file;
    &calc_avg;
    &calc_stddev;
    &output;
}


sub open_file
{
    open (STATS, "@ARGV[0]");
    @lines = <STATS>;
    close (STATS);
}

sub calc_avg
{
    foreach $line (@lines)
    {
	($hopcnt, $freq) = split (' ', $line);
	$sum = $sum + ($hopcnt * $freq);
	$total_msgs = $total_msgs + $freq;
    }

    if ($total_msgs > 0)
    {
	$avg = $sum / $total_msgs;
    }
}

sub calc_stddev
{
    foreach $line (@lines)
    {
	($hopcnt, $freq) = split (' ', $line);
	$stddev_sum = $stddev_sum + 
	    abs (($hopcnt * $freq) - ($avg * $freq));
    }

    if ($total_msgs > 0)
    {
	$stddev = $stddev_sum / $total_msgs;
    }
}

sub output
{
    print "Hop Count Statistics for ", @ARGV[0], "\n";
    print "-----------------------------------------\n";
    print "Total messages: $total_msgs\n";
    print "Average hop count: $avg\n";
    print "Standard Deviation of hop count: $stddev\n";
}


#!/usr/local/bin/perl

$usage="stddev.pl filename average";
die $usage unless @ARGV;

$avg = @ARGV[1];

&main;
exit;

sub main
{
    open (FILE, @ARGV[0]);
    @lines = <FILE>;
    close (FILE);

    $sum=0;
    $cnt=0;

    foreach $line (@lines)
    {
	@values = split (/ /, $line);
	$sum += @values[1]**2;
	$cnt++;
    }

    $stddev = sqrt (abs (($sum / $cnt) - ($avg**2)));
    print $stddev, "\n";
}


	


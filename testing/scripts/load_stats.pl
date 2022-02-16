#!/usr/local/bin/perl

$HOME="/home/stu93/allen";

&main;
#&process_stats;
exit;

sub main
{
#     push (@tsts, "testa");
#     push (@tsts, "testb");
#     push (@tsts, "testc");
#     push (@tsts, "testd");
#    push (@tsts, "test-same-location");
#    push (@tsts, "test-scalability/c14");
    push (@tsts, "test-preemption");
#    push (@envs, "sim");
    push (@envs, "quasi");
#    push (@envs, "full");
    push (@algs, "random-dpa");
    push (@algs, "centralized");
    push (@algs, "token");
    push (@its, "1");
#    push (@its, "2");
#    push (@its, "3");

    foreach $env (@envs)
    {
	foreach $tst (@tsts)
	{
	    foreach $alg (@algs)
	    {
		$ld_avg_sum=0;
		$max_unfair_sum=0;
		$stddev_sum=0;
		foreach $it (@its)
		{
		    $ld_avg_sum += `gawk -f avg.awk $HOME/thesis/testing/$alg/$env/$tst/iter$it/avg-load-data.ds`;
		    $max_unfair_sum += `gawk -f avg.awk $HOME/thesis/testing/$alg/$env/$tst/iter$it/max-unfair-data.ds`;
		    $stddev_sum += `gawk -f avg.awk $HOME/thesis/testing/$alg/$env/$tst/iter$it/stddev-data.ds`;
		}
		$cnt = @its;

		$avg_ld_avg = $ld_avg_sum / $cnt;
		$stddev_avg = $stddev_sum / $cnt;
		$max_unfair_avg = $max_unfair_sum / $cnt;
		print "$alg, $env, $tst : \n";
		print "\tAvg. Ld. Avg: $avg_ld_avg\n";
		print "\tAvg. Max Unfair: $max_unfair_avg\n";
		print "\tAvg. StdDev: $stddev_avg\n";
		print "--------------\n";
	    }
	}
    }
}

sub process_stats
{
    push (@algs, "random-dpa");
    push (@algs, "centralized");
    push (@algs, "token");
    push (@its, "1");
#    push (@its, "2");
#    push (@its, "3");
#    push (@sizes, "1");
#    push (@sizes, "5");
    push (@sizes, "14");

    foreach $size (@sizes)
    {
	foreach $alg (@algs)
	{
	    if ($size eq "5")
	    {
		$path = "$HOME/thesis/testing/$alg/quasi/testd";
	    }
	    else
	    {
		$path = "$HOME/thesis/testing/$alg/quasi/test-scalability/c$size";
	    }

	    $resp_time_sum=0;
	    $run_time_sum=0;
	    $total_time_sum=0;
	    $cnt=0;

	    foreach $it (@its)
	    {
		$result = `gawk -f $HOME/lib/awk/sort.awk -f process_avgs.awk $path/iter$it/process-data-stats.ds`;
		@results = split ('\t', $result);
		
		if (@results[0] != 0)
		{
		    $resp_time_sum += @results[0];
		    $run_time_sum += @results[1];
		    $total_time_sum += @results[2];
		    $cnt++;
		}
#		else
#		{
#		    print "not increasing count";
#z		}
	    }

	    print "$alg, $size : \n";
	    if ($cnt > 0)
	    {
		print "\tAverage resp time: ", $resp_time_sum / $cnt, "\n";
		print "\tAverage run time: ", $run_time_sum / $cnt, "\n";
		print "\tAverage total time: ", $total_time_sum / $cnt, "\n";
	    }
	    else
	    {
		print "zero data to average\n";
	    }
	    print "------------\n";
	}
    }
}











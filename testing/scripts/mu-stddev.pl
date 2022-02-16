#!/usr/local/bin/perl

$HOME="/home/stu93/allen";

&main;
exit;

sub main
{
     push (@tsts, "testa");
     push (@tsts, "testb");
     push (@tsts, "testc");
     push (@tsts, "testd");
#    push (@tsts, "test-same-location");
#    push (@tsts, "test-longer-creation-interval");
#    push (@tsts, "test-scalability/c14");
#    push (@tsts, "test-scalability/c1");
#    push (@tsts, "test-preemption");
    push (@envs, "sim");
    push (@envs, "quasi");
    push (@envs, "full");
    push (@algs, "random-dpa");
    push (@algs, "centralized");
    push (@algs, "token");
    push (@its, "1");
    push (@its, "2");
   push (@its, "3");

    foreach $env (@envs)
    {
	foreach $tst (@tsts)
	{
	    foreach $alg (@algs)
	    {
		$max_unfair_avg=0;
		$max_unfair_stddev=0;

		foreach $it (@its)
		{
		    $max_unfair_avg += `gawk -f avg.awk $HOME/thesis/testing/$alg/$env/$tst/iter$it/max-unfair-data.ds`;
		    $max_unfair_stddev += `$HOME/thesis/testing/scripts/stddev.pl $HOME/thesis/testing/$alg/$env/$tst/iter$it/max-unfair-data.ds $max_unfair_avg`
		}
		$cnt = @its;

		$max_unfair_avg_its = $max_unfair_avg / $cnt;
		$mu_stddev = $max_unfair_stddev / $cnt;

		print "$alg, $env, $tst : \n";
		print "\tAvg. Max Unfair: $max_unfair_avg_its\n";
		print "\tAvg. StdDev of MU: $mu_stddev\n";
		print "--------------\n";
	    }
	}
    }
}

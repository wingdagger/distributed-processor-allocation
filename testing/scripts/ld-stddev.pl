#!/usr/local/bin/perl

$HOME="/home/stu93/allen";

&main;
exit;

sub main
{
#     push (@tsts, "testa");
#     push (@tsts, "testb");
#     push (@tsts, "testc");
#     push (@tsts, "testd");
    push (@tsts, "test-same-location");
    push (@tsts, "test-longer-creation-interval");
#    push (@tsts, "test-scalability/c14");
#    push (@tsts, "test-scalability/c1");
#    push (@tsts, "test-preemption");
#    push (@envs, "sim");
    push (@envs, "quasi");
#    push (@envs, "full");
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
		$ld_avg_sum=0;
		$ld_stddev=0;

		foreach $it (@its)
		{
		    $cdir="$HOME/thesis/testing/$alg/$env/$tst/iter$it";
		    system ("cat $cdir/data?.ds > $cdir/all_loads.txt");
		    $ld_avg_sum += `gawk -f avg.awk $HOME/thesis/testing/$alg/$env/$tst/iter$it/all_loads.txt`;
		    $ld_stddev += `$HOME/thesis/testing/scripts/stddev.pl $HOME/thesis/testing/$alg/$env/$tst/iter$it/all_loads.txt $max_unfair_avg`
		}
		$cnt = @its;

		$ld_avg_its = $ld_avg_sum / $cnt;
		$ld_stddev = $ld_stddev / $cnt;

		print "$alg, $env, $tst : \n";
		print "\tAvg. Load: $ld_avg_its\n";
		print "\tAvg. StdDev of Loads: $ld_stddev\n";
		print "--------------\n";
	    }
	}
    }
}

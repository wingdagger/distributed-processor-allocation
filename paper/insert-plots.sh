#! /bin/zsh -f

# just quasi:
# test-longer-creation-interval test-fault-tolerance test-same-location
# test-scalability


foreach alg in random-dpa token centralized; do
    foreach env in quasi; do
	foreach tst in test-scalability/c1 test-scalability/c14; do
	    foreach iter in 1; do
		foreach psfile in \
		 `find ../testing/$alg/$env/$tst/iter$iter -name "*.ps" -print`; do
		    echo "\insertgnuplot{$psfile}"
		    echo "   {X$alg Algorithm -- X$env environment --"
                    echo "   Test Case X$tst -- "
		    echo "   X$psfile:t}"
		    echo 
		done
	    done
	done
    done
done



#\insertgnuplot{../testing/random-dpa/quasi/testa/iter1/stddev-data.ps}
#              {Q-learning Algorithm -- Quasi-simulation environment -- Test
#		Case A -- Iteration 1 -- Standard Deviation data over Time}

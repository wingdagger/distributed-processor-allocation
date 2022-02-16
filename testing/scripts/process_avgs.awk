BEGIN {
#    print "process id\t response time\t total_time";
#    print "----------\t -------------\t ----------";
}

{
    stats[1] = $2;
    stats[2] = $3;
    stats[3] = $4;
    sort(stats, 3)

    resp_time_sum += (stats[2] - stats[1]);
    run_time_sum += (stats[3] - stats[2]);
    total_time_sum += (stats[3] - stats[1]);
    cnt++;
}

END {
  if (cnt > 0)
    {
      avg_resp_time = resp_time_sum / cnt / 1000;
      avg_run_time = run_time_sum / cnt / 1000;
      avg_total_time = total_time_sum / cnt / 1000;
    }
  else
    {
      avg_resp_time = 0;
      avg_run_time = 0;
      avg_total_time = 0;
    }

  print avg_resp_time, "\t", avg_run_time, "\t", avg_total_time
}

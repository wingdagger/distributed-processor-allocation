BEGIN {
}

{
    sum += $2
    count += 1
}

END {
  print sum / count
}



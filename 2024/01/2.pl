#!perl -nl

/(\d+)\s+(\d+)/ && ($a{$1}++, $b{$2}++);

END {
    $sum += $_ * $a{$_} * $b{$_} for (keys %a);
    print $sum;
}

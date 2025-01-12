#!perl -nl

if (/(\d+)\s+(\d+)/) {
  my $a = $1;
  my $b = $2;
  print "$a $b";
  push(@a, $1);
  push(@b, $2);
}

END {
  my @a = sort @a;
  my @b = sort @b;
  for (0..$#a) {
    my $a = $a[$_];
    my $b = $b[$_];
    my $delta = abs($a - $b);
    # print "$a $b ($delta)";
    $d += $delta;
  };
  print $d;
}

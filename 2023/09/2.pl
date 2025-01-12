#!/usr/bin/perl -F'\s' -lan
sub delta {
    my @numbers = @_;
    my @deltas = map { $numbers[$_] - $numbers[$_-1 ] } 1..$#numbers;
    $_ != 0 && goto work for @deltas;
  finished:
    return 0;
  work:
    my $delta = $deltas[0] - delta(@deltas);
    print "\tFound deltas : @deltas (returning $delta)";
    return $delta;
}

print "numbers : @F";
my $final =  $F[0] - delta(@F);
$s += $final;

END { print $s }

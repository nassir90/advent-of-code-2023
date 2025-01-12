#!/usr/bin/perl -F'\s' -lan
sub delta {
    my @numbers = @_;
    my @deltas = map { $numbers[$_] - $numbers[$_-1 ] } 1..$#numbers;
    $_ != 0 && return $deltas[0] - delta(@deltas) for @deltas;
}
$s += $F[0] - delta(@F);
END { print $s }

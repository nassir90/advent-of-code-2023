#!/usr/bin/perl -nl
BEGIN {
    @b = qw/one two three four five six seven eight nine/;
    %c = map { $b[$_] => $_+1 } 0..$#b;
    @d = map { scalar reverse } @b;
    %e = map { $d[$_] => $_+1 } 0..$#d
}

sub pattern {
    "([1-9]|".(join "|", @_).")"
}

{
    $x = ($_ =~ pattern @b)[0];
    $y = ((scalar reverse $_) =~ pattern @d)[0];
    $s+=(@c{$x}||$x).(@e{$y}||$y)
}

END {
    print $s
}

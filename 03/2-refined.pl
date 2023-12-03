#!/usr/bin/env perl
@lines = (<>);

for $y (keys @lines) {
    $line = $lines[$y];
    while ($line =~ /[0-9]+/g) {
        $left = $-[0] ? $-[0] - 1 : 0;
        $number = $&;
        for my $y ($y-1..$y+1) {
            next unless $lines[$y];
            $row = substr $lines[$y], $left, length($number)+2;
            next unless $row =~ /\*/n;
            $x = $left + $-[0];
            push @{$gears{"$x.$y"}}, $number;
            last;
        }
    }
}

for (values %gears) {
    $s += $_->[0] * $_->[1] if scalar(@{$_}) == 2;
}

print "DONE ($s)"

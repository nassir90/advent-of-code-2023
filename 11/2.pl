$expansion = 1_000_000 - 1;
while (<>) {
    $h++, $w = length($_) - 1;
    $nullrow[$#nullrow+1] = $.-1                          if (!/#/);
    $realcolumn{$-[0]} = 1, push @galaxies, [$.-1, $-[0]] while (/#/g)
}

for my $nullrow (reverse @nullrow) {
    print "Found nullrow $nullrow\n";
    for (@galaxies) { $_->[0] += $expansion if ($_->[0] > $nullrow) }
    $eh += $expansion;
}

for my $column (0..$w) {
    $column = $w - $column;
    next if $realcolumn{$column};
    print "Found nullcolumn $column\n";
    for (@galaxies) { $_->[1] += $expansion if ($_->[1] > $column) }
    $ew += $expansion;
}

for $g1 (0..$#galaxies-1) {
    for $g2 ($g1+1..$#galaxies) {
        ($r1, $c1) = $galaxies[$g1]->@*;
        ($r2, $c2) = $galaxies[$g2]->@*;
        $dist = abs($r2 - $r1) + abs($c2 - $c1);
        print "Found pair $g1 ($r1, $c1) → $g2 ($r2, $c2) with distance $dist\n";
        $s += $dist;
    }
}

print "Answer: $s\n";

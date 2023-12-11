$expansion = 1_000_000;
while (<>) {
    $h++, $w = length($_) - 1;
    $nullrow[$#nullrow+1] = $.-1                          if (!/#/);
    $realcolumn{$-[0]} = 1, push @galaxies, [$.-1, $-[0]] while (/#/g)
}
for my $nullrow (reverse @nullrow) {
    for (@galaxies) { $_->[0] += $expansion - 1if ($_->[0] > $nullrow) }
}
for my $column (0..$w) {
    next if $realcolumn{$column = $w - $column};
    for (@galaxies) { $_->[1] += $expansion - 1 if ($_->[1] > $column) }
}
for $g1 (0..$#galaxies-1) {
    for $g2 ($g1+1..$#galaxies) {
        ($r1, $c1) = $galaxies[$g1]->@*;
        ($r2, $c2) = $galaxies[$g2]->@*;
        $s += abs($r2 - $r1) + abs($c2 - $c1);
    }
}
print "Answer: $s\n";

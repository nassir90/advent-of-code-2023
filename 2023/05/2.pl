# destination source count (just like /proc/$$/x_map)
($seeds, @lines) = (<>);
for (@lines) {
    $source=$1, $targets{$1} = $2 if /([a-z]+)-to-([a-z]+) map:/;
    if (/(\d+) (\d+) (\d+)/) {
        push @{$ranges{$source}}, [$1, $2, $3];
        $right{$source} = $2 + $3 if ($2 + $3 > $right{$source});
    }
}
$ranges{$_}->@* = sort { $a->[1] <=> $b->[1] } $ranges{$_}->@* for (keys %ranges);
push @blocks, [$1, $1 + $2 - 1] while ($seeds =~ /(\d+) (\d+)/g);
$step = "seed";
while ($step ne "location") {
    my @nextblocks = ();
    while ($block = shift @blocks) {
        ($start, $end) = $block->@*;
        for $range ($ranges{$step}->@*) {
            ($destination, $source, $length) = $range->@*;
            # case 1 there is a block before this range
            if ($start < $source) {
                # get free component
                if ($end < $source) {
                    # forward whole component as it is before any map
                    push @nextblocks, [$start, $end] ; last;
                } else {
                    # truncate maximal free component in this case
                    push @nextblocks, [$start, $source - 1] ; $start = $source;
                }
            }
            # in this case we have a block component that starts at least after source
            if ($end < $source + $length) {
                # map whole block
                push @nextblocks, [$start - $source + $destination, $end - $source + $destination] ; last;
            } elsif ($start < $source + $length) {
                # map part of block and let the next range handle the rest
                push @nextblocks, [$start - $source + $destination, ($source + $length - 1) - $source + $destination] ; $start = $source + $length;
            } elsif  ($start >= $right{$step}) {
                # as for the next part, if it can be processed by some other range, add it to blocks. otherwise, pass it through directly.
                push @nextblocks, [$start, $end] ; last;
            }
        }
    }
    @blocks = @nextblocks;
    $step = $targets{$step};
}
$want = Inf;
for (@blocks) {
    $want = $_->[0] if ($want > $_->[0]);
}
print "Final number $want\n";

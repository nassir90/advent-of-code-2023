# for each map, parse intervals and source by source start
# bundle ranges as a series of blocks in the form (start, end)
# for each mapping step:
#    for each block:
#        for each interval:
#            if i overlap?
#               if there an unmapped part i need to get rid of it?
#                   dump that part
#               construct new block out of overlapping segment
#                   dump that part
# sort blocks by start index
# take smallest block

# destination source count (just like /proc/$$/x_map)
($seeds, @lines) = (<>);
for (@lines) {
    next if /^\s*$/;
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
        print "Processing block $start, $end ($step → $right{$step})\n";
        for $range ($ranges{$step}->@*) {
            ($destination, $source, $length) = $range->@*;
            print "\tProcessing range $destination, $source, $length\n";
            
            # case 1 there is a block before this range
            if ($start < $source) {
                print "HERE #1\n";
                # get free component
                if ($end < $source) {
                    # forward whole component as it is before any map
                    print "HERE #1.1\n";
                    push @nextblocks, [$start, $end] ; last;
                } else {
                    # truncate maximal free component in this case
                    print "HERE #1.2\n";
                    push @nextblocks, [$start, $source - 1] ; $start = $source;
                }
            }

            # in this case we have a block component that starts
            # at least after source
            if ($end < $source + $length) {
                print "HERE #2\n";
                # map whole block
                push @nextblocks, [$start - $source + $destination, $end - $source + $destination] ; last;
            } else {
                # map part of block and let the next range handle
                # the rest
                print "HERE #3\n";
                if ($start < $source + $length) {
                    print "HERE #3.1\n";
                    push @nextblocks, [$start - $source + $destination, ($source + $length - 1) - $source + $destination] ; $start = $source + $length;
                }
                # as for the next part, if it can be processed by
                # some other range, add it to blocks. otherwise,
                # pass it through directly.
                if  ($start >= $right{$step}) {
                    print "HERE #3.2\n";
                    push @nextblocks, [$start, $end] ; last;
                } else {
                    print "HERE #3.3 block ($start, $end) does not overlap with ($source, ".($source+$length-1).")\n";
                }
            }
        }
    }
    @blocks = @nextblocks;
    $step = $targets{$step};
}

$want = Inf;

for $nb (@blocks) {
    ($v, $a) = $nb->@*;
    $want = $v if ($want > $v);
    print "\t$v, $a\n";
}

print "Final number $want\n";

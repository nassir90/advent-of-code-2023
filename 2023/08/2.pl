@directions = split //, <> =~ s/[^RL]//r;
$n = scalar @directions;
<>; # skip separating line
/(...) = \((...), (...)\)/, $edge{$1} = { L => $2, R => $3 } while (<>);
@at = grep { /A$/ } keys %edge;
@s = map { 0 } @at;
advance: {
    # Chose the furthest behind ghost to advance
    my $chosen = 0;
    for $_ (0..$#at) {
        $chosen = $_ if $s[$_] < $s[$chosen];
    }
    
    my $initial = $at[$chosen], $next = $at[$chosen], $delta = 0;
    my $key = $initial."-".($s[$chosen] % $n);
    print "Advancing ghost #$chosen $key with distance $s[$chosen]\n";
    if (defined $cache{$key}) {
        # If this path has been memoised just use the cache
        ($next, $delta) = $cache{$key}->@*;
        $s[$chosen] += $delta;
        $at[$chosen] = $next;
        print "\tUsing memoised delta where $initial → $next with delta $delta\n"
    } else {
        # Otherwise we need to compute this hoe
        print "\tNeed to compute delta for $initial\n";
        while (1) {
            $next = $edge{$next}{$directions[$s[$chosen] % $n]};
            exit unless defined $next;
            $delta++, $s[$chosen]++;
            if ($next =~ /Z$/) {
                # memoize here.
                print "\nMemoizing path $initial → $next with cost $delta\n";
                $cache{$key} = [$next, $delta];
                $at[$chosen] = $next;
                last;
            }
        }
    }

    print "Dumping cache state\n";
    for (sort { $cache{$a}[1] <=> $cache{$b}[1] } keys %cache) {
        next unless /Z-/;
        print "$_ → $cache{$_}[0] ($cache{$_}[1])\n";
    }

    print "---\n";

    print "Dumping cache state\n";
    for (sort { $cache{$a}[1] <=> $cache{$b}[1] } keys %cache) {
        next unless /A-/;
        print "$_ → $cache{$_}[0] ($cache{$_}[1])\n";
    }

    # mincut max flow ?
    # shortest path?
    
    # terminating condition is when all nodes are at the same z. at
    # each step, we advance the node with the smallest distance.
    for (@s) {
        if ($_ != $s[0]) {
            print "\t\t$_ does not match $s[0]!\n";
            redo advance;
        }
    }
    $cap++:
    last if $cap > 1000000;
}

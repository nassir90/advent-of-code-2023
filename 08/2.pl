@directions = split //, <> =~ s/[^RL]//r;
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
    
    print "Advancing ghost $initial with distance $s[$chosen]\n";
    
    if (defined $cache{$initial}) {
        # If this path has been memoised just use the cache
        ($next, $delta) = $cache{$initial}->@*;
        print "\tUsing memoised delta where $initial → $next with delta $delta\n"
    } else {
        # Otherwise we need to compute this hoe
        print "\tNeed to compute delta for $initial\n";
        while (1) {
            $direction = $directions[($s[$chosen]+$delta) % scalar @directions];
            $next = $edge{$next}{$direction};
            exit unless defined $next;
            $delta++;
            if ($next =~ /Z$/) {
                # memoize here.
                print "\nMemoizing path $initial → $at with cost $delta\n";
                $cache{$initial} = [$next, $delta];
                last;
            }
        }
    }
    
    $s[$chosen] += $delta;
    $at[$chosen] = $next;
    
    # terminating condition is when all nodes are at the same z.  at
    # each step, we advance the node with the smallest distance.
    for (@s) {
        redo advance if $_ != $s[0];
    }
}

print "$initial reaches $at in $s[0] steps\n";

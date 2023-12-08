# Parse input
@directions = split //, <> =~ s/[^RL]//r;
<>;
/(...) = \((...), (...)\)/, $edge{$1} = { L => $2, R => $3 } while (<>);
@ghosts = grep { /A$/ } keys %edge;

# Find distances
for $initial (@ghosts) {
    my $next = $initial;
    while ($next !~ /Z$/) {
        $next = $edge{$next}{$directions[$distance{$initial} % scalar @directions]};
        $distance{$initial}++;
    }
}

# Get factors of distances
for $value (values %distance) {
    $factors{$_} = 1 for (`factor $value` =~ s/.*://r =~ /(\d+)/g);
}

# Reduce on [*]
$final = 1;
$final *= $_ for keys %factors;
print $final;

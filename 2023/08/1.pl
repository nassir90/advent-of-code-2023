@directions = split //, <> =~ s/[^RL]//r;
<>;
/(...) = \((...), (...)\)/, $edge{$1} = { L => $2, R => $3 } while (<>);
$at = AAA;
while (1) {
    for $direction (@directions) {
        $s++;
        $next = $edge{$at}{$direction};
        print "Going $direction from $at to $next\n";
        exit unless defined $next;
        $at = $next;
        goto finish if $at eq ZZZ;
    }
}
 finish:
    print $s;

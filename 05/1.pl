#for each seed
# bundle seed as (type, index).
# pass in through next[type].
# - find the range index falls inside
# - return mapping
# Is Type location?
#  -> print number
#  -> go back again

# destination source count (just like /proc/$$/x_map)
($seeds, @lines) = (<>);
$want = Inf;
for $number ($seeds =~ /(\d+)/g) {
    my $state = "reading";
    for (@lines) {
        next if /^\s*$/;
        if (/:/) {
            $state = "reading";
            ($name) = /(\S*) map:/;
        } elsif ($state eq "reading") {
            ($destination, $source, $length) = split " ";
            if ($number < $source + $length && $number >= $source) {
                $state = "found";
                $number = $destination + ($number - $source);
            }
        }
    }
    $want = $number if $number < $want;
}
print "Final number $want\n";

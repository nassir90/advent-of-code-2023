#!/usr/bin/env perl
# sum numbers by symbols
sub at {
    @d = split //, $d;
    @d[$_[0]+$_[1]*$w]
}

while (<>) {
    $l ++;
    $w = length($_) - 1;
    $d .= substr $_, 0, $w;
}

for $y (0..$l-1) {
    $p = substr $d, $y*$w, $w;
    while ($p =~ /([0-9]+)/g) {
        my $m = $1;
        for $h ($y-1..$y+1) {
            next unless $h >= 0 && $h < $l;
            $r = substr $d, pos($p)-length($1)-1 + $h*$w, length($1)+2;
            $s += $m, last if $r =~ /[^0-9.]/n;
        }
    }
}

print "DONE ($s)";

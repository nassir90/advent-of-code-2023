#!/usr/bin/env perl
# numbers by gears
while (<>) {
    $l ++;
    $w = length($_) - 1;
    $d .= substr $_, 0, $w;
}

my %gears;

# for each line
for $y (0..$l-1) {
    # for each number
    $p = substr $d, $y*$w, $w;
    while ($p =~ /[0-9]+/g) {
        # for each row around that number (of which there are three)
        $x = @-[0];
        $m = $&;
        for $h ($y-1..$y+1) {
            next unless $h >= 0 && $h < $l;
            $left = $x == 0 ? 0 : $x-1;
            $r = substr $d, $left + $h*$w, length($m)+2;
            if ($r =~ /\*/) {
                $starposition =  $-[0] + $left;
                push @{$gears{"$starposition.$h"}}, $m;
                last;
            }
        }
    }
}

for (keys %gears) {
    @arr = @{$gears{$_}};
    $s += $arr[0] * $arr[1] if scalar(@arr) == 2;
}

print "DONE ($s)";

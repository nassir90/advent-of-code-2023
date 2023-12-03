#!/usr/bin/perl -F'\s*[:;]\s*' -ln
$r=0;
$g=0;
$b=0;
for (@F[1..$#F]) {
    for (split /,/) {
        $r = $_ if /red/ && $_ > $r;
        $g = $_ if /green/ && $_ > $g;
        $b = $_ if /blue/ && $_ > $b;
    }
}
$s += $r * $g * $b;
END { print $s }

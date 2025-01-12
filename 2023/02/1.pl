#!/usr/bin/perl -F'\s*[:;]\s*' -ln
$g = ((shift @F) =~ /Game (\d+)/)[0];
for (@F) {
    for (split /,\s*/) {
        goto after if /red/   && $_>12;
        goto after if /green/ && $_>13;
        goto after if /blue/  && $_>14;
    }
}
$s += $g;
 after:;
END { print $s }

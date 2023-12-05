#!/usr/bin/perl -nl

my $score;
($card, $winning, $owned) = split /:|\|/;
$card = (/Card (\d+)/)[0];
%winning = map { $_ => 1} split " ", $winning;
for (split " ", $owned) {
    if ($winning{$_}) {
        if (!defined $score) {
            $score = 1;
        } else {
            $score *= 2;
        }
    }
}
$s += $score;
END { print $s }

#!/usr/bin/perl -F':|\|' -ln
my ($title, $winning, $have) = @F;
my $score = 1 + shift @leading;
$s += $score;
my $next = 0;
for (split " ", $winning) {
    $next++ if $have =~ ('\b'.($_+0).'\b');
}
$leading[$_] += $score for (0..$next-1);

END { print $s; }

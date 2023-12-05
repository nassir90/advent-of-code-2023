#!/usr/bin/perl
while (<>) {
    my ($left, $right) = ($_ =~ /: (.*) \| (.*)/);
    %{$winning[$.-1]} = map { $_ => 1 } split " ", $left;
    @{$have[$.-1]} = split " ", $right;
}
sub win {
    # card, number
    $winning[$_[0]]->{$_[1]}
} 
%cards = map { $_ => 1 } 0..$#have;

print "yes" if win 0, 41; 

for my $card (0..$#have) {
    $count = $cards{$card};
    my $score;
    for (@{$have[$card]}) {
        $score += 1 if win $card, $_
    }
    for ($card+1..$card+$score) {
        last if $_ > $#have;
        $cards{$_} += 1 * $count;
    }
}
for (0..$#have) {
    print "$_ → $cards{$_}\n";
    $s += $cards{$_};
}

print $s;

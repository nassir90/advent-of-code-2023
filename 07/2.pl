@c = (A,K,Q,T,9,8,7,6,5,4,3,2,J);
@r{@c} = map { sprintf "%02d", $#c-$_ } 0..$#c;

while (<>) {
    my ($card, $reward) = split " ";
    my @chars = (split //, $card), @d = (), %frequencies = (), $max = A;
    $frequencies{$_}++ for @chars;
    $_ ne J && $frequencies{$max} < $frequencies{$_} && ($max=$_) for @c;
    $frequencies{$max} += $frequencies{J}, $frequencies{J} = 0;
    $d[$_]++ for (values %frequencies);
    $type=6, $name = "five of a kind"  if $d[5] == 1;
    $type=5, $name = "four of a kind"  if $d[4] == 1;
    $type=4, $name = "full house"      if $d[3] == 1 && $d[2] == 1;
    $type=3, $name = "three of a kind" if $d[3] == 1 && $d[1] == 2;
    $type=2, $name = "two pair"        if $d[2] == 2 && $d[1] == 1;
    $type=1, $name = "one pair"        if $d[2] == 1 && $d[1] == 3;
    $type=0, $name = "high card"       if $d[1] == 5;
    @digits = ($type, @r{@chars});
    print "@digits $card → $name ($max)\n";
    push @ids, ["@digits", $card, $name, $reward];
}

print "---\n";

@ids = sort { $a->[0] cmp $b->[0] } @ids;
for $rank (0..$#ids) {
    ($digits, $card, $name, $reward) = $ids[$rank]->@*;
    $s += ($absolutereward = ($rank+1) * $reward);
    print "$digits $card → $name ($rank*$reward=$absolutereward)\n";
}

print "$s\n";


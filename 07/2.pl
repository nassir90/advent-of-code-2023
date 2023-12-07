@c = (A,K,Q,T,9,8,7,6,5,4,3,2,J);
%r = map { $c[$_] => sprintf "%02d", $#c-$_ } 0..$#c;
push @cards, [split " "] while (<>);
for (@cards) {
    my ($card, $reward) = @$_;
    my @d;
    @f{@c} = map { scalar [$card=~/$_/g]->@* } @c;
    my $max = A;
    $_ ne J && $f{$max} < $f{$_} && ($max=$_) for @c;
    @f{@c} = map { scalar [$card=~s/J/$max/gra=~/$_/g]->@* } @c;
    $d[$_]++ for (values %f);
    
    $type="06", $name = "five of a kind"  if $d[5] == 1;
    $type="05", $name = "four of a kind"  if $d[4] == 1;
    $type="04", $name = "full house"      if $d[3] == 1 && $d[2] == 1;
    $type="03", $name = "three of a kind" if $d[3] == 1 && $d[1] == 2;
    $type="02", $name = "two pair"        if $d[2] == 2 && $d[1] == 1;
    $type="01", $name = "one pair"        if $d[2] == 1 && $d[1] == 3;
    $type="00", $name = "high card"       if $d[1] == 5;
    
    @m = ($type, map { $r{$_} } split //, $card);
    $number = "@m";
    print "$number $card → $name ($max)\n";
    push @ids, [$number, $card, $name, $reward];
}

print "---\n";

@ids = sort { $a->[0] cmp $b->[0] } @ids;
for $rank (0..$#ids) {
    ($number, $card, $name, $reward) = $ids[$rank]->@*;
    $absolutereward = ($rank+1) * $reward;
    $s += $absolutereward;
    print "$number $card → $name ($rank*$reward=$absolutereward)\n";
}
print "$s\n";


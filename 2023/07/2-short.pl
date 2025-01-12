# Take in a list of hands (sets of five cards) and order them based on
# a value, which is based on the class of the hand ($value) and the
# contents of the hand (@r{@chars}). After ordering, sum the products
# of the rank-bid pairs. Part 2 introduced jokers that are treated as
# the most common non-joker card in a deck for hand class computation.
@c = (A,K,Q,T,9,8,7,6,5,4,3,2,J);
@r{@c} = map { sprintf "%02d", $#c-$_ } 0..$#c;
@ids = sort { @$a[0] cmp @$b[0] } map {
    my ($card, $reward) = split " ";
    my @chars = (split //, $card), %frequencies = (), $value = 0, $max = A;
    $frequencies{$_}++ for @chars;
    $_ ne J && $frequencies{$max} < $frequencies{$_} && ($max=$_) for @c;
    $frequencies{$max} += $frequencies{J}, $frequencies{J} = 0;
    $value += $_*$_ for (values %frequencies);
    @digits = ((sprintf "%02d", $value), @r{@chars});
    ["@digits", $reward]
} (<>);
$s += ($_+1)*$ids[$_][1] for (0..$#ids);
print "$s";

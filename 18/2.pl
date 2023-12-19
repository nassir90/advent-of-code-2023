use List::Util qw/uniq/;
$stupid = $ENV{stupid} // 0;
@R = (0, 1), @L = (0, -1), @U = (-1, 0), @D = (1, 0);
@map = (R, D, L, U);
my ($row, $column) = (0, 0);
while (my ($direction, $delta, undef, $color, undef) = split /[ ()]/, <>) {
  if ($stupid) {
    $delta = hex substr $color, 1, 5;
    $direction = $map[hex substr $color, 6, 1];
    ($dr, $dc) = @$direction;
  } else {
    ($dr, $dc) = @$direction;
  }
  my $endrow    = $row    + $dr * $delta;
  my $endcolumn = $column + $dc * $delta;
  
  if ($dr) {
    # The horizontals around a vertical are described by
    # $horizontals[$vertical{last}] and
    # $horizontals[($vertical{last}+1) % @horizontals]
    # At any vertical setpoint, you check its horizontals.
    # - If there are none, upper ^= 1, lower ^= 1.
    # - If vertical lower = horizontal right XOR upper
    # - If vertical upper = horizontal right XOR lower
    # - Otherwise if the current X intersects with the current HRANGE,
    # XOR both.
    my ($start, $end) = sort { $a <=> $b } $row, $endrow;
    push @verticals, {column => $column, start => $start, end => $end, last => $#horizontals};
  } elsif ($dc) {
    my ($start, $end) = sort { $a <=> $b } $column, $endcolumn;
    push @horizontals, {row => $row, start => $start, end => $end};
  }
  ($row, $column) = ($endrow, $endcolumn);
  $maximumrow = $row if ($row > $maximumrow);
  $minimumrow = $row if ($row < $minimumrow);
  $maximumcolumn = $column if ($column > $maximumcolumn);
  $minimumcolumn = $column if ($column < $minimumcolumn);
}
$height = $maximumrow - $minimumrow;
$width = $maximumcolumn - $minimumcolumn;
print "Done parsing ($height [$minimumrow → $maximumrow] x $width [$minimumcolumn → $maximumcolumn)\n";
@verticals = sort { $$a{column} <=> $$b{column} } @verticals;
@ranks = uniq
  sort { $a <=> $b }
  map { $$_{row}, $$_{row}+1 }
  @horizontals;
for my $rank (0..$#ranks) {
  my $size = $rank == $#ranks
    ? $size = 1
    : $size = $ranks[$rank+1] - $ranks[$rank];
  my ($top, $bottom) = ($ranks[$rank], $ranks[$rank]+$size-1);
  my $count;
  my $inside;
  my $upper;
  my $lower;
  my $last = $minimumcolumn;
  for my $vertical (@verticals) {
    next unless $$vertical{start} <= $top <= $bottom <= $$vertical{end};
    my ($upperhorizontal, $lowerhorizontal) = sort
      { $$a{row} <=> $$b{row} }
      $horizontals[$$vertical{last}],
      $horizontals[($$vertical{last}+1)%@horizontals];
    my ($lastupper, $lastinside, $lastlower) = ($upper, $inside, $lower);
    if ($$upperhorizontal{row} == $top) {
      $lower ^= 1; #  ## <- us
                   # ##
    } elsif ($$lowerhorizontal{row} == $bottom) {
      $upper ^= 1; # ##
                   #  ## <- us
    } else {
      $upper ^= 1; # #
      $lower ^= 1; # #  <- us
    }
    $inside = $upper | $lower;
    my $delta;
    if ($lastinside) {
      $delta = $$vertical{column} - $last;
    } if (!$lastinside and $inside) {
      $delta = 1;
    }
    $count += $delta * $size;
    $last = $$vertical{column};
  }
  $sum += $count;
}

print "Answer: $sum\n";

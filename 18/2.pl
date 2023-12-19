use Data::Dumper;
use List::Util qw/uniq/;

# more flood fill crap! now i can use polygon in polygon shit tho 👀
# after looking at the solution for previous days.
$stupid = $ENV{stupid} // 0;

@R = (0, 1), @L = (0, -1), @U = (-1, 0), @D = (1, 0);
@map = (R, D, L, U);
my ($row, $column) = (0, 0);

while (my ($direction, $delta, undef, $color, undef) = split /[ ()]/, <>) {
  
  if ($stupid) {
    $delta = hex substr $color, 1, 5;
    $direction = $map[hex substr $color, 6, 1];
    ($dr, $dc) = @$direction;
    printf "Color: %s, Delta: $delta, Direction: $direction\n", substr($color, 1, 5), substr($color, 6);
  } else {
    ($dr, $dc) = @$direction;
  }
  
  my $endrow    = $row    + $dr * $delta;
  my $endcolumn = $column + $dc * $delta;
  
  if ($dr) {
    # The horizontals around a vertical are described by
    #
    # $horizontals[$vertical{last}] and
    # $horizontals[($vertical{last}+1) % @horizontals]
    #
    # At any vertical setpoint, you check its horizontals.
    #
    # - If there are none, upper ^= 1, lower ^= 1.
    # 
    # - If vertical lower = horizontal right XOR upper
    #
    # - If vertical upper = horizontal right XOR lower
    #
    # - Otherwise if the current X intersects with the current HRANGE,
    # XOR both.
    #
    my ($start, $end) = sort { $a <=> $b } $row, $endrow;
    print "\tvertical:   ($start → $end)\n";
    push @verticals, {column => $column, start => $start, end => $end, last => $#horizontals};
  } elsif ($dc) {
    my ($start, $end) = sort { $a <=> $b } $column, $endcolumn;
    # print "\thorizontal: ($row, $column) → ($endrow, $endcolumn)\n";
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

# for my $vertical (@verticals) {
#   @rizz = ($horizontals[$$_{last}],
#            $horizontals[($$_{last}+1)%@horizontals]);
#   $lower = grep { $$_{start} == $$vertical{
# }
# print Dumper $horizontals[$verticals[0]{last}];
# print Dumper $horizontals[($verticals[0]{last}+1)%@verticals];
# print "Ok that worked\n";
# print Dumper $horizontals[$verticals[$#verticals]{last}];
# print Dumper $horizontals[($verticals[$#verticals]{last}+1)%@verticals];

# if rank size is one, we are dealing with a line.
@ranks = uniq
  sort { $a <=> $b }
  map { $$_{row}, $$_{row}+1 }
  @horizontals;

# print Dumper @ranks;

my $sum;

for my $rank (0..$#ranks) {
  my $size;
  
  if ($rank == $#ranks) {
    $size = 1;
  } else {
    $size = $ranks[$rank+1] - $ranks[$rank];
  }

  # print "Difference $ranks[$rank] → $ranks[$rank+1]\n";

  my ($top, $bottom) = ($ranks[$rank], $ranks[$rank]+$size-1);
  
  print "$size $top → $bottom ";

  my $count;
  my $inside;
  my $upper;
  my $lower;
  my $last = $minimumcolumn;
  
  for my $vertical (@verticals) {
    if ($$vertical{start} <= $top <= $bottom <= $$vertical{end}) {
      print "HERE ($top $bottom) ($$vertical{start} $$vertical{end}) at $$vertical{column}\n";
    } else {
      print "SKIP ($top $bottom) ($$vertical{start} $$vertical{end}) at $$vertical{column}\n";
      next;
    }
    my ($upperhorizontal, $lowerhorizontal) = sort
      { $$a{row} <=> $$b{row} }
      $horizontals[$$vertical{last}],
      $horizontals[($$vertical{last}+1)%@horizontals];
    # print "($$vertical{start}<>$$lowerhorizontal{row} [$$lowerhorizontal{start},$$lowerhorizontal{end}])→($$vertical{end}<>$$upperhorizontal{row} [$$upperhorizontal{start},$$upperhorizontal{end}]) ";
    my ($lastupper, $lastinside, $lastlower) = ($upper, $inside, $lower);
    if ($$upperhorizontal{row} == $top) {
      #
      #  ## <- us
      # ##
      #
      $lower ^= 1;
    } elsif ($$lowerhorizontal{row} == $bottom) {
      #
      # ##
      #  ## <- us
      #
      $upper ^= 1;
    } else {
      #
      # # <- u
      # # <- s
      #
      $upper ^= 1;
      $lower ^= 1;
    }

    $inside = $upper | $lower;

    my $delta;
    if ($lastinside and $inside) {
      $delta = $$vertical{column} - $last;
      print "🏃 ($delta) ";
    } elsif ($lastinside and !$inside) {
      $delta = $$vertical{column} - $last;
      print "🏃 ($delta) ";
    } if (!$lastinside and $inside) {
      $delta = 1;
    }
    $count += $delta * $size;
    
    $last = $$vertical{column};

    my $i = $inside ? "i" : "o";
    my $u = $upper  ? "u" : "_";
    my $l = $lower  ? "l" : "_";

    print "$u$i$l, ";
  }

  $sum += $count;
  print " : $count\n";
}

print "Sum: $sum\n";

exit;

for my $row ($minimumrow..$maximumrow) {
  my ($upper, $inside, $lower) = (0, 0, 0);
  for my $column ($minimumcolumn..$maximumcolumn) {
    if ($marked{$row,$column}) {
      $count++;
      $upper  ^= $marked{$row-1,$column};
      $lower  ^= $marked{$row+1,$column};
      $inside = $upper && $lower;
      # print "#";
    } else {
      if ($inside) {
        $count++;
        # print "\e[48;2;100;0;0m.\e[m";
      } else {
        # print ".";
      }
    }
  }
  print "Answer: $count\n";
}





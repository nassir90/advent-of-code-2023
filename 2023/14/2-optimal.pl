use Digest::MD5 qw/md5_base64/;

my $target = $ENV{TARGET} // 1_000_000_000;

# Define position deltas corresponding to each cardinal direction
@north = (-1,  0), @west  = ( 0, -1), @south = ( 1,  0), @east  = ( 0,  1);
%previous = (west => north, south => west, east => south, north => east);

# Read the map from standard input, removing newlines in the process.
my @lines = map { chomp; $_ } <>;
# I extend the borders of the map to contain blocks (stones that don't
# roll) as it makes the logic easier further down the line.
my ($width, $height) = (2 + length($lines[0]), 2 + @lines);
$_ = "#" . $_ . "#" for @lines;
unshift @lines, '#' x $width;
push    @lines, '#' x $width;

# Log the location of each block / rock
my $lines = join "", @lines;
while ($lines =~ /[O#]/g) {
  $rocks{int($-[0] / $width), $-[0] % $width } = "O" if "O" eq $&;
  $blocks{int($-[0] / $width), $-[0] % $width} = "#" if "#" eq $&;
}

# This is the first bit of preparation for the representation of the
# rocks in later parts of the code. Rocks will not be represented in
# terms of their positions; the %rocks hash actually only pertains to
# initial rocks. Rather, after any movement, we store the number of
# rocks proceeding from any one block and can derive the block
# locations from that if we need to. For any derived rock position,
# the %magnet hash stores the block / border to which it would move if
# the map was tilted to move the rocks in that direction.
for $block (keys %blocks) {
  my ($blockrow, $blockcolumn) = split $;, $block;
  for (north, south, east, west) {
    my ($direction, $dr, $dc) = ($_, @$_);
    my ($row, $column) = ($blockrow, $blockcolumn);
    while ($row -= $dr, $column -= $dc, not $blocks{$row, $column}) {
      last unless 0 <= $row < $width and 0 <= $column < $height;
      $magnet{$direction}{$row, $column} = [$blockrow, $blockcolumn];
    }
  }
}

print "Constructed block LUT\n";

# This function uses the rock representation and actually does the
# movement. It takes in the %have hash, which in combination with the
# $previous direction can be used to derive all of the rock
# locations. It then applies each derived rock location to %magnet to
# simulate the rock moving in $direction.
sub engage {
  my ($direction) = @_;
  my ($dr, $dc) = @$direction;
  my $previous = $previous{$direction};
  my ($pdr, $pdc) = @$previous;
  
  my %next;
    
  for (keys %have) {
    my ($row, $column) = split $;;
    for (1..$have{$row, $column}) {
      my ($rockrow, $rockcolumn) = ($row - $pdr * $_, $column - $pdc * $_);
      my ($nextrow, $nextcolumn) = @{$magnet{$direction}{$rockrow, $rockcolumn}};
      # print "Based on ($row $column), previous=$previous ($pdr $pdc), next=$direction\n";
      # print "\tMoving rock ($rockrow $rockcolumn) $direction → ($nextrow $nextcolumn)\n";
      $next{$nextrow, $nextcolumn}++;
    }
  }
  
  return %next;
}

sub signature {
  my ($have) = @_;
  join $;,
    sort { $a cmp $b }
    map { $_ . $; . %$have{$_} }
    keys %$have
}

# Takes the data encoded in a %have hash passed by reference and 
sub formathave {
  my ($have) = @_;
  my ($dr, $dc) = @_[1..2];
  my %virtual;
  for (keys %$have) {
    my ($blockrow, $blockcolumn) = split $;;
    for (1..$$have{$blockrow, $blockcolumn}) {
      $virtual{$blockrow - $dr * $_, $blockcolumn - $dc * $_} = "O";
    }
  }
  my @virtual;
  my @have;
  for $row (0..$width-1) {
    for $column (0..$height-1) {
      $virtual[$row] .= $virtual{$row, $column}
        ? "O" :
        $blocks{$row, $column} ? "#" : ".";
      $have[$row] .= $blocks{$row, $column}
        ? 0 + $$have{$row, $column}
        : ".";
    }
  }
  return \(@virtual, @have);
}

sub score {
  my ($have, $dr, $dc) = @_;
  my %have = $have->%*;
  my $score;
  for (keys %have) {
    my ($blockrow, $blockcolumn) = split $;;
    for (1..$have{$blockrow, $blockcolumn}) {
      # The logic for computing scores is different to what you might
      # expect here, as we augment the map. The real block row is
      # obtained by subtracting the top row of introduced '#'
      # symbols. The real height is the current height minus 2.
      my $rockrow = $blockrow - $dr * $_ - 1;
      $score += $height - 2 - $rockrow;
    }
  }
  return $score;
}

my $cycles = 10_000;

# The first cycle has to be done making reference to the initial input
# file.
for (keys %rocks) {
  my ($row, $column) = split $;;
  my ($nextrow, $nextcolumn) = @{$magnet{north}{$row, $column}};
  $have{$nextrow, $nextcolumn}++;
}
print "Completed first move north\n";
%have = engage $_ for (west, south, east);

# my ($virtual, $have) = formathave \%have, @east;
# print "\nBelow: $scores[$cycle]\n";
# print "@$virtual[$_]|@$have[$_]\n" for (0..$height-1);
my %signature = signature \%have;
$scores[$cycle] = score \%have, @east;
$digests[$cycle] = md5_base64($signature);
$seen{$signature} = 1;
$cycle++;

while ($cycle < $cycles) {
  %have = engage $_ for (north, west, south, east);
  
  # my ($virtual, $have) = formathave \%have, @east;
  # print "\nBelow: $scores[$cycle]\n";
  # print "@$virtual[$_]|@$have[$_]\n" for (0..$height-1);
  my $signature = signature \%have;
  $scores[$cycle] = score \%have, @east;
  $digests[$cycle] = md5_base64($signature);

  if ($seen{$signature}) {
    $cyclestart = $seen{$signature};
    print "it repeats thanks be to god (at cycle $cycle)\n";
    print "\t$cyclestart <=> $cycle\n";
    last;
  }
  
  $seen{$signature} = $cycle;
} continue {
  $cycle++;
}

print "All scores until repeating section\n";
printf "\t%3d. $scores[$_] ($digests[$_])\n", $_ for (0..$#scores);

if ($target < @cycle) {
  # in this case we didn't go beyond the loop, so we can just index
  # into the scores array.
  printf "Is the answer: %d?\n", $scores[$target];
} else {
  # in this case, we went beyond the cycle. the final scores index is
  # the cycle starting position, plus the number of cycles after that
  # modulo the cylce period.
  my $offsettarget = $target - $cyclestart - 1;
  my @cycle = @scores[$cyclestart..$#scores-1];
  print "Repeating section\n";
  printf "\t%3d. $cycle[$_] ($digests[$cyclestart+$_])\n", $_ for (0..$#cycle);
  printf "Is the answer: %d? ($target %% %d)\n", $cycle[$offsettarget % @cycle], scalar @cycle;
}

use Digest::MD5 qw/md5_base64/;

@north = (-1,  0), @west  = ( 0, -1), @south = ( 1,  0), @east  = ( 0,  1);
%previous = (west => north, south => west, east => south, north => east);

my @lines = map { chomp; $_ } <>;
my ($width, $height) = (2 + length($lines[0]), 2 + @lines);
$lines[$_] = "#$lines[$_]#" for 0..$#lines;
unshift @lines, '#' x $width;
push    @lines, '#' x $width;

my $lines = join "", @lines;
while ($lines =~ /[O#]/g) {
  $rocks{int($-[0] / $width), $-[0] % $width } = "O" if "O" eq $&;
  $blocks{int($-[0] / $width), $-[0] % $width} = "#" if "#" eq $&;
}

sub valid {
  my ($row, $column) = @_;
  0 <= $row < $height and 0 <= $column < $width
}

sub at {
  my ($row, $column) = @_;
  return "#" if $blocks{$row, $column} or not valid $row, $column;
  return ".";
}

for $block (keys %blocks) {
  my ($blockrow, $blockcolumn) = split $;, $block;
  for (north, south, east, west) {
    my ($direction, $dr, $dc) = ($_, @$_);
    my ($row, $column) = ($blockrow, $blockcolumn);
    # we move opposite the current direction
    while ($row -= $dr, $column -= $dc, "#" ne at $row, $column) {
      $magnet{$row, $column}{$direction} = [$blockrow, $blockcolumn];
      # my $c = at $row, $column;
      # print "$direction of $row, $column → $blockrow, $blockcolumn\n";
    }
  }
}

print "Constructed block LUT\n";

# prepare initial solution
my %have;
my %seen;

sub engage {
  my ($direction) = @_;
  my ($dr, $dc) = @$direction;
  
  my $previous = $previous{$direction};
  my ($pdr, $pdc) = @$previous;
  
  my %next;
  
  # my $index = $dr == 0 ?   1 :    0;
  # my $flip  = $dr == 0 ? -$dc : -$dr;
  # my @blocks =
  #   sort { $flip * ($a->[$index] <=> $b->[$index]) }
  #   map { [split $;, $_ ] } keys %have;
    
  for (keys %have) {
    my ($row, $column) = split $;;
    for (1..$have{$row, $column}) {
      my ($rockrow, $rockcolumn) = ($row - $pdr * $_, $column - $pdc * $_);
      my ($nextrow, $nextcolumn) = @{$magnet{$rockrow, $rockcolumn}{$direction}};
      # print "Based on ($row $column), previous=$previous ($pdr $pdc), next=$direction\n";
      # print "\tMoving rock ($rockrow $rockcolumn) $direction → ($nextrow $nextcolumn)\n";
      
      my $delta = ++$next{$nextrow, $nextcolumn};
    }
  }
  
  return %next;
}

sub formathave {
  my %have = %{$_[0]};
  my ($dr, $dc) = @_[1..2];
  
  my %virtual;
  for (keys %have) {
    my ($row, $column) = split $;;
    for (1..$have{$row, $column}) {
      my ($rockrow, $rockcolumn) = ($row - $dr * $_, $column - $dc * $_);
      $virtual{$rockrow, $rockcolumn} = "O";
    }
  }
    
  my @virtual;
  my @have;
    
  for $row (0..$width-1) {
    my $virtual;
    my $have;
    for $column (0..$height-1) {
      die "Conflict on $row, $column"
        if $virtual{$row, $column} and "#" eq at $row, $column;
      $virtual .= $virtual{$row, $column} ? "O" : at $row, $column;
      $have .= $blocks{$row, $column} ? 0 + $have{$row, $column} : ".";
    }
    push @virtual, $virtual;
    push @have, $have;
  }
  
  return \(@virtual, @have);
}

sub score {
  my ($have, $dr, $dc) = @_;
  my %have = $have->%*;
  # my %check;
  
  my $score;
  for (keys %have) {
    my ($blockrow, $blockcolumn) = split $;;
    # printf "(%d %d) has %d\n", $blockrow, $blockcolumn, $have{$blockrow, $blockcolumn};
    for (1..$have{$blockrow, $blockcolumn}) {
      # the actual row of the block without extended guards
      my $rockrow = $blockrow - $dr * $_ - 1;
      # $check{$rockrow+1}++;
      # add to the score the height of this block using the height of
      # the map without guards
      $score += $height - 2 - $rockrow;
    }
  }
  # for (sort { $a <=> $b } keys %check) {
  #   print "Row $_: O x $check{$_}\n";
  # }
  
  return $score;
}

my $cycles = 10_000;

# The first cycle has to be done making reference to the initial input
# file.
for (keys %rocks) {
  my ($row, $column) = split $;;
  $block = $magnet{$row, $column}{north};
  $have{@$block[0], @$block[1]}++;
  # print "rock ($row $column) → (@$block)\n";
}

print "Completed first move north\n";

%have = engage west;
%have = engage south;
%have = engage east;
my ($virtual, $have) = formathave \%have, @east;
# print "\nBelow: $scores[$cycle]\n";
# print "@$virtual[$_]|@$have[$_]\n" for (0..$height-1);
$scores[$cycle] = score \%have, @east;
$digests[$cycle] = md5_base64("@$virtual");
$seen{"@$virtual"} = 1;
$cycle++;

# stores the start of the cycle
my $parallel;

while ($cycle < $cycles) {
  %have = engage north;
  %have = engage west;
  %have = engage south;
  %have = engage east;
  
  my ($virtual, $have) = formathave \%have, @east;
  # print "\nBelow: $scores[$cycle]\n";
  # print "@$virtual[$_]|@$have[$_]\n" for (0..$height-1);
  $scores[$cycle] = score \%have, @east;
  $digests[$cycle] = md5_base64("@$virtual");

  if ($seen{"@$virtual"}) {
    $parallel = $seen{"@$virtual"};
    print "it repeats thanks be to god (at cycle $cycle)\n";
    print "\t$parallel <=> $cycle\n";
    last;
  }
  $seen{"@$virtual"} = $cycle;
} continue {
  $cycle++;
}

my $offsetbillion = 1_000_000_000 - $parallel - 1;
my @cyclicalscores = @scores[$parallel..$#scores-1];

print "All scores\n";
print "\t${_}. $scores[$_] ($digests[$_])\n" for (0..$#scores);
print "Repeating section\n";
print "\t${_}. $cyclicalscores[$_] ($digests[$parallel+$_])\n" for (0..$#cyclicalscores);

printf "Is the answer: %d? (billion %% %d)\n", $cyclicalscores[$offsetbillion % @cyclicalscores], scalar @cyclicalscores;

END { print "$s\n" }

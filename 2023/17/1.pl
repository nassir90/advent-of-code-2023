use warnings;

# each vertex is either visited or unvisted
# for each visited vertex we store
# - the heat loss so far
# - the steps since the last turn
# our minimum implementation by
# - heat loss so far
# and excludes
# - vertices whose stesps since last turn >= 3

# we will have a distTo hash
# we will have a vertexTo hash also
# we will have a stretchTo hash (length of the straight)
# delete the minimum
# - filter by last turn < 3
# - pick vertex with minimum heat loss
# - advance that hoe

# write perl priority queue afterwards

our @north = (-1, 0);
our @west  = (0, -1);
our @south = (1, 0);
our @east  = (0, 1);

our @lines = map { chomp; $_ } <>;
our $width = length $lines[0];
our $height = @lines;

my $targetrow = $height - 1;
my $targetcolumn = $width - 1;

our %dist;
$dist{0, 0} = 0;

our @queue = ([0,0]);
our %stretchTo;
our %directionTo;
our %vertexTo;

our %colors = (red => [255,000,000],
             orange => [125, 50, 50],
             yellow => [150, 150, 0 ]);

sub paint {
  my ($color, $string, $start, $size) = @_;
  return $string unless $start < length($string);
  $size = $size // 1;
  local $" = ";", $color = "@{$colors{$color}}";
  substr $string, $start, $size, "\e[48;2;${color}m" . substr($string, $start, $size) . "\e[m";
  return $string;
}

sub realtruck {
  my %painted;
  my @lines = @lines;
  for (@_) {
    my ($color, $row, $column) = @$_;
    next if $painted{$row, $column};
    $lines[$row] = paint $color, $lines[$row], $column, 1;
    # print "Painting $row, $column $color\n";
    $painted{$row, $column} = 1;
  }
  print "$_\n" for @lines;
}

sub dumptruck {
  my @marks;
  push @marks, [orange, @$_] for (@queue);
  push @marks, [yellow, split $;] for (keys %dist);
  @marks = sort {@$b[2] <=> @$a[2]} @marks;
  realtruck @marks;
}

sub esotericdumptruck {
  my ($row, $column) = @_;
  my @marks;
  while (not $row == 0 && $column == 0) {
    push @marks, [orange, $row, $column];
    ($row, $column) = @{$vertexTo{$row, $column}};
  }
  realtruck @marks;
}

search: while (1) {
  my $minimumrow;
  my $minimumcolumn;
  my $minimumdist = Inf;
  my $chosen = 0;
  for (0..$#queue) {
    my ($row, $column) = @{$queue[$_]};
    if ($dist{$row, $column} < $minimumdist) {
      $minimumrow = $row;
      $minimumcolumn = $column;
      $minimumdist = $dist{$row, $column};
      $chosen = $_;
    }
  }
  die unless defined $minimumrow;
  print "Chosen ($minimumrow $minimumcolumn) as starting off pount with distance $minimumdist\n";
  splice @queue, $chosen, 1, ();
  my $previousdirection = $directionTo{$minimumrow, $minimumcolumn};
  my ($sourcerow, $sourcecolumn) = @{$vertexTo{$minimumrow, $minimumcolumn}};

  my @neighbors;
  my @directions;
  
  print("Generating neighbors for ($minimumrow $minimumcolumn)\n");
  for my $direction (north, south, east, west) {
    print("\t$direction out of bounds from ($minimumrow $minimumcolumn) [0 → $height]\n"), next
      unless 0 <= (my $row = $minimumrow + @$direction[0]) < $height;
    print("\t$direction out of bounds from ($minimumrow $minimumcolumn) [0 → $width]\n"), next
      unless 0 <= (my $column = $minimumcolumn + @$direction[1]) < $width;
    print("\tcannot go back as ($row $column) == ($sourcerow $sourcecolumn) from ($minimumcolumn $minimumrow)\n"), next
      if defined($sourcerow) && ($row == $sourcerow and $column == $sourcecolumn);
    my $stretch = $stretchTo{$minimumrow, $minimumcolumn};
    if ($direction eq $previousdirection  && 3 <= 1 + $stretchTo{$minimumrow, $minimumcolumn}) {
      print("\tcan not go straight for longer than three units ($previousdirection → $direction) ($stretch)\n");
      next;
    } else {
      print("\tcan go straight in this case ($previousdirection → $direction) (stretch=$stretch)\n");
    }
    if ($row == $targetrow and $column == $targetcolumn) {
      my $dist = $dist{$minimumrow, $minimumcolumn};
      print "Done: $dist\n";
      $vertexTo{$targetrow, $targetcolumn} = [$minimumrow, $minimumcolumn];
      last search;
    }
    push @neighbors, [$row, $column];
    push @directions, $direction;
  }
  
 idekanymore: {
    my ($destinationrow, $destinationcolumn, $direction) = (@{shift @neighbors}, shift @directions);
    unless (defined $direction) {
      die "all should be defined at the same time" if defined $destinationrow;
      last;
    };
    my $cost = substr $lines[$destinationrow], $destinationcolumn, 1;
    my $dist = $minimumdist + $cost;
    my $existingdist = $dist{$destinationrow, $destinationcolumn} // Inf;
    print "From $previousdirection → ( $minimumrow $minimumcolumn) $direction → ($destinationrow, $destinationcolumn) ";
    print "[cost=$cost, resultingdistance=$dist, existingdistance=$existingdist]\n";
    if ($dist < $existingdist) {
      $dist{$destinationrow, $destinationcolumn} = $dist;
      $vertexTo{$destinationrow, $destinationcolumn} = [$minimumrow, $minimumcolumn];
      $stretchTo{$destinationrow, $destinationcolumn} = $direction eq $directionTo{$minimumrow, $minimumcolumn}
        ? 1 +  $stretchTo{$minimumrow, $minimumcolumn}
        : 0;
      $directionTo{$destinationrow, $destinationcolumn} = $direction;
      push @queue, [$destinationrow, $destinationcolumn];
      esotericdumptruck $destinationrow, $destinationcolumn
    }
    redo;
  }
  # dumptruck; #unless $m % 1000;
  if ($m++ > 200) {
    print "💀\n";
    last;
  }
}

esotericdumptruck $targetrow, $targetcolumn;

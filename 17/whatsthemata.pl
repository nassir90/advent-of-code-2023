# what did I learn from this? trying to enumerate all paths will make
# you run out of memory! who knew! My next experiment will use a tree
# representation and only update relevant tails of the tree.

use Data::Dumper;
local $Data::Dumper::Purity = 1;
local $Data::Dumper::Deepcopy = 1;

sub paint {
  my %colors = (red => [255,000,000],
                orange => [125, 50, 50],
                yellow => [150, 150, 0 ]);
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
  @_ = sort {@$b[2] <=> @$a[2]} @_;
  for (@_) {
    my ($color, $row, $column) = @$_;
    next if $painted{$row, $column};
    $lines[$row] = paint $color, $lines[$row], $column, 1;
    # print "Painting $row, $column $color\n";
    $painted{$row, $column} = 1;
  }
  print "$_\n" for @lines;
}

# okay, so the rule where you are guaranteed to be fine replacing the
# path to one place with another holds when both paths are going in
# the same direction and have the same stretch value.

@lines = map { chomp; $_ } <>;
$width = length($lines[0]);
$height = @lines;

# what is in a path?
# - a hash mapping positions on the path to a [stretch, distance, direction] array.

sub collapse {
  my ($row, $column, @direction) = @_;
  my @candidates;
  my %delete;
  for (0..$#paths) {
    my ($otherstretch, $otherdistance, @otherdirection) = @{$paths[$_]{$row, $column}};
    if ($paths[$_]{$row, $column} and "@direction" eq "@otherdirection") {
      # print "other path is in sync at ($row $column)\n";
      push @candidates, $_;
      $delete{$_} = 1;
    }
  }
  
  my $s = sub { $paths[$_[0]]->{$row, $column}[0] };
  my $d = sub { $paths[$_[0]]->{$row, $column}[1] };
  
  # sort by distance and try to eliminate by stretch
  @candidates = sort { $d->($a) <=> $d->($b) } @candidates;
  my $minimum = 0;
  my @next;
  for (0..$#candidates) {
    $minimum = $candidates[$_] if $s->($candidates[$_]) < $s->($minimum);
    next unless
      $_ == $#candidates ||   # final
      $d->($candidates[$_]) != $d->($candidates[$_+1]); # middle
    push @next, $minimum;
  }

  # sort by stretch and try to eliminate by distance
  @candidates = sort { $s->($a) <=> $s->($b) } @next;
  my $minimum = 0;
  my @next;
  for (0..$#candidates) {
    $minimum = $candidates[$_] if $d->($candidates[$_]) < $d->($minimum);
    next unless
      $_ == $#candidates ||   # final
      $s->($candidates[$_]) != $s->($candidates[$_+1]); # middle
    delete $delete{$minimum};
    push @next, $minimum;
  }

  splice @paths, $_, 1, () for (sort { $b <=> $a } keys %delete);

  return @paths;
}

# test filtering by distance
{
  local @paths;
  @north = (-1, 0), @west = (0, -1), @south = (1, 0), @east = (0, 1);
  $paths[0]->{0, 1} = [1, 10, @east];
  $paths[1]->{0, 1} = [1, 30, @east];
  @paths = collapse 0, 1, @east;
  for (@paths) {
    print "\t@{$$_{0,1}}\n";
  }
}

# test filtering by distance
{
  local @paths;
  @north = (-1, 0), @west = (0, -1), @south = (1, 0), @east = (0, 1);
  $paths[0]->{0, 1} = [1, 10, @east];
  $paths[1]->{0, 1} = [1, 30, @east];
  $paths[2]->{0, 1} = [2, 30, @east];
  @paths = collapse 0, 1, @east;
  print "This should be the same as the above\n";
  for (@paths) {
    print "\t@{$$_{0,1}}\n";
  }
}

# test filtering by distance
{
  local @paths;
  @north = (-1, 0), @west = (0, -1), @south = (1, 0), @east = (0, 1);
  $paths[0]->{0, 1} = [1, 10, @east];
  $paths[1]->{0, 1} = [2, 10, @east];
  $paths[2]->{0, 1} = [1, 30, @east];
  $paths[3]->{0, 1} = [2, 30, @east];
  $paths[4]->{0, 1} = [3, 30, @east];
  @paths = collapse 0, 1, @east;
  print "This should be the same as the above\n";
  for (@paths) {
    print "\t@{$$_{0,1}}\n";
  }
}

@paths = ({ tail => [0,0] });

while (1)  {
  my @children;
  my %interesting;
  for my $path (@paths) {
    my ($row, $column) = @{$$path{tail}};
    if ($column == $width - 1 && $row == $height - 1) {
      print "Not continuing finished path\n";
      next;
    }
    my ($stretch, $distance, @direction) = @{$$path{$row, $column}};
    die unless $row == 0 && $column == 0 || @direction;
    for my $nextdirection (east, south, west, north) {
      my ($dr, $dc) = @$nextdirection;
      my ($nextrow, $nextcolumn) = ($row + $dr, $column + $dc);
      unless (0 <= $nextrow < $height and 0 <= $nextcolumn < $width) {
        # print ("$nextdirection ($nextrow $nextcolumn) is OOB\n");
        next;
      }
      if ($nextrow == 0 && $nextcolumn == 0 or exists $$path{$nextrow, $nextcolumn}) {
        # print ("$nextdirection ($nextrow $nextcolumn) is on this path already\n");
        next;
      }
      my $nextstretch = "@direction" eq "@$nextdirection" ? 1 + $stretch : 1;

      if ($nextstretch >= 3) {
        # print "Cannot stretch longer 😩\n";
        next;
      }
      
      my $nextdistance = $distance + substr $lines[$nextrow], $nextcolumn, 1;
      my %newpath;
      $newpath{tail} = [$nextrow, $nextcolumn, @$nextdirection];
      $newpath{$$_[0], $$_[1]} = $$path{$$_[0], $$_[1]} for @{$$path{steps}};
      $newpath{steps} = [@{$$path{steps}}, [$nextrow, $nextcolumn]];
      $newpath{$nextrow, $nextcolumn} = [$nextstretch, $nextdistance, @$nextdirection];
      # print Dumper($newpath);
      push @children, { %newpath };
      $interesting{$nextrow, $nextcolumn, "@$nextdirection"} = 1;
    }
  }
  for (keys %interesting) {
    my ($row, $column, $direction) = split $;;
    @direction = split $", $direction;
    @paths = collapse $row, $column, @direction;
  }
  push @paths, @children;
  printf "At $m with %d paths\n", scalar @paths;
  last if $m++ > 100;
}

my @lastpaths = grep { $$_{$height-1, $width-1} } @paths;
my @lastdistances = map { $$_{$height-1, $width-1}[1] } @lastpaths;

print Dumper(@paths);
printf "Paths created are: %d\n", scalar @paths;

$DB::single = 2;

$x ++;

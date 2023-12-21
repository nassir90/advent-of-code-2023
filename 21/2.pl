use integer;
@lines = map { chomp; $_ } <>;
$height = @lines;
$width = length($lines[0]);
$max = 1000;

sub free {
  my ($row, $column) = @_;
  # printf "%d ($width)\n", $column % $width;
  my $c = substr $lines[$row % $height], $column % $width, 1;
  "." eq $c || "S" eq $c
}

sub bfs {
  my ($row, $column) = @_;
  my $lowerdistance = 0;
  my @queue = ([$row, $column, 0]);
  while (my ($row, $column, $distance) = @{shift @queue}) {
    next if exists $distance{$row, $column};
    $distance{$row, $column} = $distance;
    if (($max - $distance) % 2 == 0) {
      # substr($lines[$row], $column, 1) = "O";
      $O++;
    }
    next if $distance == $max;
    push @queue, [$row+1, $column, $distance+1]
      if free($row+1, $column) and !exists $distance{$row+1, $column};
    push @queue, [$row-1, $column, $distance+1]
      if free($row-1, $column) and !exists $distance{$row-1, $column};
    push @queue, [$row, $column+1, $distance+1]
      if free($row, $column+1) and !exists $distance{$row, $column+1};
    push @queue, [$row, $column-1, $distance+1]
      if free($row, $column-1) and !exists $distance{$row, $column-1};
  }
}

bfs $height/2, $width/2;

for (@lines) {
  local $" = "";
  print "$_\n";
}

print "$O\n";

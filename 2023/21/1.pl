use integer;
@lines = map { chomp; $_ } <>;
$height = @lines;
$width = length($lines[0]);
$max = 64;



sub bfs {
  my ($row, $column) = @_;
  my @queue = ([$row, $column, 0]);
  while (my ($row, $column, $distance) = @{shift @queue}) {
    next if exists $distance{$row, $column};
    next unless substr($lines[$row], $column, 1) =~ /[S.]/;
    next if $distance > $max;
    $distance{$row, $column} = $distance;
    if (($max - $distance) % 2 == 0) {
      substr($lines[$row], $column, 1) = "O";
      $O++;
    }
    push @queue, [$row+1, $column, $distance+1];
    push @queue, [$row-1, $column, $distance+1];
    push @queue, [$row, $column+1, $distance+1];
    push @queue, [$row, $column-1, $distance+1];
  }
}

bfs $height/2, $width/2;

for (@lines) {
  local $" = "";
  print "$_\n";
}

print "$O\n";

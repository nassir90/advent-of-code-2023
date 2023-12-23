# i can use polygon in polygon tests
@R = (0, 1), @L = (0, -1), @U = (-1, 0), @D = (1, 0);
($row, $column) = (0, 0);
$marked{0, 0} = 1;
while (my ($direction, $delta, $color) = split / /, <>) {
  my ($dr, $dc) = @$direction;
  $row += $dr, $column += $dc, $marked{$row, $column} = 1 for (1..$delta);
  $maximumrow = $row if ($row > $maximumrow);
  $minimumrow = $row if ($row < $minimumrow);
  $maximumcolumn = $column if ($column > $maximumcolumn);
  $minimumcolumn = $column if ($column < $minimumcolumn);
}

for my $row ($minimumrow..$maximumrow) {
  my ($upper, $inside, $lower) = (0, 0, 0);
  my $i = $row - $minimumrow;
  printf "%03d. ", $i;
  for my $column ($minimumcolumn..$maximumcolumn) {
    if ($marked{$row, $column}) {
      $count++;
      $upper  ^= $marked{$row-1, $column};
      $lower  ^= $marked{$row+1, $column};
      $inside = $upper && $lower;
      print "#";
    } else {
      if ($inside) {
        $count++;
        print "\e[48;2;100;0;0m.\e[m";
      } else {
        print ".";
      }
    }
  }
  print " answer: $count\n";
}

my @lines = (<>);
my ($width, $height) = (length($lines[0]), scalar @lines);
my $lines = join "", @lines;
while ($lines =~ /O/gm) {
  push @rocks, [int($-[0] / $width), $-[0] % $width];
}
sub at {
  my ($row, $column) = @_;
  substr $lines[$row], $column, 1
}

sub candrop {
  my ($row, $column) = @_;
  print "cannot drop #/.\n", return        unless "O" eq at $row,   $column;
  print "cannot drop into #/EOF\n", return unless "." eq at $row-1, $column;
  return 1
}

sub drop {
  my ($row, $column) = @_;
  die unless candrop $row, $column;
  substr($lines[$row],   $column, 1) = ".";
  substr($lines[$row-1], $column, 1) = "O";
}
for my $rock (@rocks) {
  my ($row, $column) = $rock->@*;
  printf "Rock at %d, %d\n", $row, $column;
  while ($row > 0) {
    if (candrop $row, $column) {
      print "\tDropping to $row, $column\n";
      drop $row, $column;
      $row--;
    } else {
      last
    }
  }
}

$lines = join "", @lines;
printf "%s\n", join "", @lines;
while ($lines =~ /O/gm) {
  my ($row, $column) = (int($-[0] / $width), $-[0] % $width);
  $score += $height - $row;
}

END { print $score }

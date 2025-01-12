my @lines = (<>);
my ($width, $height) = (length($lines[0]), scalar @lines);
my $lines = join "", @lines;
$lines[$_] =~ s/O/./g for @lines;
$rocks{int($-[0] / $width), $-[0] % $width } = "O" while ($lines =~ /O/gm);

my @north = (-1,  0);
my @west  = ( 0, -1);
my @south = ( 1,  0);
my @east  = ( 0,  1);

sub at {
  my ($row, $column) = @_;
  return "#" if "#" eq substr($lines, $width * $row + $column, 1);
  return "O" if $rocks{$row, $column};
  return ".";
}

sub candrop {
  my ($row, $column, $dr, $dc) = @_;
  if (not 0 <= $row + $dr < $width && 0 <= $column + $dc < $height) {
    # print "cannot drop into EOF";
  } elsif (not "O" eq at $row,     $column) {
    # print "cannot drop #/.\n"
  } elsif (not "." eq at $row+$dr, $column+$dc) {
    # print "cant drop into non dot?";
  } else {
    return 1
  }
}

sub drop {
  my ($row, $column, $dr, $dc) = @_;
  die unless candrop $row, $column, $dr, $dc;
  delete $rocks{$row, $column};
  $rocks{$row + $dr, $column + $dc} = "O";
}

sub dumptruck {
  my @newlines;
  for $row (0..$height-1) {
    my $r;
    for $column (0..$width-2) {
      $r .= at $row, $column;
    }
    push @newlines, $r;
  }
  my $newlines = join "\n", @newlines;
  print "$newlines\n\n";
  return $newlines;
}

sub engage {
  my ($dr, $dc) = @_;
  my $index = $dr == 0 ?   1 :    0;
  my $flip  = $dr == 0 ? -$dc : -$dr;
  @rocks =
    sort { $flip * ($a->[$index] <=> $b->[$index]) }
    map { [split $;, $_ ] } keys %rocks;
  for my $rock (@rocks) {
    my ($row, $column) = $rock->@*;
    # printf "Rock at %d, %d\n", $row, $column;
    while (candrop $row, $column, $dr, $dc) {
      # printf "\tDropping to %d, %d\n", $row+ $dr, $column + $dc;
      drop $row, $column, $dr, $dc;
      $row    += $dr;
      $column += $dc;
    }
  }
}

my $cycles = 111;

for (1..$cycles) {
  engage @north;
  engage @west;
  engage @south;
  engage @east;
  # print "HERE $_\n";
}

my $newlines = dumptruck;
while ($newlines =~ /O/gm) {
  my ($row, $column) = (int($-[0] / $width), $-[0] % $width);
  $score += $height - $row;
}

END { print $score }

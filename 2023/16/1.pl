@lines = map { chomp; $_ } <>;
my $width = length($lines[0]);
my $height = @lines;

@north = (-1, 0), @west  = (0, -1), @south = (1, 0), @east  = (0, 1);

$response{$east[0], $east[1]}{'\\'} = [[@south]];
$response{$east[0], $east[1]}{'/'}  = [[@north]];
$response{$east[0], $east[1]}{'|'} = [[@south], [@north]];
$response{$east[0], $east[1]}{'-'} = [[@east]];

$response{$north[0], $north[1]}{'\\'} = [[@west]];
$response{$north[0], $north[1]}{'/'}  = [[@east]];
$response{$north[0], $north[1]}{'-'} = [[@east], [@west]];
$response{$north[0], $north[1]}{'|'} = [[@north]];

$response{$west[0], $west[1]}{'\\'} = [[@north]];
$response{$west[0], $west[1]}{'/'}  = [[@south]];
$response{$west[0], $west[1]}{'|'} = [[@south], [@north]];
$response{$west[0], $west[1]}{'-'} = [[@west]];

$response{$south[0], $south[1]}{'\\'} = [[@east]];
$response{$south[0], $south[1]}{'/'}  = [[@west]];
$response{$south[0], $south[1]}{'-'} = [[@east], [@west]];
$response{$south[0], $south[1]}{'|'} = [[@south]];

$name{$east[0], $east[1]} = "→";
$name{$north[0], $north[1]} = "↑";
$name{$west[0], $west[1]} = "←";
$name{$south[0], $south[1]} = "↓";

# this is not da wae, just do rotations bro

sub ray {
  my ($row, $column, $dr , $dc) = @_;
  $row += $dr, $column += $dc;
  unless(0 <= $row < $height && 0 <= $column < $width) {
    print "Ray ($row, $column) out of bounds\n";
    return;
  }
  if (exists $rays{$dr, $dc}{$row, $column}) {
    print "Ray exists already\n";
    return;
  }
  print "Ray at $row $column $name{$dr, $dc}\n";
  $energised{$row, $column} = 1;
  $rays{$dr, $dc}{$row, $column} = 1;
  my $here = substr $lines[$row], $column, 1;
  if ("." eq $here) {
    # keep going
    ray($row, $column, $dr, $dc);
  } else {
    for my $response (@{$response{$dr, $dc}{$here}}) {
      print "Need to cast to (@$response) from ($row $column) based on $here\n";
      ray($row, $column, @$response)
    }
  }
}

sub dumptruck {
  my $e;
  for $row (0..$height-1) {
    for $column (0..$height-1) {
      if ($energised{$row, $column}) {
        print "#";
        $e++;
      } else {
        print ".";
      }
    }
    print "\n";
  }
  return $e;
}

sub dray {
  local %rays = ();
  local %energised = ();
  ray @_;
  return dumptruck;
}

push @scores, dray 0, -1, @east;

exit;

for $column (0..$width-2) {
  push @scores, dray -1,      $column,          @south;
  push @scores, dray $height, $width-$column-1, @north;
}

for $row (0..$height-2) {
  push @scores, dray $row,           -1,      @east;
  push @scores, dray $height-$row-1,  $width, @west;
}

for (@scores) {
  print "$_\n";
}

print "$e\n";

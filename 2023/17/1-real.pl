use Data::Dumper;
# local $Data::Dumper::Purity = 1;
local $Data::Dumper::Deepcopy = 1;

our $minimumblocksbeforeturn;
our $maximumblocksbeforeturn;

@north = (-1, 0), @west  = (0, -1), @south = (1, 0), @east  = (0, 1);

sub paint {
  my %colors = (red => [255,000,000], orange => [125, 50, 50], yellow => [150, 150, 0 ]);
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

sub cull {
  my ($branch) = @_;
  return unless defined $branch;
  die "branch must be a vertex reference" unless ref($branch);
  my $parent = delete $$branch{parent};
  my ($dr, $dc) = ($$branch{row} - $$parent{row},
                   $$branch{column} - $$parent{column});
  my $self = delete $$parent{to}{$dr, $dc};
  $$branch{culled} = 1;
  my $tail = $tails{$$branch{row}, $$branch{column}, $dr, $dc};
  if (defined $tail) {
    for (reverse 0..$#$tail) {
      splice(@$tail, $_, 1, ()), last if ($$tail[$_] eq $branch);
    }
  }
  my $optimal = $optimal{$$branch{row}, $$branch{column}, $dr, $dc};
  if (defined $optimal) {
    for (reverse 0..$#$optimal) {
      splice(@$optimal, $_, 1, ()), last if ($$optimal[$_] eq $branch);
    }
  }
  cull($$branch{to}{$$_[0], $$_[1]}) for (north, south, east, west);
}

@lines = map { chomp; $_ } <>;
$width = length($lines[0]);
$height = @lines;

# tree data schema
# stretch
# distance
# row, column
# north, south, east, west

# global optimality index
# row, column, direction → parent reference (for culling)

# global tails index
# row, column, direction → parent reference (for culling)
%root = (stretch => 0,
         distance => 0,
         row => 0,
         column => 0);
$tails{0, 0, 0, 0} = [\%root]; # hydra tails…
$optimal{0, 0, $$_[0], $$_[1]} = [\%root] for (north, south, east, west);

sub formattail {
  my @steps;
  my ($tail) = @_;
  while ($tail) {
    push @steps, "[$$tail{row},$$tail{column}:S$$tail{stretch},D$$tail{distance}]";
    $tail = $$tail{parent};
  }
  return reverse @steps;
}

while (1) {
  my @cull;
  for (keys %tails) {
    my ($row, $column, @direction) = split $;;
    my $tails = $tails{$row, $column, $direction[0], $direction[1]};
    while (my $tail = shift @$tails) {
      if ($row == $height -1 and $column == $width - 1) {
        print "Not continuing terminal tail, @{[formattail $tail]}\n";
        next;
      }
      for my $nextdirection (north, south, east, west) {
        my ($dr, $dc) = @$nextdirection;
        my ($nextrow, $nextcolumn) = ($row + $dr, $column + $dc);
        unless (0 <= $nextrow < $height and 0 <= $nextcolumn < $width) {
          print ("$nextdirection ($nextrow $nextcolumn) is OOB\n");
          next;
        }
        my $nextstretch;
        my @opposite = (-$direction[0], -$direction[1]);
        if ("@opposite" eq "@$nextdirection") {
          print "Cannot go backwards\n";
          next;
        } else {
          print "(@opposite) /= (@$nextdirection)\n";
        }
        if ("@direction" eq "@$nextdirection") {
          $nextstretch = 1 + $$tail{stretch};
        } else {
          $nextstretch = 0;
        }
        if ($nextstretch >= 3) {
          print "ACK!\n";
          next;
        }
        my $nextdistance = $$tail{distance} + substr $lines[$nextrow], $nextcolumn, 1;
        my %child = (
                     stretch => $nextstretch,
                     distance => $nextdistance,
                     row => $nextrow,
                     column => $nextcolumn,
                     parent => $tail
                    );
        my $optimal = $optimal{$nextrow, $nextcolumn, $dr, $dc};
      
        if (defined $optimal and @$optimal != 0) {
          my $conflict;
          for my $enemy (reverse 0..$#$optimal) {
            if ($child{distance} <= $$optimal[$enemy]{distance} and
                $child{stretch} <= $$optimal[$enemy]{stretch}) {
              # we dont need to look for any more cases
              # …
              push @cull, $$optimal[$enemy];
              # splice @$optimal, $enemy, 1, (); # shouldnt be needed
              last;
            } elsif ($child{distance} >= $$optimal[$enemy]{distance} and
                     $child{stretch} >= $$optimal[$enemy]{stretch}) {
              # $DB::single = 2;
              print "My S$child{stretch},D$child{distance} will never beat S$$optimal[$enemy]{stretch},D$$optimal[$enemy]{distance} → $nextrow,$nextcolumn\n";
              # we can never beat the potential for more steps and less
              # or equal distance. We are not providing anything then.
              $conflict = 1;
              # last # test that this makes no difference
            }
          
            # If i have a higher stretch, but a lower distance, what do
            # you do? there is ambiguity now. I need to be able to run
            # multiple in parallell. I.e. allow inferior (disatance) if
            # they have better stretch.  we only need to collapse (cull)
            # here though.
            #
            # A tail with a low stretch can compete with any distance.
            #
            # Though a tail with a high stretch cannot eliminate a tail with
            # a low stretch.
            #
            # So sort by stretch… Actually, just replace (eliminate) as much
            # as you can.
          }

          if ($conflict) {
            print "\tNot adding due to conflict\n";
          } else {
            printf "Adding S$child{stretch},D$child{distance} after battle → %s\n", "@{[formattail \%child]}";
            $$tail{to}{$dr, $dc} = \%child;
            push @{$tails{$nextrow, $nextcolumn, $dr, $dc}}, \%child;
            push @{$optimal{$nextrow, $nextcolumn, $dr, $dc}}, \%child;
          }
        } else {
          printf "Adding S$child{stretch},D$child{distance} uncontested → %s\n", "@{[formattail \%child]}";
          $$tail{to}{$dr, $dc} = \%child;
          push @{$tails{$nextrow, $nextcolumn, $dr, $dc}}, \%child;
          push @{$optimal{$nextrow, $nextcolumn, $dr, $dc}}, \%child;
        }
      }
    }
  }

  while (my $cull = shift @cull) {
    cull $cull;
  }

  # print "Culled $m\n";
  
  $DB::single = 2;

  last if $m++ > 1000;
}

print "Results:\n";

for (east, south) {
  for (@{$optimal{$height-1,$width-1,$$_[0],$$_[1]}}) {
    print "\t $_\n";
  }
}

print "Done bro\n";

$DB::single = 2;

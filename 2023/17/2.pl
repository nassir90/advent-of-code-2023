use Data::Dumper;

local $Data::Dumper::Deepcopy = 1;

our $minimumblocksbeforeturn = 4;
our $maximumblocksbeforeturn = 10;

@north = (-1, 0), @west  = (0, -1), @south = (1, 0), @east  = (0, 1);

sub paint {
  my %colors = (red => [255,000,000], orange => [125, 50, 50], yellow => [150, 150, 0 ]);
  my ($color, $string, $start, $size) = @_;
  return $string unless $start < length($string);
  $size //= 1;
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
%root = (
         stretch => 0,
         distance => 0,
         row => 0,
         column => 0
        );
$tails{0, 0, 0, 0} = [\%root]; # hydra tails…
$optimal{0, 0, $$_[0], $$_[1]} = [\%root] for (north, south, east, west);

sub path {
  my ($tail) = @_;
  my @steps = ($tail);
  unshift @steps, $tail while ($tail = $$tail{parent});
  return @steps;
}

sub formattail {
  map { "[$$_{row},$$_{column}:S$$_{stretch},D$$_{distance}]" } path @_
}

branch: while (1) {
  my @cull;
  
  for (keys %tails) {
    my ($row, $column, @direction) = split $;;
    my $tails = $tails{$row, $column, $direction[0], $direction[1]};
    while (my $tail = shift @$tails) {
      # #print "Processing tail:\n";
      # realtruck map { [orange, $$_{row}, $$_{column}] } path $tail;
      if ($row == $height -1 and $column == $width - 1) {
        #print "Not continuing terminal tail, @{[formattail $tail]}\n";
        next;
      }
      for my $nextdirection (north, south, east, west) {
        my ($dr, $dc) = @$nextdirection;
        my ($nextrow, $nextcolumn) = ($row + $dr, $column + $dc);
        
        unless (0 <= $nextrow < $height and 0 <= $nextcolumn < $width) {
          #print ("$nextdirection ($nextrow $nextcolumn) is OOB\n");
          next;
        }
        
        my @opposite = (-$direction[0], -$direction[1]);
        if ("@opposite" eq "@$nextdirection") {
          #print "Cannot go backwards\n";
          next;
        }

        my $nextstretch;
        
        if ("@direction" eq "@$nextdirection") {
          $nextstretch = 1 + $$tail{stretch};
          if ($nextstretch > $maximumblocksbeforeturn) {
            #print "ACK!\n";
            next;
          }
        } else {
          if ($tail == \%root or $$tail{stretch} >= $minimumblocksbeforeturn) {
            $nextstretch = 1;
          } else {
            #print "ACK! Can't go from (@direction → @$nextdirection) as $$tail{stretch} <= $minimumblocksbeforeturn\n";
            next;
          }
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
        my $final = $nextrow == $height - 1 && $nextcolumn == $width - 1;
      
        if (defined $optimal and @$optimal != 0) {
          my $conflict;
          
          for my $enemy (reverse 0..$#$optimal) {
            if ($child{stretch} > $minimumblocksbeforeturn and $$optimal[$enemy]{stretch} > $minimumblocksbeforeturn) {
              if ($final) {
                if ($child{distance} <= $$optimal[$enemy]{distance}) {
                  push @cull, $$optimal[$enemy];
                  splice @$optimal, $enemy, 1, (); # shouldnt be needed
                } else {
                  $conflict = 1;
                  last;
                }
              } elsif ($child{stretch} < $$optimal[$enemy]{stretch} and $child{distance} <= $$optimal[$enemy]{distance}) {
                # pick the one with lower stretch to this point
                #print "We can both stop, and I prevail (S$child{stretch},D$child{distance} vs S$$optimal[$enemy]{stretch},D$$optimal[$enemy]{distance})\n";
                push @cull, $$optimal[$enemy];
                splice @$optimal, $enemy, 1, (); # shouldnt be needed
                # last;
              } else {
                #print "We can both stop, but we are equals (S$child{stretch},D$child{distance} vs S$$optimal[$enemy]{stretch},D$$optimal[$enemy]{distance})\n";
              }
            } elsif ($child{stretch} == $$optimal[$enemy]{stretch}) {
              if ($child{distance} < $$optimal[$enemy]{distance}) {
                #print "We have the same stretch, and I prevail (S$child{stretch},D$child{distance} vs S$$optimal[$enemy]{stretch},D$$optimal[$enemy]{distance})\n";
                push @cull, $$optimal[$enemy];
                splice @$optimal, $enemy, 1, (); # shouldnt be needed
                # last;
              } else {
                #print "We have the same stretch, but I must die (S$child{stretch},D$child{distance} vs S$$optimal[$enemy]{stretch},D$$optimal[$enemy]{distance})\n";
                $conflict = 1;
                # last;
              }
            }
          }

          if ($conflict) {
            #print "\tNot adding due to conflict\n";
          } else {
            #printf "Adding S$child{stretch},D$child{distance} after battle → %s\n", "@{[formattail \%child]}";
            $$tail{to}{$dr, $dc} = \%child;
            push @{$tails{$nextrow, $nextcolumn, $dr, $dc}}, \%child;
            push @{$optimal{$nextrow, $nextcolumn, $dr, $dc}}, \%child;
          }
        } else {
          #printf "Adding S$child{stretch},D$child{distance} uncontested → %s\n", "@{[formattail \%child]}";
          $$tail{to}{$dr, $dc} = \%child;
          push @{$tails{$nextrow, $nextcolumn, $dr, $dc}}, \%child;
          push @{$optimal{$nextrow, $nextcolumn, $dr, $dc}}, \%child;
        }
      }
    }
  }
      
  cull $cull while (my $cull = shift @cull);

  for (values %tails) {
    $m++, next branch if (@$_ != 0);
  }
  
  last;
}

printf "Results (at %d,%d), reached $m\n", $height-1, $width-1;

for (sort { $$b{distance} <=> $$a{distance} }
     @{$optimal{$height-1,$width-1,$east[0],$east[1]}},
     @{$optimal{$height-1,$width-1,$south[0],$south[1]}}) {
  next if $$_{stretch} < $minimumblocksbeforeturn;
  my @path = path $_;
  my @marks = map { [orange, $$_{row}, $$_{column}] } @path;
  print "\nDistance: $path[$#path]{distance}\n";
  realtruck @marks;
}

print "Done bro\n";

$DB::single = 2;

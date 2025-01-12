local $/ = /\n\n/;

sub paint {
    my %colors = (red => [255,000,000], orange => [125, 50, 50], yellow => [150, 150, 0 ]);
  my ($color, $string, $start, $size) = @_;
  $size = $size // 1;
  local $" = ";", $color = "@{$colors{$color}}";
  return $string =~ s/(?<=.{$start})(.{$size})/\e[48;2;${color}m$&\e[m/r;
}

sub transpose {
  my @lines = @_;
  my ($w, $h) = (length($lines[0]), scalar @lines);
  my @transposed;
  for my $columnnumber (0..$w-1) {
    my $column;
    $column .= substr $lines[$_], $columnnumber, 1 for (0..$#lines);
    push @transposed, $column;
  }
  return @transposed;
}

sub compare1 {
  my ($width, $height, $reversed, @lines) = @_;
  for my $axis (1..$height) {
    last if $axis*2 > $height;
    my @lo = @lines[0..$axis-1];
    my @hi = reverse @lines[$axis..$axis*2-1];
    my $lo, my $hi;
    { local $" = ""; ($lo, $hi) = ("@lo", "@hi") };
    if (@lo and @hi and $lo eq $hi) {
      return $reversed ? @lines - $axis : $axis;
    }
  }
}
sub compare2 {
  my ($width, $height, $reversed, @lines) = @_;
  for my $axis (1..$height) {
    last if $axis*2 > $height;
    my @lo = @lines[0..$axis-1];
    my @hi = reverse @lines[$axis..$axis*2-1];
    my $lo, my $hi;
    { local $" = ""; ($lo, $hi) = ("@lo", "@hi") };
    my $diffs = 0;
    my $diffrow = 0;
    my $diffcolumn = 0;
    my $c;
    die "length mismatch" unless length($lo) == length($hi);
            
    for (0..length($lo)) {
      $loc = substr $lo, $_, 1;
      $hic = substr $hi, $_, 1;
      if ($loc ne $hic) {
        $diffs++;
        last if ($diffs > 1);
        if ($bit eq "lo") {
          $diffrow = int($_ / $width);
          $diffcolumn = int($_ % $width);
          $c = $loc;
        } elsif ($bit eq "hi") {
          # The first block in @hi corresponds to the final block in
          # the rows we are considering. We just subtract the row it
          # was found at from the current row.
          $diffrow = $axis*2 - 1 - int($_/$width);
          $diffcolumn = $_ % $width;
          $c = $hic;
        } else {
          die "Unexpected valie '$bit'";
        }
      }
    }
            
    if ($diffs == 1) {
      my $rep = $c eq '#' ? '.' : '#';
      printf "Before: %s ($height, $width) ($c→$rep at [$diffrow, $diffcolumn])\n",
        paint "orange", $lines[$diffrow], $diffcolumn;
      $lines[$diffrow] =~ s/(?<=^.{$diffcolumn})./$rep/;
      printf "After:  %s\n",
        paint "yellow", $lines[$diffrow], $diffcolumn;
      my $delta = $reversed ? @lines - $axis : $axis;
      return [[@lines], $diffrow, $diffcolumn, $delta];
    }
  }
}

main: while (my $block = <>) {
  my @lines = split /\n/, $block;
  my $w = length($lines[0]);
  my $h = @lines;

  for $bit ("lo", "hi") {
    my @lines = @lines;
    my $delta = 0;
  
    print "$bit tops\n";
    $ret = compare2 $w, $h, 0, @lines;
    if (ref($ret)) {
      print "\tfound blemish ($h, $w)\n";
      @lines = @{$ret->[0]};
      ($row, $column) = ($ret->[1], $ret->[2]);
      $delta = $ret->[3] * 100;
      goto found;
    }
    
    print "$bit bottoms\n";
    $ret = compare2 $w, $h, 1, reverse @lines;
    if (ref($ret)) {
      print "\tfound blemish ($h, $w)\n";
      @lines = reverse @{$ret->[0]};
      ($row, $column) = ($h - $ret->[1] - 1, $ret->[2]);
      $delta = $ret->[3] * 100;
      goto found;
    }
    
    print "$bit lefts\n";
    $ret = compare2 $h, $w, 0, transpose @lines;
    if (ref($ret)) {
      print "\tfound blemish ($h, $w)\n";
      @lines = transpose @{$ret->[0]};
      ($row, $column) = ($ret->[2], $ret->[1]);
      $delta = $ret->[3];
      goto found;
    }
    
    print "$bit rights\n";
    $ret = compare2 $h, $w, 1, reverse transpose @lines;
    if (ref($ret)) {
      print "\tfound blemish ($h, $w)\n";
      @lines = transpose reverse @{$ret->[0]};
      ($row, $column) = ($ret->[2], $w - $ret->[1] - 1);
      $delta = $ret->[3];
      goto found;
    }

    die "No blemish found";
    
  found:
    
    print  "$bit after\n";
    {
      die "nodef" unless defined $row and defined $column;
      my @color = @lines;
      print "Changed $row, $column\n";
      $color[$row] =~ s/(?<=^.{$column})./\e[48;2;100;100;0m$&\e[m/;

      for (0..$#color) {
        $color[$_] = $lines[$_] . "|" .$color[$_];
      }
      
      local $" = "\n";
      print "@color\n";
    }

    print "\tDelta was $delta\n";
    
    $score += $delta;

    last;
    
    # Code for part 1
    
    # print "$bit tops\n";
    # my ($r) = compare1 $w, $h, 0, @lines;
    # print("\tadding $r (horizontal)\n"), $score += 100 * $r, next main if $r;
    # print "$bit bottoms\n";
    # ($r) = compare1 $w, $h, 1, reverse @lines;
    # print("\tadding $r (horizontal)\n"), $score += 100 * $r, next main if $r;
    # print "$bit lefts\n";
    # ($r) = compare1 $h, $w, 0, transpose @lines;
    # print("\tadding $r (vertical)\n"), $score += $r, next main if $r;
    # print "$bit rights\n";
    # ($r) = compare1 $h, $w, 1, reverse transpose @lines;
    # print("\tadding $r (vertical)\n"), $score += $r, next main if $r;
  }
}

END { print "$score\n" }

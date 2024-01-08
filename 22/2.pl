while (chomp($_ = <>)) {
  last unless /~/;
  push @blocks, [split /[,~]/];
}
sub simulate {
  my ($blocks) = @_;
  my $falls = 0;
  my $name = 0;
  my %bird;
  my %bases;
  
  for my $block (@$blocks) {
    if (!$second) {
      push @$block, ++$name;
    }
    my ($sx, $sy, $sz, $ex, $ey, $ez) = @$block;
    my $highest;
    my $length = $ex - $sx + $ey - $sy;
    my ($dy, $dx);
    $dx = 1 if $sx != $ex;
    $dy = 1 if $sy != $ey;
    die if $dx + $dy > 1;
    for (0..$length) {
      my ($y, $x) = ($sy + $dy * $_, $sx + $dx * $_);
      if ($bird{$y, $x}{height} > $highest) {
        $highest = $bird{$y, $x}{height};
      }
    }
    my %base = (height => $highest + $ez - $sz + 1,
                block => $block,
                name => $$block[6]);
    $bases{\%base} = \%base;
    for (0..$length) {
      my ($y, $x) = ($sy + $dy * $_, $sx + $dx * $_);
      if (exists $bird{$y, $x} and $bird{$y, $x}{height} == $highest) {
        $bird{$y, $x}{supporting}{\%base} = \%base;
        $base{supporters}{$bird{$y, $x}} = $bird{$y, $x};
      }
      $bird{$y, $x} = \%base;
    }
    
    if (!$second) {
      $$block[2] = $highest;
      $$block[5] = $highest+($ez-$sz);
    }
    
    my $fall = $highest != $sz ? 1 : 0;
    $falls += $fall;
    # print "$highest and $sz\n";

    # printf "--- x [$$block[0] → $$block[3]], y [$$block[1] → $$block[4]], z [$$block[2] → $$block[5]] (fall=$fall) [$$block[6]]\n";
    # my ($top) = sort { $$b{height} <=> $$a{height} } values %bird;
    # for $y (-1..10) {
    #   for $x (-1..10) {
    #     my $base = $bird{$y, $x};
    #     if (defined $base) {
    #       $red = 55 + int(200 * $$base{height}/$$top{height});
    #       printf "\e[48;2;$red;0;0m%4d\e[m", $$base{height};
    #     } else {
    #       print "    ";
    #     }
    #   }
    #   print "\n";
    # }
  }
  return (
          falls => $falls,
          bases => \%bases,
          bird => \%bird
         )
}

my %result = simulate [sort { $$a[2] <=> $$b[2] } @blocks];
my %bases = %{$result{bases}};

# regurgitate
sub regurgitate {
  my ($bases) = @_;
  for my $base (@$bases) {
    my $block = $$base{block};
    my ($sx, $sy, $sz, $ex, $ey, $ez) = @$block;
    print "$sx,$sy,$sz~$ex,$ey,$ez\n";
  }
}

$second = 1;

for (keys %bases) {
  my $block = $bases{$_};
  my @supporting = values %{$$block{supporting}};
  if (@supporting == 0) {

    my %clone = %bases;
    delete $clone{$_};
    die unless values(%clone) < values(%bases);
    @without =
      sort { $$a{block}[6] <=> $$b{block}[6] }
      values %clone;
    # regurgitate \@without;
    my %result = simulate [map { $$_{block} } @without];
    print "Falls (ZERO NOT SUPPORTING [$$block{block}[6]]): $result{falls}\n";
    $m++;
  } else {
    my (@conflict, @fine);
    for my $child (@supporting) {
      my @its_supporters = values(%{$$child{supporters}});
      my @its = map { $$child{name} } @its_supporters;
      if (@its_supporters < 2) {
        push @conflict, $$child{name};
      } else {
        push @fine, sprintf "[%s . %s]", $$child{name}, "@its";
      }
    }
    
    if (@conflict) {
      
      my %clone = %bases;
      delete $clone{$_};
      die unless values(%clone) < values(%bases);
      @without =
        sort { $$a{block}[6] <=> $$b{block}[6] }
        values %clone;
      # regurgitate \@without;
      my %result = simulate [map { $$_{block} } @without];
      print "Falls: $result{falls}\n";
      print "\t$$block{name} would drop @conflict (but keep @fine)\n";
    } else {
      
      my %clone = %bases;
      delete $clone{$_};
      die unless values(%clone) < values(%bases);
      @without =
        sort { $$a{block}[6] <=> $$b{block}[6] }
        values %clone;
      # regurgitate \@without;
      my %result = simulate [map { $$_{block} } @without];
      print "Falls (ZERO NO CONFLICT): $result{falls}\n";
      print "\t$$block{name} supports @fine\n";
      $m ++;
    }
  }
}

# print "Answer: $m\n";
# print "Falls: $result{falls}\n";

print "M: $m\n";

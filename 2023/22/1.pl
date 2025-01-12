@letters = (undef, A,B,C,D,E,F,G,H,I,J,K);
while (chomp($_ = <>)) {
  last unless /~/;
  push @blocks, [split /[,~]/];
}
sub sx (_) { $_[0][0] }
sub sy (_) { $_[0][1] }
sub sz (_) { $_[0][2] }
sub ex (_) { $_[0][3] }
sub ey (_) { $_[0][4] }
sub ez (_) { $_[0][5] }

# print "@{$blocks[0]}\n";
# printf "%d\n", sy $blocks[0];

@blocks = sort { sz($a) <=> sz($b) } @blocks;

sub simulate {
  my ($blocks) = @_;
  my $falls;
  my $name;
  my %bird;
  my %bases;
  
  for my $block (@$blocks) {
    push @$block, ++$name;
    my ($sx, $sy, $sz, $ex, $ey, $ez) = @$block;
    my $highest;
    my $length = $ex - $sx + $ey - $sy;
    my ($dy, $dx);
    $dx = 1 if $sx != $ex;
    $dy = 1 if $sy != $ey;
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
    
    $falls++ if $highest != $sz;
    # print "$highest and $sz\n";

    #my ($top) = sort { $$b{height} <=> $$a{height} } values %bird;
    #for $y (-1..10) {
    #  for $x (-1..10) {
    #    my $base = $bird{$y, $x};
    #    if (defined $base) {
    #      $red = 55 + int(200 * $$base{height}/$$top{height});
    #      printf "\e[48;2;$red;0;0m%4d\e[m", $$base{height};
    #    } else {
    #      print "    ";
    #    }
    #  }
    #  print "\n";
    #}
  }
  return (
          falls => $falls,
          bases => \%bases,
          bird => \%bird,
         )
}

%result = simulate \@blocks;
%bases = %{$result{bases}};

# regurgitate
sub regurgitate {
  my ($bases) = @_;
  for my $base (@$bases) {
    my $block = $$base{block};
    my ($sx, $sy, $sz, $ex, $ey, $ez) = @$block;
    print "$sx,$sy,$sz~$ex,$ey,$ez\n";
  }
}

# while (1) {
# my $found;
for (keys %bases) {
  my $block = $bases{$_};
  my @supporting = values %{$$block{supporting}};
  if (@supporting == 0) {
    
    # print "$$block{name} is free\n";
    # for my $supporter (@{$$block{supporters}}) {
    # printf "BEFORE: %d\n", scalar @{$$supporter{supporting}};
    # print @{$$supporter{supporting}};
    # print $block;
    # my $original = @{$$supporter{supporting}};
    # @{$$supporter{supporting}} = grep { $_ != $block } @{$$supporter{supporting}};
    # printf "AFTER: %d\n", scalar @{$$supporter{supporting}};
    # my $after = @{$$supporter{supporting}};
    # $found = 1 if $original != $after;
    # }
    # delete $bird{$_};

    $m++;
  } else {
    my (@conflict, @fine);
    for my $child (@supporting) {
      if (values(%{$$child{supporters}}) < 2) {
        push @conflict, $$child{name};
      } else {
        push @fine, $$child{name};
      }
    }
    if (@conflict) {
      
      # # print "$$block{name} would drop @conflict (but keep @fine)\n";
      # my %clone = %bases;
      # # printf "BEFORE %d\n", scalar values %clone;
      # delete %clone{$_};
      # # printf "AFTER %d\n", scalar values %clone;
      # $second = 1;
      # @without =
      #   sort { $$a{block}[2] <=> $$b{block}[5] }
      #   values %clone;
      # # regurgitate \@without;
      # my %result = simulate [map { $$_{block} } @without];
      # print "Falls: $result{falls}\n";
    } else {
      # print "$$block{name} supports @fine\n";
      $m ++;
    }
  }
}
#   last unless defined $found;
# }

print "Answer: $m\n";
print "Falls: $result{falls}\n";

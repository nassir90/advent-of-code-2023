# perl sins
#
# - endless cloning
# - criminal code duplication

use Data::Dumper;
use Clone qw/clone/;

# implement binary search later
%clauses = (A => [A], B => [B]);
while (chomp($_ = <>)) {
  last unless $_;
  my ($label, @clauses) = split /[},{]/;
  $clauses{$label} = \@clauses;
}

sub feed {
  my ($tree, @set) = @_;
  return unless @set;
  my ($start, $end) = @{shift @set};
  my @beloved;
  @{$$tree{subtrees}} = sort { $$a{start} <=> $$b{start} } @{$$tree{subtrees}};
  while (my ($index, $subtree) = each @{$$tree{subtrees}}) {
    # skip until there are some ranges after us
    next if $$subtree{end} < $start;
    # finish if there are no more ranges remaining
    last if $$subtree{start} > $end;
    # cull trailing start if any
    if ($end < $$subtree{start}) {
      my %clone = (start => $start, end => $end);
      feed(\%clone, @set);
      push @beloved, \%clone;
      $start = $end + 1;
      last;
    }
    my $endinsubtree = $end <= $$subtree{end} ? $end : $$subtree{end};
    if ($$subtree{start} <= $start <= $endinsubtree <= $$subtree{end}) {
      # compute pre section
      if ($$subtree{start} < $start) {
        my $clone = clone $subtree;
        $$clone{start} = $$subtree{start};
        $$clone{end} = $start - 1;
        push @beloved, $clone;
      }
      # compute post section
      if ($end < $$subtree{end}) {
        my $clone = clone $subtree;
        $$clone{start} = $endinsubtree + 1;
        $$clone{end} = $$subtree{end};
        push @beloved, $clone;
      }
      $$subtree{start} = $start;
      $$subtree{end} = $endinsubtree;
      feed($subtree, @set);
      # Advance start to after the end that we just considered.
      $start = $endinsubtree + 1;
    }
  }
  # sort out the rest
  if ($start <= $end) {
    my %clone = (start => $start, end => $end);
    feed(\%clone, @set);
    push @beloved, \%clone;
  }
  push @{$$tree{subtrees}}, @beloved;
}

sub convert {
  # set is an array of x, m, a and s ranges
  my ($rule, $at, $set, @chain) = @_;
  my $clause = $clauses{$rule}[$at];
  return unless $clause;
  print "${offset}Clause ($clause)\n";
  if ($clause eq "A") {
    print "${offset}Adding set\n";
    push @sets, [join(" → ", @chain), $set];
  } elsif ($clause eq "R") {
    print "${offset}Dropping set\n";
  } elsif ($clause =~ /(.)>(\d+):(.*)/) {
    my ($aspect, $number, $target) = ($1, $2, $3);
    {
      my $clause = "($rule) $aspect>$number";
      print "${offset}Posittive ($clause)\n";
      my $set = clone $set;
      # case where the aspect is GREATER
      $$set{$aspect}[0] = $number + 1 unless ($$set{$aspect}[0] > $number);
      local $offset = $offset . "    ";
      convert($target, 0, $set, @chain, $clause);
    } {
      my $clause = "($rule) $aspect<=$number";
      print "${offset}Negative ($clause)\n";
      my $set = clone $set;
      # case where the aspect is LESS THAN OR EQUAL
      $$set{$aspect}[1] = $number unless ($$set{$aspect}[1] <= $number);
      local $offset = $offset . "    ";
      convert($rule, $at + 1, $set, @chain, $clause);
    }
  } elsif ($clause =~ /(.)<(\d+):(.*)/) {
    my ($aspect, $number, $target) = ($1, $2, $3);
    {
      my $clause = "($rule) $aspect<$number";
      print "${offset}Positive ($clause)\n";
      my $set = clone $set;
      my ($aspect, $number, $target) = ($1, $2, $3);
      # case where the aspect is LESS
      $$set{$aspect}[1] = $number - 1 unless ($$set{$aspect}[1] < $number);
      local $offset = $offset . "    ";
      convert($target, 0, $set, @chain, $clause);
    } {
      my $clause = "($rule) $aspect>=$number";
      print "${offset}Negative ($clause)\n";
      my $set = clone $set;
      # case where the aspect is GREATER THAN OR EQUAL
      $$set{$aspect}[0] = $number unless ($$set{$aspect}[0] >= $number);
      local $offset = $offset . "    ";
      convert($rule, $at + 1, $set, @chain, $clause);
    }
  } else {
    my $set = clone $set;
    local $offset = $offset . "    ";
    convert($clause, 0, $set, @chain, $clause);
  }
}

convert in, 0, {x => [1,4000], m => [1,4000], a => [1,4000], s => [1,4000]};

@supersets = map { $_ = $$_[1]; [ $$_{x}, $$_{m}, $$_{a}, $$_{s} ] } @sets;

my %feed = (start => 1, end => 1);

for my $superset (@supersets) {
  printf "[%4d %4d] ", @$_ for @$superset;
  print "\n";
  feed \%feed, @$superset;
}

sub aggregate {
  my ($tree) = @_;
  my $size = $$tree{end} - $$tree{start} + 1;
  if ($$tree{subtrees}) {
    my $sum;
    for my $subtree (@{$$tree{subtrees}}) {
      $sum += $size * aggregate($subtree);
    }
    return $sum;
  } else {
    return $size;
  }
}

print aggregate \%feed;

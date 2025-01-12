use Data::Dumper;

while (chomp($_ = <>)) {
  last unless $_;
  my ($label, @clauses) = split /[},{]/, $_;
  print "For $label:\n";
  my @subs;
  # handle complex clauses
  for my $clause (@clauses[0..$#clauses-1]) {
    # if rule is complex
    $clause =~ /^(?<aspect>.)(?<operator>.)(?<number>\d+):(?<target>.*)$/;
    print "\t$clause\n";
    my %matches = %+;
    push @subs, sub {
      $matches{target} if eval "${$_[0]}{$matches{aspect}} $matches{operator} $matches{number}";
    };
  }
  # handle final clause
  print "\t$clauses[-1]\n";
  push @subs, sub { $clauses[-1] };
  push @rules, { label => $label, subs => \@subs };
  $rules{$label} = \@subs;
}

object: while (chomp($_ = <>)) {
  tr/{}//d;
  my %object = split /[=,]/;
  my $rules = $rules{in};
 feed: {
    for (@$rules) {
      my $target = $_->(\%object);
      next unless $target;
      if ($target =~ /^[AR]$/) {
        print "→ $target\n";
        if ($target eq "A") {
          $sum += $object{m} + $object{x} + $object{a} + $object{s};
        }
        next object;
      } else {
        print "falling through to $target\n";
        $rules = $rules{$target};
        redo feed;
      }
    }
  }
}

print "Answer: $sum\n";

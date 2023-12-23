use Data::Dumper;

while (chomp($_ = <>)) {
  my ($name, @outputs) = split / -> |, /;
  $name =~ tr/%&//d;
  ($modules{$name}{type}) = /^(%|&)/g;
  $modules{$name}{outputs} = \@outputs;
  push @{$modules{$_}{inputs}}, $name for (@outputs);
}

# print Dumper \%modules;

while (my ($name, $module) = each %modules) {
  $reverse{$name}{to} = $$module{inputs};
}

print Dumper \%reverse;

sub press {
  my @queue = ([undef, broadcaster, 0]);
  my $low = 1;
  my $high = 0;
  
  while (my ($source, $name, $pulse) = @{shift @queue}) {
    my $module = $modules{$name};
    my $output;

    if ($name eq "rx") {
      die if $pulse == 0;
    }
  
    if ("%" eq $$module{type}) {
      $output = ($$module{memory} ^= 1) if ($pulse == 0);
    } elsif ("&" eq $$module{type}) {
      $$module{memory}{$source} = $pulse;
      $output  |= !$$module{memory}{$_} for (@{$$module{inputs}});
      push @queue
    } else {
      $output = $pulse;
    }

    if (defined $output) {
      for my $destination (@{$$module{outputs}}) {
      $high++ if $output == 1;
      $low++  if $output == 0;
        # printf "$name -%s-> $destination\n", $output ? "high" : "low";
        push @queue, [$name, $destination, $output]
      }
    }
  }
  ($low, $high)
}

for (1..1_000_000) {
  my ($l, $h) = press;
  $low  += $l;
  $high += $h;
}
$product =  $low * $high;
print "sum: $product\n";

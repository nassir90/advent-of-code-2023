use Data::Dumper;

while (chomp($_ = <>)) {
  my ($name, @outputs) = split / -> |, /;
  $name =~ tr/%&//d;
  ($modules{$name}{type}) = /^(%|&)/g;
  $modules{$name}{outputs} = \@outputs;
  push @{$modules{$_}{inputs}}, $name for (@outputs);
}


# $name, $component + %marked, %graph, @$order
sub postorder {
  my ($name, $component) = @_;
  return if exists $marked{$name};
  $marked{$name} = $component;
  postorder($_, $component) for (@{$graph{$name}});
  push @$order, $name if defined $order;
}

# generate post order trace
{
  local $order = \@order;
  local %marked;
  local %graph = map { $_ => $modules{$_}{inputs} } keys %modules;
  postorder $_, ++$marker for (keys %graph);
}

# use reversed post order trace in dfs to find strong components
{
  local %marked;
  local %graph = map { $_ => $modules{$_}{outputs} } keys %modules;
  postorder $_, ++$marker for (reverse @order);
  while (my ($name, $component) = each %marked) {
    push @{$components{$component}}, $name;
  }
}

# print Dumper \%components;

$buffer = $modules{rx}{inputs}[0];
print "Buffer is $buffer\n";
$outlet{$_} = 1 for @{$modules{$buffer}{inputs}};
# print "@{[keys %outlet]}";
$inlet{$_} = 1 for @{$modules{broadcaster}{outputs}};

for (values %components) {
  next if (@$_ == 1);
  # print Dumper $_;
  my ($inlet, $outlet);
  for (@$_) {
    $inlet = $_ if exists $inlet{$_};
    $outlet = $_ if grep { exists $outlet{$_} } @{$modules{$_}{outputs}};
  }
  print "inlet: $inlet, outlet: $outlet\n";
}


print Dumper \%reverse;

sub output {
  my ($module) = @_;
  if ("%" eq $$module{type}) {
    $$module{memory} // 0
  } elsif ("&" eq $$module{type}) {
    my $output = 0;
    $output |= !$$module{memory}{$_} for (@{$$module{inputs}});
    $output
  }
}
sub base {
  my ($module) = @_;
  return 0 == output $module if "%" eq $$module{type};
  return 1 == output $module if "&" eq $$module{type};
}

sub press {
  my ($start) = @_;
  my @queue = ([undef, $start, 0, $generation]);
  my $low = 1;
  my $high = 0;
  
  while (my ($source, $name, $pulse, $g) = @{shift @queue}) {
    $generation = $g;
    my $module = $modules{$name};
    my $output;
    
    if ("%" eq $$module{type}) {
      $output = ($$module{memory} ^= 1) if ($pulse == 0);

      if (defined $output) {
      my $a = $output;
      my $b = output $module;
      die "$a and $b" unless $a == $b;
    }
    } elsif ("&" eq $$module{type}) {
      $$module{memory}{$source} = $pulse;
      $output  |= !$$module{memory}{$_} for (@{$$module{inputs}});
      push @queue
    } else {
      $output = $pulse;
    }

    if (defined $output) {
      if ($name eq "lg") {
        print "source=$source press=$press generation=$generation pulse=$pulse\n";
      }
      
      for my $destination (@{$$module{outputs}}) {
      $high++ if $output == 1;
      $low++  if $output == 0;
        # printf "$name -%s-> $destination\n", $output ? "high" : "low";
        push @queue, [$name, $destination, $output, $g+1]
      }
    }
  }
  ($low, $high)
}

local $generation = 0;
for $press (1..50_000) {
  my ($l, $h) = press broadcaster;
  $low  += $l;
  $high += $h;
  print "\n";
}
$product =  $low * $high;
print "sum: $product\n";

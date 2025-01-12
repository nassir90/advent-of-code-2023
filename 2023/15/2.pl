#!/perl -ln
BEGIN {
  $/ = ","
}
{ local $/ = "\n"; chomp }

printf "%-8s: ", $_;

my ($label) = /(.*)[=-]/g;
my $box;
$box = ($box + ord) * 17 % 256 for (split //, $label);
my $existinglenses = $boxes[$box];
my $found;

if (/=(.*)/) {
  my $focal = $1;
  for (0..$#$existinglenses) {
    my $lens = @$existinglenses[$_];
    if ($label eq %$lens{label}) {
      print "Changing '$label' in '$box' from %$lens{focal} → $focal";
      @$existinglenses[$_]->{focal} = $focal;
      $found = 1, last;
    }
  }
  unless ($found) {
    print "Did not find '$label' ($focal) in '$box'";
    push @{$boxes[$box]}, { label => $label, focal => $focal };
  }
} elsif (/-/) {
  for (0..$#$existinglenses) {
    if ($label eq @$existinglenses[$_]->{label}) {
      print "Found '$label' in '$box'. Removing";
      splice @$existinglenses, $_, 1, ();
      $found = 1, last;
    }
  }
  unless ($found) {
    print "Did not find '$label' in $box'";
  }
}

@format = map { join " ", map { "[$_->{label} $_->{focal}]" } @$_ } @boxes;
for (0..$#format) {
  print "  Box $_: $format[$_]" if $format[$_];
}
  
END {
  my $s = 0;
  for my $box (0..$#boxes) {
    my @lenses = @{$boxes[$box]};
    for my $lens (0..$#lenses) {
      $s += (1 + $box) * (1 + $lens) * ($lenses[$lens]->{focal});
    }
  }
  print "$s\n";
}

# $text = $ARGV[0];
$blocksize = 1;
$lit = '#' x $blocksize;
$rightspacing = 1;
$text = ".???#???...";
$text =~ /^\.*(?<full>(?<qspace>[?]*)(?<hash>#+)?(?<posthash>[?#]*))(?<final>\.|$)?/;
my ($full, $qspace, $hash, $posthash, $final) = @+{full, qspace, hash, posthash, final};
my $qspacelength = length($qspace);
my $posthashlength = length($posthash);
print "Full is $full\n";
print "Hash is $hash\n";
print "Qspace is $qspace\n";
print "Posthash is $posthash\n";

if ($hash) {
    my $nomore = 0;
    if ($qspacelength + 1 <= $blocksize) {
        print "'$lit' can go from qspace through hash ($qspace$hash$posthash). We could keep on going…\n";
        $nomore = $posthashlength + 1 >= $blocksize;
        $positions = ($qspacelength + $posthashlength + 1) - $blocksize;
    } else {
        print "But we have to discard this as it would leave hashes in the body\n";
        $positions = 1;
        $nomore = 1;
    }
}

if ($qspacelength >= $blocksize + $rightspacing) {
    $times = $qspacelength - $rightspacing;
    print "Could fit '$lit' $times times in $qspace\n";
} else {
    print "Could not fit '$lit' $times in $qspace\n";
    if ($hash) {
        print "We have to start at the hash\n";
    }
}

'...???#???...';
'...???########...';

'...???#?#?#?#?...';
'....###?#?#?#?...';
'......###?#?#?...';

# maybe the best approach is to not try and simplify #es at all only
# do enumeration with all question marks, beyond that, use regeex.

# if next block has question marks > blocksize + padding
#    enumerate pure qspace matchings
#    move to after the last qspace matching + rightspace
# do we match [?.]#{blocksize}[?.]
#    increment and advance
#    is the first element the hash?
#    -> finish

'..???#???... 2';

print "\n---\n\n";

# dejavu

my $texto = "..?..";

my $offset = 0;
my $cells = 10;
my $positions = 0;
my $blocksize = 3;

while ($offset < length($texto)) {
  $texto =~ /^.{$offset}(?:(?<dspace>\.+)|(?<qspace>\?{$blocksize}\?{$rightspacing,})|(?<normal>[#?]{$blocksize}(?:$|[?.])))/;
  my $suffix = substr $texto, $offset;
  my $assumption = $suffix =~ s/^(.{$blocksize}.{$rightspacing})./$1!/r;
  print "\nSuffix: $assumption ($suffix)\n";
  if ($+{dspace}) {
    print "Found dspace '$+{dspace}'\n";
    $offset += length($+{dspace});
  } elsif ($+{qspace}) {
    my $qspace = $+{qspace};
    my $delta = length($qspace) - $blocksize;
    print "Can fit '$lit' $delta times in '$qspace' ($-[0])\n";
    $offset += $delta + $rightspacing;
    $positions += $delta;
  } elsif ($+{normal}) {
    print "Can fit '$lit' 1 time in '$suffix' (slow)\n";
    $positions += 1;
    if ($+{normal} =~ /^#/) {
      print "Going to finish...\n";
      last;
    } else {
      $offset ++;
    }
  } else {
    print "No match '$lit' in '$suffix'\n";
    $offset ++;
  }
}

print "$positions\n";

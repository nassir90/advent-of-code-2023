#!/usr/bin/perl -F'\s' -nl
sub pattern  { '^' . ($_[0] =~ s/\./\\./gr =~ s/\?/[#.]/gr) . '$' }
sub blocksum { my $blocksum; $blocksum += $_ for @_; return $blocksum }

$original = $F[0];
@blocks = split /,/, $F[1];
# print "$original\n";

@patterns = map { pattern substr $original, 0, $_ } 0..length($original);
@suffixes = map { substr $original, $_ } 0..length($original);
@hashes   = map { scalar (() = /#/g) } @suffixes;
# print "@hashes\n";
# exit;
sub gen {
  my ($string, @blocks) = @_;
  # print "HERE1: $string\n";
  if (@blocks == 0) {
    # print "\tMade: $string\n";
    $m++ if $hashes[length($string)] == 0; # if $string =~ $patterns[-1];
    return 1;
  } elsif (length($string) <= length($original)) {
    my $pattern = $patterns[length($string)];
    if ($string !~ $pattern) {
      my $l = length($string);
      my $z = substr($original, 0, length($string));
      my $x = pattern($z);
      # print "'$string' ($l) does not match '$pattern' ($z → $x)\n";
      return 0;
    }
    my $spacing = @blocks - 1;
    my $blocksum = blocksum @blocks;
    my $remaining = length($original) - length($string);
    my $needed = $blocksum + $spacing;
    # print "With spacing $spacing and blocksum $blocksum (@blocks)\n";
    if ($remaining >= $needed) {
      my $s = $suffixes[length($string)];
      # print "HERE: $string (suffix $s)\n";
      my $s = 0;
      $s += gen($string.".", @blocks)                 if $suffixes[length($string)] =~ /^[?.]/;
      my $block = shift @blocks;
      $s += gen($string.("#" x  $block).('.' x length($1)), @blocks) if $suffixes[length($string)] =~ /^[#?]{$block}($|[?.])/;
      return $s;
    } else {
      # print "ERR1: $remaining is not >= $needed\n";
    }
  } else {
    # print "'$string' does not match '$pattern'\n";
  }
}

gen("", @blocks);

END { print "$m\n" };

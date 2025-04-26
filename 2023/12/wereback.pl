#!/usr/bin/perl -F'\s' -nl
BEGIN {
  $reps = $ENV{REPS} // 2;

  %colors = (red => [255,000,000],
             orange => [125, 50, 50],
             yellow => [150, 150, 0 ]);

  sub paint {
    my ($color, $string, $start, $size) = @_;
    return $string unless $start < length($string);
    $size = $size // 1;
    local $" = ";", $color = "@{$colors{$color}}";
    substr $string, $start, $size, "\e[48;2;${color}m" . substr($string, $start, $size) . "\e[m";
    return $string;
  }
}

$patternstring = $F[0]; $patternstring .= "?$F[0]" for (1..$reps-1);
$blockstring = $F[1]; $blockstring .= ",$F[1]" for (1..$reps-1);
@blocks = split /,/, $blockstring;
@suffixes = map { substr $patternstring, $_ } 0..length($patternstring);
%combinations = (); # block index + start location  → number of lower combinations

sub gen {
  my ($block, $start) = @_;
  my $blocksize = $blocks[$block];
  printf "${offset}To fit $blocksize at '%s' ($start)\n", paint("orange", $patternstring, $start, $blocksize);
  if (exists $combinations{$block,$start}) {
    print "${offset}Using cahced combinations ($block,$start) of $combinations{$block,$start}";
  } else {
    my $sum = 0;
    my $expect = $start == 0 ? 0 : 1;
    my $suffix = $start - $expect;

    if ($block == $#blocks) {
      
      # we need a trailer with no hashes
      if ($patternstring =~ /^.{$suffix}[?.]{$expect}([?#]{$blocksize})[?.]*$/n) {
        printf "${offset}Base found '%s' for ($blocksize) '%s' \n", paint("orange", $patternstring, $start, $blocksize), $suffixes[$start-$expect];
        $sum += 1;
      }
    } else {
      # recursive case doesnt care about the trailer really
      if ($patternstring =~ /^.{$suffix}[?.]{$expect}([?#]{$blocksize})(?:$|[?.])/n) {
        printf "${offset}Recursive found '%s' for ($block) '%s' \n", paint("orange", $patternstring, $start, $blocksize), $suffixes[$start-$expect];

        # just ask the next guy whats the story without QA
        local $offset = $offset . "    ";
        $sum += gen($block+1, $start + $blocksize + 1);
      }
    }

    # base case recurses into itself until it exhausts the string
    if ($start + 1 < length($patternstring) and $patternstring !~ /^.{$start}#/) {
      local $offset = $offset . "    ";
      $sum += gen($block, $start + 1);
    }
    
    printf "${offset}Lo, fit $blocksize at '%s' ($start) is $sum\n", paint("orange", $patternstring, $start, $blocksize);
    
    $combinations{$block, $start} = $sum;
  }
  return $combinations{$block,$start};
}

$g = gen 0, 0;
$ss += $g;

print "$F[0] $F[1] (x$reps) → $g";

END {
  print $ss
}

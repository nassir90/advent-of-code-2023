#!/usr/bin/perl -F'\s' -nl
use warnings;

BEGIN {
  %colors = (
             red => [255,000,000],
             orange => [125, 50, 50],
             yellow => [150, 150, 0 ]
            );
}

sub paint {
  my ($color, $string, $start, $size) = @_;
  $size = $size // 1;
  local $" = ";", $color = "@{$colors{$color}}";
  return $string =~ s/(?<=.{$start})(.{$size})/\e[48;2;${color}m$&\e[m/r;
  return $string;
}

my @Z = @F;
my $reps = 1;
$Z[0] .= "?".$F[0], $Z[1] .= ",".$F[1] for (1..$reps-1);

$original = "$Z[0]";
$ogname = $F[0];
$ogblocks = $F[1];
@blocks = split /,/, "$Z[1]";
@suffixes = map { substr $original, $_ } 0..length($original);
@hashes   = map { scalar (() = /#/g) } @suffixes;
@absoluteblocks   = map { scalar (() = /[#?]*?#[#?]*/g) } @suffixes;
# local @prefixes = map { substr $original, 0, $_ } 0..length($original);
@blocksums = map { my $blocksum; $blocksum += $_ for @blocks[$_..$#blocks]; $blocksum } 0..$#blocks;
local $offset = "";

sub gen {
  my ($block, $start) = @_;
  print $offset, "Starting with $block";
  my $final = $block == $#blocks;
  print $offset, "Master final ($block == $#blocks)" if $final;

  my $allpositions = 0;
  my $rightspacing = $final ? 0 : 1;

  my $suffix = $suffixes[$start];
  my $blocksize = $blocks[$block];
  my $lit = ('#' x $blocksize) . ('.' x $rightspacing);

  # return if we cant satisfy the blocksums
  my $suffixlength = length($suffix);
  if ($suffixlength < $blocksums[$block]) {
    print "Need '@blocks[$block..$#blocks]' but $suffixlength characters remaining in $suffix";
    return 0;
  }

  my $blocksremaining = @blocks - $block;
  my $minimumblocks = $absoluteblocks[$start];
  if ($blocksremaining < $minimumblocks) {
    print $offset, "With ($suffix), need at least $minimumblocks blocks but have $blocksremaining";
    return 0;
  }

  # Indicates that at most #{$blocksize} may be present and that
  # there will be a single dot. proceed by induction to other cases
  die "blocksize undefined ($block of $#blocks)" unless defined $blocksize;
  die "no suffix" unless defined $suffix;
  die "spacing undefined" unless defined $rightspacing;

  # Rolling number of positions in the current cell-space.
  my $positions = 0;
  my $sawhashalready = 0;
  my $cells = $blocksize + $rightspacing;
  while ($cells <= length($suffix)) {

    # cells is just the block of characters from $start to the next
    # block. the final cell always reserved for space, you can see
    # that in the !$final block, we make sure of this.
    my $usablecells = $cells - $rightspacing;
    
    print $offset, "Master cells $cells";
    
    my $nextblock = $block+1;
    my $nextfinal = $nextblock == $#blocks;
    my $nextstart = $start+$cells;
    my $nextrightspacing = $nextfinal ? 0 : 1;
    my $nextfull;
    my $nextblocksize;
    my $nextlit;
    
    if (!$final) {
      my $beforenextstart = $nextstart - 1;
      my $beforenextsuffix = $suffixes[$beforenextstart];
      $nextblocksize = $blocks[$nextblock];
      $nextlit = ('#' x $nextblocksize) . ('.' x $nextrightspacing);
      $nextfull = paint "red", $original, $nextstart, $nextblocksize;
      if ($beforenextsuffix =~ /^([?.])([#?]{$nextblocksize})($|[?.])/) {
        print $offset, "Slave will fit '$nextlit' in '$nextfull'";
      } else {
        print $offset, "Slave will NOT fit '$nextlit' in '$nextfull'";
        next;
      }
    }

    next if $final and $cells != length($suffix);
    
    # we take the cell that must be used for the ., and we take away
    # our block size to get a range of usable cell offsets. $end
    # represents the last place the current block could be given our
    # cell layout.  relative start location for pattern
    # my $end = $cells-$blocksize-$rightspacing;
    # my $prefix = substr $suffix, $end;
    $positions = 0;
    
    #  for $prefixstart (0..$cells-$blocksize-$rightspacing)  {
    #    my $prefix = substr $suffix, $prefixstart;
    #    my $lebensaurum = $cells-$blocksize-$rightspacing-$prefixstart;
    #    if ($prefix =~ /^([#?]{$blocksize})([?.]{$lebensaurum})($|[?.])/) {
    #      $positions++;
    #      print $offset, "Master can fit '$lit' in '$prefix' ($positions)";
    #    }
    #    if ($prefix =~ /^#/) {
    #      print $offset, "Finishing (Cannot leave extra '#')";
    #      $sawhashalready = 1;
    #      last;
    #    }
    #  }

    my $cell = 0;
    my $celllimit = $usablecells-$blocksize;
    while ($cell <= $celllimit) {
      
      # print "---\nOriginal is $original";
      # print "Cell is $cell";
      # print "Blocksize is $blocksize";
      # print "Spacing is $rightspacing\n---\n";

      my $blankspace = $usablecells - $blocksize - $cell;
      my $maximumqspace = $usablecells - $cell;
      my $minimumqspace = $blocksize;
      my $suffix = $suffixes[$start + $cell];
      my $nextoffset = $cells - $cell;
      
      # print $offset,  "$nextoffset (USABLE CELLS $usablecells, CELL $cell, LIMIT $celllimit)";
      my $assumption = paint "red",    $original,   $nextstart,   $nextblocksize;
      $assumption    = paint "orange", $assumption, $start+$cell, $maximumqspace;
    
      print $offset, "Suffix: $assumption ($suffix)";
      
      if ($suffix =~ /^(?<dspace>\.+)/) {
        
        print $offset, "Found dspace '$+{dspace}'";
        $cell += length($+{dspace});
        
      } elsif ($suffix =~ /^(?<qspace>\?{$minimumqspace,$maximumqspace})/) {
        
        my $qspace = $+{qspace};
        my $qspacelength = length($qspace);
        my $delta = 1 + $qspacelength - $blocksize;
        print $offset, "Can fit '$lit' $delta times in '$qspace' ($-[0])";
        my $hash = $suffix =~ /^.{,$maximumqspace}#/;
        if ($hash) {
          my $l = length($&);
          die "matched too much ($l)" if $l > $cells;
          print $offset, "- Ignoring ?-space as adjacent # ($&)";
        } else {
          $positions += $delta;
        }
        $cell += $delta;
      } elsif ($suffix =~ /^(?<normal>[#?]{$blocksize}[?.]{$blankspace})/) {
        print $offset, "Can fit '$lit' 1 time in '$suffix' (slow)";
        $positions += 1;
        $cell++;
      } else {
        my $tried = $lit . ('.' x $blankspace);
        print $offset, "No match for '$tried'";
        $cell++;
      }
      if ($suffix =~ /^#/) {
        print $offset, "Going to finish...";
        $sawhashalready = 1;
        last;
      }
    }
    
    # print $offset, "Cell reached $cell (of $celllimit), done scan";

    if ($positions == 0) {
      my $prefix = substr $suffix, $cells-$blocksize-$rightspacing;
      print $offset, "Cannot fit '$lit' in '$prefix' of '$suffix' ($sawhashalready))";
      if ($sawhashalready) {
        print $offset, "Positions never increase if hash seen, leaving";
        last
      } else {
        next
      }
    }
    
    if ($final) {
      my $hashes = $hashes[$start+$blocksize];
      my $absolute = $absoluteblocks[$start];
      my $wantnohash = $start+$cell-1+$blocksize;
      if ($sawhashalready and $absoluteblocks[$wantnohash] != 0) {
        print $offset, "Saw hash at ", paint("red", $original, $start+$cell-1);
        print $offset, "New hash at ", paint("red", $original, $wantnohash);
      } elsif ($absolute <= 1) {
        print $offset, "Master final (hashes $absolute) $allpositions (NEW $positions)";
        $allpositions += $positions;
      } else {
        print $offset, "INVALID Master final (hashes $absolute) $allpositions (NEW $positions)";
      }
    } else {
      my $nextblocksize = $blocks[$nextblock];
      my $nextsuffix = $suffixes[$nextstart];
      my $nextlit = ('#' x $nextblocksize) . ('.' x $nextrightspacing);

      # Dont bother continuing in this case
      my $nextsuffixlength = length($nextsuffix);
      if ($nextsuffixlength < $blocksums[$nextblock]) {
        print $offset, "Need '@blocks[$nextblock..$#blocks]' but $nextsuffixlength characters remaining in '$nextsuffix'";
        last;
      }
      if ($nextfinal) {
        if ($nextsuffix =~ /^([#?]{$nextblocksize})[?.]*$/) {
          print $offset, "- Slave final can fit '$nextlit' in '$nextsuffix' (NEW $positions)";
          $allpositions += $positions;
        } else {
          print $offset, "- Slave final CANNOT fit '$nextlit' in '$nextsuffix'";
        }
      } else {
        if ($sawhashalready) {
          print $offset, "Master limit reached. Liberating slave…";
          local $offset = $offset . "    ";
          $allpositions += $positions * gen($nextblock, $nextstart);
          last;
        }
        
        # print $offset, "Hmm nb is $nextblock";
        die "nextblocksize is undefined (block $block of $#blocks)" unless defined $nextblocksize;
        # if ($nextsuffix !~ /^([#?]{$nextblocksize})($|[?.])/) {
        #   print $offset, "- Slave cannot fit '$nextlit' in '$nextsuffix'";
        # } else {
        # print $offset, "- Slave can fit '$nextlit' in '$nextsuffix'";
        print $offset, "Placing down slave ($nextfull)";
      
        my $nextnextblock = $nextblock+1;
        my $nextnextblocksize = $blocks[$nextnextblock];
        my $nextnextstart = $start+$cells+$nextblocksize+$rightspacing;
        my $nextnextsuffix = $suffixes[$nextnextstart];
      
        if ($nextnextstart >= length($original) and $nextnextblocksize > 0) {
          last;
        }

        die "suffix length errror" if (length($nextnextsuffix) >= length($nextsuffix));
      
        printf "${offset}My father had '%s'\n",                                  paint("yellow", $original, 0, $start);
        printf "${offset}His son had   '%s' (post $blocksize)\n",                paint("yellow", $original, 0, $start+$cells);
        printf "${offset}And I'll have '%s' (post $blocksize,$nextblocksize)\n", paint("yellow", $original, 0, $nextnextstart);
        my $nextpositions;
        {
          local $offset = $offset . "    ";
          $nextpositions = gen($nextblock+1, $nextnextstart)
        }
        ;
        my $delta = $positions * $nextpositions;
        print $offset, "(NEW $positions * $nextpositions = $delta)";
        $allpositions += $delta;
        # }
      }

      # new position counting makes this redundant
      # if ($nextsuffix =~ /^#+/) {
      #   print $offset, "Slave OUGHT to finish up '$nextlit' ($nextsuffix)";
      #   # I think that if I just advance the cells (padding) to the
      #   # correct next valid block place we should be gucci
      #   my $advance = length($&);
      #   print $offset, "Going to advance by $advance instead";
      #   $cells += length($advance);
      # }
    }
  } continue {
    $cells++;
  }

  return $allpositions;
}

my $g = gen(0,0);

$ss += $g;

print "$ogname $ogblocks → $g";

END {
  print $ss;
}

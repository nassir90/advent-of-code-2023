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
# @hashes   = map { scalar (() = /#/g) } @suffixes;
@absoluteblocks   = map { scalar (() = /[#?]*?#[#?]*/g) } @suffixes;
@blocksums = map { my $blocksum; $blocksum += $_ for @blocks[$_..$#blocks]; $blocksum } 0..$#blocks;

local $offset = "";

sub gen {
  my ($block, $start) = @_;
  
  #print $offset, "Starting with $block";
  
  my $final = $block == $#blocks;
  #print $offset, "Main final ($block == $#blocks)" if $final;

  my $allpositions = 0;
  
  my $rightspacing = $final ? 0 : 1;
  my $suffix = $suffixes[$start];
  my $blocksize = $blocks[$block];
  my $lit = ('#' x $blocksize) . ('.' x $rightspacing);

  # return if we cant satisfy the blocksums
  my $suffixlength = length($suffix);
  
  if ($suffixlength < $blocksums[$block]) {
    #print $offset, "Need '@blocks[$block..$#blocks]' but $suffixlength characters remaining in $suffix";
    return 0;
  }

  my $blocksremaining = @blocks - $block;
  my $minimumblocks = $absoluteblocks[$start];
  if ($blocksremaining < $minimumblocks) {
    #print $offset, "With ($suffix), need at least $minimumblocks blocks but have $blocksremaining";
    return 0;
  }

  # Indicates that at most #{$blocksize} may be present and that there
  # will be a single dot. proceed by induction to other cases
  die "blocksize undefined ($block of $#blocks)" unless defined $blocksize;
  die "no suffix" unless defined $suffix;
  die "spacing undefined" unless defined $rightspacing;

  # Rolling number of positions in the current cell-space.
  my $sawhashalready = 0;
  my $cells = $blocksize + $rightspacing;
  my $zero = 0;
  
  while ($cells <= length($suffix)) {
    #print $offset, "Main cells $cells";
    
    my $nextblock = $block+1;
    my $nextfinal = $nextblock == $#blocks;
    my $nextstart = $start+$cells;
    my $nextrightspacing = $nextfinal ? 0 : 1;
    my $nextfull;
    my $nextblocksize;
    my $nextlit;
    my $nextsuffix;
    my $nextsuffixlength;
    
    if (!$final) {
      my $beforenextstart = $nextstart - 1;
      my $beforenextsuffix = $suffixes[$beforenextstart];
      
      $nextblocksize = $blocks[$nextblock];
      $nextlit = ('#' x $nextblocksize) . ('.' x $nextrightspacing);
      $nextfull = paint "red", $original, $nextstart, $nextblocksize;
      $nextsuffix = $suffixes[$nextstart];
      $nextsuffixlength = length($nextsuffix);

      if ($nextsuffixlength < $blocksums[$nextblock]) {
        #print $offset, "Need '@blocks[$nextblock..$#blocks]' but $nextsuffixlength characters remaining in '$nextsuffix'";
        last;
      }
      
      if (!$nextfinal and length($original)-$nextstart-$nextblocksize-$nextrightspacing < $blocksums[$nextblock+1]) {
        #print $offset, "Gonna do what's called a pro-gamer move and leave";
        last;
      }
      
      if ($beforenextsuffix =~ /([?.])([#?]{$nextblocksize})($|[?.])/) {
        if ($-[0] != 0) {
          #print $offset, "Secondary will NOT fit '$nextlit' in '$nextfull'";
          $cells += $-[0] - 1;
          next;
        }
        #print $offset, "Secondary will     fit '$nextlit' in '$nextfull'";
      } else {
        #print $offset, "Secondary will NOT fit '$nextlit' in '$nextfull'";
        next;
      }
    } else {
      $cells = length($suffix);
      #print $offset, "Skipping to final cell ($cells)";
    }

    # cells is just the block of characters from $start to the next
    # block. the final cell always reserved for space, you can see
    # that in the !$final block, we make sure of this.
    my $usablecells = $cells - $rightspacing;
    my $positions = 0;
    my $cell = 0;
    my $celllimit = $usablecells - $blocksize;
    
    while ($cell <= $celllimit) {
      my $maximumqspace = $usablecells - $cell;
      my $minimumqspace = $blocksize;
      my $suffix = $suffixes[$start + $cell];
      my $prefix = substr $suffix, 0, $usablecells - $cell;
      my $nextoffset = $cells - $cell;
      
      # #print $offset,  "$nextoffset (USABLE CELLS $usablecells, CELL $cell, LIMIT $celllimit)";
      my $assumption = paint "red",    $original,   $nextstart,   $nextblocksize;
      $assumption    = paint "orange", $assumption, $start+$cell, $maximumqspace;
    
      #print $offset, "Suffix: $assumption ($suffix)";
      
      if ($prefix =~ /^\.+/n) {
        
        #print $offset, "Found dspace '$&'";
        $cell += length($&);
        
      } elsif ($prefix =~ /^(\?{$minimumqspace,})[^#]*(#)?/) {
        
        my $qspace = $1;
        my $qspacelength = length($qspace);
        my $delta = 1 + $qspacelength - $blocksize;
        
        #print $offset, "Can fit '$lit' $delta times in '$qspace' ($-[0])";
        if ($2) {
          #print $offset, "- Ignoring ?-space as adjacent # ($&)";
        } else {
          $positions += $delta;
        }
        $cell += $delta;
      } elsif ($prefix =~ /^[#?]{$blocksize}[.?]*$/n) {
        #print $offset, "Can fit '$lit' 1 time in '$suffix' (slow)";
        $positions += 1;
        $cell += 1;
      } else {
        my $tried = $lit . ('.' x ($usablecells - $blocksize));
        #print $offset, "No match for '$tried'";
        $cell++;
      }
      if ($prefix =~ /^#/n) {
        #print $offset, "Going to finish...";
        $sawhashalready = 1;
        last;
      }
    }
    
    # #print $offset, "Cell reached $cell (of $celllimit), done scan";

    if ($positions == 0) {
      my $prefix = paint "orange", $suffix, 0, $cells;
      #print $offset, "Cannot fit '$lit' in '$prefix' of '$suffix' ($sawhashalready))";
      if ($sawhashalready) {
        #print $offset, "Positions never increase if hash seen, leaving";
        last;
      } else {
        next
      }
    }
    
    if ($final) {
      #print $offset, "(NEW $positions!)";
      $allpositions += $positions;
    } else {
      if ($nextfinal) {
        if ($nextsuffix =~ /^([#?]{$nextblocksize})[?.]*$/) {
          #print $offset, "- Secondary final can fit '$nextlit' in '$nextsuffix' (NEW $positions)";
          $allpositions += $positions;
          #print $offset, "(NEW next final $positions)";
        } else {
          #print $offset, "- Secondary final CANNOT fit '$nextlit' in '$nextsuffix'";
        }
      } else {
        if ($sawhashalready) {
          #print $offset, "Main limit reached. Liberating secondary…";
          my $delta = 0;
          {
            local $offset = $offset . "    ";
            $delta = $positions * gen($nextblock, $nextstart);
          }
          $allpositions += $delta;
          #print $offset, "(NEW from secondary $delta)";
          last;
        }
        
        die "nextblocksize is undefined (block $block of $#blocks)" unless defined $nextblocksize;

        my $nextnextblock = $nextblock+1;
        my $nextnextblocksize = $blocks[$nextnextblock];
        my $nextnextstart = $start+$cells+$nextblocksize+$rightspacing;
        my $nextnextsuffix = $suffixes[$nextnextstart];

        return $allpositions if $nextnextstart >= length($original);
        
        #print $offset, "Placing down secondary ($nextfull)";
        
        die "suffix length errror" if (length($nextnextsuffix) >= length($nextsuffix));
      
        #printf "${offset}My father had '%s'\n",                                  paint("yellow", $original, 0, $start);
        #printf "${offset}His son had   '%s' (post $blocksize)\n",                paint("yellow", $original, 0, $start+$cells);
        #printf "${offset}And I'll have '%s' (post $blocksize,$nextblocksize)\n", paint("yellow", $original, 0, $nextnextstart);
        
        my $nextpositions;
        {
          local $offset = $offset . "    ";
          $nextpositions = gen($nextblock+1, $nextnextstart)
        }
        
        my $delta = $positions * $nextpositions;
        #print $offset, "(NEW $positions * $nextpositions = $delta)";
        $allpositions += $delta;
        $zero = $delta == 0;
      }
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

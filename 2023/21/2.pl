# just realised that rotating the map would be better than this
# abstraction over directions business.

use Data::Dumper;
use integer;

# PARITY
my $paritymode = "even";

my @lines = map { chomp; $_ } <>;
my $height = @lines;
my $width = length($lines[0]);
die "height ($height) != width ($width)" if $height != $width;
my @origin = ($originrow, $origincolumn) = ($height/2, $width/2);
# my $maxx = 801;
# my $maxx = 400;
my $maxx = 26501365; # (answer is 598044246091826, input ⭐)
# my $maxx = 1000;
# my $maxx = 500; # (answer is 213881, input)
# my $maxx = 501;   # (answer is 214690, input)
# my $maxx = 1000;  # (answer is 853901, input)
# my $maxx = 200; # (answer 34610, bainne)
# my $maxx = 1000;

my $minitest = 1;

our $max = $maxx;
my $context = 20;
my $inf = 1_000_000_000;
our $rot = 0;

sub real {
  my ($row, $column) = @_;
  $row -= $originrow;
  $column -= $origincolumn;
  ($row, $column) = ($row, $column)   if ($rot == 0); # good
  ($row, $column) = (-$column, $row)  if ($rot == 1); # good
  ($row, $column) = (-$row, -$column) if ($rot == 2); # good
  ($row, $column) = ($column, -$row)  if ($rot == 3); # good
  return ($row + $originrow, $column + $origincolumn);
}

sub at {
  my ($row, $column) = real @_;
  return "S" if $row == $originrow && $column == $origincolumn;
  substr($lines[$row % $height], $column % $width, 1) =~ tr/S/./r
}

sub free {
  my ($row, $column) = @_;
  my $c = at $row, $column;
  "." eq $c || "S" eq $c
}

sub printaround {
  my ($row, $column) = @_;
  my @lines;
  for my $dr (0..$context) {
    for my $dc (0..$context) {
      $lines[$dr] .= at($row + $dr - $context/2, $column + $dc - $context/2);
    }
  }
  @lines
}

# minimum/maximum row/column
sub bfs {
  my ($row, $column, %args) = @_;

  my ($minimumrow, $maximumrow) = ($args{minimumrow} // -$inf, $args{maximumrow} // $inf);
  my ($minimumcolumn, $maximumcolumn) = ($args{minimumcolumn} // -$inf, $args{maximumcolumn} // $inf);
  my $extend = $args{extend} // 5;

  my %distance;
  
  my @queue = ([$row, $column, 0]);
  my $reachable;
  while (my ($row, $column, $distance) = @{shift @queue}) {
    next unless free($row, $column);
    next unless 0 <= $distance <= $max;
    next unless ($minimumrow - $extend    // -$inf) <= $row    <= ($maximumrow + $extend    // $inf)
      && ($minimumcolumn - $extend // -$inf) <= $column <= ($maximumcolumn + $extend // $inf);
    next if exists $distance{$row, $column};
    $distance{$row, $column} = $distance;
    if ((($max - $distance) % 2 == 0) &&
        ($minimumrow    // -$inf) <= $row    <= ($maximumrow    // $inf) &&
        ($minimumcolumn // -$inf) <= $column <= ($maximumcolumn // $inf)) {
      $reachable++;
      $args{marked} and ${$args{marked}}{$row, $column} = 1;
    }
    push @queue, [$row+1, $column, $distance+1];
    push @queue, [$row-1, $column, $distance+1];
    push @queue, [$row, $column+1, $distance+1];
    push @queue, [$row, $column-1, $distance+1];
  }
  $args{distance} and %{$args{distance}} = %distance;
  return $reachable;
}

sub bfscalculate {
  my ($marked) = @_;
  my @lines;
  my $amount = 250;
  for my $row ($originrow-$amount..$originrow+$amount) {
    for my $column ($origincolumn-$amount..$origincolumn+$amount) {
      my $c = at($row, $column);
      if ($$marked{$row, $column}) {
        $lines[$row - ($originrow-$amount)] .= "\e[48;2;$amount;0;0m".$c."\e[m";
      } else {
        $lines[$row - ($origincolumn-$amount)] .= $c;
      }
    }
  }
  # print "$_\n" for (@lines);
  printf "Real Answer: %d\n", scalar keys %$marked;
}

# {
#   bfs $originrow, $origincolumn,
#     extend => 20,
#     marked => \my %marked;
#   bfscalculate \%marked;
#   $minitest = 0;
# }

bfs $originrow, $origincolumn,
  minimumrow    => 0, maximumrow    => $height-1,
  minimumcolumn => 0, maximumcolumn => $width-1,
  distance => \my %distance,
  marked => \my %marked;
printf "assumed + calculated (center): %d\n", scalar keys %marked;
my $ss = keys %marked;
# my %ss = %marked;

sub plot {
  my ($row, $column) = @_;
  print "PLOT FROM ($row, $column)\n";
  my @m = printaround $row, $column;
  substr $m[$context/2], $context/2, 1, "\e[48;2;100;0;0m".substr($m[$context/2],$context/2,1)."\e[m";
  local $", print "$_\n" for @m;
}

for ([-1,0], [1,0], [0,1], [0,-1]) {
# for ([-1,0]) {
# for ([0,-1]) {
# for (()) {
  print "\nBAR (@$_)\n\n";
  my ($dr, $dc) = @$_;
  my @primary = ($primaryrow, $primarycolumn) = ($originrow    + $dr * ($height/2 + 1) + $dc * ($height/2),
                                                 $origincolumn + $dr * ($height/2) + $dc * ($height/2 + 1));
  my @secondary = ($secondaryrow, $secondarycolumn) = ($primaryrow - $dc * ($height-1), $primarycolumn - $dr * ($width-1));
  my @tertiary = ($tertiaryrow, $tertiarycolumn) = ($primaryrow - $dc * ($height/2), $primarycolumn - $dr * ($width/2));

  plot @tertiary;
  
  my ($sdr, $sdc) = (-$dc, -$dr);
  my (%bara, %anciliarybounds, %immediatebounds, %verificationbounds);
  if ($dr) {
    ($bara{minimumcolumn}, $bara{maximumcolumn})  = (0, $width - 1);
    ($anciliarybounds{minimumrow}, $anciliarybounds{maximumrow}) =
      sort { $a <=> $b } $primaryrow, $primaryrow + $dr * $height - $dr;
    ($immediatebounds{minimumrow}, $immediatebounds{maximumrow}) =
      sort { $a <=> $b } $primaryrow + $dr * $height, $primaryrow + $dr * $height*2 - $dr;
    ($verificationbounds{minimumrow}, $verificationbounds{maximumrow}) =
      sort { $a <=> $b } $primaryrow, $dr * $inf;
  } elsif ($dc) {
    ($bara{minimumrow}, $bara{maximumrow}) = (0,  $height - 1);
    # plot $bara{minimumrow}, $primarycolumn;
    ($anciliarybounds{minimumcolumn}, $anciliarybounds{maximumcolumn}) =
      sort { $a <=> $b } $primarycolumn, $primarycolumn + $dc * $width - $dc;
    ($immediatebounds{minimumcolumn}, $immediatebounds{maximumcolumn}) =
      sort { $a <=> $b } $primarycolumn + $dc * $width, $primarycolumn + $dc * $width*2 - $dc;
    ($verificationbounds{minimumcolumn}, $verificationbounds{maximumcolumn}) =
      sort { $a <=> $b } $primarycolumn, $dc * $inf;
  }

  # {
  #   bfs $originrow, $origincolumn, %verificationbounds, %bara,
  #     extend => 20,
  #     marked => \my %marked;
  #   bfscalculate \%marked;
  # }
  
  my @anciliaryprimary = ($primaryrow + $dr * $height, $primarycolumn + $dc * $width);
  my @anciliarysecondary = ($secondaryrow + $dr * $height, $secondarycolumn + $dc * $width);
  my @anciliarytertiary = ($tertiary + $dr * $height, $tertiary + $dc * $width);

  # print Dumper "ancilliary", \%anciliarybounds;
  # print Dumper "immediate", \%immediatebounds;
  # print Dumper "bar", \%bara;
  # print Dumper "primary", \@primary;
  # print Dumper "secondary", \@secondary;

  my $primarydistance   = $distance{$primaryrow, $primarycolumn};
  my $secondarydistance = $distance{$secondaryrow, $secondarycolumn};
  my $tertiarydistance  = $distance{$tertiaryrow, $tertiarycolumn};
  
  print "primary distance (bar @$_): $primarydistance\n";
  print "secondary distance (bar @$_): $secondarydistance\n";

  # next unless $primarydistance && $secondarydistance;

  {
    my %anciliary;
    local $max = $maxx - $primarydistance - $height;
    bfs @anciliaryprimary, %bara, %immediatebounds, marked => \%anciliary;
    local $max = $maxx - $secondarydistance - $height;
    bfs @anciliarysecondary, %bara, %immediatebounds, marked => \%anciliary;
    local $max = $maxx - $tertiarydistance - $height;
    bfs @anciliarytertiary, %bara, %immediatebounds, marked => \%anciliary;
    my $anciliary = scalar keys %anciliary;

    my %immediate;
    local $max = $maxx - $primarydistance;
    bfs @primary, %bara, %anciliarybounds, marked => \%immediate;
    local $max = $maxx - $secondarydistance;
    bfs @secondary, %bara, %anciliarybounds, marked => \%immediate;
    local $max = $maxx - $tertiarydistance;
    bfs @tertiary, %bara, %anciliarybounds, marked => \%immediate;
    my $immediate = scalar keys %immediate;
    print "anciliary (bar @$_): $anciliary, immediate (bar @$_): $immediate\n";
  
    my ($blocks) = sort { $b <=> $a } 0, ($maxx - $primarydistance) / $height - 2;
    my $sum;
    
    # PARITY
    if ($paritymode eq "even") {
      $sum = ($blocks / 2) * ($anciliary + $immediate) + ($blocks % 2 and $immediate);
    } elsif ($paritymode eq "odd") {
      $sum = ($blocks / 2) * ($anciliary + $immediate) + ($blocks % 2 and $anciliary);
    } else {
      $sum = ($blocks / 2) * ($anciliary + $immediate) + ($blocks % 2 and $anciliary)
    }

    print "blocks (bar @$_): $blocks\n";
    print "sum (bar @$_): $sum\n";
  
    my %row = (%bara, %verificationbounds);
    $row{minimumrow} += $dr * $height*$blocks;
    $row{maximumrow} += $dr * $height*$blocks;
    $row{minimumcolumn} += $dc * $height*$blocks;
    $row{maximumcolumn} += $dc * $height*$blocks;

    # print Dumper "row", \%row;

    my %combine;
    my ($cornerrow, $cornercolumn) = ($primaryrow + $dr * $height*$blocks, $primarycolumn + $dc * $width*$blocks);
    local $max = $maxx - $primarydistance - $height * $blocks;
    bfs $cornerrow, $cornercolumn, %row, %bara, marked => \%combine;
    
    my ($cornerrow, $cornercolumn) = ($secondaryrow + $dr * $height*$blocks, $secondarycolumn + $dc * $width*$blocks);
    local $max = $maxx - $secondarydistance - $height * $blocks;
    bfs $cornerrow, $cornercolumn, %row, %bara, marked => \%combine;

    my ($cornerrow, $cornercolumn) = ($tertiaryrow + $dr * $height*$blocks, $tertiarycolumn + $dc * $width*$blocks);
    local $max = $maxx - $tertiarydistance - $height * $blocks;
    bfs $cornerrow, $cornercolumn, %row, %bara, marked => \%combine;

    printf "combine (@$_): %d\n", scalar keys %combine;
    printf "assumed + calculated (bar @$_): %d\n", $sum + keys %combine;
    $ss += $sum + keys %combine;

    %ss = (%ss, %immediate, %anciliary, %combine);
  
    # verification BFS
    if ($minitest and $maxx < 2000) {
      local $max = $maxx;
      bfs @origin, %verificationbounds, %bara,
        extend => $height*2,
        marked => \my %last;
      printf "bfs ground truth (bar @$_): %d\n", scalar keys %last;
    } else {
      print "bfs ground truth (bar @$_): <input size too big>\n";
    }
  }
}

# bfscalculate \%ss;
# exit;

my %ss_r;

for $rot (0, 1, 2, 3) {
# for $rot (0,1,2,3) {
# for $rot (0) {
# for $rot (()) {
  # {
  #   bfs $originrow, $origincolumn,
  #     maximumrow => -1,
  #     maximumcolumn => -1,
  #     extend => 20,
  #     marked => \my %marked;
  #   bfscalculate \%marked;
  # }
  
  print "\nQUADRANT ($rot)\n\n";
  my @source = ($originrow - $height/2 - 1, $origincolumn - $width/2 - 1);
  my @real = real @source;
  # plot @source;
  
  my $topleftcornerdistance = $distance{$real[0], $real[1]};

  print "corner distance: $topleftcornerdistance\n";
  print "real: @real\n";
  
  local $max = $maxx - $topleftcornerdistance;
  my ($cornerrow, $cornercolumn) = @source;
  bfs $cornerrow, $cornercolumn,
    minimumcolumn => $cornercolumn-$width+1,
    maximumcolumn => $cornercolumn,
    minimumrow => $cornerrow-$height+1,
    maximumrow => $cornerrow,
    marked => \my %immediate;
  my $immediate = keys %immediate;

  local $max = $maxx - $topleftcornerdistance - $height;
  my ($cornerrow, $cornercolumn) = ($source[0] - $height, $source[1]);
  bfs $cornerrow, $cornercolumn,
    minimumcolumn => $cornercolumn-$width+1,
    maximumcolumn => $cornercolumn,
    minimumrow => $cornerrow-$height+1,
    maximumrow => $cornerrow,
    marked => \my %anciliary;
  my $anciliary = keys %anciliary;
  printf "immediate: %d, anciliary: %d\n", scalar(keys %immediate), scalar(keys %anciliary);

  my ($blocks) = sort { $b <=> $a } 0, ($maxx - $topleftcornerdistance) / $height - 1;
  my $sum = 0;
  my $row;
  for my $block (reverse 1..$blocks) {
    # how many horizontal blocks
    # my $remaining = ($maxx - $topleftcornerdistance - $height * $block);

    $sum += ($block / 2) * ($anciliary + $immediate);
    # PARITY
    if ($paritymode eq "even") {
      $sum += $row ? $anciliary : $immediate if ($block % 2 == 1);
    } elsif ($paritymode eq "odd") {
      $sum += $row ? $immediate : $anciliary if ($block % 2 == 1);
    } else {
      $sum += $row ? $immediate : $anciliary if ($block % 2 == 1);
    }
    $row ^= 1;
  }

  print "blocks: $blocks\n";
  print "Sum: $sum\n";

  my %last;
  my ($free, $occluded);
  for my $block (0..$blocks) {
    my ($dr, $dc) = ($block, $blocks - $block);
    local $max = $maxx - $topleftcornerdistance - $dr * $height - $dc * $width;
    my ($cornerrow, $cornercolumn) = ($source[0] - $dr * $height, $source[1] - $dc * $width);
    my $marked = %last;
    my $c = bfs $cornerrow, $cornercolumn,
      minimumrow => -$inf,
      minimumcolumn => -$inf,
      maximumrow => $cornerrow,
      maximumcolumn => $cornercolumn,
      marked => \%last;
    my $delta = %last - $marked;
    # printf "\tdelta: %d\n", $delta;
    $m = keys %last;
    $occluded = $delta;
    last if defined $free;
    $free = $delta unless defined $free;
    die if defined $free && $delta != $occluded;
  }
  my $calculated = $free + $blocks * $occluded;
  print "free: $free, occluded: $occluded\n";
  printf "bfs calculated: %d\n", $calculated;
  printf "assumed + calculated (quadrant $rot): %d\n", $calculated + $sum;
  $ss += $calculated + $sum;

  %ss_r = (%immediate, %anciliary, %last);
 
  for (keys %ss_r) {
    my ($row, $column) = real split $;;
    $ss{$row, $column} = 1;
  }

  if ($minitest and $maxx < 1000) {
    local $max = $maxx;
    my ($cornerrow, $cornercolumn) = ($source[0], $source[1]);
    bfs @origin,
      minimumrow => -$inf,
      minimumcolumn => -$inf,
      maximumrow => $cornerrow,
      maximumcolumn => $cornercolumn,
      extend => $height * 2,
      marked => \my %last;
    printf "bfs ground truth (quadrant $rot): %d\n", scalar keys %last;
  } else {
    print "bfs ground truth (quadrant $rot): <input size too big>\n";
  }
}

# bfscalculate \%ss;

print "Answer: $ss\n";

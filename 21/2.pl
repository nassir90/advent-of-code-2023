use Data::Dumper;
use integer;

@lines = map { chomp; $_ } <>;
$height = @lines;
$width = length($lines[0]);
my ($originrow, $origincolumn) = ($height/2, $width/2);
$max = 500;
$maxx = $max;
$context = 10;
$inf = 1_000_000_000;

sub at {
  my ($row, $column) = @_;
  # printf "%d ($width)\n", $column % $width;
  substr $lines[$row % $height], $column % $width, 1
}

sub free {
  my ($row, $column) = @_;
  # printf "%d ($width)\n", $column % $width;
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
  
  my $minimumrow = $args{minimumrow} // $minimumrow // -$inf;
  my $maximumrow = $args{maximumrow} // $maximumrow // $inf;
  my $minimumcolumn = $args{minimumcolumn} // $minimumcolumn // -$inf;
  my $maximumcolumn = $args{maximumcolumn} // $maximumcolumn // $inf;

  my %distance;
  
  my @queue = ([$row, $column, 0]);
  my $reachable;
  while (my ($row, $column, $distance) = @{shift @queue}) {
    next unless free($row, $column);
    next unless $distance <= $max;
    next unless (($minimumrow // -$inf) - 2 <= $row <= ($maximumrow // $inf) + 2) && (($minimumcolumn // -$inf) - 2 <= $column <= ($maximumcolumn // $inf) + 2);
    next if exists $distance{$row, $column};
    $distance{$row, $column} = $distance;
    if (($max - $distance) % 2 == 0) {
      # printf "$distance: %d, %d\n", $row - $height/2, $column - $width / 2;
      # substr($lines[$row], $column, 1) = "O";
      if ((($minimumrow // -$inf) <= $row <= ($maximumrow // $inf)) && (($minimumcolumn // -$inf) <= $column <= ($maximumcolumn // $inf))) {
        $reachable++;
        exists $args{marked} and ${$args{marked}}{$row, $column} = 1;
      }
    }
    push @queue, [$row+1, $column, $distance+1];
    push @queue, [$row-1, $column, $distance+1];
    push @queue, [$row, $column+1, $distance+1];
    push @queue, [$row, $column-1, $distance+1];
  }
  exists $args{distance} and %{$args{distance}} = %distance;
  $reachable
}

my ($topleftcornerrow, $topleftcornercolumn) = ($originrow - $height/2 - 1, $origincolumn - $width/2);
my ($toprightcornerrow, $toprightcornercolumn) = ($originrow - $height/2 - 1, $origincolumn + $width/2);
my ($bottomleftcornerrow, $bottomleftcornercolumn) = ($originrow + $height/2 - 1, $origincolumn - $width/2);
my ($bottomrightcornerrow, $bottomrightcornercolumn) = ($originrow + $height/2 - 1, $origincolumn + $width/2);
bfs $originrow, $origincolumn,
  minimumrow => $topleftcornerrow,
  maximumrow => $bottomleftcornerrow,
  minimumcolumn => $topleftcornercolumn,
  maximumcolumn => $toprightcornercolumn,
  distance => \%distance;
$topleftcornerdistance = $distance{$topleftcornerrow, $topleftcornercolumn};
$toprightcornerdistance = $distance{$toprightcornerrow, $toprightcornercolumn};
$bottomleftcornerdistance = $distance{$bottomleftcornerrow, $bottomleftcornercolumn};
$bottomrightcornerdistance = $distance{$bottomrightcornerrow, $bottomrightcornercolumn};

print "TL: $topleftcornerdistance\n";
print "TR: $toprightcornerdistance\n";
print "BL: $bottomleftcornerdistance\n";
print "BR: $bottomrightcornerdistance\n";

# # height is an odd number...
# # print "HEIGHT IS $height\n";
# # exit;

# compute immediate parity count

# compute anciliary parity coutn

# take the amount remaining after getting to the top left corner

# divide by height

# divide by 2 and store the remainder

# add 2 * (immediate + anciliary)

# add immediate if remainder

# BAR

{  
  my %bar = (minimumcolumn => 0, maximumcolumn => $width-1);
  local $max = $maxx - $topleftcornerdistance - $height;
  my ($cornerrow, $cornercolumn) = ($originrow - $height/2 - $height - 1, $origincolumn - $width/2);
  my %row = (minimumrow => $cornerrow-$height+1, maximumrow => $cornerrow);
  bfs $cornerrow, $cornercolumn, %row, %bar, marked => \%anciliary;
  local $max = $maxx - $toprightcornerdistance - $height;
  my ($cornerrow, $cornercolumn) = ($originrow - $height/2 - $height - 1, $origincolumn + $width/2);
  bfs $cornerrow, $cornercolumn, %row, %bar, marked => \%anciliary;
  my $anciliary = scalar keys %anciliary;

  my %NIG = (%row, %bar);
  print Dumper \%NIG;
  
  local $max = $maxx - $topleftcornerdistance;
  my ($cornerrow, $cornercolumn) = ($originrow - $height/2 - 1, $origincolumn - $width/2);
  my %row = (minimumrow => $cornerrow-$height+1, maximumrow => $cornerrow);
  bfs $cornerrow, $cornercolumn, %row, %bar, marked => \%immediate;
  local $max = $maxx - $toprightcornerdistance;
  my ($cornerrow, $cornercolumn) = ($originrow - $height/2 - 1, $origincolumn + $width/2);
  bfs $cornerrow, $cornercolumn, %row, %bar, marked => \%immediate;
  my $immediate = scalar keys %immediate;
  
  print "immediate: $immediate\n";
  print "anciliary: $anciliary\n";
  
  my $blocks = ($maxx - $topleftcornerdistance) / $height - 1;
  my $sum = ($blocks / 2) * ($anciliary + $immediate) + ($blocks % 2 and $immediate);

  print "blocks: $blocks\n";
  print "sum: $sum\n";
  
  local $number = $blocks;
  local $max = $maxx - $topleftcornerdistance - $height * $number;
  
  my ($cornerrow, $cornercolumn) = ($originrow - $height/2 - $height*$number - 1, $origincolumn - $width/2);
  my %row = (minimumrow => -$inf, maximumrow => $cornerrow);
  bfs $cornerrow, $cornercolumn, %row, %bar, marked => \%combine;
  my ($cornerrow, $cornercolumn) = ($originrow - $height/2 - $height*$number - 1, $origincolumn + $width/2);
  local $max = $maxx - $toprightcornerdistance - $height * $number;
  bfs $cornerrow, $cornercolumn, %row, %bar, marked => \%combine;

  printf "combine: %d\n", scalar keys %combine;
  printf "absolute combine: %d\n", $sum + scalar keys %combine;

  # print "HERE LIES MICHAEL\n";
  # my @m = printaround $cornerrow, $cornercolumn;
  # substr $m[$context/2], $context/2, 1, "\e[48;2;100;0;0m".substr($m[$context/2],$context/2,1)."\e[m";
  # local $", print "$_\n" for @m;
  
  # verification BFS
  
  local $max = $maxx - $toprightcornerdistance;
  my ($cornerrow, $cornercolumn) = ($originrow - $height/2 - 1, $origincolumn - $width/2);
  my %row = (minimumrow => -$inf, maximumrow => $cornerrow);
  bfs $cornerrow, $cornercolumn, %row, %bar, marked => \%last;

  local $max = $maxx - $toprightcornerdistance;
  my ($cornerrow, $cornercolumn) = ($originrow - $height/2 - 1, $origincolumn + $width/2);
  bfs $cornerrow, $cornercolumn, %row, %bar, marked => \%last;
  printf "Zoomer: %d\n", scalar keys %last;
}

print "\nQUADRANT\n\n";

{
  my $blocks = ($maxx - $topleftcornerdistance) / $height - 2;

  local $max = $maxx - $topleftcornerdistance;
  my ($cornerrow, $cornercolumn) = ($originrow - $height/2 - 1, $origincolumn - $width/2 - 1);
  bfs $cornerrow, $cornercolumn,
    minimumcolumn => $cornercolumn-$width+1,
    maximumcolumn => $cornercolumn,
    minimumrow => $cornerrow-$height+1,
    maximumrow => $cornerrow,
    marked => \my %immediate;
  my $immediate = scalar keys %immediate;
  printf "immediate: %d\n", scalar keys %immediate;

  local $max = $maxx - $topleftcornerdistance - $height;
  my ($cornerrow, $cornercolumn) = ($originrow - $height/2 - $height - 1, $origincolumn - $width/2 - 1);
  bfs $cornerrow, $cornercolumn,
    minimumcolumn => $cornercolumn-$width+1,
    maximumcolumn => $cornercolumn,
    minimumrow => $cornerrow-$height+1,
    maximumrow => $cornerrow,
    marked => \my %anciliary;
  my $anciliary = scalar keys %anciliary;
  printf "ancilliary: %d\n", scalar keys %anciliary;

  my $blocks = ($maxx - $topleftcornerdistance) / $height - 1;
  my $sum = 0;
  my $row;
  for my $block (reverse 1..$blocks) {
    # how many horizontal blocks
    my $remaining = ($maxx  - $topleftcornerdistance - $height * $block);
    $sum += ($block / 2) * ($anciliary + $immediate);
    $sum += $row ? $anciliary : $immediate if ($block % 2 == 1);
    $row ^= 1;
  }

  print "Blocks: $blocks\n";
  print "Sum: $sum\n";

  my %last;
  for my $block (0..$blocks) {
    my ($dr, $dc) = ($block, $blocks - $block);
    local $max = $maxx - $topleftcornerdistance - $dr * $height - $dc * $width;
    my ($cornerrow, $cornercolumn) = (
                                      $originrow - $height/2 - 1 - $dr * $height,
                                      $origincolumn - $width/2 - 1 - $dc * $width
                                     );
    my $c = bfs $cornerrow, $cornercolumn,
      minimumrow => -$inf,
      minimumcolumn => -$inf,
      maximumrow => $cornerrow,
      maximumcolumn => $cornercolumn,
      marked => \%last;
    printf "c: $c\n";
  }
  my $zener = scalar keys %last;
  printf "Zener: %d\n", $zener;
  printf "Absolute combine: %d\n", $zener + $sum;

  local $max = $maxx - $topleftcornerdistance;
  my ($cornerrow, $cornercolumn) = ($originrow - $height/2 - 1, $origincolumn - $width/2 - 1);
  # print "HERE LIES MICHAEL\n";
  # my @m = printaround $cornerrow, $cornercolumn;
  # substr $m[$context/2], $context/2, 1, "\e[48;2;100;0;0m".substr($m[$context/2],$context/2,1)."\e[m";
  # local $", print "$_\n" for @m;
  bfs $cornerrow, $cornercolumn,
    minimumrow => -$inf,
    minimumcolumn => -$inf,
    maximumrow => $cornerrow,
    maximumcolumn => $cornercolumn,
    marked => \my %last;
  printf "Zoomer: %d\n", scalar keys %last;
}

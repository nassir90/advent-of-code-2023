# I hate this. Thought that I was good with graphs but apparently not!
sub north  { $_[0] - 1, $_[1]     }
sub east   { $_[0],     $_[1] + 1 }
sub south  { $_[0] + 1, $_[1]     }
sub west   { $_[0],     $_[1] - 1 }
sub at     { $rows[$_[0]][$_[1]] if $_[0] >= 0 && $_[1] >= 0 }
sub marked { $marked[$_[0]][$_[1]] || ($_[0] < 0 || $_[0] > $#rows) || ($_[1] < 0 || $_[1] > $rows[0]->$#*) }

while (<>) {
    push @rows, [split //];
    for $column (0..$rows[0]->$#*) {
        $row = $#rows;
        $_ = $rows[$row][$column];
        $edge[$#rows][$column] = [[north $#rows, $column], [south $#rows, $column]] if /\|/;
        $edge[$#rows][$column] = [[east  $#rows, $column], [west  $#rows, $column]] if /-/;
        $edge[$#rows][$column] = [[north $#rows, $column], [east  $#rows, $column]] if /L/;
        $edge[$#rows][$column] = [[north $#rows, $column], [west  $#rows, $column]] if /J/;
        $edge[$#rows][$column] = [[south $#rows, $column], [west  $#rows, $column]] if /7/;
        $edge[$#rows][$column] = [[south $#rows, $column], [east  $#rows, $column]] if /F/;
        @at = ($#rows, $column)                                                     if /S/;
    }
}

push $edge[$at[0]][$at[1]]->@*, [north @at] if (at north @at) =~ /[7|F]/;
push $edge[$at[0]][$at[1]]->@*, [east @at]  if (at east  @at) =~ /[-7J]/;
push $edge[$at[0]][$at[1]]->@*, [south @at] if (at south @at) =~ /[J|L]/;
push $edge[$at[0]][$at[1]]->@*, [west @at]  if (at west  @at) =~ /[-LF]/;

cycle: while (1) {
    $marked[$at[0]][$at[1]] = ++$s;
    for $next ($edge[$at[0]][$at[1]]->@*) {
        @prev = @at, @at = @$next, last if !marked @$next;
        last cycle                      if "@$next" ne "@prev" && "S" eq at @$next;
    }
}

print "Answer #1: @{[$s/2]}\n\n";

for my $row (0..$#rows) {
    for my $column (0..$rows[0]->$#*) {
        for my $row ($row*3..$row*3+2) {
            $hyper[$row][$_] = "\e[38;2;255;255;255mI\e[m" for ($column*3..$column*3+2);
        }
        if (marked $row, $column) {
            $hyper[$row*3+1][$column*3+1] = "P";
            $hyper[$row*3+1+(@$_[0]-$row)][$column*3+1+(@$_[1]-$column)] = "P" for ($edge[$row][$column]->@*)
        }
    }
}

@oldrows = @rows;
@rows = @hyper;

# print "Expanded map\n";
# local $" = "", print "@$_\n" for (@rows);

sub dfr {
    my $c = at @_;
    return if $c eq "P" || $c eq "" || $c =~ /O/;
    $rows[$_[0]][$_[1]] = "\e[38;2;0;100;0mO\e[m";
    dfr(north      @_);
    dfr(north east @_);
    dfr(east       @_);
    dfr(south east @_);
    dfr(south      @_);
    dfr(south west @_);
    dfr(west       @_);
    dfr(north west @_);
}

dfr 0, 0;

print "DFRed map\n";
local $" = "", print "@$_\n" for (@rows);

print "Compressed map map\n";
for my $row (0..$#oldrows) {
    for my $column (0..$oldrows[0]->$#*) {
        $c = $rows[$row*3+1][$column*3+1];
        print $c; 
        $m++ if $c =~ /I/;
    }
    print "\n";
}

print "\nAnswer #2: $m\n";

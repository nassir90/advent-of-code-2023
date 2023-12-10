# I hate this. Thought that I was better at graphs but apparently not!
sub north { ($_[0] // $row) - 1, ($_[1] // $column)     }
sub east  { ($_[0] // $row),     ($_[1] // $column) + 1 }
sub south { ($_[0] // $row) + 1, ($_[1] // $column)     }
sub west  { ($_[0] // $row),     ($_[1] // $column) - 1 }
sub at    { $rows[$_[0]][$_[1]] if $_[0] >= 0 && $_[1] >= 0 }

$row = 0;
for my $line (<>) {
    @columns = split //, $line;
    push @rows, [@columns];
    for $column (0..$#columns) {
        $_ = $columns[$column];
        $name{"$row $column"} = [N,S], $edge{"$row $column"} = [[north], [south]] if /\|/;
        $name{"$row $column"} = [E,W], $edge{"$row $column"} = [[east],  [west]]  if /-/;
        $name{"$row $column"} = [N,E], $edge{"$row $column"} = [[north], [east]]  if /L/;
        $name{"$row $column"} = [N,W], $edge{"$row $column"} = [[north], [west]]  if /J/;
        $name{"$row $column"} = [S,W], $edge{"$row $column"} = [[south], [west]]  if /7/;
        $name{"$row $column"} = [S,E], $edge{"$row $column"} = [[south], [east]]  if /F/;
        @start = ($row, $column)                                                  if /S/;
    }
    $row++;
}

sub marked {
    return $marked{"$_[0] $_[1]"} || ($_[0] < 0 || $_[0] > $#rows) || ($_[1] < 0 || $_[1] > $#columns);
}

push $edge{"@start"}->@*, [north @start] if (at north @start) =~ /[|7F]/;
push $edge{"@start"}->@*, [east @start]  if (at east @start)  =~ /[-7J]/;
push $edge{"@start"}->@*, [south @start] if (at south @start) =~ /[|JL]/;
push $edge{"@start"}->@*, [west @start]  if (at west @start)  =~ /[-FL]/;

@at = @start;

while (1) {
    $marked{"@at"} = 1;
    @dirs =$name{"@at"}->@*;
    print "At @at with @dirs\n";
    $s++;
    for $next ($edge{"@at"}->@*) {
        print "\t...\n";
        next if "@$next" eq "@prev";
        if (!marked @$next) {
            print "\t@$next not marked\n";
            @prev = @at;
            @at = @$next;
            $to{"@at"} = [@prev];
            last;
        } else {
            print "\t@$next marked\n";
        }
        if ("S" eq at @$next) {
            print "Done ($at → $nextrow $nextcolumn), (prev = $prev)!\n";
            goto end;
        }
    }
}

 end:
    for my $row (0..$#rows) {
        for my $column (0..$#columns) {
            my $conduit = $edge{"$row $column"};
            
            if (marked($row, $column) && defined $conduit) {
                for my $row ($row*3..$row*3+2) {
                    for my $column ($column*3..$column*3+2) {
                        $hyper[$row][$column] = "\e[38;2;255;255;255mI\e[m";
                    }
                }
                $hyper[$row*3+1][$column*3+1] = "P";
                for (@$conduit) {
                    ($y, $x) = @$_;
                    $hyper[$row*3+1+($y-$row)][$column*3+1+($x-$column)] = "P";
                }
            } else {
                for my $row ($row*3..$row*3+2) {
                for my $column ($column*3..$column*3+2) {
                    $hyper[$row][$column] =  "\e[38;2;255;255;255mI\e[m";
                }
            }
            }
        }
}

@oldrows = @rows;
@rows = @hyper;

for my $row (@rows) {
    for my $column (@$row) {
        print $column;
    }
    print "\n";
}


sub dfr {
    my $c = at @_;
    # print "Hmm: '$c'\n";
    return unless $c ne "" && $c !~ /O/;
    return if $c eq "P";
    # print "> continues into @_\n";
    $rows[$_[0]][$_[1]] = "\e[38;2;0;100;0mO\e[m";
    dfr(north @_);
    dfr(north east @_);
    dfr(east  @_);
    dfr(south east @_);
    dfr(south @_);
    dfr(south west @_);
    dfr(west  @_);
    dfr(north west @_);
}

for my $row (0..$#rows) {
    dfr $row, 0;
    dfr $row, $rows[0]->$#*; 
}


for my $column (0..$#columns) {
    dfr 0,      $column;
    dfr $#rows, $column;
}

for my $row (0..$#rows) {
    for my $column (0..$rows[0]->$#*) {
        print $rows[$row][$column];
    }
    print "\n";
}

for my $row (0..$#oldrows) {
    for my $column (0..$oldrows[0]->$#*) {
        $c = $rows[$row*3+1][$column*3+1];
        print $c;
        $m++ if $c =~ /I/;
    }
    print "\n";
}

print "\n$m\n";







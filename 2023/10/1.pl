# I hate this. Thought that I was better at graphs but apparently not!
sub north { ($_[0] // $row) - 1, ($_[1] // $column)     }
sub east  { ($_[0] // $row),     ($_[1] // $column) + 1 }
sub south { ($_[0] // $row) + 1, ($_[1] // $column)     }
sub west  { ($_[0] // $row),     ($_[1] // $column) - 1 }
sub at    { $rows[$_[0]][$_[1]] }

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

push $edge{"@start"}->@*, [north @start] if ("|" eq at north @start);
push $edge{"@start"}->@*, [east @start] if ("-" eq at east @start);
push $edge{"@start"}->@*, [south @start] if ("|" eq at south @start);
push $edge{"@start"}->@*, [west @start] if ("-" eq at west @start);

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

 end: for my $row (0..$#rows) {
     for my $column (0..$#columns) {
         print $marked{"$row $column"} ? $rows[$row][$column] : " ";
     }
     print "\n";
}
$s = $s / 2;
print "$s\n";


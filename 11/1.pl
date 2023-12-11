sub at { $lines[$_[0]][$_[1]] if $_[0] >= 0 and $_[1] >= 0 }

while (<>) {
    chomp($_);
    @columns = split //, $_;
    $columns[$_] eq "#" && ($hash{$_} = 1) for (0..$#columns);
    if (/([^.])/) {
        push @lines, [@columns];
    } else {
        push @lines, [@columns], [@columns];
    }
}

for $col (0..$#columns) {
    $col = $#columns - $col;
    if (!defined $hash{$col}) {
        print "$col has nothing\n";
        for $column (@lines) {
            @column = $column->@*;
            for ($col..$#column) {
                $a = @column + ($col-$_);
                $b = @column + ($col-$_-1);
                print "\tAssigning $b ($column[$b]) to $a ($column[$a])\n";
                $column[$a] = $column[$b];
            }
            $column[$col+1] = ".";
            $column->@* = @column;
        }
    }
}

local $" = "", print "@$_\n" for (@lines);

for $r (0..$#lines) {
    for $c (0..$lines[0]->$#*) {
        if ($lines[$r][$c] eq "#") {
            push @galaxies, [$r, $c];
        }
    }
}

for $g1 (0..$#galaxies-1) {
    for $g2 ($g1+1..$#galaxies) {
        ($r, $c) = $galaxies[$g1]->@*;
        ($rt, $ct) = $galaxies[$g2]->@*;
        # print "Found pair $g1 ($r,$c) → $g2 ($rt,$ct)\n";
        my $s = 0;
        while ($r != $rt or $c != $ct) {
            # print "\tAt $r,$c\n";
            if ($rt > $r) {
                $dr = 1;
            } elsif ($rt < $r) {
                $dr = -1;
            }

            if ($ct > $c) {
                $dc = 1;
            } elsif ($ct < $c) {
                $dc = -1;
            }

            if (($r+$dr-$rt)**2+($c-$ct)**2 < ($r-$rt)**2+($c+$dc-$ct)**2) {
                $r += $dr, $s++;
            } else {
                $c += $dc, $s++;
            }
        }
        # print "\t$s\n";
        $ss+=$s;
    }
}

print "$ss\n";

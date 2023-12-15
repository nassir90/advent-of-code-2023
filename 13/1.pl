local $/ = /\n\n/;

sub transpose {
    my @lines = @_;
    my $w = length($lines[0]);
    my $h = @lines;
    my @transposed;

    for my $columnnumber (0..$w-1) {
        my $column;
        for my $line (0..$#lines) {
            $column .= substr $lines[$line], $columnnumber, 1;
        }
        push @transposed, $column;
    }

    return @transposed;
}

sub compare {
    my ($width, $height, $reversed, @lines) = @_;
    my $score = 0;
    for my $axis (0..$height) {
        $axis *= 2;
        last if $axis > $height;
        my @lo = @lines[0..int($axis/2)-1];
        my @hi = reverse @lines[int($axis/2)..$axis-1];
        my ($lo, $hi) = ("@lo", "@hi");
        
        # print "@lo\n, @hi\n\n";
        if (@lo and @hi and $lo eq $hi) {
            # print "Found symetry\n";
            my $lowstart = int($axis/2);
            $score += $reversed ? @lines - $lowstart : $lowstart;
        } else {
            my $diffs = 0;
            my $difloc = 0;
            die "length mismatch" unless length($lo) == length($hi);
            for (0..length($lo)) {
                $loc = substr $lo, $_, 1;
                $hic = substr $hi, $_, 1;
                $diffs++ if $loc ne $hic;
                $difloc = $_;
            }
            # print "DIFF CANDIDATE\n" if $diffs == 1;
        }
    }
    return $score;
}

while (my $block = <>) {
    @lines = split /\n/, $block;
    my $w = length($lines[0]);
    my $h = @lines;
    # print "w is $w, h is $h\n";
    my $s;
    local $" = " ";

    # print "Tops\n";
    $score += 100 * compare $w, $h, 0, @lines;
    # print "Bottoms\n";
    $score += 100 * compare $w, $h, 1, reverse @lines;
    # print "Lefts\n";
    $score += compare $h, $w, 0, transpose @lines;
    # print "Rights\n";
    $score += compare $h, $w, 1, reverse transpose @lines;
}

END { print "$score\n" }

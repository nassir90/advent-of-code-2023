#!/usr/bin/perl -F'\\s|,' -nl

$offset = "";

sub count {
    my ($pattern, @blocks) = @_;
    unless (@blocks) {
        local $" = ",";
        print $offset, "DONE ($pattern:@blocks)";
        return 1 if $pattern !~ /#/;
    }
    print $offset, "With blocks @blocks and pattern $pattern\n";
    my $block = shift @blocks;
    my $s = 0;
    $pattern =~ s/^\.*//;
    my %discovered;
    for (0..length($pattern)) {
        print $offset, "index $_";
        if ($pattern =~ /^[?.]{$_}([?#]{$block})($|[?.])/) {
            my $pattern = substr $pattern, $_;
            print $offset, "#0: '$pattern' (seeking $block)";
            my $next = ('.' x length($`)) . ('#' x $block) . ('.' x length($2)) . $';
            print $offset, "#1: '$next' (and @blocks)";
            print($offset, "skipping $next"), next if $discovered{$next};
            $discovered{$next} = 1;
            $next =~ s/^\.*#*//;
            # $next = substr $next, $+[1];
            print $offset, "#2: '$next' (and @blocks)";
            local $offset = $offset . "    ";
            $s += count($next, @blocks);
        }
    }
    return $s;
}

# if ($F[0] eq "?#?#?#?#?#?#?#?") {
my $g = count @F;
print "$F[0] :: $g";
$ss += $g;
# }

END { print $ss }

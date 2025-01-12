#!/usr/bin/perl -F'\\s' -nl

$offset = "";

sub need { my $need; $need+=$_ for @_; $need }
sub have { scalar (() = /[#?]/g) }

sub count {
    my ($pattern, @blocks) = @_;
    # strip fixed blocks from the end
    while (@blocks and $pattern =~ /#\.*$/) {
      # last must be block -1. if not return
      $pattern =~ /(^|[?.])[?#]{$blocks[-1]}\.*$/ or return 0 ;
      $pattern = substr $pattern, 0, $-[0];
      pop @blocks;
      print $offset, "stripped end: $pattern";
    }

    # return if we are done
    unless (@blocks) {
      # local $" = ",", print $offset, "DONE ($pattern:@blocks)";
      return 1 if $pattern !~ /#/;
    } else {
        local $" = ",";
        print $offset, "It's not over yet 😏 ($pattern:@blocks)";
    }
    
    print $offset, "With blocks @blocks (need: $need) and pattern $pattern (have: $have)\n";
    my $block = shift @blocks;
    my $s = 0;
    
    while (length($pattern)) {
      $pattern =~ s/^\.*//;
      my $have = () = $pattern =~ /[?#]/g;
      my $need = need @blocks;
      last unless $have >= $need;

      if ($pattern =~ /^([?#]{$block})($|[?.])/) {
        my $found = "#" x $block;
        my $next = $';
        print $offset, "Found '$found' in '$pattern'. Now using \e[48;2;255;0;0m$&\e[m$next";
        local $offset = $offset . "    ", $s += count($next, @blocks);
      }
      
      last if $pattern =~ /^\#/;
      $pattern =~ s/^\?//;
    }
    
    return $s;
}

# if ($F[0] eq "?#?#?#?#?#?#?#?") {
# $unfoldedpattern = "$F[0]?$F[0]?$F[0]?$F[0]?$F[0]";
# $unfoldedtrailer = "$F[1],$F[1],$F[1],$F[1],$F[1]";
$unfoldedpattern = $F[0];
$unfoldedtrailer = $F[1];

my $needed = () = $unfoldedpattern =~ /#/;
my @blocks = split /,/, $unfoldedtrailer;
my $g = count $unfoldedpattern, @blocks;
print "$F[0] $F[1] → $g";
$ss += $g;
# }

END { print $ss }

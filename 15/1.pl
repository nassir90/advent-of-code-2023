#!/perl -ln
BEGIN { $/ = "," }
{ local $/ = "\n"; chomp }
my $hash;
print "record: $_";
for (split //) {
  $hash = ($hash + ord) * 17 % 256;
  printf "\t'$_' (0x%02x) → $hash${\}", ord;
}
print "";
$s += $hash;
END { print $s }

# minus b formula applied ✅
($t, $d) = (<>);
@t = $t =~ /(\d+)/g;
@d = $d =~ /(\d+)/g;
sub minusb {
    ($t, $d) = @_;
    ($t-sqrt($t**2 - 4*$d))/2, ($t+sqrt($t**2 - 4*$d))/2
}
$z = 1;
for (0..$#t) {
    my $t = $t[$_];
    my $d = $d[$_];
    my $a;
    for (1..$t) {
        $r = $t - $_;
        if ($r * $_ > $d) {
            $a++;
        }
    }
    $z *=  $a;
}
print $z;

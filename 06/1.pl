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
    ($a, $b) = minusb $t[$_], $d[$_]+0.1;
    $diff = int($b - $a+0.5);
    $raw = $b - $a;
    print "$a, $b\n";
    $z *= int($b + 0.99) - int($a + 0.99);
}
print $z;

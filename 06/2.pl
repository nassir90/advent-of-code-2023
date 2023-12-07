# minus b formula applied ✅
($t, $d) = map { s/[^0-9]//gr } (<>);
$d = sqrt($t**2-4*$d);
print int(($t+$d)/2 + 0.99) - int(($t-$d)/2 + 0.99);

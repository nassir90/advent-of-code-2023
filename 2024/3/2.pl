#!perl -nl
BEGIN { $sum = 0; $state = "do"; }

while ($_ =~ /(do\(\))|(don't\(\))|mul\((\d+),(\d+)\)/g) {
        print "matches: $1, $2, $3, $4";
        ($state = "do") if ($1 eq "do()");
        ($state = "don't") if ($2 eq "don't()");
        if ($state eq "do" && $3 && $4) {
                $p = $3 * $4 ;
                print "$3, $4 →  $p";
                $sum += $p;
        }
}
END { print $sum; }

#! /usr/bin/awk -f

{ sum += int($0 / 3.0) - 2 }
END { print sum }

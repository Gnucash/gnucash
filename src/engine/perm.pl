#!/usr/bin/perl
#
# This is a short perl script that prints all permutations 
# of five objects; should be easy to generalize to more.

sub rotate {
  local ($n, $i);
  $n = $_[0];

  $tmp = $arr[0];
  for ($i=0; $i<$n-1; $i++) {
     $arr[$i] = $arr[$i+1];
  }
  $arr[$n-1] = $tmp;
}

sub recur {
  local ($n, $i);
  $n = $_[0];

  if (3>=$n) { 
    print "DECLARE ($arr[4], $arr[3], $arr[2])\n";
    return; 
  }

  for ($i=0; $i<$n-1; $i++) {
    &rotate ($n-1);
    &recur ($n-1);
  }
}

@arr=(DESC,MEMO,AMT,NUM,DATE);
# @arr=(1..5);

&recur (6);


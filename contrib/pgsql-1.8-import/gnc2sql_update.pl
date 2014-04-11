#!/usr/bin/perl
###
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License version 2,
## as published by the Free Software Foundation.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## copyright (C) 2007 Klaus Dahlke <klaus.dahlke@gmx.de>
## ####

use XML::SAX::Simple;
use Data::Dumper;
use Date::Manip;
use IO::Uncompress::Gunzip qw(gunzip $GunzipError) ;
use DBI qw(neat);

use strict ;
use warnings ;

my ($dbh, $rows);
my ($accountguid, $parentguid, $bookguid, $accountname, $accountcode, $description, $act_type, $commodity, $version, $iguid);
my ($cmdty_space, $cmdty_id, $cmdty_sep);
my ($splitguid, $transguid, $memo, $action, $reconsiled, $date_reconsiled, $amount, $value, @split_count, $split_anz);
my ($last_modified, $date_entered, $date_posted, $num, $currency, $quantity);
my ($fullname, $namespace, $mnemonic, $code, $fraction);
my ($priceguid, $price_time, $source, $price_type, $valuenum, $valuedenom, $curr_space, $curr_id, $price_value);
my ($old_date, $last_post, $delta_days);
my ($new_date);



###
# get last update to database
###
open(READFILE, "<last_sql.txt");
$old_date=<READFILE>;
close(READFILE);
$last_post =  ParseDate($old_date);



###
# read gnc file and convert to internal hash
###
my $input = new IO::File "<konten.xac"
        or die "Cannot open 'konten.xac': $!\n" ;
my $buffer ;
    gunzip $input => \$buffer 
        or die "gunzip failed: $GunzipError\n";
my $data = XMLin($buffer);



###
# few constant to be used
###
$bookguid = $data->{"gnc:book"}->{"book:id"}->{"content"};
$version=$data->{"gnc:book"}->{"version"};
$version=substr($version,0,1);
$cmdty_sep= '::';
my @com_count = @{$data->{"gnc:book"}->{"gnc:commodity"}};
my @trn_count = @{$data->{"gnc:book"}->{"gnc:transaction"}};
my @acc_count = @{$data->{"gnc:book"}->{"gnc:account"}};
my @price_count = @{$data->{"gnc:book"}->{"gnc:pricedb"}->{price}};
my $com_anz = @com_count;
my $trn_anz = @trn_count;
my $acc_anz = @acc_count;
my $price_anz = @price_count;
print "No. commodities, ",$com_anz, "\n";
print "No. transactions, ",$trn_anz, "\n";
print "No. accounts, ",$acc_anz, "\n";
print "No. prices in db, ",$price_anz, "\n";

###
# connect to database
###
$dbh = DBI->connect("dbi:Pg:dbname=konten", "klaus", "");


###
## pricedb
###
for (my $k=0; $k<$price_anz; $k++) {
      $priceguid=$data->{"gnc:book"}->{"gnc:pricedb"}->{"price"}->[$k]->{"price:id"}->{"content"};
      $cmdty_space = $data->{"gnc:book"}->{"gnc:pricedb"}->{"price"}->[$k]->{"price:commodity"}->{"cmdty:space"};
      $cmdty_id = $data->{"gnc:book"}->{"gnc:pricedb"}->{"price"}->[$k]->{"price:commodity"}->{"cmdty:id"};
      $commodity="$cmdty_space$cmdty_sep$cmdty_id";
      $curr_space = $data->{"gnc:book"}->{"gnc:pricedb"}->{"price"}->[$k]->{"price:currency"}->{"cmdty:space"};
      $curr_id = $data->{"gnc:book"}->{"gnc:pricedb"}->{"price"}->[$k]->{"price:currency"}->{"cmdty:id"};
      $currency="$curr_space$cmdty_sep$curr_id";
      $price_type=$data->{"gnc:book"}->{"gnc:pricedb"}->{"price"}->[$k]->{"price:type"};
      $price_time=$data->{"gnc:book"}->{"gnc:pricedb"}->{"price"}->[$k]->{"price:time"}->{"ts:date"};
      $price_value=$data->{"gnc:book"}->{"gnc:pricedb"}->{"price"}->[$k]->{"price:value"};
      $source=$data->{"gnc:book"}->{"gnc:pricedb"}->{"price"}->[$k]->{"price:source"};
      $valuenum=substr($price_value,0, index($price_value,"/"));
      $valuedenom=substr($price_value, index($price_value,"/")+1,);
      $delta_days = Date_Cmp($last_post, $price_time);
      if ($delta_days < 0){ 
         $rows = $dbh->do ("insert into gncprice (priceguid, bookguid, commodity, currency, time, source, type, valuenum, valuedenom, version) values ('$priceguid', '$bookguid', '$commodity', '$currency', '$price_time', '$source', '$price_type', '$valuenum', '$valuedenom', '$version')");
       }
}
###
## transactions and splits
###
for (my $l=0; $l<$trn_anz; $l++) {
   $transguid=$data->{"gnc:book"}->{"gnc:transaction"}->[$l]->{"trn:id"}->{"content"};
   $date_entered=$data->{"gnc:book"}->{"gnc:transaction"}->[$l]->{"trn:date-entered"}->{"ts:date"};
   $date_posted=$data->{"gnc:book"}->{"gnc:transaction"}->[$l]->{"trn:date-posted"}->{"ts:date"};
   if (exists($data->{"gnc:book"}->{"gnc:transaction"}->[$l]->{"trn:num"})) {
      $num=$data->{"gnc:book"}->{"gnc:transaction"}->[$l]->{"trn:num"};
      }
   else {
      $num = '';
   }
   $description=$data->{"gnc:book"}->{"gnc:transaction"}->[$l]->{"trn:description"};
   $description=~ s/'/\\\'/g;
   $curr_space=$data->{"gnc:book"}->{"gnc:transaction"}->[$l]->{"trn:currency"}->{"cmdty:space"};   
   $curr_id=$data->{"gnc:book"}->{"gnc:transaction"}->[$l]->{"trn:currency"}->{"cmdty:id"};   
   $currency="$curr_space$cmdty_sep$curr_id";
   $delta_days = Date_Cmp($last_post, $date_entered);
   if ($delta_days < 0){
      $rows = $dbh->do ("insert into gnctransaction (transguid, date_entered, date_posted, num, description, currency, version) values ('$transguid', '$date_entered', '$date_posted', '$num', '$description', '$currency', '$version')");
##
# splits per transaction
##
     @split_count = @{$data->{"gnc:book"}->{"gnc:transaction"}->[$l]->{"trn:splits"}->{"trn:split"}}; 
     $split_anz = @split_count;
     for (my $m=0; $m<$split_anz; $m++) {
        $splitguid=$data->{"gnc:book"}->{"gnc:transaction"}->[$l]->{"trn:splits"}->{"trn:split"}->[$m]->{"split:id"}->{"content"};
        $accountguid=$data->{"gnc:book"}->{"gnc:transaction"}->[$l]->{"trn:splits"}->{"trn:split"}->[$m]->{"split:account"}->{"content"};
        $quantity=$data->{"gnc:book"}->{"gnc:transaction"}->[$l]->{"trn:splits"}->{"trn:split"}->[$m]->{"split:quantity"};
        $quantity=substr($quantity,0, index($quantity,"/"));
        $value=$data->{"gnc:book"}->{"gnc:transaction"}->[$l]->{"trn:splits"}->{"trn:split"}->[$m]->{"split:value"};
        $value=substr($value,0, index($value,"/"));
        $rows = $dbh->do ("insert into gncsplit (splitguid, accountguid, transguid, amount, value) values ('$splitguid', '$accountguid', '$transguid', '$quantity', '$value')");
     }
   }
}


###
# disconnect from database
###
my $rc  = $dbh->disconnect;


###
# write date/time to file to have the date of last update available
###
$new_date = ParseDate("today");
open(WRITEFILE, ">last_sql.txt");
print WRITEFILE $new_date;
close(WRITEFILE);

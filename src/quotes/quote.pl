#!/usr/bin/perl
#######################################################################
# $Id$
# Looks up investment prices on the web, and builds a report
# to summarize the results
#######################################################################
$HISTORYFILE="/home/cbbrowne/financial/histprices";
$newprices = "NO";  # Haven't found *any* new prices so far...

#######################################################################
# Start by initializing the security list
#######################################################################
# My TSE stocks...  
#######################################################################
&add_stock("T", 100, 1670.5, "TSE");
&add_stock("NVA", 100, 1345.5, "TSE");
&add_stock("PCA", 100, 1733, "TSE");
&add_stock("TOC", 50, 21.035*50, "TSE");
&add_stock("RY", 50, 32.785*50, "TSE");

#######################################################################
# My Canada Trust Everest funds...
#######################################################################
# Original numbers of units...
#&add_stock("CTMM", 83.951, 839.51, "CTE");  # Not strictly correct..
#&add_stock("CTBOND", 121.747, 1315.17, "CTE");
#&add_stock("CTSTK", 57.177, 832.73, "CTE");
#&add_stock("CTSPEC", 28.263, 504.07, "CTE");
#&add_stock("CTAMER", 177.723, 1983.05, "CTE");
#&add_stock("CTUSEQ", 19.644, 294.91, "CTE");
#&add_stock("CTASIA", 28.224, 251.99, "CTE");
#&add_stock("CTEURO", 60.136, 530.50, "CTE");
#&add_stock("CTEMER", 27.463, 266.67, "CTE");
#&add_stock("CTIBND", 84.879, 907.37, "CTE");

# Units as at Dec 31/97
&add_stock("CTMM", 87.033, 839.51, "CTE"); 
&add_stock("CTBOND", 131.818, 1315.17, "CTE");
&add_stock("CTSTK", 57.425, 832.73, "CTE");
&add_stock("CTSPEC", 28.263, 504.07, "CTE");
&add_stock("CTAMER", 206.301, 1983.05, "CTE");
&add_stock("CTUSEQ", 19.644, 294.91, "CTE");
&add_stock("CTASIA", 28.238, 251.99, "CTE");
&add_stock("CTEURO", 71.689, 530.50, "CTE");
&add_stock("CTEMER", 27.463, 266.67, "CTE");
&add_stock("CTIBND", 91.056, 907.37, "CTE");

# Stuff on NYSE
&add_stock("MOT", 15, (62+5/8)*15, "NYSE");
#&add_stock("IFMXE", 0.1, 0.1, "NYSE");
&add_stock("TSG", 13.0289, 43.02*5+43.75+44.48, "NYSE");


#######################################################################
# Working Ventures fund... Not currently updatable via the Web...
#######################################################################
&add_stock("WORKVE", 219.701, 3000, "OTHER");
$PRICE{"WORKVE"} = 3000/219.701;   # Hack in a price for Working Ventures
$OLD{"WORKVE"} = "yes";  # Indicate that this is *not* a "new" price...


#######################################################################
# Map CT Everest fund names to the "short" IDs 
#######################################################################
# I don't want to get long names; this builds an array that
# shortens the names used in the CT Everest Funds web page to
# the ones used above
#######################################################################
&ct_fund_ids;   # Get mapping of CT "long" fund names to "short" names

&load_prices;   # Load old prices, so that failure to get a price
                # means we fall back to the previously-found
                # price

$CURRNAME{"CDN"} = "Canadian";
$CURRNAME{"USD"} = "United States";
$CURRATE{"CDN"} = 1;
$CURRATE{"USD"} = 1.37;
&get_currency("USD");
#######################################################################
# Now, get the current prices by sundry queries of web sites
#######################################################################
&search_web_for_prices();   # Find new closing prices
&calc_variances();          # Calculate variances

#######################################################################
# Run report that details portfolio value based on the prices, costs
# and quantities of shares...
#######################################################################
if ($newprices eq "YES") {
    &show_report();     # Display a report summarizing the results...
#    &show_variances();
#    &hilo_portfolios();
    &save_prices();     # Save prices...
}
exit 0;   # Done

#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
######################## End of the main body #########################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

#######################################################################
#######################################################################
############################# Subroutines #############################
#######################################################################
#######################################################################


#######################################################################
############### &add_stock("NVA", 100, 1345.5, "TSE"); ################
#######################################################################
# Add a security to the "active list," including the "ticker symbol,"
# quantity of shares, original total cost, and the exchange to look it
# up on. 
#######################################################################
sub add_stock {
    local($ticker, $num, $cost, $exchange) = @_;
    $NUM{$ticker} = $num;
    $COST{$ticker} = $cost;
    $EXCHANGE{$ticker} = $exchange;
    local ($currency) = "CDN";
    if ($exchange eq "TSE") {
        $currency = "CDN";
    }
    if ($exchange eq "NYSE") {
        $currency = "USD";
    }
    if ($exchange eq "NY") {
        $currency = "USD";
    }
    $CURRENCY{$ticker} = $currency;
}

#######################################################################
######## &get_url_command("http://www.conline.com/~cbbrowne");#########
#######################################################################
# Build the appropriate command that accesses (in raw form) the
# requested URL.  Probably ought to change this to use the w3c
# "line mode" utility, as it's minscule
#######################################################################
sub get_url_command {
    local ($url) = @_;
    return "lynx -source '$url'";
}

#######################################################################
###################### &search_web_for_prices(); ######################
#######################################################################
# This program searches the %EXCHANGE array, determining how
# the security price should be searched out, and invokes the
# appropriate method.
#######################################################################
sub search_web_for_prices {
    local ($ticker);
    
    # Do some per-ticker searching
    foreach $ticker (keys EXCHANGE) {
	local($exchange) = $EXCHANGE{$ticker};
	if ($exchange eq "TSE") {
	    &get_TSE($ticker);
	    &get_yahoo($ticker, "TSE");
	}
      
	if ($exchange eq "CTE") {
	    # Do nothing; data is coming
	    # in en masse via &get_all_everest_rates;
	}
	if ($exchange eq "NYSE") {
	    &get_yahoo($ticker, $EXCHANGE{$ticker});
	}
    }
    
    # search for CT Everest fund data
    &get_CT_Everest;  # Get Everest mutual fund data
    
    # Search for Working Ventures fund data
    &get_WV;
    
    # get all of the TSE stock prices
    #    foreach $ticker (ord("a")..ord("z")) {
    #      &get_TSE( chr($ticker) );
    #  }
}

#######################################################################
########################## &get_TSE ("RY"); ###########################
#######################################################################
# Look up a stock's price on the TSE using the Telenium service
#######################################################################
sub get_TSE {
    local ($stock) = @_;
    local ($page) = "http://www.telenium.ca/TSE/" . 
	lc(
           substr($stock, 0, 1)
          ) 
        . ".html";
    if ($OLD{$stock} eq "NO!") {  # We've already priced this stock
	return;
    }
    local ($command) = &get_url_command($page) . " | grep ' $stock'";
    local ($line);
    open(GETPAGE, "$command |");
#    open(GETPAGE, "<a:/t~1.htm");
    while ($line = <GETPAGE>) {
	chop $line;
#	print $line, "\n";
	if ($line =~ /(\d*\.\d*)\D+\d+\S*\s+\d+\s+\S+\s+\d+\/\d+\s+(\S*)\s*$/) {
	    local($lp, $ls) = (sprintf("%.4f", $1), $2);
            
#	    print "Found $ls @ $lp\n";
            #... 21.3  21.35 21.2  21.2 -0.15   142300  193     02/14     T
            #... 19.4  19.5  19.2  19.35-0.1    687239  181     02/04     T
            #... 51.5  52    50.6  51.4  0     1527378  583     02/05     RY
	    
	    #... 1.18  1.2   1.1   1.2   0.02   44700   29      02/03     NBX
	    #                      ^^^
	    #                      Want this "close" value...
	    if ($lp < 0.001) {
		# Do nothing
	    } elsif ($ls eq $stock) {
		if ((($lp * 1.0) != $PRICE{$stock}) && ($price > 0)) {
		    $newprices = "YES";
		    $PRICE{$stock} = sprintf("%.4f", $price);
		    print "NEW PRICE: $stock, $lp\n";
		} else {
		    $price = $PRICE{$stock};
		}
		$PRICE{$stock} = sprintf("%.4f", $lp);
		$OLD{$stock} = "NO!";  # Indicate that this is an update...
	    }
	}
    }
    close GETPAGE;
}

#######################################################################
############################ &get_yahoo(); ############################
#######################################################################
# Look up a stock via "yahoo"
#######################################################################
sub get_yahoo {
    local ($stock, $exchange) = @_;
    local ($price);
    if (($exchange eq "NY") || ($exchange eq "NYSE")) {
	$exchange = "";
    }
    if ($exchange eq "TSE") {
	$exchange = ".TO";
    }
    local ($page) = "http://quote.yahoo.com/download/quotes.csv?symbols=" . 
           $stock . $exchange .
	       "&format=sl1d1t1c1ohgv&ext=.csv";
    if ($OLD{$stock} eq "NO!") {  # We've already priced this stock
	return;
    }
    local ($command) = &get_url_command($page);
    local ($line);
#    print $command, "\n";
    open(GETPAGE, "$command |");
    while ($line = <GETPAGE>) {
#	print "$line";
	local ($trash, $price)=split(/,/, $line);
#	print "$trash, $price\n";
#	print "Old price: ", $PRICE{$stock}, "\n";
	if ((($price * 1.0) != $PRICE{$stock}) && ($price > 0) ) {
	    $newprices = "YES";
	    print "NEW PRICE: $stock, $price\n";
	} else {
	    $price = $PRICE{$stock};
#	    print "Keep old price: $price\n";
	}
	$PRICE{$stock} = sprintf("%.4f", $price);
	$OLD{$stock} = "NO!";  # Indicate that this is an update...
#	print "Done - ", $stock, $PRICE{$stock}, $OLD{$stock}, "\n";
    }
}

#######################################################################
########################## &get_CT_Everest(); #########################
#######################################################################
# Look up all of the CT Everest mutual fund prices
#######################################################################
sub get_CT_Everest {
    local ($line, $fund, $value);
    local ($url) = "http://www.canadatrust.com/test/ctrates/EV.html";
    local ($command) = &get_url_command($url);
    open(FUNDS, "$command |");
    while ($line = <FUNDS>) {
	if ($line =~ /.*\s+\d+.\d+\s+\d+.\d+\s+.\d+.\d+/) {
#	    print "CT: $line";
	    $fund = substr($line, 0, 15);
#	    print "fund: [", $fund, "]  ID:[", $ID{$fund}, "]\n";
	    if ($ID{$fund} ne "") {
		$value = substr($line, 30, 11);
		if (($value*1.0) != $PRICE{$ID{$fund}}) {
		    $newprices = "YES";
		    print "NEW PRICE: $fund, $value\n";

		}	
		$PRICE{$ID{$fund}} = $value;
		$OLD{$ID{$fund}} = "NO!";  # Indicate that this is an update...
#		print "CT Price: $value ", $ID{$fund}, "\n";
	    }
	}
    }
    close FUNDS;
}

#######################################################################
############################## &get_WV(); #############################
#######################################################################
# Look up Working Ventures fund price
#######################################################################
sub get_WV {
    local ($url) = "http://www.workingventures.ca/main_cell.html";
    local ($command) = &get_url_command($url) . " | grep 'share price' ";
    open(FUNDS, "$command |");
    while (<FUNDS>) {
       if (/\D+\$(\d+\.\d+)\D+/) {
              # matches .... $13.66 ...
		if (($1 * 1.0) != $PRICE{"WORKVE"}) {
		    $newprices = "YES";
		    print "NEW PRICE: WV, $1\n";
		}	
		$PRICE{"WORKVE"} = sprintf("%.4f", $1);
		$OLD{"WORKVE"} = "NO!";  # Indicate that this is an update...
	}
    }
    close FUNDS;
}

#######################################################################
########################### &ct_fund_ids (); ##########################
#######################################################################
# Build an associative array that lets one take the names used in the
# CT mutual fund web page, and associate them with the short forms
# used as "ticker" codes
#######################################################################
sub ct_fund_ids {
    %ID = ("MONEY MARKET   ", "CTMM", 
	   "PREMIUM MMF    ", "CTPMM", 
	   "SHORT TERM BND ", "CTSTB", 
	   "MORTGAGE       ", "CTMTG", 
	   "BALANCED       ", "CTBAL", 
	   "BOND           ", "CTBOND", 
	   "DIV INCOME     ", "CTDIV", 
	   "STOCK          ", "CTSTK", 
	   "SPECIAL EQUITY ", "CTSPEC", 
	   "INT'L BOND     ", "CTIBND", 
	   "NORTH AMERICAN ", "CTNA",
	   "AMERIGROWTH    ", "CTAMER", 
	   "U.S. EQUITY    ", "CTUSEQ", 
	   "ASIAGROWTH     ", "CTASIA", 
	   "EUROGROWTH     ", "CTEURO", 
	   "GLOBALGROWTH   ", "CTGLOB", 
	   "INT'L EQUITY   ", "CTINTL", 
	   "EMERGING MKTS. ", "CTEMER"
	   );
}


#######################################################################
############################ &save_prices()############################
#######################################################################
# Dump data out to a file
#######################################################################
sub save_prices {
    local ($date) = `date '+%Y/%m/%d-%H:%M:%S'`;
    chop $date;
    local ($ticker, $price);
    open(HISTPRICES, ">>$HISTORYFILE");
    while ($ticker = each %PRICE) {
	if ($OLD{$ticker} eq "yes") {
	    # Don't dump it; we didn't get a new price...
	} else {
	    $key = $date . "" . $ticker;
	    $price = $PRICE{$ticker};
            print HISTPRICES $date, "|", $ticker, "|", $price, "\n";;
	}
    }
    close HISTPRICES;
}

#######################################################################
########################### &load_prices();############################
#######################################################################
# Preload prices, thus always keeping around the latest successfully
# located price.
# This means that if the web search process fails, we still do have 
# pricing, even if it's somewhat out of date...
#######################################################################
sub load_prices {
    local ($date) = `date '+%Y/%m/%d-%H:%M:%S'`;
    chop $date;
    local ($ticker, $price, $line);
    open(HISTPRICES, "<$HISTORYFILE");
    while ($line = <HISTPRICES>) {
	($date, $ticker, $price) = split(/\|/, $line);
	$PRICE{$ticker} = $price;
	$OLD{$ticker} = "yes"; # Indicate that this is an old price
                               # and thus not to be redumped
    }
    close HISTPRICES;
}   


#######################################################################
############################ &show_report();###########################
#######################################################################
# Look up all of the securities, and build a report listing the cost,
# value, and net profit/loss on each security, along with totals.
########################################################################
sub show_report {
    local($date) = `date`;
    chop $date;
    local($ticker, $shares, $price, $value, $cost, $profitloss);
    local ($tp, $tv, $tc);
format STDOUT_TOP =
                          Portfolio Valuation
                  as at @<<<<<<<<<<<<<<<<<<<<<<<<<<<
$date                          
Ticker     Shares  Recent Price   Value        Cost       Profit/Loss
------------------------------------------------------------------------




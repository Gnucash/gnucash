#
# FILE:
# getquotes.pl
#
# FUNCTION:
# perl utilities to fetch stock prices & volumes from various internet sources.
#
# HISTORY:
# Contributed by C.B.Browne circa March 1998
#
#######################################################################
######## &get_url_command("http://www.conline.com/~cbbrowne");#########
#######################################################################
# Build the appropriate command that accesses (in raw form) the
# requested URL.  Probably ought to change this to use any of
# -- the w3c "line mode" utility"
# -- curl
# -- lynx,
# -- urlget
# depending on which are installed.
#######################################################################
sub get_url_command {
    local ($url) = @_;
    return "lynx -source '$url'";
}


#######################################################################
###################### &get_quote_from_yahoo(); ######################
#######################################################################
# Look up a stock via "yahoo"
# usage: &get_yaho ("IBM", "NYSE");
# 
# returns values in associateive arrays:
# $last, $lastdate,$lasttime, $change,$opening,$hi, $lo, $volume
# adds the fetchs symbol to the indexed array $symbol
#
# sample usage:
# require "getquotes.pl";
# &get_quote_from_yahoo ("IBM", "NYSE");
# $sym = $symbol[0];
# printf "$sym last=$last{$sym} hi=$hi{$sym} lo=$lo{$sym} chg=$change{$sym} vol=$volume{$sym}\n";
#
#######################################################################
sub get_quote_from_yahoo {
    local ($stock, $exchange) = @_;
    local ($price);

    if (($exchange eq "NY") || ($exchange eq "NYSE")) {
	$exchange = "";
    }
    if ($exchange eq "TSE") {
	$exchange = ".TO";
    }
    
    # build URL for quote
    local ($page) = "http://quote.yahoo.com/download/quotes.csv?symbols=" . 
           $stock . $exchange .
	       "&format=sl1d1t1c1ohgv&ext=.csv";

    local ($command) = &get_url_command($page);
    # print "Will issue command: $command \n";

    local ($line);
    open(GETPAGE, "$command |");
    while ($line = <GETPAGE>) {
	#print "Received the following: $line";
	local ($sym,$slast,$slastdate,$slasttime,
               $schange,$sopening,$shi,$slo,$svolume)=split(/,/, $line);

        $symbol [$#symbol+1] = $sym;
        $last{$sym} = $slast;   
        $lastdate{$sym} = $slastdate;   
        $lastime{$sym} = $slastime;   
        $change{$sym} = $schange;   
        $opening{$sym} = $sopening;   
        $hi{$sym} = $shi;   
        $lo{$sym} = $slo;   
        $volume{$sym} = $svolume;   
    }
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
# TSE == toronoto stock exchange, primarily canadian stocks
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

1;


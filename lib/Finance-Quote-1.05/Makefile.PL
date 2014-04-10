#!/usr/bin/perl -w
use strict;

use ExtUtils::MakeMaker;

WriteMakefile( NAME         => "Finance::Quote",
               PREREQ_PM    => { "LWP::UserAgent" => 0,
                                 "HTTP::Request::Common" => 0,
				 "HTML::TableExtract" => 0},
               VERSION_FROM => "lib/Finance/Quote.pm"
             );

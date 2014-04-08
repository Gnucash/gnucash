Summary: A program to keep track of finances.
Name: xacc
Version: 1.0.17
Release: 1
Source: http://www.cs.hmc.edu/~rclark/xacc/download/xacc-1.0.17.tar.gz
Source1: xacc.wmconfig
URL: http://www.cs.hmc.edu/~rclark/xacc/
Group: Applications/Finance
Copyright: GPL

%description
X-Accountant is a program to keep track of your finances. Some of the 
features are: 

- Multiple accounts, which can be open at the same time. Create one
  xacc account for each of your bank accounts. 

- Each account keeps a running balance and a reconciled balance, so
  you can keep track of the checks that have cleared your account. 

- A simple interface. If you can use the register in the back of your
  checkbook, you can use xacc.  Automatic account reconciling.  At the 
  end of the month, open up the reconcile window, enter your bank 
  statement's ending balance, and check off the transactions that appear 
  in the bank statement. This makes it easy to track down any discrepancies. 

- QuickFill... if you begin typing in the description field, and the
  text matches a previous transaction, hitting TAB will copy that
  previous transaction. Handy if you have similar transactions on a
  regular basis.

- Stock/Mutual Fund Portfolios. Track stocks individually (one per
  account) or in portfolio of accounts (a group of accounts that can
  be displayed together).

- Quicken File Import. Import Quicken QIF files. 

%changelog

* Tue Feb 17 1998 Otto Hammersmith <otto@redhat.com>

- updated to 1.0.17, author hopes it will fix some problems with core dumps

* Mon Jan 26 1998 Otto Hammersmith <otto@redhat.com>

- built the package

%prep
%setup 

%build
./configure --prefix=/usr
make depend
make

%install
install -d /usr/share/xacc
install -m 755 xacc /usr/bin/xacc
install -m 755 xacc.bin /usr/bin/xacc.bin
cp -pr Docs /usr/share/xacc

install -d /etc/X11/wmconfig
install -m 644 -o root -g root $RPM_SOURCE_DIR/xacc.wmconfig /etc/X11/wmconfig/xacc

%files
%doc README TODO
/usr/bin/xacc
/usr/bin/xacc.bin
/usr/share/xacc
/etc/X11/wmconfig/xacc

Name: gnucash
Summary: GnuCash is an application to keep track of your finances.
Version: 1.1.20
Release: 1
Copyright: Free Software Foundation
Group: Applications/Finance
Source: ftp://ftp.gnucash.org/pub/gnucash/gnucash-1.1.20.tar.gz
Patch0: configure.in.patch
Packager: Eugene Kanter (eugene@bgs.com)

BuildRoot: /tmp/gnucash-build


%description
GnuCash is a personal finance manager.  A check-book like
register GUI allows you to enter and track bank accounts,
stocks, income and even currency trades.  The interface is
designed to be simple and easy to use, but is backed with
double-entry accounting principles to ensure balanced books.


%prep
%setup
%patch -p1

%build
mkdir -p share
autoconf
X_LIBS=-lXp ./configure --prefix=/usr
make motif


%install
make prefix=$RPM_BUILD_ROOT/usr install


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(444,root,root,755)
%attr(555,-,-) /usr/bin/gnucash.motif
/usr/bin/gnucash
/usr/doc/gnucash
/usr/etc
/usr/share/gnucash

%doc README CHANGES TODO

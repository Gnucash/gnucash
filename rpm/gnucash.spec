Name: gnucash
Summary: GnuCash is an application to keep track of your finances.
Version: 1.3.0
Release: 1
Copyright: Free Software Foundation
Group: Applications/Finance
Source: http://www.gnucash.org/pub/gnucash/sources/stable/gnucash-%{PACKAGE_VERSION}.tar.gz
Packager: Dave Peticolas <peticola@cs.ucdavis.edu>
BuildRoot: /var/tmp/gnucash-%version


%description
GnuCash is a personal finance manager. A check-book like
register GUI allows you to enter and track bank accounts,
stocks, income and even currency trades. The interface is
designed to be simple and easy to use, but is backed with
double-entry accounting principles to ensure balanced books.


%prep
%setup -q


%build
./configure --prefix=/usr --sysconfdir=/etc
make gnome


%install
rm -rf $RPM_BUILD_ROOT
make prefix=$RPM_BUILD_ROOT/usr sysconfdir=$RPM_BUILD_ROOT/etc install


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(444,root,root,755)
%attr(555,-,-) /usr/bin/gnucash.gnome
/usr/bin/gnucash
/usr/bin/gnc-prices
/usr/lib/gnucash
/usr/share/gnucash
/usr/share/gnome/apps/Applications/gnucash.desktop
%doc /usr/doc/gnucash

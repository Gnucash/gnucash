# Copyright 1999-2006 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header$

# This script should work fine for the whole 1.9.x (and hopefully 2.0.x)
# releases with a simple rename. See
# http://bugs.gentoo.org/show_bug.cgi?id=122337 for discussion and history
# about this file.  

# -- jsled-gentoo@asynchronous.org

inherit eutils gnome2 

DOC_VER="1.9.0"

DESCRIPTION="A personal finance manager (unstable version)."
HOMEPAGE="http://www.gnucash.org/"
SRC_URI="mirror://sourceforge/gnucash/${P}.tar.gz
         mirror://sourceforge/gnucash/gnucash-docs-${DOC_VER}.tar.gz"
LICENSE="GPL-2"
SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE="postgres ofx hbci chipcard doc debug quotes"
# mt940

RDEPEND=">=dev-libs/glib-2.4.0
	>=dev-util/guile-1.6
	amd64? ( >=dev-util/guile-1.6.4-r2 )
	>=dev-libs/slib-2.3.8
	>=sys-libs/zlib-1.1.4
	>=dev-libs/popt-1.5
	>=x11-libs/gtk+-2.4
	>=gnome-base/libgnomeui-2.4
	>=gnome-base/libgnomeprint-2.10
	>=gnome-base/libgnomeprintui-2.10
	>=gnome-base/libglade-2.4
	>=gnome-extra/gtkhtml-3.6
	>=dev-libs/libxml2-2.5.10
	>=dev-libs/g-wrap-1.3.4
	>=gnome-base/gconf-2
	>=app-text/scrollkeeper-0.3
	>=x11-libs/goffice-0.0.4
	doc? ( app-doc/doxygen )
	ofx? ( >=dev-libs/libofx-0.7.0 )
	hbci? ( net-libs/aqbanking
		chipcard? ( sys-libs/libchipcard )
	)
	quotes? ( dev-perl/DateManip
		dev-perl/Finance-Quote
		dev-perl/HTML-TableExtract )
	postgres? ( dev-db/postgresql )
	app-text/docbook-xsl-stylesheets
	=app-text/docbook-xml-dtd-4.1.2*"

DEPEND="${RDEPEND}
	dev-util/pkgconfig
	nls? ( sys-devel/gettext )"

pkg_setup() {
	built_with_use libgsf gnome || die "libgsf must be built with gnome"
	built_with_use goffice gnome || die "goffice must be built with gnome"
}

src_compile() {
	local myconf_warnings="--enable-error-on-warning --enable-compile-warnings"
	econf \
		$(use_enable debug) \
		$(use_enable postgres sql) \
		$(use_enable ofx) \
		$(use_enable doc doxygen) \
		$(use_enable hbci) \
		${myconf_warnings} \
			|| die "econf failed"
	emake -j1 || die "emake failed"

    cd ${WORKDIR}/gnucash-docs-${DOC_VER}
    econf || die "doc econf failed"
    emake || die "doc emake failed"
}

# See http://bugs.gentoo.org/show_bug.cgi?id=132862 regarding gconf schema install

src_install() {
	gnome2_src_install || die "gnome2_src_install failed"
	dodoc AUTHORS ChangeLog* DOCUMENTERS HACKING INSTALL LICENSE NEWS TODO README* doc/README*
	make_desktop_entry ${P} "GnuCash ${PV}" \
		/usr/share/${PN}/pixmaps/appicon.png "Office;Finance"

	cd ${WORKDIR}/${PN}-docs-${DOC_VER}
	make DESTDIR=${D} \
		scrollkeeper_localstate_dir=${D}/var/lib/scrollkeeper \
		install || die "doc install failed"
	rm -rf ${D}/var/lib/scrollkeeper
}

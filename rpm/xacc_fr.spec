Summary: Un programme de suivi de vos finances personnelles.
Name: xacc
Version: 1.0.18
Release: 1
Source: ftp://ftp.gnucash.org/pub/xacc/xacc_fr-1.0.18.tar.gz
Source1: xacc.wmconfig
URL: http://www.gnucash.org/xacc/
Group: Applications/Finance
Copyright: GPL

%description
X-Accountant est un programme de suivis de vos finances personnelles.
Quelques une de ces fonctions sont :

- Des comptes multiples, qui peuvent etre ouvert en meme temps.Creez un
  compte xacc pour chacun de vos compptes bancaires.

- Chaque compte conserve un solde actuel et un solde de rapprochement,
  alors vous pouvez suivre les verifications qui ont ete pointes sur
  votre compte.

- Une interface simple. Si vous savez utiliser le talon de votre
  chequier, vous savez utiliser xacc. Rapprochement automatique des
  comptes. A la fin du mois, ouvrez la fenetre de rapprochement,
  entrez le solde final de votre releve de compte et pointez les
  transactions qui apparaissent sur votre releve de compte.Cela rend
  facile le depistage de n'importe quelle discordance.   

- QuickFill (saisie rapide)... lorsque vous commencez une saisie 
  dans les champs de description , s'il retrouve une precedente transaction,
  il vous la propose,frappez <TAB> copiera alors la precedente transaction. 
  Commode si vous avez des transactions semblables assez regulierement .

- Portefeuille d'actions/de societes d'investissements (n.d.t: pour ces 
  dernieres FCP et SICAV en France). Suivis d'actions individuellement
  (une par compte)ou dans un portefeuille de comptes (un groupe de comptes
  qui peuvent etre affiches ensemble ).
  
- Importation des fichiers QIF de Quicken .

%changelog

* Dim 14 Jun 1998 Linas Vepstas <linas@linas.org>

- mise a jour vers 1.0.18, diverses corrections, quelques plantages


* Mar 17 Fev 1998 Otto Hammersmith <otto@redhat.com>

- mise a jour vers 1.0.17, l'auteur espere qu'il fixera quelques
  problemes avec les plantages.

* Lun 26 Jan 1998 Otto Hammersmith <otto@redhat.com>

- construction du paquet

%prep
%setup 

%build
./configure --prefix=/usr
make depend
make
make static

%install
install -d /usr/share/xacc
install -m 755 xacc /usr/bin/xacc
install -m 755 xacc.bin /usr/bin/xacc.bin
install -m 755 xacc-static.bin /usr/bin/xacc-static.bin
cp -pr Docs /usr/share/xacc

install -d /etc/X11/wmconfig
install -m 644 -o root -g root $RPM_SOURCE_DIR/xacc.wmconfig /etc/X11/wmconfig/xacc

%files
%doc README TODO CHANGES COPYING README.francais LISEZ.MOI
/usr/bin/xacc
/usr/bin/xacc.bin
/usr/bin/xacc-static.bin
/usr/share/xacc
/etc/X11/wmconfig/xacc

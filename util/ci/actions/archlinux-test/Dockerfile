from archlinux:latest

run echo "NoExtract = !*locale*/fr*/* !usr/share/i18n/locales/fr_FR*" >> /etc/pacman.conf

run pacman -Syu --quiet --noconfirm glibc gcc cmake make boost python3 pkg-config gettext gtk3 guile git ninja gtest gmock sqlite3 webkit2gtk swig gwenhywfar aqbanking intltool libxslt libofx postgresql-libs libmariadbclient libdbi libdbi-drivers wayland-protocols > /dev/null

run echo en_US.UTF-8 UTF-8 > /etc/locale.gen
run echo en_GB.UTF-8 UTF-8 >> /etc/locale.gen
run echo fr_FR.UTF-8 UTF-8 >> /etc/locale.gen
run locale-gen
copy entrypoint.sh /
entrypoint /entrypoint.sh

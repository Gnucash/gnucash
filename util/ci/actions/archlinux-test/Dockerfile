from archlinux:latest

run echo "NoExtract = !*locale*/fr*/* !usr/share/i18n/locales/fr_FR*" >> /etc/pacman.conf

run pacman -Syu --noconfirm glibc gcc cmake make boost boost-libs python3 pkg-config gettext gtk4 guile git ninja gtest gmock sqlite3 webkitgtk-6.0 swig gwenhywfar aqbanking intltool libxslt libofx postgresql-libs libmariadbclient libdbi libdbi-drivers wayland-protocols xorg-server-xvfb dbus

run dbus-uuidgen --ensure=/etc/machine-id

run echo en_US.UTF-8 UTF-8 > /etc/locale.gen
run echo en_GB.UTF-8 UTF-8 >> /etc/locale.gen
run echo fr_FR.UTF-8 UTF-8 >> /etc/locale.gen
run locale-gen

copy entrypoint.sh /
entrypoint /entrypoint.sh

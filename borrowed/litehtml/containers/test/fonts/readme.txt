Note: Fonts in this directory are not automatically picked up, you have to update Font::installed_fonts.

Terminus Font:
Sizes: 6x12, 8x14, 8x16, 10x18, 10x20, 11x22, 12x24, 14x28, 16x32.
https://terminus-font.sourceforge.net
https://sourceforge.net/projects/terminus-font/files/terminus-font-4.49/terminus-font-4.49.1.tar.gz

bdf -> yaff conversion:
https://github.com/robhagemans/monobit
pip install monobit
monobit-convert ter-u14b.bdf to ter-u14b.yaff
then manually removed non-ascii chars

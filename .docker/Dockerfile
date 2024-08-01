# syntax=docker/dockerfile:1

FROM python:3.11-bookworm

RUN apt update
RUN apt install -y python3-gnucash gnucash libdbd-sqlite3
RUN cp -r /usr/lib/python3/dist-packages/gnucash /usr/local/lib/python3.11/site-packages/
RUN python3.11 -c "import gnucash"
RUN pip install pytest

WORKDIR /opt/gnucash-program



if USE_LIBQOF
SUBDIRS = qsf file ${SQL_DIR} ${RPC_DIR}
DIST_SUBDIRS = qsf file net postgres rpc
else
SUBDIRS = file ${SQL_DIR} ${RPC_DIR}
DIST_SUBDIRS = qsf file net postgres rpc
endif

noinst_HEADERS = gnc-backend-api.h

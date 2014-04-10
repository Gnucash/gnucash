# RPC library Makefile.am file.
#  Created By:	Derek Atkins <warlord@MIT.EDU>

# The GNC RPC Backend
pkglib_LTLIBRARIES = libgncmod-backend-rpc.la

libgncmod_backend_rpc_la_SOURCES = \
  gncmod-backend-rpc.c \
  RpcBackend.c	\
  RpcServer.c \
  RpcSock.c \
  RpcUtils.c \
  gncRpc_xdr.c \
  gncRpc_clnt.c \
  gncRpc_svc.c \
  gncRpc_server.c \
  clnt_thrd.c \
  svc_thrd.c \
  xprt_thrd.c

libgncmod_backend_rpc_la_LDFLAGS = -version-info 0:0:0

noinst_HEADERS = \
  RpcBackend.h \
  RpcServer.h \
  RpcServerP.h \
  RpcSock.h \
  RpcUtils.h \
  gncRpc.h \
  clnt_thrd.h \
  svc_thrd.h \
  xprt_thrd.h

RPCGEN_FILES = \
  gncRpc.h \
  gncRpc_xdr.c \
  gncRpc_clnt.c \
  gncRpc_svc.c

EXTRA_DIST = \
  .cvsignore \
  README \
  $(RPCGEN_SRCS)

LDADD = -lpthread

AM_CFLAGS = \
  -I.. -I../../engine -I../../gnc-module ${GLIB_CFLAGS} -DGNCACCT_COMMODITY

#RPCGEN=rpcgen -M
RPCGEN=rpcgen -DGNCACCT_COMMODITY

RPCGEN_SRCS = gncAccount.x gncCommodity.x gncKVP.x gncGUID.x \
	gncPrice.x gncQuery.x gncRpc.x gncSplit.x gncTxn.x

#gncRpc.h: $(RPCGEN_SRCS)
#	$(RM) $@
#	$(RPCGEN) -h -o $@ gncRpc.x

gncRpc_xdr.c: $(RPCGEN_SRCS)
	$(RM) $@
	$(RPCGEN) -c -o $@ gncRpc.x

#gncRpc_clnt.c: $(RPCGEN_SRCS)
#	$(RM) $@
#	$(RPCGEN) -l -o $@ gncRpc.x

#gncRpc_svc.c: $(RPCGEN_SRCS)
#	$(RM) $@
#	$(RPCGEN) -m -o $@ gncRpc.x

#gncRpc_server_stubs.c: $(RPCGEN_SRCS)
#	$(RPCGEN) -Ss -o $@ gncRpc.x

rpcgen: $(RPCGEN_FILES)

# Make sure the system knows to run rpcgen first
$(libgncmod_backend_rpc_la_SOURCES): gncRpc.h

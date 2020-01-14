set(ACCOUNTS_INSTALL_DIR ${CMAKE_INSTALL_DATADIR}/gnucash/accounts)
set(ACCOUNTS_BUILD_DIR ${DATADIR_BUILD}/gnucash/accounts)



add_subdirectory(C)
add_subdirectory(ca)
add_subdirectory(cs)
add_subdirectory(da)
add_subdirectory(de_AT)
add_subdirectory(de_CH)
add_subdirectory(de_DE)
add_subdirectory(el_GR)
add_subdirectory(en_GB)
add_subdirectory(en_IN)
add_subdirectory(es_ES)
add_subdirectory(es_MX)
add_subdirectory(fi_FI)
add_subdirectory(fr_BE)
add_subdirectory(fr_CA)
add_subdirectory(fr_CH)
add_subdirectory(fr_FR)
add_subdirectory(he)
add_subdirectory(hr)
add_subdirectory(hu)
add_subdirectory(it)
add_subdirectory(ja)
add_subdirectory(ko)
add_subdirectory(lt)
add_subdirectory(lv)
add_subdirectory(nb)
add_subdirectory(nl)
add_subdirectory(pl)
add_subdirectory(pt_BR)
add_subdirectory(pt_PT)
add_subdirectory(ru)
add_subdirectory(sk)
add_subdirectory(sv_AX)
add_subdirectory(sv_FI)
add_subdirectory(sv_SE)
add_subdirectory(tr_TR)
add_subdirectory(zh_CN)
add_subdirectory(zh_HK)
add_subdirectory(zh_TW)

set_local_dist(dist_list CMakeLists.txt )

set(accounts_DIST ${C_DIST} ${CA_DIST} ${CS_DIST} ${DA_DIST} ${DE_AT_DIST} ${DE_CH_DIST} ${DE_DE_DIST} ${EL_GR_DIST}
                  ${EN_GB_DIST} ${EN_IN_DIST} ${ES_ES_DIST} ${ES_MX_DIST} ${FI_FI_DIST} ${FR_BE_DIST} ${FR_CA_DIST}
                  ${FR_CH_DIST} ${FR_FR_DIST} ${HE_DIST} ${HR_DIST} ${HU_DIST} ${IT_DIST} ${JA_DIST} ${KO_DIST} ${LT_DIST}
                  ${LV_DIST} ${NB_DIST} ${NL_DIST} ${PL_DIST} ${PT_BR_DIST} ${PT_PT_DIST} ${RU_DIST} ${SK_DIST}
                  ${SV_AX_DIST} ${SV_FI_DIST} ${SV_SE_DIST} ${TR_TR_DIST} ${ZH_CN_DIST}
                  ${ZH_HK_DIST} ${ZH_TW_DIST} ${dist_list} PARENT_SCOPE)


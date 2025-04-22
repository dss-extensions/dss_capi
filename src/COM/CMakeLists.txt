#TODO
# DSSExtensions.tlb

set(MIDL_OUTPUT
    ${CMAKE_CURRENT_BINARY_DIR}/IFace.h
    ${CMAKE_CURRENT_BINARY_DIR}/GUIDS.c
)
set(MIDL_FILE
    ${CMAKE_CURRENT_LIST_DIR}/DSSExtensions.idl
)
add_custom_command(
    OUTPUT ${MIDL_OUTPUT}
    COMMAND midl /h IFace.h /iid GUIDS.c /proxy Proxy.c /dlldata ProxyDll.c ${MIDL_FILE}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${MIDL_FILE}
    MAIN_DEPENDENCY ${MIDL_FILE}
    VERBATIM
)
target_sources(${PROJECT_NAME} PRIVATE ${MIDL_FILE} ${MIDL_OUTPUT})
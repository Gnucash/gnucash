# Extract the parameters set in _VCS_INFO_FILE and export them as environment variables

# _VCS_INFO_FILE should be the full path to gnc-vcs-info.h

function (versioninfo2env _VCS_INFO_FILE)
    file(STRINGS ${_VCS_INFO_FILE} lines REGEX "#define")
    foreach(line ${lines})
        string(REGEX REPLACE "^.* (.*) \"(.*)\"" "\\1;\\2" _param_val ${line})
        list(GET _param_val 0 _param)
        list(GET _param_val 1 _val)
        set(${_param} ${_val} PARENT_SCOPE)
    endforeach()
endfunction()

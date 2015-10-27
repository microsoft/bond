set_property (GLOBAL PROPERTY USE_FOLDERS On)

#
# Add the target to "current" folder
#
function (add_target_to_folder target)
    set_property (GLOBAL APPEND PROPERTY bond_targets ${target})
endfunction()

#
# add_subfolder (subdirectory folder)
# similar to built-in add_subdirectory but groups targets in specified folder
#
macro (add_subfolder dir folder)
    set_property (GLOBAL PROPERTY bond_targets)
    add_subdirectory (${dir})
    get_property (targets GLOBAL PROPERTY bond_targets)
    set_target_properties (${targets} PROPERTIES FOLDER ${folder})
endmacro()


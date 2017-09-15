# Top-level meta-target to build the entire Java world. Implicitly depends on
# all targets created by add_gradle_build.
add_custom_target (java)

add_subdirectory (gradle-plugin)
add_subdirectory (core)
add_subdirectory (compat)

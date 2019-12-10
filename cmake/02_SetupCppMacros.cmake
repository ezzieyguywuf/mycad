# Note: this is a macro on purpose .This way, it gets expanded in the calling scope, and
# as such any variables described here will exist in the calling scope.
#
# This is done in order to eliminate the need for `set(MyVar MyVal PARENT_SCOPE)`, which
# can get cumbersome and difficult to do well when we're doing things like using
# `add_compile_definitions`
macro(SetupCppMacros)
    add_compile_definitions(MYCAD_VERSION="${PROJECT_VERSION}")
endmacro()

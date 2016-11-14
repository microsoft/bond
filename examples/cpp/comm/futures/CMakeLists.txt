add_bond_test (futures futures.bond futures.cpp COMM)

# TODO: needed for lock, should be abstracted away in platform specific stuff
target_compile_definitions (futures PRIVATE -D_WIN32_WINNT=0x0600 -D_ENABLE_ATOMIC_ALIGNMENT_FIX)

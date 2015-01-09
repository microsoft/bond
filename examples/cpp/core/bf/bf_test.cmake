function (run)
    execute_process (
        COMMAND ${ARGV}
        RESULT_VARIABLE error)
    if (error)
        message (FATAL_ERROR)
    endif()
endfunction()

run (${BF} ${COMPAT_DATA}/compat.simple2.dat --to=compact2 --output=bf.compact2.dat --from=simple2 --schema=${COMPAT_DATA}/compat.schema.dat)
run (${BF} bf.compact2.dat                   --to=fast     --output=bf.fast.dat     --from=compact2)
run (${BF} bf.fast.dat                       --to=compact  --output=bf.compact.dat)
run (${BF} bf.compact.dat                    --to=simple   --output=bf.simple.dat   --schema=${COMPAT_DATA}/compat.schema.dat)

run (${BOND_COMPAT} simple -d bf.simple.dat expected.bf.simple deserialized.bf.simple)

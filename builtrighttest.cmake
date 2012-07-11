##execute_process(COMMAND ${TEST_PROG}
##                RESULT_VARIABLE HAD_ERROR)

##execute_process(COMMAND ${CMAKE_COMMAND} -P builtrighttest.sh)
##                RESULT_VARIABLE HAD_ERROR)

##if(HAD_ERROR)
##    message(FATAL_ERROR "Test failed")
##endif()

## this test simply runs dfsim to output 
## because this process cannot easily redirect the 
## output from within here, you have to create a 
## temp script that generates the output.
execute_process(COMMAND sh ./builtrighttest.sh)

execute_process(COMMAND ${CMAKE_COMMAND} -E compare_files
    output.txt ${SOURCEDIR}/expected.txt
    RESULT_VARIABLE DIFFERENT)
if(DIFFERENT)
    message(FATAL_ERROR "Test failed - files differ")
endif()



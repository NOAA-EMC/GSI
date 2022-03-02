add_executable(ndate.x ndate.f)
target_link_libraries(ndate.x PRIVATE bacio::bacio_4)
target_link_libraries(ndate.x PRIVATE w3emc::w3emc_4)

# Install executable targets
install(TARGETS ndate.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})


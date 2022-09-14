# Include the iso_varying_string project
if(NOT TARGET "iso_varying_string")
  if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/iso_varying_string/CMakeLists.txt)
    add_subdirectory("iso_varying_string")
  else()
    set("iso_varying_string-url" "https://gitlab.com/everythingfunctional/iso_varying_string")
    message(STATUS "Retrieving iso_varying_string from ${iso_varying_string-url}")
    include(FetchContent)
    FetchContent_Declare(
      "iso_varying_string"
      GIT_REPOSITORY "${iso_varying_string-url}"
      GIT_TAG "v3.0.3"
    )
    FetchContent_MakeAvailable("iso_varying_string")
  endif()
endif()

list(
  APPEND lib-deps
  "iso_varying_string"
)

# Include the exceptional_numbers project
if(NOT TARGET "exceptional_numbers")
  if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/exceptional_numbers/CMakeLists.txt)
    add_subdirectory("exceptional_numbers")
  else()
    set("exceptional_numbers-url" "https://gitlab.com/everythingfunctional/exceptional_numbers")
    message(STATUS "Retrieving exceptional_numbers from ${exceptional_numbers-url}")
    include(FetchContent)
    FetchContent_Declare(
      "exceptional_numbers"
      GIT_REPOSITORY "${exceptional_numbers-url}"
      GIT_TAG "v0.1.1"
    )
    FetchContent_MakeAvailable("exceptional_numbers")
  endif()
endif()

list(
  APPEND lib-deps
  "exceptional_numbers"
)

set(lib-deps "${lib-deps}" PARENT_SCOPE)

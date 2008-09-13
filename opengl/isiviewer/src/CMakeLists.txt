# Make sure the compiler can find include files from the trackball library.
INCLUDE_DIRECTORIES(${ISIVIEWER_SOURCE_DIR}/lib)

# Make sure the linker can find the trackball library once it is built.
LINK_DIRECTORIES(${ISIVIEWER_BINARY_DIR}/lib)

# Add executable called "isiviewer" that is built from the source files
# "isiviewer.c" and "object.c".  The extensions are automatically found.
ADD_EXECUTABLE(isiviewer isiviewer.c objects.c )

# Link the executable to the trackball library.
TARGET_LINK_LIBRARIES(isiviewer LibTrackball)

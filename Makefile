CFLAGS=-Wall -mavx2 -msse2 -msse4.1 -mpclmul -O3
SOURCE="src/main/c/hwsl2_HWSL2.c"
JNIHEADERS="-I/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk/System/Library/Frameworks/JavaVM.framework/Versions/A/Headers"

.PHONY: libhwsl2.so libhwsl2.dylib clean

all: libhwsl2.so libhwsl2.dylib

libhwsl2.so:
	clang $(CFLAGS) $(SOURCE) $(JNIHEADERS) -shared -o libhwsl2.so

libhwsl2.dylib:
	clang $(CFLAGS) $(SOURCE) $(JNIHEADERS) -dynamiclib -o libhwsl2.dylib

clean:
	-rm -f libhwsl2.so
	-rm -f libhwsl2.dylib

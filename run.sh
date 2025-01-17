
cmake --build ./build;
./build/EvaLang;
# lli 
clang++ -O3 -I/usr/include/gc ./output.ll /usr/lib/x86_64-linux-gnu/libgc.a -o eva;
./eva;
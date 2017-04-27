echo "Building Shrdlu..."
mkdir build && cd build
ocamlc -o shrdlu ../src/main.ml
mv shrdlu ..
cd ..
rm -r build
echo "Done"

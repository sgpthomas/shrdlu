echo "Building Shrdlu..."
mkdir build && cd build
cp -r ../src .
ocamlc -o shrdlu src/main.ml
mv shrdlu ..
cd ..
rm -r build
echo "Done"

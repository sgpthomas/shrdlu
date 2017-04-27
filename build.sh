echo "Building Shrdlu..."
IFS=$'\n' read -d '' -r -a lines < build.list
echo "Using Sources:"
printf ' * %s\n' "${lines[@]}"
mkdir build && cd build
cp -r ../src .
ocamlc -o shrdlu ${lines[@]}
mv shrdlu ..
cd ..
rm -r build
echo "Done"

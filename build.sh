echo "Building Shrdlu..."

# getting sources from build.list
IFS=$'\n' read -d '' -r -a lines < build.list
echo "Using Sources:"
printf ' * %s\n' "${lines[@]}"

make build folder, copy src into it, and run build
mkdir build && cd build
cp -r ../src .
ocamlc -o shrdlu ${lines[@]}

# move executable to main directory, clean build
mv shrdlu ..
cd ..
rm -r build

echo "Done"

mv Cargo.toml Cargo.desktop.toml
mv Cargo.wasm.toml Cargo.toml

wasm-pack build

rm -r skiff-lang
mv pkg skiff-lang

mv Cargo.toml Cargo.wasm.toml
mv Cargo.desktop.toml Cargo.toml
echo ""
echo "Package built"
echo "Remember to add snippets directory"
echo "'wasm-pack publish' to publish"

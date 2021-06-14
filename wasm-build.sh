mv Cargo.toml Cargo.desktop.toml
mv Cargo.wasm.toml Cargo.toml
~/.cargo/bin/wasm-pack build
mv Cargo.toml Cargo.wasm.toml
mv Cargo.desktop.toml Cargo.toml
echo ""
echo "Package built"
echo "Remember to update name, version, and add snippets directory"
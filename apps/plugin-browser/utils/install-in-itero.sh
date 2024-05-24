# Install dependencies in the Itero Plasmo build environment
curl https://sh.rustup.rs -sSf | sh -s -- -y
. "$HOME/.cargo/env"
yarn config set ignore-engines true
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
yarn

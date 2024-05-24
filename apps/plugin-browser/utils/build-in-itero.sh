# Build in the Itero environment
apt update
apt install default-jre -y
curl https://sh.rustup.rs -sSf | sh -s -- -y
. "$HOME/.cargo/env"
yarn config set ignore-engines true
npx turbo build --filter @apps/plugin-browser

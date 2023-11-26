class RipgrepBin < Formula
  version '13.0.0'
  desc "Recursively search directories for a regex pattern."
  homepage "https://github.com/BurntSushi/ripgrep"

  if OS.mac?
      url "https://github.com/BurntSushi/ripgrep/releases/download/#{version}/ripgrep-#{version}-x86_64-apple-darwin.tar.gz"
      sha256 "d60e1df731119e742c4ca4f30655b697250d3b41153a6e80b925d7b9f992a9aa"
  elsif OS.linux?
      url "https://github.com/BurntSushi/ripgrep/releases/download/#{version}/ripgrep-#{version}-x86_64-unknown-linux-musl.tar.gz"
      sha256 "a18e0b611f9d9976b11ab8c3355a92c68c8bd1965b0d99366777751343e33938"
  end

  conflicts_with "ripgrep"

  def install
    bin.install "rg"
    man1.install "doc/rg.1"

    bash_completion.install "complete/rg.bash"
    zsh_completion.install "complete/_rg"
  end
end

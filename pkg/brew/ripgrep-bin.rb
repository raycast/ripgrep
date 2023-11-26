class RipgrepBin < Formula
  version '14.0.1'
  desc "Recursively search directories for a regex pattern."
  homepage "https://github.com/BurntSushi/ripgrep"

  if OS.mac?
      url "https://github.com/BurntSushi/ripgrep/releases/download/#{version}/ripgrep-#{version}-x86_64-apple-darwin.tar.gz"
      sha256 "927f3f02929ded0bae21e8a93283b5466c8807b38cea94a96bbec0acc6a22786"
  elsif OS.linux?
      url "https://github.com/BurntSushi/ripgrep/releases/download/#{version}/ripgrep-#{version}-x86_64-unknown-linux-musl.tar.gz"
      sha256 "e0ca32aabfc3426c00201301fd258c7da2b18431af4edac715c56da5e4326538"
  end

  conflicts_with "ripgrep"

  def install
    bin.install "rg"
    man1.install "doc/rg.1"

    bash_completion.install "complete/rg.bash"
    zsh_completion.install "complete/_rg"
  end
end

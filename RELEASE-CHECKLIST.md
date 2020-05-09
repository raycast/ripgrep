Release Checklist
-----------------
* Run `cargo update` and review dependency updates. Commit updated
  `Cargo.lock`.
* Run `cargo outdated` and review semver incompatible updates. Unless there is
  a strong motivation otherwise, review and update every dependency.
* Review changes for every crate in `crates` since the last ripgrep release.
  If the set of changes is non-empty, issue a new release for that crate. Check
  crates in the following order. After updating a crate, ensure minimal
  versions are updated as appropriate in dependents. If an update is required,
  run `cargo-up --no-push crates/{CRATE}/Cargo.toml`.
    * crates/globset
    * crates/ignore
    * crates/cli
    * crates/matcher
    * crates/regex
    * crates/pcre2
    * crates/searcher
    * crates/printer
    * crates/grep (bump minimal versions as necessary)
    * crates/core (do **not** bump version, but update dependencies as needed)
* Edit the `Cargo.toml` to set the new ripgrep version. Run
  `cargo update -p ripgrep` so that the `Cargo.lock` is updated. Commit the
  changes. Alternatively, use
  `cargo-up --no-push --no-release Cargo.toml {VERSION}`.
* Create a new signed tag for the ripgrep release. Push it to GitHub.
* Wait for CI to finish creating the release. If the release build fails, then
  delete the tag from GitHub, make fixes, re-tag, delete the release and push.
* Copy the relevant section of the CHANGELOG to the tagged release notes.
* Run `ci/build-deb` locally and manually upload the deb package to the
  release.
* Run `cargo publish`.
* Run `ci/sha256-releases >> pkg/brew/ripgrep-bin.rb`. Then edit
  `pkg/brew/ripgrep-bin.rb` to update the version numbers and sha256 hashes.
  Remove extraneous stuff added by `ci/sha256-releases`. Commit changes.

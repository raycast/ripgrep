use std::{ffi::OsStr, path::Path};

use crate::walk::DirEntry;

/// Returns true if and only if this entry is considered to be hidden.
///
/// This only returns true if the base name of the path starts with a `.`.
///
/// On Unix, this implements a more optimized check.
#[cfg(unix)]
pub(crate) fn is_hidden(dent: &DirEntry) -> bool {
    use std::os::unix::ffi::OsStrExt;

    if let Some(name) = file_name(dent.path()) {
        name.as_bytes().get(0) == Some(&b'.')
    } else {
        false
    }
}

/// Returns true if and only if this entry is considered to be hidden.
///
/// On Windows, this returns true if one of the following is true:
///
/// * The base name of the path starts with a `.`.
/// * The file attributes have the `HIDDEN` property set.
#[cfg(windows)]
pub(crate) fn is_hidden(dent: &DirEntry) -> bool {
    use std::os::windows::fs::MetadataExt;
    use winapi_util::file;

    // This looks like we're doing an extra stat call, but on Windows, the
    // directory traverser reuses the metadata retrieved from each directory
    // entry and stores it on the DirEntry itself. So this is "free."
    if let Ok(md) = dent.metadata() {
        if file::is_hidden(md.file_attributes() as u64) {
            return true;
        }
    }
    if let Some(name) = file_name(dent.path()) {
        name.to_str().map(|s| s.starts_with(".")).unwrap_or(false)
    } else {
        false
    }
}

/// Returns true if and only if this entry is considered to be hidden.
///
/// This only returns true if the base name of the path starts with a `.`.
#[cfg(not(any(unix, windows)))]
pub(crate) fn is_hidden(dent: &DirEntry) -> bool {
    if let Some(name) = file_name(dent.path()) {
        name.to_str().map(|s| s.starts_with(".")).unwrap_or(false)
    } else {
        false
    }
}

/// Determine if the file is an online-only file, i.e., one whose contents a
/// cloud provider would have to download before they could be read.
///
/// This is the kernel's own `SF_DATALESS` mark, which is set for File Provider
/// items whichever provider backs them (iCloud Drive, Google Drive, OneDrive,
/// Dropbox, third-party mounts under `~/Library/CloudStorage`).
///
/// It replaces an earlier `st_blocks == 0` heuristic, which asked whether the
/// file allocates any blocks — a different question. Every empty file answers
/// zero, as do provider-generated stubs (a Google Drive shortcut file was
/// observed at 112 bytes across zero blocks with no dataless mark). Callers skip
/// reading an ignore file when this returns true, so each such misread silently
/// discards whatever rules that file held.
#[cfg(target_os = "macos")]
pub(crate) fn is_online_only_path<P: AsRef<Path>>(path: P) -> bool {
    use std::os::macos::fs::MetadataExt;

    /// `SF_DATALESS` from `sys/stat.h`. Reading it needs no privileges; only
    /// setting an `SF_*` flag does.
    const SF_DATALESS: u32 = 0x4000_0000;

    if let Ok(md) = std::fs::symlink_metadata(path) {
        return md.st_flags() & SF_DATALESS != 0;
    }
    false
}

/// Determine if the file is an online-only file.
///
/// These targets have no cloud-placeholder concept, so no file is online-only
/// and every read is served locally.
#[cfg(all(unix, not(target_os = "macos")))]
pub(crate) fn is_online_only_path<P: AsRef<Path>>(_path: P) -> bool {
    false
}

/// Determine if the file is an online-only file.
#[cfg(windows)]
pub(crate) fn is_online_only_path<P: AsRef<Path>>(path: P) -> bool {
    use std::os::windows::fs::MetadataExt;
    use winapi_util::file;

    if let Ok(md) = std::fs::symlink_metadata(path) {
        if file::is_online_only(md.file_attributes() as u64) {
            return true;
        }
    }
    false
}

/// Determine if the file is an online-only file.
pub(crate) fn is_online_only(dent: &DirEntry) -> bool {
    is_online_only_path(dent.path())
}

/// Strip `prefix` from the `path` and return the remainder.
///
/// If `path` doesn't have a prefix `prefix`, then return `None`.
#[cfg(unix)]
pub(crate) fn strip_prefix<'a, P: AsRef<Path> + ?Sized>(
    prefix: &'a P,
    path: &'a Path,
) -> Option<&'a Path> {
    use std::os::unix::ffi::OsStrExt;

    let prefix = prefix.as_ref().as_os_str().as_bytes();
    let path = path.as_os_str().as_bytes();
    if prefix.len() > path.len() || prefix != &path[0..prefix.len()] {
        None
    } else {
        Some(&Path::new(OsStr::from_bytes(&path[prefix.len()..])))
    }
}

/// Strip `prefix` from the `path` and return the remainder.
///
/// If `path` doesn't have a prefix `prefix`, then return `None`.
#[cfg(not(unix))]
pub(crate) fn strip_prefix<'a, P: AsRef<Path> + ?Sized>(
    prefix: &'a P,
    path: &'a Path,
) -> Option<&'a Path> {
    path.strip_prefix(prefix).ok()
}

/// Returns true if this file path is just a file name. i.e., Its parent is
/// the empty string.
#[cfg(unix)]
pub(crate) fn is_file_name<P: AsRef<Path>>(path: P) -> bool {
    use std::os::unix::ffi::OsStrExt;

    use memchr::memchr;

    let path = path.as_ref().as_os_str().as_bytes();
    memchr(b'/', path).is_none()
}

/// Returns true if this file path is just a file name. i.e., Its parent is
/// the empty string.
#[cfg(not(unix))]
pub(crate) fn is_file_name<P: AsRef<Path>>(path: P) -> bool {
    path.as_ref().parent().map(|p| p.as_os_str().is_empty()).unwrap_or(false)
}

/// The final component of the path, if it is a normal file.
///
/// If the path terminates in ., .., or consists solely of a root of prefix,
/// file_name will return None.
#[cfg(unix)]
pub(crate) fn file_name<'a, P: AsRef<Path> + ?Sized>(
    path: &'a P,
) -> Option<&'a OsStr> {
    use memchr::memrchr;
    use std::os::unix::ffi::OsStrExt;

    let path = path.as_ref().as_os_str().as_bytes();
    if path.is_empty() {
        return None;
    } else if path.len() == 1 && path[0] == b'.' {
        return None;
    } else if path.last() == Some(&b'.') {
        return None;
    } else if path.len() >= 2 && &path[path.len() - 2..] == &b".."[..] {
        return None;
    }
    let last_slash = memrchr(b'/', path).map(|i| i + 1).unwrap_or(0);
    Some(OsStr::from_bytes(&path[last_slash..]))
}

/// The final component of the path, if it is a normal file.
///
/// If the path terminates in ., .., or consists solely of a root of prefix,
/// file_name will return None.
#[cfg(not(unix))]
pub(crate) fn file_name<'a, P: AsRef<Path> + ?Sized>(
    path: &'a P,
) -> Option<&'a OsStr> {
    path.as_ref().file_name()
}

#[cfg(all(test, target_os = "macos"))]
mod tests {
    use super::is_online_only_path;

    fn scratch_dir(name: &str) -> std::path::PathBuf {
        let dir = std::env::temp_dir()
            .join(format!("ignore-online-only-{}-{}", std::process::id(), name));
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    /// A file whose data is on this disk is never online-only, however few blocks
    /// it allocates. An empty file allocates none, which is exactly what the
    /// previous `st_blocks == 0` heuristic mistook for a cloud placeholder.
    #[test]
    fn resident_files_are_not_online_only() {
        let dir = scratch_dir("resident");

        let empty = dir.join("empty");
        std::fs::write(&empty, b"").unwrap();
        assert!(
            !is_online_only_path(&empty),
            "an empty file allocates no blocks but its contents are right here"
        );

        let with_rules = dir.join("gitignore");
        std::fs::write(&with_rules, b"*.log\n").unwrap();
        assert!(!is_online_only_path(&with_rules));

        assert!(
            !is_online_only_path(dir.join("absent")),
            "a path that is not there has nothing to download"
        );

        std::fs::remove_dir_all(&dir).unwrap();
    }

    /// Proves the signal against a genuinely evicted file, which no temporary file
    /// can stand in for: only a cloud provider can create a dataless object. Point
    /// `IGNORE_TEST_DATALESS_FILE` at one (anything under `~/Library/CloudStorage`
    /// or `~/Library/Mobile Documents` shown as not downloaded) to run it. Skipped
    /// when unset so the suite stays hermetic.
    #[test]
    fn an_evicted_file_is_online_only() {
        let path = match std::env::var("IGNORE_TEST_DATALESS_FILE") {
            Ok(path) => path,
            Err(_) => return,
        };
        assert!(
            is_online_only_path(&path),
            "{} should be recognised as online-only",
            path
        );
    }
}

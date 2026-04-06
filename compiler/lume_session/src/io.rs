use std::path::{Path, PathBuf};

use indexmap::IndexMap;

pub trait FileLoader: Clone + Send + Sync {
    /// Determines whether a file exists at the given path.
    fn exists(&self, path: &Path) -> bool;

    /// Reads the contents of a file at the given path into memory.
    fn read(&self, path: &Path) -> std::io::Result<String>;

    /// Reads the contents of a directory at the given path into memory.
    fn read_dir(&self, path: &Path) -> std::io::Result<Vec<PathBuf>>;
}

#[derive(Default, Clone)]
pub struct FileSystemLoader;

impl FileLoader for FileSystemLoader {
    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }

    fn read(&self, path: &Path) -> std::io::Result<String> {
        std::fs::read_to_string(path)
    }

    fn read_dir(&self, path: &Path) -> std::io::Result<Vec<PathBuf>> {
        let mut entries = Vec::new();

        if !path.is_dir() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::IsADirectory,
                format!("cannot read directory: {}", path.display()),
            ));
        }

        for entry in std::fs::read_dir(path)? {
            let entry = entry?;
            let meta = entry.metadata()?;

            if meta.is_dir() && !meta.is_symlink() {
                let mut subdir = self.read_dir(&entry.path())?;
                entries.append(&mut subdir);
            }

            if meta.is_file() {
                entries.push(entry.path());
            }
        }

        Ok(entries)
    }
}

#[derive(Clone)]
pub struct VirtualFileSystem<IO> {
    root: PathBuf,
    files: IndexMap<PathBuf, String>,

    proxy: IO,
}

impl<IO> VirtualFileSystem<IO> {
    pub fn new<P: AsRef<Path>>(root: P, proxy: IO) -> Self {
        Self {
            root: root.as_ref().to_path_buf(),
            files: IndexMap::new(),
            proxy,
        }
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn write_mapped<P: AsRef<Path>>(&mut self, path: P, content: String) {
        let path = self.root.join(path.as_ref());

        self.files.insert(path, content);
    }

    pub fn delete_mapped<P: AsRef<Path>>(&mut self, path: P) {
        let path = self.root.join(path.as_ref());

        self.files.swap_remove(&path);
    }
}

impl<IO: FileLoader> FileLoader for VirtualFileSystem<IO> {
    fn exists(&self, path: &Path) -> bool {
        let exists = self.files.contains_key(&self.root.join(path));

        if !exists {
            return self.proxy.exists(path);
        }

        exists
    }

    fn read(&self, path: &Path) -> std::io::Result<String> {
        let result = self.files.get(&self.root.join(path)).cloned().ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("file not found: {}", path.display()),
            )
        });

        if result.is_err() {
            return self.proxy.read(path);
        }

        result
    }

    fn read_dir(&self, path: &Path) -> std::io::Result<Vec<PathBuf>> {
        let entries = self
            .files
            .keys()
            .filter(|file| file.starts_with(path))
            .cloned()
            .collect::<Vec<_>>();

        if entries.is_empty() {
            return self.proxy.read_dir(path);
        }

        Ok(entries)
    }
}

use crate::proto::Directory;

pub trait Blob: std::io::BufRead /* + std::io::Seek */ {}

pub trait StoreClient {
    fn open_blob(&self, digest: Vec<u8>) -> std::io::Result<Box<dyn Blob>>;

    // TODO: stat_blob, put_blob?
    fn get_directory(&self, digest: Vec<u8>) -> std::io::Result<Option<Directory>>;

    // TODO: put_directory
}

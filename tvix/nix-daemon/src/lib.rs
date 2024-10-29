// use std::{error::Error, future, sync::Arc};
// use std::io;


// use nix_compat::{nix_daemon::{daemon::NixDaemon, io::NixDaemonIO}, path_info::PathInfo, store_path::{Digest, StorePath}};
// use tokio_listener::{Listener, SystemOptions, UserOptions};
// use tvix_castore::{blobservice, directoryservice};
// use tvix_store::pathinfoservice::{self, PathInfoService};

// struct DaemonIO {
//     path_info_service: Arc<dyn PathInfoService>
// }

// impl DaemonIO {
//     fn new(path_info_service: Arc<dyn PathInfoService>) -> Self {
//         Self {path_info_service : path_info_service}
//     }
// }

// impl NixDaemonIO for DaemonIO {
//     async fn is_valid_path(&self, path: &StorePath<String>) -> io::Result<bool> {
//         Ok(self.path_info_service.get(*path.digest()).await?.is_some())
//     }
    
//     async fn query_path_info(&self, path: &StorePath<String>) -> io::Result<Option<PathInfo>> {
//         Ok(self.path_info_service.get(*path.digest()).await?)
        
//     }
    
//     async fn query_referrers(&self, path: &StorePath<String>) -> io::Result<Vec<StorePath<String>>> {
//         // Oops! PathInfo only knows its references, not referrers.
//         // iiuc it will be O(SizeOfStore) to query this, which is prohibitively expensive!
//         Ok(self.query_path_info(path).await?.map_or(vec![], |i| vec![]))
//     }
    
//     async fn query_valid_derivers(&self, path: &StorePath<String>) -> io::Result<Vec<StorePath<String>>> {
//         Ok(self.query_path_info(path).await?.map_or(vec![], |i| i.deriver.map_or(vec![], |d| vec![d])))
//     }

//     async fn query_path_from_hash_part(&self, hash: Digest) -> io::Result<Option<StorePath<String>>> {
//         Ok(self.path_info_service.get(hash).await?.map(|p| p.store_path))
//     }
// }

// // fn foo() {
// //     let var_name = directoryservice::from_addr("memory://").await?;
// //     var_name
// // }

// // #[tokio::main]
// // async fn main() -> Result<(), Box<dyn Error>> {
// //     println!("Hello, world!");

// //     let listen_address = "/tmp/socket.sock".to_string().parse()?;
// //     let mut listener = Listener::bind(
// //         &listen_address,
// //         &SystemOptions::default(),
// //         &UserOptions::default(),
// //     )
// //     .await?;

// //     // let daemon = Arc::new(NixDaemon::new(DaemonIO{}));

// //     // while let Ok((connection, _)) = listener.accept().await {
// //     //     let d= daemon.clone();
// //     //     tokio::spawn(async move {
// //     //         d.handle_client(connection).await;
// //     //     });
// //     // }

// //     // loop {
// //     //     let (socket, _) = listener.accept().await?;
// //     // }

// //     // daemon.handle();
// //     Ok(())
// // }


// #[tokio::test]
// async fn hello() -> Result<(), Box<dyn Error>> {
//     let directory_servie = directoryservice::from_addr("memory:");
//     let blob_service = blobservice::from_addr("memory:");

//     let p = pathinfoservice::from_addr("memory:", None).await;
//     assert!(p.is_ok(), "failed to create service");

//     let d_io = DaemonIO::new(p.unwrap());
//     let path = d_io.query_path_from_hash_part([0; 20]).await?;
//     assert(path.is_some(), "not present");
//     //assert!(d_io.is_valid_path(path).await.unwrap() == true, "Hello");
//     Ok(())
// }
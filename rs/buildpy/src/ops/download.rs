use std::path::Path;
use downloader::Downloader;

pub fn download_file(target: &str) {
    let mut downloader = Downloader::builder()
        .download_folder(std::path::Path::new("/tmp"))
        .parallel_requests(1)
        .build()
        .unwrap();

    let d1 = downloader::Download::new(target);

    let result = downloader.download(&[d1]).unwrap();

    for r in result {
        match r {
            // Err(e) => print!("Error occurred! {}", e.to_string()),
            Err(e) => print!("Error occurred! {}", e),
            Ok(s) => print!("Success: {}", &s),
        };
    }
}

pub fn git_clone(url: &str, branch: &str, to_dir: &str) {
    if let Some(stem) = Path::new(url).file_stem() {
        if let Some(target) = Path::new(to_dir).join(stem).into_os_string().to_str() {
            super::run("git", &[url, "-b", branch, target]);
        }
    }
}
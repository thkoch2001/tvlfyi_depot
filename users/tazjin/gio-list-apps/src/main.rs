use gio::traits::AppInfoExt;
use gio::AppInfo;
use serde_json::json;

fn main() {
    for app in AppInfo::all() {
        if app.should_show() {
            if let Some(cmd) = app.commandline() {
                println!(
                    "{}",
                    json!({
                        "name": app.name().as_str(),
                        "display_name": app.display_name().as_str(),
                        "commandline": cmd,
                    })
                );
            }
        }
    }
}

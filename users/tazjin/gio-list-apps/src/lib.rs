use emacs::{defun, Env, IntoLisp, Result, Value};
use gio::traits::AppInfoExt;
use gio::AppInfo;

emacs::plugin_is_GPL_compatible!();

#[emacs::module(defun_prefix = "taz", mod_in_name = false)]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

/// Returns an alist of the currently available XDG applications (through their
/// `.desktop' shortcuts), and the command line parameters needed to start them.
///
/// Hidden applications or applications without specified command-line
/// parameters are not included.
#[defun]
fn list_xdg_apps(env: &Env) -> Result<Value> {
    let mut visible_apps: Vec<Value> = vec![];

    for app in AppInfo::all().into_iter().filter(AppInfo::should_show) {
        if let Some(cmd) = app
            .commandline()
            .and_then(|p| Some(p.to_str()?.to_string()))
        {
            visible_apps.push(env.cons(app.name().as_str().into_lisp(env)?, cmd.into_lisp(env)?)?);
        }
    }

    env.list(&visible_apps)
}

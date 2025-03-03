use niri_ipc::socket::Socket;
use niri_ipc::{Action, Reply, Request, Response, Window, Workspace};

fn sock() -> Socket {
    Socket::connect().expect("could not connect to Niri socket")
}

fn list_workspaces() -> Vec<Workspace> {
    let (reply, _) = sock()
        .send(Request::Workspaces)
        .expect("failed to send workspace request");

    match reply {
        Reply::Err(err) => panic!("failed to list workspaces: {}", err),
        Reply::Ok(Response::Workspaces(w)) => w,
        Reply::Ok(other) => panic!("unexpected reply from Niri: {:#?}", other),
    }
}

fn list_windows() -> Vec<Window> {
    let (reply, _) = sock()
        .send(Request::Windows)
        .expect("failed to send window request");

    match reply {
        Reply::Err(err) => panic!("failed to list windows: {}", err),
        Reply::Ok(Response::Windows(w)) => w,
        Reply::Ok(other) => panic!("unexpected reply from Niri: {:#?}", other),
    }
}

fn reap_window(window: u64, workspace: u64) {
    let (reply, _) = sock()
        .send(Request::Action(Action::MoveWindowToWorkspace {
            window_id: Some(window),
            reference: niri_ipc::WorkspaceReferenceArg::Id(workspace),
        }))
        .expect("failed to send window move request");

    reply.expect("failed to move window to workspace");
}

fn get_active_workspace(workspaces: &[Workspace]) -> &Workspace {
    workspaces
        .iter()
        .filter(|w| w.is_focused)
        .next()
        .expect("expected an active workspace")
}

fn move_workspace_up() {
    let (result, _) = sock()
        .send(Request::Action(Action::MoveWorkspaceUp {}))
        .expect("failed to send workspace move command");

    result.expect("failed to move workspace up");
}

fn main() {
    let mut workspaces = list_workspaces();
    let mut active_workspace = get_active_workspace(&workspaces);

    // Ensure that the current workspace is the first one, to avoid issues with
    // indices changing during the window moves.
    while active_workspace.idx > 1 {
        move_workspace_up();
        workspaces = list_workspaces();
        active_workspace = get_active_workspace(&workspaces);
    }

    let orphan_workspaces = workspaces
        .iter()
        .filter(|w| w.output == active_workspace.output)
        .filter(|w| w.idx > 1)
        .map(|w| w.id)
        .collect::<Vec<_>>();

    if orphan_workspaces.is_empty() {
        return;
    }

    let reapable = list_windows()
        .into_iter()
        .filter(|w| match w.workspace_id {
            Some(id) => orphan_workspaces.contains(&id),
            None => true,
        })
        .collect::<Vec<_>>();

    for window in reapable.iter().rev() {
        reap_window(window.id, active_workspace.id);
    }
}

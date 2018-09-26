//! TODO: port the door docs

extern crate finito;

use finito::FSM;

type Code = usize;
type Attempts = usize;

#[derive(Debug, PartialEq)]
pub enum DoorState {
    /// This state represents an open door.
    Opened,

    /// This state represents a closed door.
    Closed,

    /// This state represents a locked door on which a given code
    /// is set. It also carries a number of remaining attempts
    /// before the door is permanently disabled.
    Locked { code: Code, attempts: Attempts },

    /// This state represents a disabled door. The police will
    /// need to unlock it manually!
    Disabled,
}

#[derive(Debug)]
pub enum DoorEvent {
    Open,
    Close,
    Lock(Code),
    Unlock(Code),
}

#[derive(Debug, PartialEq)]
pub enum DoorAction {
    NotifyIRC(String),
    CallThePolice,
}

impl FSM for DoorState {
    const FSM_NAME: &'static str = "door";
    type Event = DoorEvent;
    type Action = DoorAction;

    fn handle(self, event: DoorEvent) -> (Self, Vec<DoorAction>) {
        match (self, event) {
            (DoorState::Opened, DoorEvent::Close) => return (DoorState::Closed, vec![]),

            (DoorState::Closed, DoorEvent::Open) => return (DoorState::Opened, vec![]),

            (DoorState::Closed, DoorEvent::Lock(code)) => {
                return (DoorState::Locked { code, attempts: 3 }, vec![])
            }

            (DoorState::Locked { code, attempts }, DoorEvent::Unlock(unlock_code)) => {
                if code == unlock_code {
                    return (DoorState::Closed, vec![]);
                }

                if attempts == 1 {
                    return (DoorState::Disabled, vec![DoorAction::CallThePolice]);
                }

                return (
                    DoorState::Locked {
                        code,
                        attempts: attempts - 1,
                    },
                    vec![DoorAction::NotifyIRC("invalid code entered".into())],
                );
            }

            (current, _) => (current, vec![]),
        }
    }

    fn enter(&self) -> Vec<DoorAction> {
        let msg = match self {
            DoorState::Opened => "door was opened",
            DoorState::Closed => "door was closed",
            DoorState::Locked { .. } => "door was locked",
            DoorState::Disabled => "door was disabled",
        };

        vec![DoorAction::NotifyIRC(msg.into())]
    }

    fn act(action: DoorAction) -> Vec<DoorEvent> {
        match action {
            DoorAction::NotifyIRC(msg) => {
                // TODO: write to file in example
                println!("IRC: {}", msg);
                vec![]
            }

            DoorAction::CallThePolice => {
                // TODO: call the police
                println!("The police was called! For real!");
                vec![]
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use finito::advance;

    fn test_fsm<S: FSM>(initial: S, events: Vec<S::Event>) -> (S, Vec<S::Action>) {
        events.into_iter().fold((initial, vec![]), |(state, mut actions), event| {
            let (new_state, mut new_actions) = advance(state, event);
            actions.append(&mut new_actions);
            (new_state, actions)
        })
    }

    #[test]
    fn test_door() {
        let initial = DoorState::Opened;
        let events = vec![
            DoorEvent::Close,
            DoorEvent::Open,
            DoorEvent::Close,
            DoorEvent::Lock(1234),
            DoorEvent::Unlock(1234),
            DoorEvent::Lock(4567),
            DoorEvent::Unlock(1234),
        ];
        let (final_state, actions) = test_fsm(initial, events);

        assert_eq!(final_state, DoorState::Locked { code: 4567, attempts: 2 });
        assert_eq!(actions, vec![
            DoorAction::NotifyIRC("door was closed".into()),
            DoorAction::NotifyIRC("door was opened".into()),
            DoorAction::NotifyIRC("door was closed".into()),
            DoorAction::NotifyIRC("door was locked".into()),
            DoorAction::NotifyIRC("door was closed".into()),
            DoorAction::NotifyIRC("door was locked".into()),
            DoorAction::NotifyIRC("invalid code entered".into()),
        ]);
    }
}

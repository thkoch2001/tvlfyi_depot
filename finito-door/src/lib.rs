//! Example implementation of a lockable door in Finito
//!
//! # What & why?
//!
//! This module serves as a (hopefully simple) example of how to
//! implement finite-state machines using Finito. Note that the
//! concepts of Finito itself won't be explained in detail here,
//! consult its library documentation for that.
//!
//! Reading through this module should give you a rough idea of how to
//! work with Finito and get you up and running modeling things
//! *quickly*.
//!
//! Note: The generated documentation for this module will display the
//! various components of the door, but it will not inform you about
//! the actual transition logic and all that stuff. Read the source,
//! too!
//!
//! # The Door
//!
//! My favourite example when explaining these state-machines
//! conceptually has been to use a simple, lockable door. Our door has
//! a keypad next to it which can be used to lock the door by entering
//! a code, after which the same code must be entered to unlock it
//! again.
//!
//! The door can only be locked if it is closed. Oh, and it has a few
//! extra features:
//!
//! * whenever the door's state changes, an IRC channel receives a
//!   message about that
//!
//! * the door calls the police if the code is intered incorrectly more
//!   than a specified number of times (mhm, lets say, three)
//!
//! * if the police is called the door can not be interacted with
//!   anymore (and honestly, for the sake of this example, we don't
//!   care how its functionality is restored)
//!
//! ## The Door - Visualized
//!
//! Here's a rough attempt at drawing a state diagram in ASCII. The
//! bracketed words denote states, the arrows denote events:
//!
//! ```text
//!          <--Open---    <--Unlock-- correct code? --Unlock-->
//!      [Opened]    [Closed]            [Locked]            [Disabled]
//!          --Close-->    ----Lock-->
//! ```
//!
//! I'm so sorry for that drawing.
//!
//! ## The Door - Usage example
//!
//! An interaction session with our final door could look like this:
//!
//! ```rust,ignore
//! use finito_postgres::{insert_machine, advance};
//!
//! let door = insert_machine(&conn, &DoorState::Opened)?;
//!
//! advance(&conn, &door, DoorEvent::Close)?;
//! advance(&conn, &door, DoorEvent::Lock(1337))?;
//!
//! format!("Door is now: {}", get_machine(&conn, &door)?);
//! ```
//!
//! Here we have created, closed and then locked a door and inspected
//! its state. We will see that it is locked, has the locking code we
//! gave it and three remaining attempts to open it.
//!
//! Alright, enough foreplay, lets dive in!

#[macro_use] extern crate serde_derive;

extern crate failure;
extern crate finito;

use finito::FSM;

/// Type synonym to represent the code with which the door is locked. This
/// exists only for clarity in the signatures below and please do not email me
/// about the fact that an integer is not actually a good representation of
/// numerical digits. Thanks!
type Code = usize;

/// Type synonym to represent the remaining number of unlock attempts.
type Attempts = usize;

/// This type represents the possible door states and the data that they carry.
/// We can infer this from the "diagram" in the documentation above.
///
/// This type is the one for which `finito::FSM` will be implemented, making it
/// the wooden (?) heart of our door.
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum DoorState {
    /// In `Opened` state, the door is wide open and anyone who fits through can
    /// go through.
    Opened,

    /// In `Closed` state, the door is shut but does not prevent anyone from
    /// opening it.
    Closed,

    /// In `Locked` state, the door is locked and waiting for someone to enter
    /// its locking code on the keypad.
    ///
    /// This state contains the code that the door is locked with, as well as
    /// the remaining number of attempts before the door calls the police and
    /// becomes unusable.
    Locked { code: Code, attempts: Attempts },

    /// This state represents a disabled door after the police has been called.
    /// The police will need to unlock it manually!
    Disabled,
}

/// This type represents the events that can occur in our door, i.e. the input
/// and interactions it receives.
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum DoorEvent {
    /// `Open` means someone is opening the door!
    Open,

    /// `Close` means, you guessed it, the exact opposite.
    Close,

    /// `Lock` means somebody has entered a locking code on the
    /// keypad.
    Lock(Code),

    /// `Unlock` means someone has attempted to unlock the door.
    Unlock(Code),
}

/// This type represents the possible actions, a.k.a. everything our door "does"
/// that does not just impact itself, a.k.a. side-effects.
///
/// **Note**: This type by itself *is not* a collection of side-effects, it
/// merely describes the side-effects we want to occur (which are then
/// interpreted by the machinery later).
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum DoorAction {
    /// `NotifyIRC` is used to display some kind of message on the
    /// aforementioned IRC channel that is, for some reason, very interested in
    /// the state of the door.
    NotifyIRC(String),

    /// `CallThePolice` does what you think it does.
    ///
    /// **Note**: For safety reasons, causing this action is not recommended for
    /// users inside the US!
    CallThePolice,
}

/// This trait implementation turns our 'DoorState' into a type actually
/// representing a finite-state machine. To implement it, we need to do three
/// main things:
///
/// * Define what our associated `Event` and `Action` type should be
///
/// * Define the event-handling and state-entering logic (i.e. the meat of the
/// ... door)
///
/// * Implement the interpretation of our actions, i.e. implement actual
///   side-effects
impl FSM for DoorState {
    const FSM_NAME: &'static str = "door";

    // As you might expect, our `Event` type is 'DoorEvent' and our `Action`
    // type is 'DoorAction'.
    type Event = DoorEvent;
    type Action = DoorAction;

    // For error handling, the door simply uses `failure` which provides a
    // generic, chainable error type. In real-world implementations you may want
    // to use a custom error type or similar.
    type Error = failure::Error;

    // The implementation of `handle` provides us with the actual transition
    // logic of the door.
    //
    // The door is conceptually not that complicated so it is relatively short.
    fn handle(self, event: DoorEvent) -> (Self, Vec<DoorAction>) {
        match (self, event) {
            // An opened door can be closed:
            (DoorState::Opened, DoorEvent::Close) => return (DoorState::Closed, vec![]),

            // A closed door can be opened:
            (DoorState::Closed, DoorEvent::Open) => return (DoorState::Opened, vec![]),

            // A closed door can also be locked, in which case the locking code
            // is stored with the next state and the unlock attempts default to
            // three:
            (DoorState::Closed, DoorEvent::Lock(code)) => {
                return (DoorState::Locked { code, attempts: 3 }, vec![])
            }

            // A locked door receiving an `Unlock`-event can do several
            // different things ...
            (DoorState::Locked { code, attempts }, DoorEvent::Unlock(unlock_code)) => {
                // In the happy case, entry of a correct code leads to the door
                // becoming unlocked (i.e. transitioning back to `Closed`).
                if code == unlock_code {
                    return (DoorState::Closed, vec![]);
                }

                // If the code wasn't correct and the fraudulent unlocker ran
                // out of attempts (i.e. there was only one attempt remaining),
                // it's time for some consequences.
                if attempts == 1 {
                    return (DoorState::Disabled, vec![DoorAction::CallThePolice]);
                }

                // If the code wasn't correct, but there are still some
                // remaining attempts, the user doesn't have to face the police
                // quite yet but IRC gets to laugh about it.
                return (
                    DoorState::Locked {
                        code,
                        attempts: attempts - 1,
                    },
                    vec![DoorAction::NotifyIRC("invalid code entered".into())],
                );
            }

            // This actually already concludes our event-handling logic. Our
            // uncaring door does absolutely nothing if you attempt to do
            // something with it that it doesn't support, so the last handler is
            // a simple fallback.
            //
            // In a real-world state machine, especially one that receives
            // events from external sources, you may want fallback handlers to
            // actually do something. One example could be creating an action
            // that logs information about unexpected events, alerts a
            // monitoring service, or whatever else.
            (current, _) => (current, vec![]),
        }
    }

    // The implementation of `enter` lets door states cause additional actions
    // they are transitioned to. In the door example we use this only to notify
    // IRC about what is going on.
    fn enter(&self) -> Vec<DoorAction> {
        let msg = match self {
            DoorState::Opened => "door was opened",
            DoorState::Closed => "door was closed",
            DoorState::Locked { .. } => "door was locked",
            DoorState::Disabled => "door was disabled",
        };

        vec![DoorAction::NotifyIRC(msg.into())]
    }

    // The implementation of `act` lets us perform actual side-effects.
    //
    // Again, for the sake of educational simplicity, this does not deal with
    // all potential (or in fact any) error cases that can occur during this toy
    // implementation of actions.
    //
    // Additionally the `act` function can return new events. This is useful for
    // a sort of "callback-like" pattern (cause an action to fetch some data,
    // receive it as an event) but is not used in this example.
    fn act(action: DoorAction) -> Result<Vec<DoorEvent>, failure::Error> {
        match action {
            DoorAction::NotifyIRC(msg) => {
                use std::fs::OpenOptions;
                use std::io::Write;

                let mut file = OpenOptions::new()
                    .append(true)
                    .create(true)
                    .open("/tmp/door-irc.log")?;

                write!(file, "<doorbot> {}\n", msg)?;
                Ok(vec![])
            }

            DoorAction::CallThePolice => {
                // TODO: call the police
                println!("The police was called! For real!");
                Ok(vec![])
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

//! # What & why?
//!
//! Most processes that occur in software applications can be modeled
//! as finite-state machines (FSMs), however the actual states, the
//! transitions between them and the model's interaction with the
//! external world is often implicit.
//!
//! Making the states of a process explicit using a simple language
//! that works for both software developers and other people who may
//! have opinions on processes makes it easier to synchronise thoughts,
//! extend software and keep a good level of control over what is going
//! on.
//!
//! This library aims to provide functionality for implementing
//! finite-state machines in a way that balances expressivity and
//! safety.
//!
//! Finito does not aim to prevent every possible incorrect
//! transition, but aims for somewhere "safe-enough" (please don't
//! lynch me) that is still easily understood.
//!
//! # Conceptual overview
//!
//! The core idea behind Finito can be expressed in a single line and
//! will potentially look familiar if you have used Erlang in a
//! previous life. The syntax used here is the type-signature notation
//! of Haskell.
//!
//! ```text
//! advance :: state -> event -> (state, [action])
//! ```
//!
//! In short, every FSM is made up of three distinct types:
//!
//!   * a state type representing all possible states of the machine
//!
//!   * an event type representing all possible events in the machine
//!
//!   * an action type representing a description of all possible
//!     side-effects of the machine
//!
//! Using the definition above we can now say that a transition in a
//! state-machine, involving these three types, takes an initial state
//! and an event to apply it to and returns a new state and a list of
//! actions to execute.
//!
//! With this definition most processes can already be modeled quite
//! well. Two additional functions are required to make it all work:
//!
//! ```text
//! -- | The ability to cause additional side-effects after entering
//! -- a new state.
//! > enter :: state -> [action]
//! ```
//!
//! as well as
//!
//! ```text
//! -- | An interpreter for side-effects
//! act :: action -> m [event]
//! ```
//!
//! **Note**: This library is based on an original Haskell library. In
//! Haskell, side-effects can be controlled via the type system which
//! is impossible in Rust.
//!
//! Some parts of Finito make assumptions about the programmer not
//! making certain kinds of mistakes, which are pointed out in the
//! documentation. Unfortunately those assumptions are not
//! automatically verifiable in Rust.
//!
//! == Example
//!
//! Please consult `finito-door` for an example representing a simple,
//! lockable door as a finite-state machine. This gives an overview
//! over Finito's primary features.
//!
//! If you happen to be the kind of person who likes to learn about
//! libraries by reading code, you should familiarise yourself with the
//! door as it shows up as the example in other finito-related
//! libraries, too.
//!
//! # Persistence, side-effects and mud
//!
//! These three things are inescapable in the fateful realm of
//! computers, but Finito separates them out into separate libraries
//! that you can drag in as you need them.
//!
//! Currently, those libraries include:
//!
//!   * @finito@: Core components and classes of Finito
//!
//!   * @finito-in-mem@: In-memory implementation of state machines
//!     that do not need to live longer than an application using
//!     standard library concurrency primitives.
//!
//!   * @finito-postgres@: Postgres-backed, persistent implementation
//!     of state machines that, well, do need to live longer. Uses
//!     Postgres for concurrency synchronisation, so keep that in mind.
//!
//! Which should cover most use-cases. Okay, enough prose, lets dive
//! in.
//!
//! # Does Finito make you want to scream?
//!
//! Please reach out! I want to know why!

use std::mem;

/// Primary trait that needs to be implemented for every state type
/// representing the states of an FSM.
///
/// This trait is used to implement transition logic and to "tie the
/// room together", with the room being our triplet of types.
pub trait FSM where Self: Sized {
    /// A human-readable string uniquely describing what this FSM
    /// models. This is used in log messages, database tables and
    /// various other things throughout Finito.
    const FSM_NAME: &'static str;

    /// The associated event type of an FSM represents all possible
    /// events that can occur in the state-machine.
    type Event;

    /// The associated action type of an FSM represents all possible
    /// actions that can occur in the state-machine.
    type Action;

    /// `handle` deals with any incoming events to cause state
    /// transitions and emit actions. This function is the core logic
    /// of any state machine.
    ///
    /// Implementations of this function **must not** cause any
    /// side-effects to avoid breaking the guarantees of Finitos
    /// conceptual model.
    fn handle(self, event: Self::Event) -> (Self, Vec<Self::Action>);

    /// `enter` is called when a new state is entered, allowing a
    /// state to produce additional side-effects.
    ///
    /// This is useful for side-effects that event handlers do not
    /// need to know about and for resting assured that a certain
    /// action has been caused when a state is entered.
    ///
    /// FSM state types are expected to be enum (i.e. sum) types. A
    /// state is considered "new" and enter calls are run if is of a
    /// different enum variant.
    fn enter(&self) -> Vec<Self::Action>;

    /// `act` interprets and executes FSM actions. This is the only
    /// part of an FSM in which side-effects are allowed.
    fn act(Self::Action) -> Vec<Self::Event>;
}

/// This function is the primary function used to advance a state
/// machine. It takes care of both running the event handler as well
/// as possible state-enter calls and returning the result.
///
/// Users of Finito should basically always use this function when
/// advancing state-machines manually, and never call FSM-trait
/// methods directly.
pub fn advance<S: FSM>(state: S, event: S::Event) -> (S, Vec<S::Action>) {
    // Determine the enum variant of the initial state (used to
    // trigger enter calls).
    let old_discriminant = mem::discriminant(&state);

    let (new_state, mut actions) = state.handle(event);

    // Compare the enum variant of the resulting state to the old one
    // and run `enter` if they differ.
    let new_discriminant = mem::discriminant(&new_state);
    let mut enter_actions = if old_discriminant != new_discriminant {
        new_state.enter()
    } else {
        vec![]
    };

    actions.append(&mut enter_actions);

    (new_state, actions)
}

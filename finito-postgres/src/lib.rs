//! This module implements ... TODO when I can write again.

#[macro_use] extern crate postgres;
#[macro_use] extern crate postgres_derive;

extern crate chrono;
extern crate finito;
extern crate serde;
extern crate serde_json;
extern crate uuid;

mod error;
pub use error::{Result, Error};

use chrono::prelude::{DateTime, Utc};
use finito::FSM;
use postgres::GenericConnection;
use serde::Serialize;
use serde::de::DeserializeOwned;
use serde_json::Value;
use std::fmt;
use std::marker::PhantomData;
use uuid::Uuid;

/// This struct represents rows in the database table in which
/// machines (i.e. the current state of a Finito state machine) are
/// persisted.
#[derive(Debug, ToSql, FromSql)]
struct MachineT {
    /// ID of the persisted state machine.
    id: Uuid,

    /// Time at which the FSM was first created.
    created: DateTime<Utc>,

    /// Name of the type of FSM represented by this state.
    fsm: String,

    /// Current state of the FSM (TODO: Can the serialised FSM type be
    /// used?)
    state: Value,
}

/// This struct represents rows in the database table in which events
/// are persisted.
#[derive(Debug, ToSql, FromSql)]
struct EventT {
    /// ID of the persisted event.
    id: Uuid,

    /// Timestamp at which the event was stored.
    created: DateTime<Utc>,

    /// Name of the type of FSM that this state belongs to.
    fsm: String,

    /// ID of the state machine belonging to this event.
    fsm_id: Uuid,

    /// Serialised content of the event.
    event: Value,
}

/// This enum represents the possible statuses an action can be in.
#[derive(Debug, ToSql, FromSql)]
enum ActionStatus {
    /// The action was requested but has not run yet.
    Pending,

    /// The action completed successfully.
    Completed,

    /// The action failed to run. Information about the error will
    /// have been persisted in Postgres.
    Failed,
}

/// This struct represents rows in the database table in which actions
/// are persisted.
#[derive(Debug, ToSql, FromSql)]
struct ActionT {
    /// ID of the persisted event.
    id: Uuid,

    /// Timestamp at which the event was stored.
    created: DateTime<Utc>,

    /// Name of the type of FSM that this state belongs to.
    fsm: String,

    /// ID of the state machine belonging to this event.
    fsm_id: Uuid,

    /// ID of the event that resulted in this action.
    event_id: Uuid,

    /// Serialised content of the action.
    action: Value,

    /// Current status of the action.
    status: ActionStatus,

    /// Serialised error representation, if an error occured during
    /// processing. TODO: Use some actual error type. Maybe failure
    /// has serialisation support?
    error: Option<String>,
}

// The following functions implement the public interface of
// `finito-postgres`.

/// This type is used as a type-safe wrapper around the ID of a state
/// machine. It carries information about the FSM type and is intended
/// to add a layer of checking to prevent IDs from being mixed up.
#[derive(Clone)]
pub struct MachineId<S: FSM> {
    uuid: Uuid,
    phantom: PhantomData<S>,
}

/// Custom debug implementation to format machine IDs using the name
/// of the FSM and their UUID.
impl <S: FSM> fmt::Debug for MachineId<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", S::FSM_NAME, self.uuid.hyphenated())
    }
}

impl <S: FSM> MachineId<S> {
    /// Convert a UUID into a strongly typed machine ID.
    pub fn from_uuid(uuid: Uuid) -> Self {
        MachineId {
            uuid,
            phantom: PhantomData,
        }
    }

    /// Return the UUID contained in a machine ID.
    pub fn to_uuid(&self) -> Uuid {
        self.uuid
    }
}

/// Insert a single state-machine into the database and return its
/// newly allocated, random UUID.
pub fn insert_machine<C, S>(conn: &C, initial: S) -> Result<MachineId<S>> where
    C: GenericConnection,
    S: FSM + Serialize {
    let query = r#"
      INSERT INTO machines (id, created, fsm, state)
      VALUES ($1, NOW(), $2, $3)
    "#;

    let id = Uuid::new_v4();
    let fsm = S::FSM_NAME.to_string();
    let state = serde_json::to_value(initial).expect("TODO");

    conn.execute(query, &[&id, &fsm, &state]).expect("TODO");

    return Ok(MachineId::from_uuid(id));
}

/// Insert a single event into the database and return its UUID.
fn insert_event<C, S>(conn: &C,
                      fsm_id: &MachineId<S>,
                      event: &S::Event) -> Result<Uuid>
where
    C: GenericConnection,
    S: FSM,
    S::Event: Serialize {
    let query = r#"
      INSERT INTO events (id, created, fsm, fsm_id, event)
      VALUES ($1, NOW(), $2, $3, $4)
    "#;

    let id = Uuid::new_v4();
    let fsm = S::FSM_NAME.to_string();
    let event_value = serde_json::to_value(event).expect("TODO");

    conn.execute(query, &[&id, &fsm, &fsm_id.to_uuid(), &event_value]).expect("TODO");
    return Ok(id)
}

/// Insert a single action into the database and return its UUID.
fn insert_action<C, S>(conn: &C,
                       fsm_id: &MachineId<S>,
                       event_id: Uuid,
                       action: &S::Action) -> Result<Uuid> where
    C: GenericConnection,
    S: FSM,
    S::Action: Serialize {
    let query = r#"
      INSERT INTO actions (id, created, fsm, fsm_id, event_id, action, status)
      VALUES ($1, NOW(), $2, $3, $4, $5, $6)
    "#;

    let id = Uuid::new_v4();
    let fsm = S::FSM_NAME.to_string();
    let action_value = serde_json::to_value(action).expect("TODO");

    conn.execute(query, &[&id, &fsm, &fsm_id.to_uuid(), &event_id,
                          &action_value, &ActionStatus::Pending]).expect("TODO");
    return Ok(id)
}

/// Update the state of a specified machine.
fn update_state<C, S>(conn: &C,
                      fsm_id: &MachineId<S>,
                      state: &S) -> Result<()> where
    C: GenericConnection,
    S: FSM + Serialize {
    let query = r#"
      UPDATE machines SET state = $1 WHERE id = $2
    "#;

    let state_value = serde_json::to_value(state).expect("TODO");
    let res_count = conn.execute(query, &[&state_value, &fsm_id.to_uuid()])
        .expect("TODO");

    if res_count != 1 {
        // TODO: not found error!
        unimplemented!()
    } else {
        Ok(())
    }
}

/// Retrieve the current state of a state machine from the database,
/// optionally locking the machine state for the duration of some
/// enclosing transaction.
pub fn get_machine<C, S>(conn: &C,
                         id: &MachineId<S>,
                         for_update: bool) -> Result<S> where
    C: GenericConnection,
    S: FSM + DeserializeOwned {
    let query = r#"
      SELECT (id, created, fsm, state) FROM machines WHERE id = $1
    "#;

    // If the machine is being fetched in the context of a
    // transaction, with the intention to update it, the relevant
    // clause needs to be appended:
    let query = match for_update {
        false => query.to_string(),
        true  => format!("{} FOR UPDATE", query),
    };

    let rows = conn.query(&query, &[&id.to_uuid()]).expect("TODO");
    let mut machines = rows.into_iter().map(|row| MachineT {
        id: row.get(0),
        created: row.get(1),
        fsm: row.get(2),
        state: row.get(3),
    });

    if let Some(machine) = machines.next() {
        Ok(serde_json::from_value(machine.state).expect("TODO"))
    } else {
        // TODO: return appropriate not found error
        Err(Error::SomeError)
    }
}

/// Advance a persisted state machine by applying an event, and
/// storing the event as well as all resulting actions.
///
/// This function holds a database-lock on the state's row while
/// advancing the machine.
///
/// **Note**: This function returns the new state of the machine
/// immediately after applying the event, however this does not
/// necessarily equate to the state of the machine after all related
/// processing is finished as running actions may result in additional
/// transitions.
pub fn advance<C, S>(conn: &C,
                     id: &MachineId<S>,
                     event: S::Event) -> Result<S> where
    C: GenericConnection,
    S: FSM + Serialize + DeserializeOwned,
    S::Event: Serialize,
    S::Action: Serialize {
    let tx = conn.transaction().expect("TODO");
    let state = get_machine(&tx, id, true).expect("TODO");

    // Advancing the FSM consumes the event, so it is persisted first:
    let event_id = insert_event(&tx, id, &event).expect("TODO");

    // Core advancing logic is run:
    let (new_state, actions) = finito::advance(state, event);

    // Resulting actions are persisted (TODO: and interpreted)
    for action in actions {
        insert_action(&tx, id, event_id, &action).expect("TODO");
    }

    // And finally the state is updated:
    update_state(&tx, id, &new_state).expect("TODO");
    Ok(new_state)
}

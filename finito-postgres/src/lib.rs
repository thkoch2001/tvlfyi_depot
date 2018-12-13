//! PostgreSQL-backed persistence for Finito state machines
//!
//! This module implements ... TODO when I can write again.
//!
//! TODO: events & actions should have `SERIAL` keys

#[macro_use] extern crate postgres;
#[macro_use] extern crate postgres_derive;

extern crate chrono;
extern crate finito;
extern crate serde;
extern crate serde_json;
extern crate uuid;

#[cfg(test)] mod tests;
#[cfg(test)] extern crate finito_door;

mod error;
pub use error::{Result, Error, ErrorKind};

use error::ResultExt;
use chrono::prelude::{DateTime, Utc};
use finito::{FSM, FSMBackend};
use postgres::{Connection, GenericConnection};
use postgres::transaction::Transaction;
use serde::Serialize;
use serde::de::DeserializeOwned;
use serde_json::Value;
use std::marker::PhantomData;
use uuid::Uuid;

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
#[derive(Debug, PartialEq, ToSql, FromSql)]
#[postgres(name = "actionstatus")]
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
    #[postgres(name = "content")] // renamed because 'action' is a keyword in PG
    action: Value,

    /// Current status of the action.
    status: ActionStatus,

    /// Detailed (i.e. Debug-trait formatted) error message, if an
    /// error occured during action processing.
    error: Option<String>,
}

// The following functions implement the public interface of
// `finito-postgres`.

/// TODO: Write docs for this type, brain does not want to do it right
/// now.
pub struct FinitoPostgres<S> {
    state: S,
    // TODO: Use connection pool?
    conn: Connection,
}

impl <State: 'static> FSMBackend<State> for FinitoPostgres<State> {
    type Key = Uuid;
    type Error = Error;

    fn insert_machine<S: FSM + Serialize>(&self, initial: S) -> Result<Uuid> {
        let query = r#"
          INSERT INTO machines (id, fsm, state)
          VALUES ($1, $2, $3)
        "#;

        let id = Uuid::new_v4();
        let fsm = S::FSM_NAME.to_string();
        let state = serde_json::to_value(initial).context("failed to serialise FSM")?;

        self.conn.execute(query, &[&id, &fsm, &state]).context("failed to insert FSM")?;

        return Ok(id);

    }

    fn get_machine<S: FSM + DeserializeOwned>(&self, key: Uuid) -> Result<S> {
        get_machine_internal(&self.conn, key, false)
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
    fn advance<'a, S>(&'a self, key: Uuid, event: S::Event) -> Result<S>
    where S: FSM + Serialize + DeserializeOwned,
          S::State: From<&'a State>,
          S::Event: Serialize + DeserializeOwned,
          S::Action: Serialize + DeserializeOwned {
        let tx = self.conn.transaction().context("could not begin transaction")?;
        let state = get_machine_internal(&tx, key, true)?;

        // Advancing the FSM consumes the event, so it is persisted first:
        let event_id = insert_event::<_, S>(&tx, key, &event)?;

        // Core advancing logic is run:
        let (new_state, actions) = finito::advance(state, event);

        // Resulting actions are persisted (TODO: and interpreted)
        let mut action_ids = vec![];
        for action in actions {
            let action_id = insert_action::<_, S>(&tx, key, event_id, &action)?;
            action_ids.push(action_id);
        }

        // And finally the state is updated:
        update_state(&tx, key, &new_state)?;
        tx.commit().context("could not commit transaction")?;

        self.run_actions::<S>(key, action_ids);

        Ok(new_state)
    }
}

impl <State: 'static> FinitoPostgres<State> {
    /// Execute several actions at the same time, each in a separate
    /// thread. Note that actions returning further events, causing
    /// further transitions, returning further actions and so on will
    /// potentially cause multiple threads to get created.
    fn run_actions<'a, S>(&'a self, fsm_id: Uuid, action_ids: Vec<Uuid>) where
        S: FSM + Serialize + DeserializeOwned,
        S::Event: Serialize + DeserializeOwned,
        S::Action: Serialize + DeserializeOwned,
        S::State: From<&'a State> {
        let state: S::State = (&self.state).into();

        for action_id in action_ids {
            let tx = self.conn.transaction().expect("TODO");

            // TODO: Determine which concurrency setup we actually want.
            if let Ok(events) = run_action(tx, action_id, &state, PhantomData::<S>) {
                for event in events {
                    self.advance::<S>(fsm_id, event).expect("TODO");
                }
            }
        }
    }
}



/// Insert a single state-machine into the database and return its
/// newly allocated, random UUID.
pub fn insert_machine<C, S>(conn: &C, initial: S) -> Result<Uuid> where
    C: GenericConnection,
    S: FSM + Serialize {
    let query = r#"
      INSERT INTO machines (id, fsm, state)
      VALUES ($1, $2, $3)
    "#;

    let id = Uuid::new_v4();
    let fsm = S::FSM_NAME.to_string();
    let state = serde_json::to_value(initial).context("failed to serialize FSM")?;

    conn.execute(query, &[&id, &fsm, &state])?;

    return Ok(id);
}

/// Insert a single event into the database and return its UUID.
fn insert_event<C, S>(conn: &C,
                      fsm_id: Uuid,
                      event: &S::Event) -> Result<Uuid>
where
    C: GenericConnection,
    S: FSM,
    S::Event: Serialize {
    let query = r#"
      INSERT INTO events (id, fsm, fsm_id, event)
      VALUES ($1, $2, $3, $4)
    "#;

    let id = Uuid::new_v4();
    let fsm = S::FSM_NAME.to_string();
    let event_value = serde_json::to_value(event)
        .context("failed to serialize event")?;

    conn.execute(query, &[&id, &fsm, &fsm_id, &event_value])?;
    return Ok(id)
}

/// Insert a single action into the database and return its UUID.
fn insert_action<C, S>(conn: &C,
                       fsm_id: Uuid,
                       event_id: Uuid,
                       action: &S::Action) -> Result<Uuid> where
    C: GenericConnection,
    S: FSM,
    S::Action: Serialize {
    let query = r#"
      INSERT INTO actions (id, fsm, fsm_id, event_id, content, status)
      VALUES ($1, $2, $3, $4, $5, $6)
    "#;

    let id = Uuid::new_v4();
    let fsm = S::FSM_NAME.to_string();
    let action_value = serde_json::to_value(action)
        .context("failed to serialize action")?;

    conn.execute(
        query,
        &[&id, &fsm, &fsm_id, &event_id, &action_value, &ActionStatus::Pending]
    )?;

    return Ok(id)
}

/// Update the state of a specified machine.
fn update_state<C, S>(conn: &C,
                      fsm_id: Uuid,
                      state: &S) -> Result<()> where
    C: GenericConnection,
    S: FSM + Serialize {
    let query = r#"
      UPDATE machines SET state = $1 WHERE id = $2
    "#;

    let state_value = serde_json::to_value(state).context("failed to serialize FSM")?;
    let res_count = conn.execute(query, &[&state_value, &fsm_id])?;

    if res_count != 1 {
        Err(ErrorKind::FSMNotFound(fsm_id).into())
    } else {
        Ok(())
    }
}

/// Conditionally alter SQL statement to append locking clause inside
/// of a transaction.
fn alter_for_update(alter: bool, query: &str) -> String {
    match alter {
        false => query.to_string(),
        true  => format!("{} FOR UPDATE", query),
    }
}

/// Retrieve the current state of a state machine from the database,
/// optionally locking the machine state for the duration of some
/// enclosing transaction.
fn get_machine_internal<C, S>(conn: &C,
                              id: Uuid,
                              for_update: bool) -> Result<S> where
    C: GenericConnection,
    S: FSM + DeserializeOwned {
    let query = alter_for_update(for_update, r#"
      SELECT state FROM machines WHERE id = $1
    "#);

    let rows = conn.query(&query, &[&id]).context("failed to retrieve FSM")?;

    if let Some(row) = rows.into_iter().next() {
        Ok(serde_json::from_value(row.get(0)).context("failed to deserialize FSM")?)
    } else {
        Err(ErrorKind::FSMNotFound(id).into())
    }
}

/// Retrieve an action from the database, optionally locking it for
/// the duration of some enclosing transaction.
fn get_action<C, S>(conn: &C, id: Uuid) -> Result<(ActionStatus, S::Action)> where
    C: GenericConnection,
    S: FSM,
    S::Action: DeserializeOwned {
    let query = alter_for_update(true, r#"
      SELECT status, content FROM actions
      WHERE id = $1 AND fsm = $2
    "#);

    let rows = conn.query(&query, &[&id, &S::FSM_NAME])?;

    if let Some(row) = rows.into_iter().next() {
        let action = serde_json::from_value(row.get(1))
            .context("failed to deserialize FSM action")?;
        Ok((row.get(0), action))
    } else {
        Err(ErrorKind::ActionNotFound(id).into())
    }
}

/// Update the status of an action after an attempt to run it.
fn update_action_status<C, S>(conn: &C,
                              id: Uuid,
                              status: ActionStatus,
                              error: Option<String>,
                              _fsm: PhantomData<S>) -> Result<()> where
    C: GenericConnection,
    S: FSM {
    let query = r#"
      UPDATE actions SET status = $1, error = $2
      WHERE id = $3 AND fsm = $4
    "#;

    let result = conn.execute(&query, &[&status, &error, &id, &S::FSM_NAME])?;

    if result != 1 {
        Err(ErrorKind::ActionNotFound(id).into())
    } else {
        Ok(())
    }
}

/// Execute a single action in case it is pending or retryable. Holds
/// a lock on the action's database row while performing the action
/// and writes back the status afterwards.
///
/// Should the execution of an action fail cleanly (i.e. without a
/// panic), the error will be persisted. Should it fail by panicking
/// (which developers should never do explicitly in action
/// interpreters) its status will not be changed.
fn run_action<S>(tx: Transaction, id: Uuid, state: &S::State, _fsm: PhantomData<S>)
                 -> Result<Vec<S::Event>> where
    S: FSM,
    S::Action: DeserializeOwned {
    let (status, action) = get_action::<Transaction, S>(&tx, id)?;

    let result = match status {
        ActionStatus::Pending => {
            match S::act(action, state) {
                // If the action succeeded, update its status to
                // completed and return the created events.
                Ok(events) => {
                    update_action_status(
                        &tx, id, ActionStatus::Completed, None, PhantomData::<S>
                    )?;
                    events
                },

                // If the action failed, persist the debug message and
                // return nothing.
                Err(err) => {
                    let msg = Some(format!("{:?}", err));
                    update_action_status(
                        &tx, id, ActionStatus::Failed, msg, PhantomData::<S>
                    )?;
                    vec![]
                },
            }
        },

        _ => {
            // TODO: Currently only pending actions are run because
            // retryable actions are not yet implemented.
            vec![]
        },
    };

    tx.commit().context("failed to commit transaction")?;
    Ok(result)
}

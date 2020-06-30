use super::*;

use finito_door::*;
use postgres::{Connection, TlsMode};

// TODO: read config from environment
fn open_test_connection() -> Connection {
    Connection::connect("postgres://finito:finito@localhost/finito", TlsMode::None)
        .expect("Failed to connect to test database")
}

#[test]
fn test_insert_machine() {
    let conn = open_test_connection();
    let initial = DoorState::Opened;
    let door = insert_machine(&conn, initial).expect("Failed to insert door");
    let result = get_machine(&conn, &door, false).expect("Failed to fetch door");

    assert_eq!(result, DoorState::Opened, "Inserted door state should match");
}

#[test]
fn test_advance() {
    let conn = open_test_connection();

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

    let door = insert_machine(&conn, initial).expect("Failed to insert door");

    for event in events {
        advance(&conn, &door, event).expect("Failed to advance door FSM");
    }

    let result = get_machine(&conn, &door, false).expect("Failed to fetch door");
    let expected = DoorState::Locked { code: 4567, attempts: 2 };

    assert_eq!(result, expected, "Advanced door state should match");
}
